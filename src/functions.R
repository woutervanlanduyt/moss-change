# code from inbodb::get_florabank_taxon_ifbl_year
# but with extra where clause to exclude Gewestelijke bosinventarisatie
# Tbl.Waarneming.BronCode <> 18
get_florabank_taxon_ifbl_year_2 <- function(connection,
         starting_year = 2010,
         ifbl_resolution = c("1km-by-1km",
                             "4km-by-4km"),
         taxongroup = c("Vaatplanten",
                        "Mossen",
                        "Lichenen (korstmossen)",
                        "Kranswieren"),
         collect = FALSE) {
  require(assertthat)
  require(glue)
  require(dplyr)
  require(rlang)
  
  assert_that(inherits(connection, what = "Microsoft SQL Server"),
              msg = "Not a connection object to database.")
  assert_that(connection@info$dbname == "D0021_00_userFlora")
  
  assert_that(is.numeric(starting_year))
  assert_that(starting_year <= as.numeric(format(Sys.Date(), '%Y')))
  
  ifbl_resolution = match.arg(ifbl_resolution)
  taxongroup = match.arg(taxongroup)
  
  
  
  if (ifbl_resolution == "4km-by-4km") {
    glue_statement <- glue_sql(
      "SELECT DISTINCT
    tblIFBLHok.Code AS hok
    , SUBSTRING(tblIFBLHok.Code, 1, 5) AS ifbl_4by4
    , Year(tblWaarneming.BeginDatum) AS Jaar
    , relTaxonTaxon.TaxonIDParent
    , tblTaxon.Code AS Taxoncode
    FROM
    (((tblMeting INNER JOIN
    (tblIFBLHok INNER JOIN tblWaarneming ON tblIFBLHok.ID = tblWaarneming.IFBLHokID)
    ON tblMeting.WaarnemingID = tblWaarneming.ID)
    INNER JOIN relTaxonTaxon ON tblMeting.TaxonID = relTaxonTaxon.TaxonIDChild)
    INNER JOIN tblTaxon ON relTaxonTaxon.TaxonIDParent = tblTaxon.ID)
    INNER JOIN relTaxonTaxonGroep ON tblTaxon.ID = relTaxonTaxonGroep.TaxonID
    INNER JOIN tblTaxonGroep ON relTaxonTaxonGroep.TaxonGroepID = tblTaxonGroep.ID
    WHERE
    tblIFBLHok.Code LIKE '%-%' AND
    tblTaxon.Code NOT LIKE '%-sp' AND
    Year([tblWaarneming].[BeginDatum]) >={starting_year} AND
    (Year([tblWaarneming].[BeginDatum])=Year([tblWaarneming].[EindDatum])) AND
    (tblTaxonGroep.Naam={taxongroup}) AND
    (tblMeting.MetingStatusCode='GDGA' OR tblMeting.MetingStatusCode='GDGK') AND
    tblWaarneming.BronCode <> '18'
    ORDER BY
    Year(tblWaarneming.BeginDatum) DESC OFFSET 0 ROWS",
      starting_year = starting_year,
      taxongroup = taxongroup,
      .con = connection)
    glue_statement <- iconv(glue_statement, from =  "UTF-8", to = "latin1")
    query_result <- tbl(connection, sql(glue_statement))
    
    
    query_result <- query_result %>%
      group_by(.data$ifbl_4by4, .data$Jaar, .data$TaxonIDParent,
               .data$Taxoncode) %>%
      #paste with collapse does not translate to sql
      #str_flatten() is not available for Microsoft SQL Server
      #sql(STRING_AGG("hok", ",")) also does not work
      #fix this later
      summarize(#ifbl_squares = paste(hok, collapse = '|'),
        ifbl_number_squares = n()) %>%
      ungroup()
    
    if (!isTRUE(collect)) {
      return(query_result)
    } else {
      query_result <- query_result %>%
        collect()
      return(query_result)
    }
  }
  
  glue_statement <- glue_sql(
    "SELECT DISTINCT
    tblIFBLHok.Code AS ifbl_1by1
    , SUBSTRING(tblIFBLHok.Code, 1, 5) AS ifbl_4by4
    , Year(tblWaarneming.BeginDatum) AS Jaar
    , relTaxonTaxon.TaxonIDParent
    , tblTaxon.Code AS Taxoncode
    FROM
    (((tblMeting INNER JOIN
    (tblIFBLHok INNER JOIN tblWaarneming ON tblIFBLHok.ID = tblWaarneming.IFBLHokID)
    ON tblMeting.WaarnemingID = tblWaarneming.ID)
    INNER JOIN relTaxonTaxon ON tblMeting.TaxonID = relTaxonTaxon.TaxonIDChild)
    INNER JOIN tblTaxon ON relTaxonTaxon.TaxonIDParent = tblTaxon.ID)
    INNER JOIN relTaxonTaxonGroep ON tblTaxon.ID = relTaxonTaxonGroep.TaxonID
    INNER JOIN tblTaxonGroep ON relTaxonTaxonGroep.TaxonGroepID = tblTaxonGroep.ID
    WHERE
    tblIFBLHok.Code LIKE '%-%-%' AND
    tblTaxon.Code NOT LIKE '%-sp' AND
    Year([tblWaarneming].[BeginDatum]) >={starting_year} AND
    (Year([tblWaarneming].[BeginDatum])=Year([tblWaarneming].[EindDatum])) AND
    (tblTaxonGroep.Naam={taxongroup}) AND
    (tblMeting.MetingStatusCode='GDGA' OR tblMeting.MetingStatusCode='GDGK') AND
    tblWaarneming.BronCode <> '18'
    ORDER BY
    Year(tblWaarneming.BeginDatum) DESC OFFSET 0 ROWS",
    starting_year = starting_year,
    taxongroup = taxongroup,
    .con = connection)
  glue_statement <- iconv(glue_statement, from =  "UTF-8", to = "latin1")
  query_result <- tbl(connection, sql(glue_statement))
  if (!isTRUE(collect)) {
    return(query_result)
  } else {
    query_result <- query_result %>%
      collect()
    return(query_result)
  }
}