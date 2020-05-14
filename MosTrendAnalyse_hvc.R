#library(RODBC)
library(ggplot2)
#library(nlme)
#library(mgcv)
#library(lme4)
library(MASS)
#library(splines)
#library(stats)

library(readxl)
library(rprojroot)
library(tidyverse)

Basistabel2 <- read_excel(
  path = find_root_file("data/qryBasistabel.xlsx",
                        criterion = is_git_root)
  )

# source('C:/Users/wouter_vanlanduyt/Google Drive/Scriptjes_R/connect_to_access.R')
# 
# db_file_name <- "C:/Users/wouter_vanlanduyt/Google Drive/mossen en lichenen/RodeLijstMossen/MosTrendAnalyse/MosTrendAnalyse.accdb"
# connection <- connect_to_access_rodbc(db_file_name)
# 
# sqlCode <-"SELECT qryBasistabel.*  from qryBasistabel;"
# Basistabel <- sqlQuery(channel = connection, sqlCode)
# 
# head(Basistabel)
# str(Basistabel)
# 
# Basistabel2 <- Basistabel[Basistabel$TaxonGroep != "hornworts", ]
# str(Basistabel2)

Basistabel2$EllenbergN <- factor(Basistabel2$EllenbergN)
Basistabel2$EllenbergL <- factor(Basistabel2$EllenbergL)
Basistabel2$EllenbergR <- factor(Basistabel2$EllenbergR)
Basistabel2$EllenbergF <- factor(Basistabel2$EllenbergF)

# Telfer methode op uurhok??
# hoe is totaal aantal hokken bepaald?
# hoe gevallen proportie 0 en 1 aangepakt?
# waar is de analyse waarmee CIuurhok (= residuals van de Telfer analyse?) bepaald werd?



ggplot(Basistabel2, aes(x=AantalKmGO_1980_1999, y= AantalKmGO_2000_2019)) + 
  geom_point() + 
  geom_smooth(method=lm)

ggplot(Basistabel2, aes(x=AantalUurhokGO_1980_1999, y= AantalUurhokGO_2000_2019)) + 
  geom_point() + 
  geom_smooth(method=lm)

ggplot(Basistabel2, aes(x=CIKmhok, y =  CIUurhok)) + 
  geom_point () + 
  geom_smooth (method=lm)

ggplot(Basistabel2, aes(x=TaxonGroep, y =  CIUurhok)) + 
  geom_boxplot ()

ggplot(Basistabel2, aes(x=EllenbergN, y = CIUurhok)) + 
  geom_boxplot () + 
  facet_wrap(~TaxonGroep)

ggplot(Basistabel2, aes(x=EllenbergL, y =  CIUurhok)) + 
  geom_boxplot() + 
  facet_wrap(~TaxonGroep)

ggplot(Basistabel2, aes(x=EllenbergF, y =  CIUurhok)) + 
  geom_boxplot() + 
  facet_wrap(~TaxonGroep)

ggplot(Basistabel2, aes(x=EllenbergT, y =  CIUurhok)) + 
  geom_boxplot() + 
  facet_wrap(~TaxonGroep)

ggplot(Basistabel2, aes(x=Substraat,y=  CIUurhok)) + 
  geom_boxplot () + 
  facet_wrap(~TaxonGroep) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0))

ggplot(Basistabel2, aes(x=Biotoop,y=  CIUurhok)) + 
  geom_boxplot () + 
  facet_wrap(~TaxonGroep) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0))

ModelCIUurhok <- lm(CIUurhok ~  
                      TaxonGroep 
                    + Substraat 
                    + Biotoop 
                    + EllenbergN 
                    + EllenbergL 
                    + EllenbergR
                    + EllenbergF 
                    + EllenbergT
                    ,
                    data = Basistabel2 )

n_parameters <- Basistabel2 %>%
  select(TaxonGroep, Substraat, Biotoop, starts_with("Ellen")) %>%
  summarise_all(n_distinct) %>%
  mutate(total =  rowSums(.))

nrow(Basistabel2) / n_parameters$total # veel te weinig => overfitting
# oplossing: fit Ellenbergwaarden als continue variabele (indifferent wordt dan NA)

Basistabel2 <- Basistabel2 %>%
  mutate_if(is.factor, as.character) %>%
  mutate_at(.vars = vars(starts_with("Ellen")), 
            .funs = parse_number)
# de "indifferenten" :
problems(x = Basistabel2$EllenbergF) # te weinig om iets zinnig over te zeggen
problems(x = Basistabel2$EllenbergN) # geen problemen
problems(x = Basistabel2$EllenbergR) # te weinig
problems(x = Basistabel2$EllenbergT) # 32 gevallen 
problems(x = Basistabel2$EllenbergL) # 10 gevallen

#remove missing values

Analyseset <- Basistabel2 %>%
  select(CIUurhok,
         TaxonGroep, 
         Substraat,
         Biotoop, 
         starts_with("Ellenb")) %>%
  filter(complete.cases(.))


table(Analyseset$TaxonGroep)
# verwijder de hornworths
Analyseset <- Analyseset %>%
  filter(TaxonGroep != "hornworts")

biotopen <- Analyseset %>% count(Biotoop)
substraten <- Analyseset %>% count(Substraat)

# remove all biotopes/substrates that have few species
# alternatively consider fitting these as random effects
few <- 5
biotopen <- biotopen %>%
  filter(n > few)
substraten <- substraten %>%
  filter(n > few)

Analyseset <- Analyseset %>%
  semi_join(biotopen) %>%
  semi_join(substraten)


Analyseset %>%
  count(Substraat, Biotoop) %>%
  arrange(n) %>%
  View()

# collineariteit
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
Analyseset %>%
  select(starts_with("Ellenb")) %>%
  pairs(upper.panel = panel.cor)
# possibly between R and N

#run the model again

ModelCIUurhok <- lm(CIUurhok ~  
                      TaxonGroep 
                    + Substraat 
                    + Biotoop 
                    + EllenbergN 
                    + EllenbergL 
                    + EllenbergR
                    + EllenbergF 
                    + EllenbergT
                    ,
                    data = Analyseset )



anova(ModelCIUurhok, test  = "F")

plot(ModelCIUurhok)

stepAIC(ModelCIUurhok)

ModelCIUurhok2 <- stepAIC(ModelCIUurhok)

summary(ModelCIUurhok2)

# interaction with TaxonGroep
TGxS <- Analyseset %>%
  count(TaxonGroep, Substraat) %>%
  arrange(n) %>%
  filter(n > few)

TGxB <- Analyseset %>%
  count(TaxonGroep, Biotoop) %>%
  arrange(n) %>%
  filter(n > few)


Analyseset2 <- Analyseset %>%
  semi_join(TGxS) %>%
  semi_join(TGxB)



ModelCIUurhokI <- lm(CIUurhok ~  TaxonGroep * (
  Substraat 
  + Biotoop 
  + EllenbergN 
  + EllenbergL 
  + EllenbergR 
  + EllenbergF 
  + EllenbergT
),
data = Analyseset )

summary(ModelCIUurhokI)
anova(ModelCIUurhokI, test  = "F")
ModelCIUurhokI2 <- stepAIC(ModelCIUurhokI)
summary(ModelCIUurhokI2)
anova(ModelCIUurhokI2)
Tuk <- TukeyHSD(aov(ModelCIUurhokI), 
                "Substraat", ordered = TRUE)
Tuk
plot(Tuk)


Analyseset3 <- Analyseset[Analyseset$TaxonGroep != "liverworts", ]
ModelCIUurhokMosses <- lm(CIUurhok ~  Substraat + Biotoop + EllenbergN + EllenbergL + 
                     EllenbergR + EllenbergF + EllenbergT,data = Analyseset3)
summary(ModelCIUurhokMosses)
anova(ModelCIUurhokMosses, test  = "F")
plot(ModelCIUurhokMosses)


# random effects model
library(lme4)
library(ggeffects)
m_full <- lmer(CIUurhok ~
                 (EllenbergF
               + EllenbergR
               + EllenbergL
               + EllenbergN
               + EllenbergT)
               * TaxonGroep
               + (1|Substraat)
               + (1|Biotoop)
               ,
               data = Analyseset)


summary(m_full)
predictions <- ggpredict(m_full, terms = "EllenbergF")











#verder uit te werken
sqlCode2 <-"SELECT qryCCA.*  from qryCCA;"
SubstraatCI <- sqlQuery(channel = connection, sqlCode2)

head(SubstraatCI)
str(SubstraatCI)

ggplot (SubstraatCI, aes(x=Substraat,y=  CIKmhok)) + geom_boxplot ()

library(vegan)

Substraat # species (rows) by substrate ; cells = 0 1 2 3 

modca <- cca(SubstraatCI ~ 1)
scores(modca, ) #extract row scores
biplot(modca)
plot(modca)







