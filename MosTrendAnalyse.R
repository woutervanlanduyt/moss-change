library(RODBC)
library(ggplot2)
library(nlme)
library(mgcv)
library(lme4)
library(MASS)
library(splines)
library(stats)

source('C:/Users/wouter_vanlanduyt/Google Drive/Scriptjes_R/connect_to_access.R')

db_file_name <- "C:/Users/wouter_vanlanduyt/Google Drive/mossen en lichenen/RodeLijstMossen/MosTrendAnalyse/MosTrendAnalyse.accdb"
connection <- connect_to_access_rodbc(db_file_name)

sqlCode <-"SELECT Basistabel.*  from Basistabel;"
Basistabel <- sqlQuery(channel = connection, sqlCode)

head(Basistabel)
str(Basistabel)

#Basistabel2 <- Basistabel[complete.cases(Basistabel), ]
Basistabel2 <- Basistabel2[Basistabel2$TaxonGroep != "hornworts", ]
str(Basistabel2)

Basistabel2$TaxonGroep <- factor(Basistabel2$TaxonGroep)
Basistabel2$EllenbergN <- factor(Basistabel2$EllenbergN)
Basistabel2$EllenbergL <- factor(Basistabel2$EllenbergL)
Basistabel2$EllenbergR <- factor(Basistabel2$EllenbergR)
Basistabel2$EllenbergF <- factor(Basistabel2$EllenbergF)

ggplot (Basistabel2, aes(x=AantalKmGO_1980_1999, y= AantalKmGO_2000_2019)) + geom_point () + geom_smooth (method=lm)
ggplot (Basistabel2, aes(x=AantalUurhokGO_1980_1999, y= AantalUurhokGO_2000_2019)) + geom_point () + geom_smooth (method=lm)
ggplot (Basistabel2, aes(x=CIKmhok,y=  CIUurhok)) + geom_point () + geom_smooth (method=lm)
ggplot (Basistabel2, aes(x=TaxonGroep,y=  CIKmhok)) + geom_boxplot ()
ggplot (Basistabel2, aes(x=EllenbergN,y=  CIKmhok)) + geom_boxplot () + facet_wrap(~TaxonGroep)
ggplot (Basistabel2, aes(x=EllenbergL,y=  CIKmhok)) + geom_boxplot () + facet_wrap(~TaxonGroep)
ggplot (Basistabel2, aes(x=EllenbergF,y=  CIKmhok)) + geom_boxplot () + facet_wrap(~TaxonGroep)
ggplot (Basistabel2, aes(x=EllenbergT,y=  CIKmhok)) + geom_boxplot () + facet_wrap(~TaxonGroep)
ggplot (Basistabel2, aes(x=Substraat,y=  CIKmhok)) + geom_boxplot () + facet_wrap(~TaxonGroep) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0))
ggplot (Basistabel2, aes(x=Biotoop,y=  CIKmhok)) + geom_boxplot () + facet_wrap(~TaxonGroep) + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0))

ModelCIKmhok <- lm(CIKmhok ~  TaxonGroep + Substraat + Biotoop + EllenbergN + EllenbergL + 
                     EllenbergR + EllenbergF + EllenbergT,data = Basistabel2 )
anova(ModelCIKmhok, test  = "F")
plot(ModelCIKmhok)
stepAIC(ModelCIKmhok)
ModelCIKmhok2 <-stepAIC(ModelCIKmhok)
summary(ModelCIKmhok2)

ModelCIKmhokI <- aov(CIKmhok ~  TaxonGroep * (Substraat + Biotoop + EllenbergN + EllenbergL + 
                     EllenbergR + EllenbergF + EllenbergT),data = Basistabel2 )

summary(ModelCIKmhokI)
anova(ModelCIKmhokI, test  = "F")
ModelCIKmhokI2 <- stepAIC(ModelCIKmhokI)
summary(ModelCIKmhokI2)

Tuk <- TukeyHSD(ModelCIKmhokI2, "Substraat", ordered = TRUE)
Tuk
plot(Tuk)
summary(ModelCIKmhok)


Basistabel3 <- Basistabel2[Basistabel2$TaxonGroep != "liverworts", ]
ModelCIKmhokMosses <- lm(CIKmhok ~  Substraat + Biotoop + EllenbergN + EllenbergL + 
                     EllenbergR + EllenbergF + EllenbergT,data = Basistabel3 )
summary(ModelCIKmhokMosses)
anova(ModelCIKmhokMosses, test  = "F")
plot(ModelCIKmhokMosses)


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







