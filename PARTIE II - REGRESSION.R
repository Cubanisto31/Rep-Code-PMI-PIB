#Code de la partie regression pour l'article PMI-PIB#

library(ggthemes)
library(ggplot2)
library(readr)
library(dplyr)
library(kableExtra)
library(stargazer)
library(readxl)
library(Synth)
library(base)
library(utilities)


#Importer la base de donnees

db <- read_excel("~/M2 PP/Memoire M2/Memoire algo/Base sans NZ v2.xlsx")

#Mettre la bd au bon format 

db <-as.data.frame(db)


#Faire une regression lineaire afin de verifier la "contribution" des pays du
#pool donateur au pays que l'on veut regarder

verif <- lm(db$paysnum=1,c(9:252) ~ db$paysnum=2,c(9:252) + db$paysnum=3,c(9:252)
            + db$paysnum=4,c(9:252) + db$paysnum=5,c(9:252))

db[]