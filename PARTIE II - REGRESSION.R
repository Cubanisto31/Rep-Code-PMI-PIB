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

################################################################################
#                                                                              #
# L'objectif de cette partie est de tester la contribution des pays du groupe  #
# donateur afin de comparer celle-ci avec la ponderation de ces memes pays     #
# lors de la synthetisation de la serie                                        #
#                                                                              #
################################################################################

#Ce code doit etre lance apres le code "PARTIE I - SYNTH.R"

source("PARTIE I - SYNTH.R")




fra.tempo <- as.vector(fra$Z1)
  #penser a changer le fra tempo au moment de l'automatisation du code 
grp.don <- as.data.frame(fra$Z0)
colnames(grp.don) <- c("All", "Ita", "Esp")

#Faire une regression lineaire afin de verifier la "contribution" des pays du
#pool donateur au pays que l'on veut regarder

reg <- lm(fra.tempo ~ grp.don$All + grp.don$Ita + grp.don$Esp)

summary(reg)

#A significativite egale, tentative de comparaison des contributions des 
#differents pays donateurs 
contribution.totale <- abs(reg$coefficients[2])+abs(reg$coefficients[3])+abs(reg$coefficients[4])

contribution.All <- as.numeric(abs(reg$coefficients[2])*100/contribution.totale)
contribution.Ita <- as.numeric(abs(reg$coefficients[3])*100/contribution.totale)
contribution.Esp <- as.numeric(abs(reg$coefficients[4])*100/contribution.totale)

print(contribution.All)
print(contribution.Ita)
print(contribution.Esp)

ponderation.synth <- list.synth.fra$solution.w

colnames(ponderation.synth) <- "Poids"
rownames(ponderation.synth) <- c("All", "Ita", "Esp")


print(ponderation.synth)
#Benjamin je ne sais pas si c'est tres pertinent de faire le calcul supra pour 
#comparer les effets de chaque X dans la regression mais au moins cela permet de
#se donner un ordre d'idee en comparant avec les ponderations de la synthetisation

#Pour rappel les ponderations de la synthetisation sont les suivantes