#Code permettant la synthetisation#

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
# L'objectif de cette partie est de montrer comment maitriser la synthetisation#
# tout en pointant en CCL les aspects qui peuvent etre ameliores afin de       #
# rendre le code plus souple.                                                  #
#                                                                              #
################################################################################

#Ce code doit etre lance apres le code "PARTIE 0 - DATA.R"

source("PARTIE 0 - DATA.R")


############################################################
############################################################
############################################################

              #PARTIE SYNTHETISATION#

############################################################
############################################################
############################################################

# PARTIE 0 #
#Initialisation du programme 


# PARTIE 1 #

#La fonction dataprep() du package Synth permet de creer des sous-bases pour 
#automatiser et faciliter la synthetisation

fra <- dataprep(foo = PMI,
             predictors = "Composite" ,
             predictors.op = "mean" ,
             time.predictors.prior = 9:252,
             dependent = "Composite",
             unit.variable = "Pays_num",
             unit.names.variable = "Pays_nom",
             time.variable = "Periodes",
             treatment.identifier = 2,
             controls.identifier = c(3:5),
             time.optimize.ssr = 50:252,
             time.plot = 9:252
)

#Axes d'ameliorations :

#(i) faire tourner avec d'autres variables predictors, pour constater la potentielle
#amelioration de l'estimation (Hyp: elle sera forcemment plus precise car elle est 
#au moins aussi precise que l'estimation qui considere seulement la variable composite)

#(ii) completer la df pour faire tourner avec plus de pays donateurs sur des
#periodes plus grandes


# PARTIE 2 #

#Creation du vecteur de ponderations de la contribution des pays donateurs 
#"Solution w"

#Creation du vecteur de ponderations de la contribution des variables Predictors 
#"Solution v" (=1 car seulement "Composite")

list.synth.fra <- synth(data.prep.obj = fra,
                           method = "BFGS")

#Pi : Il est possible d'utiliser plusieurs algos d'optimisation c("Nelder-Mead', 'BFGS', 'CG', 'L-BFGS-B', 'nlm', 'nlminb', 'spg', and 'ucminf")

#Le BFGS est le quasi-Newton algorithm

#Axes d'amelioration : 

#(i) Verifier le fonctionnement des differents algos, les tester si cela parait pertinent

serie.synth.fra <- fra$Y0plot %*% list.synth.fra$solution.w

gaps.list.synth.fra <- fra$Y1plot - serie.synth.fra

# PARTIE 3 #

#Tracer les graphiques pour visualiser les ecarts entre les series reelles 
# et les series synthetiques


#Avec fonction du package(Synth)
#Ecart
gaps.plot(synth.res = list.synth.fra,
           dataprep.res = fra,
           Ylab = c("Ecart"),
           Xlab = c("Periodes"),
             Main = c("Ecart : Traite - Synthetique"))

#Avec fonction ggplot 
#Ecart
date.plot <- PMI$Date[c(9:252)]
gaps.list.synth.fra <- gaps.list.synth.fra

ecart.fra.plot <- data.frame(date.plot, as.numeric(gaps.list.synth.fra))
colnames(ecart.fra.plot) <- c("Dates", "Ecarts")

ggplot(ecart.fra.plot, aes(Dates)) +
  geom_line(aes(y = Ecarts)) +
  geom_hline(yintercept=(0),linetype="dotted")+
  xlab("Dates")+
  ylab("Ecarts")
theme_classic()

#Tracer la serie reelle et la serie synth
date.plot <- PMI$Date[c(9:252)]
serie.reelle.fra <- fra$Y1plot
serie.synth.fra <- serie.synth.fra 

fra.plot <- data.frame(date.plot, as.numeric(serie.reelle.fra), as.numeric(serie.synth.fra))
colnames(fra.plot) <- c("Dates", "Synth", "Reelle") 


ggplot(fra.plot, aes(Dates)) +
  geom_line(aes(y = Synth), color = "blue") +
  geom_line(aes(y = Reelle), color = "red") +
  geom_hline(yintercept=(50),linetype="dotted")+
  xlab("Dates")+
  ylab("Composite")
  theme_classic()

#Axe d'amelioration : Lorsque je nomme les colonnes par seulement une lettre,
  #j'obtiens un graph sur fond blanc (plus clean pour exporter)
  
  
#Axe d'amelioration : Une fois que le code est termine, changer le code en y 
  # integrant des variables generiques et expliquer en amont les donnees que 
  #l'on doit attribuer aux variables pour effectuer les differents tests
  
# PARTIE 4 #
  
#Calcul de l'erreur quadratique moyenne entre la serie reelle et la serie 
  #synthetisee

mse <- (gaps.list.synth.fra)^2

rmse<-0

for (i in 1:244) {rmse<-rmse+mse[i]}
rmse<-rmse/244
rmse<-sqrt(rmse)

View(rmse)

#L'erreur quadratique moyenne permet de comparer la precision de notre synthetisation 

#Elle permet de comparer la surface entre entre la courbe de la serie synth 
#et celle de la serie reelle

#Axe d'amelioration : Il serait interessant de trouver un lien entre le rmse 
#lors de la creation de notre serie synthetique et le rmse de estimation du PIB


###########################################################
###########################################################
###########################################################


                 ### CONCLUSION ###


###########################################################
###########################################################
###########################################################

#La partie ci dessus permet donc de synthetiser des series a partir d'un groupe donateur 

#Afin d'ameliorer la structure du programme :

#  - Trouver un moyen de faciliter au maximum la realisation de differents
#tests en automatisant l'entree des inputs en amont 
#(ex: pays test, groupe donateur, periode) 
#  - Faire apparaitre facilement la ponderation des differents pays donateurs

#Afin d'ameliorer les reglages du programme :

#  - Tester en ajoutant des variables predictors 
#  - Tester d'autres algo d'optimisation que le "BFGS"

#Ne pas oublier de supprimer tous les objets qui ne servent qu'a la construction
