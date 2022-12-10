#Code de la partie synthetisation pour l'article PMI-PIB#

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


###############################################################################
#
# L'objectif de cette partie est de montrer comment maitriser la synthetisation
# tout en pointant en CCL les aspects qui peuvent etre ameliores afin de 
# rendre le
#
#
#
#
#
#
#
#
############################################################
############################################################
############################################################

               #PARTIE BASE DE DONNEES#

############################################################
############################################################
############################################################


#Axes d'ameliorations : 
# (i) Ne sachant pas comment importer un excel ayant plusieurs feuilles et bien
# la traiter
#et (ii) ne sachant pas comment faire pour considerer les dates en "dates"

# -> J'ai decide de tout fusionner dans une page d'excel et de creer une nouvelle 
#variable "periodes" pour pouvoir traiter ces infos.

#Tout ce travail a ete fait prealablement sur excel

#/!\ REMIND : un "pays" Synth a ete creer dans la bd pour faciliter la synthetisation, 
#il sera peut etre amene a disparaitre inutile

db <- read_excel("~/M2 PP/Memoire M2/Memoire algo/Base sans NZ v2.xlsx")

#Apres verification a la louche il semblerait que toutes les variables soient
#numeriques

#La base de donnees est au format "tibble" il faut donc la transformer en 
#data frame pour pouvoir la synthetiser

db <-as.data.frame(db)



############################################################
############################################################
############################################################

              #PARTIE SYNTHETISATION#

############################################################
############################################################
############################################################

# PARTIE 1 #

#La fonction dataprep() du package Synth permet de creer des sous-bases pour 
#automatiser et faciliter la synthetisation

fra <- dataprep(foo = db,
             predictors = "composite" ,
             predictors.op = "mean" ,
             time.predictors.prior = 9:252,
             
             
             dependent = "composite",
             unit.variable = "paysnum",
             unit.names.variable = "paysnom",
             time.variable = "periodes",
             treatment.identifier = 1,
             controls.identifier = c(2:5),
             time.optimize.ssr = 9:252,
             time.plot = 9:252
)

#Axes d'ameliorations :

# (i) chercher a faire tourner la fonction avec les dates et non des periodes
#(ii) faire tourner avec d'autres variables predictors, pour constater la potentielle
#amelioration de l'estimation (Hyp: elle sera forcemment plus precise car elle est 
#au moins aussi precise que l'estimation qui considere seulement la variable composite)


#Pi : 
#"foo =" indique la base de donnees dans laquelle on travaille
#"predictors =" permet d'indiquer a partir de quel(les) variable(s) on realisera la synth
#"predictors.op=" permet d'indiquer la methode de synthetisation (moyenne par default)
#"time.predictors.op =" permet d'indiquer la periode sur laquelle on synthetisera
#"dependent ="  permet d'indiquer quelle variable sera synthetisee
#"unit.variable=" permet d'indiquer le nom de la variable numérique qui sert a identifier les series
#"unit.names.variable=" permet d'indiquer le nom de la variable alpha qui sert a identifier les series
#"time.variable=" permet d'indiquer le nom de la variable temporelle
#"treatment.identifier=" permet d'indiquer la serie que l'on voudra synthetiser
#"controls.identifier=" permet d'indiquer les series de notre pool donateur
#"time.optimize.ssr=" permet d'indiquer la periode sur laquelle les coeff de ponderations sont calcules (impact sur le n de l des matrices Z0 et Z1)
#"time.plot =" permet de definir la periode sur laquelle les resultats sont indiques

View(fra)

#Affiche les differentes matrices construites en suivant les commandes entrees precedemment

# PARTIE 2 #

#Creation du vecteur de ponderations de la contribution des pays donateurs 
#"Solution w"

#Creation du vecteur de ponderations de la contribution des variables Predictors 
#"Solution v" (=1 car seulement "Composite")

synth.fra <- synth(data.prep.obj = fra,
                           method = "BFGS")

#Pi : Il est possible d'utiliser plusieurs algos d'optimisation c("Nelder-Mead', 'BFGS', 'CG', 'L-BFGS-B', 'nlm', 'nlminb', 'spg', and 'ucminf")

#Le BFGS est le quasi-Newton algorithm

#Axe d'amelioration : Verifier le fonctionnement des differents algos, les tester si cela parait pertinent


gaps.synth.fra <- fra$Y1plot - (fra$Y0plot %*% synth.fra$solution.w)

#Permet d'observer les differences de composite sur la periode pre-intervention
#entre notre France synthetique et notre vraie France

gaps.plot(synth.res = synth.fra,
           dataprep.res = fra,
           Ylab = c("Ecart"),
           Xlab = c("Periodes"),
             Main = c("Ecart : Traite - Synthetique"))

#Gaps.plot est une fonction du package synth qui permet de tracer. Petit bémol 
#Cela peut impliquer un changement de forme entre les graphs de ggplot()

#Peut etre plus clair de faire tous nos graphiques avec la meme fonction (ex : ggplot())

# PARTIE 3 #

mse<-((fra$Y1plot - (fra$Y0plot %*% synth.fra$solution.w))^2)

rmse<-0

for (i in 1:244) {rmse<-rmse+mse[i]}
rmse<-rmse/244
rmse<-sqrt(rmse)

rmse

#Cette partie permet de comparer la precision de notre estimation 
#Elle permet de comparer la surface entre entre la courbe de la serie synth 
#et celle de la vraie serie

#Il serait interessant de trouver un lien entre le rmse lors de la creation 
#de notre serie synthetique et le rmse de estimation du PIB
###########################################################
###########################################################
###########################################################


                 ### CONCLUSION ###


###########################################################
###########################################################
###########################################################

#La partie ci dessus permet donc de synthetiser des series a partir d'un groupe donateur 

#Afin d'ameliorer la structure de "la machine" on retiendra qu'il :

#  - Faut ameliorer l'importation de la bd
#  - Faut trouver un moyen de raisonner en date plutot qu'en periode
#  - Faut supprimer le pays Synth cree dans la bd
#  - Faut s'accorder sur un moyen pratique de tracer nos series (utiliser la meme fonction)

#Afin d'ameliorer les reglages de "la machine" on retiendra que l'on peut :

#  - Tester en ajoutant des variables predictors 
#  - Tester d'autres algo d'optimisation que le "BFGS"


