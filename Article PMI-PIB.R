#Code pour l'article PMI-PIB#

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

#J'ai decide de tout fusionner dans une page d'excel et de creer une nouvelle 
#variable "periodes" pour pouvoir traiter ces infos.

#Tout ce travail a ete fait prealablement sur excel

#/!\ REMIND : un "pays" Synth a ete creer dans la bd pour faciliter la synthetisation, 
#il sera peut etre amene a disparait si pas utile

db <- read_excel("Base sans NZ v2.xlsx")

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
# (ii) bien avoir en tete l'utilite et le fonctionnement de time.optimize.ssr


# PARTIE 2 #

#Creation du vecteur de ponderations de la contribution des pays donateurs 
#"Solution w"

#Creation du vecteur de ponderations de la contribution des variables Predictors 
#"Solution v" (=1 car seulement "Composite")

synth.fra <- synth(data.prep.obj = fra,
                           method = "BFGS")

#Axe d'amelioration :

#(i) faire tourner avec d'autres variables predictors, pour constater la potentielle
#amelioration de l'estimation (Hyp: elle sera forcemment plus precise car elle est 
#au moins aussi precise que l'estimation qui considere seulement la variable composite)

stargazer(synth.fra$solution.w)
