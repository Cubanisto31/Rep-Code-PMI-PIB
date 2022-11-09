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
#"time.optimize.ssr=" permet d'indiquer la periode sur laquelle les coeff de ponderations sont calcules
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





  
