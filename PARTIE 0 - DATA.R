#Code pour ouvrir la base de donnees PMI#

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
library(stringr)

###############################################################################
#                                                                             #
# L'objectif de cette partie est d'ouvrir et de mettre en forme la base de    #
# donnees "PMI" qui servira a la suite de notre etudes.                       #
#                                                                             #
###############################################################################

#Creation d'une df pour chaque pays

#Axes d'ameliorations :

# (i) Automatiser l'ouverture de chaque feuille avec une boucle for et un vecteur
# de nom de pays

# (ii) Actualiser la base de donnees d'origine et essayer de trouver des bases pour
# reduire le nombre de N/A

PMI_euro <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
             sheet = 1,
             col_names = c("Date","Composite", "Synthetique industrie",
                           "Production passee", "Emploi_industrie", 
                           "Nouvelles commandes", "Stocks", 
                           "Delais de livraison", "Commandes export", 
                           "Prix output industrie", "Prix input industrie", 
                           "Synthetique services = activites", 
                           "Nouvelles affaires", "Emploi_services", 
                           "Backlogs of work","Activite future", 
                           "Prix output services", "Prix input services"), 
             col_types = c("date","numeric","numeric","numeric","numeric",
                           "numeric","numeric","numeric","numeric","numeric",
                           "numeric","numeric","numeric","numeric","numeric",
                           "numeric","numeric","numeric")
             )

PMI_euro <- PMI_euro[c(6:257),]



PMI_Fra <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                    sheet = 2,
                    col_names = c("Date","Composite", "Synthetique industrie",
                                  "Production passee", "Emploi_industrie", 
                                  "Nouvelles commandes", "Stocks", 
                                  "Delais de livraison", "Commandes export", 
                                  "Prix output industrie", "Prix input industrie", 
                                  "Synthetique services = activites", 
                                  "Nouvelles affaires", "Emploi_services", 
                                  "Backlogs of work","Activite future", 
                                  "Prix output services", "Prix input services"), 
                    col_types = c("date","numeric","numeric","numeric","numeric",
                                  "numeric","numeric","numeric","numeric","numeric",
                                  "numeric","numeric","numeric","numeric","numeric",
                                  "numeric","numeric","numeric")
                    )

PMI_Fra <- PMI_Fra[c(6:257),]

PMI_All <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 3,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_All <- PMI_All[c(6:257),]

PMI_Ita <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 4,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_Ita <- PMI_Ita[c(6:257),]

PMI_Esp <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 5,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_Esp <- PMI_Esp[c(6:257),]

PMI_UK <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 6,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_UK <- PMI_UK[c(6:257),]

PMI_USA <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 7,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_USA <- PMI_USA[c(6:257),]

PMI_Chi <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 8,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_Chi <- PMI_Chi[c(6:257),]

PMI_Ind <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 9,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_Ind <- PMI_Ind[c(6:257),]

PMI_Bra <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 10,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_Bra <- PMI_Bra[c(6:257),]

PMI_Jap <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 11,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_Jap <- PMI_Jap[c(6:257),]

PMI_Rus <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 12,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_Rus <- PMI_Rus[c(6:257),]

PMI_Pol <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 13,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_Pol <- PMI_Pol[c(6:257),]

PMI_Mex <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 14,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_Mex <- PMI_Mex[c(6:257),]

PMI_Can <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 15,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_Can <- PMI_Can[c(6:257),]

PMI_Tur <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 16,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_Tur <- PMI_Tur[c(6:257),]

PMI_Hol <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 17,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_Hol <- PMI_Hol[c(6:257),]

PMI_Aus <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 18,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_Aus <- PMI_Aus[c(6:257),]

PMI_Indo <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 19,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_Indo <- PMI_Indo[c(6:257),]

PMI_NZ <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 20,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_NZ <- PMI_NZ[c(6:257),]

PMI_Egy <- read_excel("C:/Users/paulf/Downloads/Test Git/Data PMI_memoire.xlsx", 
                      sheet = 21,
                      col_names = c("Date","Composite", "Synthetique industrie",
                                    "Production passee", "Emploi_industrie", 
                                    "Nouvelles commandes", "Stocks", 
                                    "Delais de livraison", "Commandes export", 
                                    "Prix output industrie", "Prix input industrie", 
                                    "Synthetique services = activites", 
                                    "Nouvelles affaires", "Emploi_services", 
                                    "Backlogs of work","Activite future", 
                                    "Prix output services", "Prix input services"), 
                      col_types = c("date","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric","numeric","numeric",
                                    "numeric","numeric","numeric")
)

PMI_Egy <- PMI_Egy[c(6:257),]

#############################################################

#Fusion de toutes ces df 

Periodes <- rep(1:252, 21)

Pays_nom <- c(rep("Euro", 252), rep("Fra", 252), rep("All", 252), rep("Ita", 252)
         , rep("Esp", 252), rep("UK", 252), rep("USA", 252), rep("Chi", 252)
         , rep("Ind", 252), rep("Bra", 252), rep("Jap", 252), rep("Rus", 252)
         , rep("Pol", 252), rep("Mex", 252), rep("Can", 252), rep("Tur", 252)
         , rep("Hol", 252), rep("Aus", 252), rep("Indo", 252), rep("NZ", 252)
         , rep("Egy", 252))

Pays_num <- c(rep(1:21, each = 252))

PMI <- rbind(PMI_euro,PMI_Fra,PMI_All,PMI_Ita,PMI_Esp,PMI_UK,PMI_USA,PMI_Chi
             ,PMI_Ind,PMI_Bra,PMI_Jap,PMI_Rus,PMI_Pol,PMI_Mex,PMI_Can,PMI_Tur
             ,PMI_Hol,PMI_Aus,PMI_Indo,PMI_NZ,PMI_Egy)


PMI <- cbind(Periodes, PMI)
PMI <- cbind(Pays_nom, PMI)
PMI <- cbind(Pays_num, PMI)

#############################################################

#Supprimer les bases qui seront inutiles 

rm(PMI_euro)
rm(PMI_Fra)
rm(PMI_All)
rm(PMI_Ita)
rm(PMI_Esp)
rm(PMI_UK)
rm(PMI_USA)
rm(PMI_Chi)
rm(PMI_Ind)
rm(PMI_Bra)
rm(PMI_Jap)
rm(PMI_Rus)
rm(PMI_Pol)
rm(PMI_Mex)
rm(PMI_Can)
rm(PMI_Tur)
rm(PMI_Hol)
rm(PMI_Aus)
rm(PMI_Indo)
rm(PMI_NZ)
rm(PMI_Egy)

rm(Pays_nom)
rm(Pays_num)
rm(Periodes)
