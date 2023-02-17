##########################
#### Europe Code
##########################

#Neccessary packages
require(dlnm)
require(mgcv)
require(visreg)
require(corrplot)
require(mctest)
require(rlist)
require(ggplot2)
require(RColorBrewer)
require(gridExtra)
require(oddsratio)
require(dplyr)
require(patchwork)
require(hrbrthemes)
require(forestplot)
require(metafor)

####################
## Initial Period
####################

####################
## Period P1
####################

#Load data and model functions
load("Data/P1_Europe.RData")
source("Code/GAM_Function.R")

#########################
# Apply GAM models
#########################

#Relation between transmission and temperature
res_P1_Temp_Europe <- GamModelCrossbasics(Re, Temperature, StringencyIndex, 
    Retail, Work, Residential, Vac, variants)


#Relation between transmission and relative humidity
res_P1_RH_Europe <- GamModelCrossbasics_RH(Re, RH, StringencyIndex, Retail, Work, 
    Residential, Vac, variants)

#Save models results P1 adn data
setwd("Data")
save(res_P1_Temp_Europe, res_P1_RH_Europe,
    file = "GAM_Europe_P1.RData")
setwd("..")

#Save tables models results P1
setwd("Tables")
write.csv(round(res_P1_Temp_Europe$Gam,3), file = "Results_P1_Temp_Europe.csv")
write.csv(round(res_P1_RH_Europe$Gam,3), file = "Results_P1_RH_Europe.csv")
setwd("..")
rm(list = ls())


#############################
### Final Period Analysis
#############################
#Load data and model functions
load("Data/P2_Europe.RData")
source("Code/GAM_Function.R")


#########################
# Apply GAM models
#########################

#Relation between transmission and temperature
res_P2_Temp_Europe <- GamModelCrossbasics(Re, Temperature, StringencyIndex, 
    Retail, Work, Residential, Vac, variants)


#Relation between transmission and relative humidity
res_P2_RH_Europe <- GamModelCrossbasics_RH(Re, RH, StringencyIndex, Retail, Work, 
    Residential, Vac, variants)

#Save models results P2 and data
setwd("Data")
save(res_P2_Temp_Europe, res_P2_RH_Europe,
    file = "GAM_Europe_P2.RData")
setwd("..")

#Save tables models results P2
setwd("Tables")
write.csv(round(res_P2_Temp_Europe$Gam,3), file = "Results_P2_Temp_Europe.csv")
write.csv(round(res_P2_RH_Europe$Gam,3), file = "Results_P2_RH_Europe.csv")
setwd("..")
rm(list = ls())



