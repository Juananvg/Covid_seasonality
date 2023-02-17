##########################
#### Spain Code
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
## Period P1
####################

#Load data and model functions
load("Data/P1_Spain.RData")
source("Code/GAM_Function.R")


#########################
# Apply GAM models
#########################

#Relation between transmission and temperature
res_P1_Temp_Spain <- GamModelCrossbasics(Re, Temperature, StringencyIndex, 
    Retail, Work, Residential, Vac, variants)


#Relation between transmission and relative humidity
res_P1_RH_Spain <- GamModelCrossbasics_RH(Re, RH, StringencyIndex, Retail, Work, 
    Residential, Vac, variants)

#Save models results P1 adn data
setwd("Data")
save(res_P1_Temp_Spain, res_P1_RH_Spain,
    file = "GAM_Spain_P1.RData")
setwd("..")

#Save tables models results P1
setwd("Tables")
write.csv(round(res_P1_Temp_Spain$Gam,3), file = "Results_P1_Temp_Spain.csv")
write.csv(round(res_P1_RH_Spain$Gam,3), file = "Results_P1_RH_Spain.csv")
setwd("..")
rm(list = ls())

#############################
### Final Period Analysis
#############################
#Load data and model functions
load("Data/P2_Spain.RData")
source("Code/GAM_Function.R")


#########################
# Apply GAM models
#########################

#Relation between transmission and temperature
res_P2_Temp_Spain <- GamModelCrossbasics(Re, Temperature, StringencyIndex, 
    Retail, Work, Residential, Vac, variants)


#Relation between transmission and relative humidity
res_P2_RH_Spain <- GamModelCrossbasics_RH(Re, RH, StringencyIndex, Retail, Work, 
    Residential, Vac, variants)

#Save models results P2 and data
setwd("Data")
save(res_P2_Temp_Spain, res_P2_RH_Spain,
    file = "GAM_Spain_P2.RData")
setwd("..")

#Save tables models results P2
setwd("Tables")
write.csv(round(res_P2_Temp_Spain$Gam,3), file = "Results_P2_Temp_Spain.csv")
write.csv(round(res_P2_RH_Spain$Gam,3), file = "Results_P2_RH_Spain.csv")
setwd("..")
rm(list = ls())


