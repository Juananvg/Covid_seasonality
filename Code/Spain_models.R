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
require(patchwork) # To display 2 charts together
require(hrbrthemes)
require(forestplot)
require(metafor)

####################
## Initial Period
####################

#Load data
#setwd("..") #Subir al directorio anterior
load("Data/InitialPeriod_var_Spain.RData")
#setwd("..")
source("GAM_Function.R")

#########################
# Apply GAM models
#########################
#Relation between transmission and temperature
res_Rt <- GamModelCrossbasics(Rt, Temperature, Measures, Retail, 
    Work, Residential, Vac, variants)
res_Rt_GAM <- round(res_Rt$Gam,3)
res_Rt_COR <- round(res_Rt$Cor,3)
setwd("Data")
res_ini_Spain <- res_Rt
save(res_ini_Spain, file = "GAM_Spain_Initial.RData")
setwd("..")

rm(list = ls())

#############################
### Final Period Analysis
#############################
#Load data
#setwd("..") #Subir al directorio anterior
load("Data/FinalPeriod_var_Spain.RData")
#setwd("..")
source("GAM_Function.R")


#########################
# Apply GAM models
#########################

#Relation between transmission and temperature
res_Rt <- GamModelCrossbasics(Rt, Temperature, Measures, Retail, Work, 
    Residential, Vac, variants)
resultados_Rt_GAM <- round(res_Rt$Gam,3)
resultados_Rt_COR <- round(res_Rt$Cor,3)
setwd("Data")
res_fin_Spain <- res_Rt
save(res_fin_Spain, file = "GAM_Spain_Final.RData")
setwd("..")

rm(list = ls())


