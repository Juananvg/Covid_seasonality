##################
#Lauch all code
##################

#Neccessary packages
#library(dlnm)
#library(mgcv)
#library(visreg)
#library(corrplot)
#library(mctest)
#library(rlist)
#library(ggplot2)
#library(RColorBrewer)
#library(gridExtra)
#library(oddsratio)
#library(dplyr)
#library(patchwork) # To display 2 charts together
#library(ggplotify)
#library(hrbrthemes)
#library(forestplot)
#library(metafor)

#Change period, lag, variables:

# system("Rscript RawData/Process_Spain_Raw_Rollmean.R")

# system("Rscript RawData/Process_Europe_Raw_Rollmean.R")

#system("Rscript RawData/Process_Spain_Raw.R")
system("Rscript Code/Full_Period_code_Spain.R")
system("Rscript Code/Spain_models.R")
system("Rscript Code/Plots_Spain.R")
#ystem("Rscript RawData/Process_Europe_Raw.R")
system("Rscript Code/Full_Period_code_Europe.R")
system("Rscript Code/Europe_models.R")
system("Rscript Code/Plots_Europe.R")



