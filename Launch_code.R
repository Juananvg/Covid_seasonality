##################
#Lauch all code
##################

dir.create("Figures")
dir.create("Supplementary_Figures")
dir.create("Tables")

#Spain
system("Rscript Code/Full_Period_code_Spain.R")
system("Rscript Code/Spain_models.R")
system("Rscript Code/Plots_Spain.R")
#Europe
system("Rscript Code/Full_Period_code_Europe.R")
system("Rscript Code/Europe_models.R")
system("Rscript Code/Plots_Europe.R")



