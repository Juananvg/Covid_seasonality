#################################
## GAM and OR Plots - Spain
#################################
#Abbreviation
#Com -> Communities
#Ini -> Intial
#Fin -> Final
#Res/r -> Results

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


#Load GAM results
load("Data/GAM_Spain_Initial.RData")
load("Data/GAM_Spain_Final.RData")

############################
#Prepare Data for plots
############################
r_ini <- res_ini_Spain$Data
r_fin <- res_fin_Spain$Data
names_com <- names(r_ini)
names_com[c(1,7,8,9,10,16,17)] <- c("Andalusia", 
    "Catalonia", "Madrid", "Navarra", "C. Valenciana", 
    "Asturias", "Murcia")
names(r_ini) <- names_com
names(r_fin) <- names_com

#Comunities that are going to be represented
com_ini = r_ini[c(1,6,7,8,10,3,13,17,2)]
com_fin <- r_fin[c(1,6,7,8,10,3,13,17,2)]
for (i in names(com_ini)){
    com_ini[[i]]$Country = i
    com_fin[[i]]$Country = i
}

com_ini = do.call("rbind",com_ini)
com_fin = do.call("rbind",com_fin)

com_ini$Period <- rep("Without Inmunity", nrow(com_ini))
com_fin$Period <- rep("With Inmunity", nrow(com_fin))

res_total <- rbind(com_ini, com_fin)

#Create and Save the plot
setwd("Paper_images")
#dev.off()
jpeg(filename = "Gam_Spain_Communities.jpeg", 
    height = 800, width = 1000)
vars <- c("Without Inmunity"="#4292c6", "With Inmunity" ="#53b594")
ggplot() +
    theme_bw() + 
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total, aes(x = Temp, y = vd, 
        colour = Period), method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "#b4b1b1") +
    scale_color_manual(name = "Period:", values = vars) +
    theme(legend.position = "bottom")+ 
    geom_hline(yintercept = 1, color = "red", linetype = 2) +
    labs(x = "Temperature ºC",y = bquote(R[e]))
dev.off()
setwd("..")

#######################
## OR Plots
#######################

######## Initial Period

#OR calculation
OR <- matrix(NA, nrow = 17, ncol = 3, 
    dimnames = list(names(res_ini_Spain$Data), c("mean", "lower", "upper")))
for (i in names(res_ini_Spain$Data)){
    res <- or_gam(data = res_ini_Spain$Data[[i]], 
        model = res_ini_Spain$Models[[i]], pred = "Temp", values = c(20,25))
    OR[i,1] <- res$oddsratio
    if(res$`CI_low (2.5%)` < res$`CI_high (97.5%)`){
        OR[i,2] <- res$`CI_low (2.5%)`
        OR[i,3] <- res$`CI_high (97.5%)`}
    else{
        OR[i,2] <- res$`CI_high (97.5%)`
        OR[i,3] <- res$`CI_low (2.5%)`}
}
yi <- log(OR[,1])
vi <- (log(OR[,2]) - yi) /1.96
vi <- vi^2
effectOR <- rma(yi,vi)

OR <- rbind(c(NA, NA, NA), OR)
OR <- rbind(c(NA, NA, NA), OR)
OR <- rbind(OR, c(NA, NA, NA))

OR <- rbind(OR, c(exp(effectOR$beta), exp(effectOR$ci.lb), exp(effectOR$ci.ub)))
rownames(OR) <-  NULL
OR <- data.frame(OR)
OR <- round(OR,3)


# OR plot
tabletext <- cbind(c("Region","", names(r_ini), NA, "Summary"), 
    c("OR", round(OR[-1,1],3)))
rownames(tabletext) <- NULL

graf1 <- forestplot(x = OR, labeltext = tabletext, 
    is.summary = c(rep(TRUE, 2), rep(FALSE, 17), TRUE),
    clip = c(0.5, 2), 
    xlog = TRUE,
    lty.zero  =  4,
    col = fpColors(box = "royalblue",
        line = "royalblue",
        summary = "#be961e"),
    title = "Period without inmunity: 2020/06/01 - 2020/12/31")

########### Final Period
#OR calculation
OR <- matrix(NA, nrow = 17, ncol = 3, 
    dimnames = list(names(res_fin_Spain$Data), c("mean", "lower", "upper")))
for (i in names(res_fin_Spain$Data)){
    res <- or_gam(data = res_fin_Spain$Data[[i]], 
        model = res_fin_Spain$Models[[i]], pred = "Temp", values = c(20,25))
    OR[i,1] <- res$oddsratio
    if(res$`CI_low (2.5%)` < res$`CI_high (97.5%)`){
        OR[i,2] <- res$`CI_low (2.5%)`
        OR[i,3] <- res$`CI_high (97.5%)`}
    else{
        OR[i,2] <- res$`CI_high (97.5%)`
        OR[i,3] <- res$`CI_low (2.5%)`}
}
yi <- log(OR[,1])
vi <- (log(OR[,2]) - yi) /1.96
vi <- vi^2
effectOR <- rma(yi,vi)

OR <- rbind(c(NA, NA, NA), OR)
OR <- rbind(c(NA, NA, NA), OR)
OR <- rbind(OR, c(NA, NA, NA))

OR <- rbind(OR, c(exp(effectOR$beta), exp(effectOR$ci.lb), exp(effectOR$ci.ub)))
rownames(OR) <-  NULL
OR <- data.frame(OR)
OR <- round(OR,3)

#OR plot
tabletext <- cbind(c("Region","", names(r_fin), NA, "Summary"), 
    c("OR", round(OR[-1,1],3)))
rownames(tabletext) <- NULL

graf2 <- forestplot(x = OR, labeltext = tabletext, 
    is.summary = c(rep(TRUE, 2), rep(FALSE, 17), TRUE),
    clip = c(0.5, 2), 
    xlog = TRUE,
    lty.zero  =  4,
    col = fpColors(box = "royalblue",
        line = "royalblue",
        summary = "#be961e"),
    title = "Period with inmunity: 2021/06/01 - 2021/12/31")

#Arrange both Odds plots
require(ggplotify)
require(patchwork)
graf1 <- grid2grob(print(graf1))
graf2 <- grid2grob(print(graf2))
graf_both <- wrap_elements(graf1) * wrap_elements(graf2)


##########################
#Save OR plot
setwd("Paper_images")
dev.off()
jpeg(filename = "Odds_Spain.jpeg", height = 1500, width = 1500)
graf_both
dev.off()
rm(list = ls())
setwd("..")



