#################################
## GAM and OR Plots - Europe
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
load("Data/GAM_Europe_Initial.RData")
load("Data/GAM_Europe_Final.RData")

############################
#Prepare Data for plots
############################
r_ini <- res_ini_Europe$Data
r_fin <- res_fin_Europe$Data

#Comunities that are going to be represented
for (i in names(r_ini)){
    r_ini[[i]]$Country = i
    r_fin[[i]]$Country = i
}

r_ini = do.call("rbind",r_ini)
r_fin = do.call("rbind",r_fin)

r_ini$Period <- rep("P1", nrow(r_ini))
r_fin$Period <- rep("P2", nrow(r_fin))

res_total <- rbind(r_ini, r_fin)

#Create and Save the plot
setwd("Paper_images")
#dev.off()
jpeg(filename = "Gam_Temp_Europe.jpeg", 
    height = 800, width = 1000)
vars <- c("P1"="#4292c6", "P2" ="#53b594")
ggplot() +
    theme_bw() + 
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total, aes(x = Temp, y = vd, 
        colour = Period), method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "#b4b1b1") +
    scale_color_manual(name = "Period:", values = vars) +
    theme(legend.position = "bottom", text = element_text(size = 20))+ 
    geom_hline(yintercept = 1, color = "red", linetype = 2) +
    labs(x = "Temperature ºC",y = bquote(R[e]))
dev.off()
setwd("..")

#######################
## OR Plots
#######################

######## Initial Period
rang <- c(7.71, 24.94)
#OR calculation
OR <- matrix(NA, nrow = length(res_ini_Europe$Data), ncol = 3, 
    dimnames = list(names(res_ini_Europe$Data), c("mean", "lower", "upper")))
for (i in names(res_ini_Europe$Data)){
    res <- or_gam(data = res_ini_Europe$Data[[i]], 
        model = res_ini_Europe$Models[[i]], pred = "Temp", values = c(rang[[1]],rang[[2]]))
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

#OR <- rbind(c(NA, NA, NA), OR)
#OR <- rbind(c(NA, NA, NA), OR)
#OR <- rbind(OR, c(NA, NA, NA))

OR <- rbind(OR, c(exp(effectOR$beta), exp(effectOR$ci.lb), exp(effectOR$ci.ub)))
rownames(OR) <-  NULL
OR <- data.frame(OR)
OR <- round(OR,3)

################
# Improve OR
################
resum <- OR[nrow(OR),]
OR <- OR[-nrow(OR),]
OR$Region <- names(res_fin_Europe$Data)
OR$OR <- as.character(OR$mean)



graf1 <- OR %>% 
    forestplot(labeltext = c(Region,OR),
        clip = c(0.5, 2),
        xlog = TRUE,
        vertices = TRUE,
        title = "Period without vaccination (P1)",
        boxsize = 0.3,
        txt_gp = fpTxtGp(cex = 1.2)) %>%
    fp_set_style(box = "#6baed6",
        line = "#6baed6",
        summary = "#4292c6") %>%
    fp_add_header(Region = c("", "Region"),
        OR = c("", "OR")) %>%
    fp_append_row(mean  = resum[[1]],
        lower = resum[[2]],
        upper = resum[[3]],
        Region = "Summary",
        OR = as.character(resum[[1]]),
        is.summary = TRUE) %>%
    fp_set_zebra_style("#EFEFEF")

# OR plot
#tabletext <- cbind(c("Region", names(res_ini_Europe$Data), "Summary"), 
#    c("OR", round(OR[-1,1],3)))
#rownames(tabletext) <- NULL
#
#graf1 <- forestplot(x = OR, labeltext = tabletext, 
#    is.summary = c(rep(TRUE, 1), rep(FALSE, 4), TRUE),
#    clip = c(0.5, 2), 
#    xlog = TRUE,
#    lty.zero  =  4,
#    col = fpColors(box = "royalblue",
#        line = "royalblue",
#        summary = "#be961e"),
#    title = "Period without inmunity: 2020/06/01 - 2020/12/31")

########### Final Period
#OR calculation
OR <- matrix(NA, nrow = length(res_ini_Europe$Data), ncol = 3, 
    dimnames = list(names(res_fin_Europe$Data), c("mean", "lower", "upper")))
for (i in names(res_fin_Europe$Data)){
    res <- or_gam(data = res_fin_Europe$Data[[i]], 
        model = res_fin_Europe$Models[[i]], pred = "Temp", values = c(rang[[1]],rang[[2]]))
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

#OR <- rbind(c(NA, NA, NA), OR)
#OR <- rbind(c(NA, NA, NA), OR)
#OR <- rbind(OR, c(NA, NA, NA))

#OR <- rbind(OR, c(exp(effectOR$beta), exp(effectOR$ci.lb), exp(effectOR$ci.ub)))
#rownames(OR) <-  NULL
#OR <- data.frame(OR)
#OR <- round(OR,3)
#OR <- OR[-c(2,12),]


OR <- rbind(OR, c(exp(effectOR$beta), exp(effectOR$ci.lb), exp(effectOR$ci.ub)))
rownames(OR) <-  NULL
OR <- data.frame(OR)
OR <- round(OR,3)

################
# Improve OR
################
resum <- OR[nrow(OR),]
OR <- OR[-nrow(OR),]
OR$Region <- names(res_fin_Europe$Data)
OR$OR <- as.character(OR$mean)


graf2 <- OR %>% 
    forestplot(labeltext = c(Region,OR),
        clip = c(0.5, 2),
        xlog = TRUE,
        vertices = TRUE,
        title = "Period with vaccination (P2)",
        boxsize = 0.3,
        txt_gp = fpTxtGp(cex = 1.2)) %>%
    fp_set_style(box = "#6baed6",
        line = "#6baed6",
        summary = "#4292c6") %>%
    fp_add_header(Region = c("", "Region"),
        OR = c("", "OR")) %>%
    fp_append_row(mean  = resum[[1]],
        lower = resum[[2]],
        upper = resum[[3]],
        Region = "Summary",
        OR = as.character(resum[[1]]),
        is.summary = TRUE) %>%
    fp_set_zebra_style("#EFEFEF")



#OR plot
#tabletext <- cbind(c("Region", names(res_ini_Europe$Data), "Summary"), 
#    c("OR", round(OR[-1,1],3)))
#rownames(tabletext) <- NULL
#
#graf2 <- forestplot(x = OR, labeltext = tabletext, 
#    is.summary = c(rep(TRUE, 1), rep(FALSE, 4), TRUE),
#    clip = c(0.2, 1.5), 
#    xlog = TRUE,
#    lty.zero  =  4,
#    col = fpColors(box = "royalblue",
#        line = "royalblue",
#        summary = "#be961e"),
#    title = "Period with inmunity: 2021/06/01 - 2021/12/31")

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
jpeg(filename = "Odds_Temp_Europe.jpeg", height = 1500, width = 1500)
graf_both
dev.off()
rm(list = ls())
setwd("..")


#############################
# Relative Humidity
#############################

#Load GAM results
load("Data/GAM_Europe_Initial.RData")
load("Data/GAM_Europe_Final.RData")

############################
#Prepare Data for plots
############################
r_ini <- resRH_ini_Europe$Data
r_fin <- resRH_fin_Europe$Data

#Comunities that are going to be represented
for (i in names(r_ini)){
    r_ini[[i]]$Country = i
    r_fin[[i]]$Country = i
}

r_ini = do.call("rbind",r_ini)
r_fin = do.call("rbind",r_fin)

r_ini$Period <- rep("P1", nrow(r_ini))
r_fin$Period <- rep("P2", nrow(r_fin))

res_total <- rbind(r_ini, r_fin)

#Create and Save the plot
setwd("Supplementary_images")
#dev.off()
jpeg(filename = "Gam_RH_Europe.jpeg", 
    height = 800, width = 1000)
vars <- c("P1"="#4292c6", "P2" ="#53b594")
ggplot() +
    theme_bw() + 
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total, aes(x = RH, y = vd, 
        colour = Period), method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "#b4b1b1") +
    scale_color_manual(name = "Period:", values = vars) +
    theme(legend.position = "bottom", text = element_text(size = 20))+ 
    geom_hline(yintercept = 1, color = "red", linetype = 2) +
    labs(x = "Relative Humidity %",y = bquote(R[e]))
dev.off()
setwd("..")

#######################
## OR Plots
#######################

######## Initial Period
rang <- c(46.6, 86.1)
#OR calculation
OR <- matrix(NA, nrow = length(resRH_ini_Europe$Data), ncol = 3, 
    dimnames = list(names(resRH_ini_Europe$Data), c("mean", "lower", "upper")))
for (i in names(resRH_ini_Europe$Data)){
    res <- or_gam(data = resRH_ini_Europe$Data[[i]], 
        model = resRH_ini_Europe$Models[[i]], pred = "RH", values = c(rang[[1]],rang[[2]]))
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

OR <- rbind(OR, c(exp(effectOR$beta), exp(effectOR$ci.lb), exp(effectOR$ci.ub)))
rownames(OR) <-  NULL
OR <- data.frame(OR)
OR <- round(OR,3)

################
# Improve OR
################
resum <- OR[nrow(OR),]
OR <- OR[-nrow(OR),]
OR$Region <- names(resRH_fin_Europe$Data)
OR$OR <- as.character(OR$mean)



graf1 <- OR %>% 
    forestplot(labeltext = c(Region,OR),
        clip = c(0.5, 2),
        xlog = TRUE,
        vertices = TRUE,
        title = "Period without vaccination (P1)",
        boxsize = 0.3,
        txt_gp = fpTxtGp(cex = 1.2)) %>%
    fp_set_style(box = "#6baed6",
        line = "#6baed6",
        summary = "#4292c6") %>%
    fp_add_header(Region = c("", "Region"),
        OR = c("", "OR")) %>%
    fp_append_row(mean  = resum[[1]],
        lower = resum[[2]],
        upper = resum[[3]],
        Region = "Summary",
        OR = as.character(resum[[1]]),
        is.summary = TRUE) %>%
    fp_set_zebra_style("#EFEFEF")



########### Final Period
#OR calculation
OR <- matrix(NA, nrow = length(resRH_ini_Europe$Data), ncol = 3, 
    dimnames = list(names(resRH_fin_Europe$Data), c("mean", "lower", "upper")))
for (i in names(resRH_fin_Europe$Data)){
    res <- or_gam(data = resRH_fin_Europe$Data[[i]], 
        model = resRH_fin_Europe$Models[[i]], pred = "RH", values = c(rang[[1]],rang[[2]]))
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


OR <- rbind(OR, c(exp(effectOR$beta), exp(effectOR$ci.lb), exp(effectOR$ci.ub)))
rownames(OR) <-  NULL
OR <- data.frame(OR)
OR <- round(OR,3)

################
# Improve OR
################
resum <- OR[nrow(OR),]
OR <- OR[-nrow(OR),]
OR$Region <- names(resRH_fin_Europe$Data)
OR$OR <- as.character(OR$mean)


graf2 <- OR %>% 
    forestplot(labeltext = c(Region,OR),
        clip = c(0.5, 2),
        xlog = TRUE,
        vertices = TRUE,
        title = "Period with vaccination (P2)",
        boxsize = 0.3,
        txt_gp = fpTxtGp(cex = 1.2)) %>%
    fp_set_style(box = "#6baed6",
        line = "#6baed6",
        summary = "#4292c6") %>%
    fp_add_header(Region = c("", "Region"),
        OR = c("", "OR")) %>%
    fp_append_row(mean  = resum[[1]],
        lower = resum[[2]],
        upper = resum[[3]],
        Region = "Summary",
        OR = as.character(resum[[1]]),
        is.summary = TRUE) %>%
    fp_set_zebra_style("#EFEFEF")


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
jpeg(filename = "Odds_RH_Europe.jpeg", height = 1500, width = 1500)
graf_both
dev.off()
rm(list = ls())
setwd("..")

rm(list = ls())
#OR calculation
#OR <- matrix(NA, nrow = 9, ncol = 3, 
#    dimnames = list(names(res_ini_Europe$Data), c("mean", "lower", "upper")))
#for (i in names(res_ini_Europe$Data)){
#    res <- or_gam(data = res_ini_Europe$Data[[i]], 
#        model = res_ini_Europe$Models[[i]], pred = "RH", values = c(60,80))
#    OR[i,1] <- res$oddsratio
#    if(res$`CI_low (2.5%)` < res$`CI_high (97.5%)`){
#        OR[i,2] <- res$`CI_low (2.5%)`
#        OR[i,3] <- res$`CI_high (97.5%)`}
#    else{
#        OR[i,2] <- res$`CI_high (97.5%)`
#        OR[i,3] <- res$`CI_low (2.5%)`}
#}
#yi <- log(OR[,1])
#vi <- (log(OR[,2]) - yi) /1.96
#vi <- vi^2
#effectOR <- rma(yi,vi)
#
#OR <- rbind(c(NA, NA, NA), OR)
#OR <- rbind(c(NA, NA, NA), OR)
#OR <- rbind(OR, c(NA, NA, NA))
#
#OR <- rbind(OR, c(exp(effectOR$beta), exp(effectOR$ci.lb), exp(effectOR$ci.ub)))
#rownames(OR) <-  NULL
#OR <- data.frame(OR)
#OR <- round(OR,3)
#OR <- OR[-c(2,12), ]
#
#
## OR plot
#tabletext <- cbind(c("Region", names(res_ini_Europe$Data), "Summary"), 
#    c("OR", round(OR[-1,1],3)))
#rownames(tabletext) <- NULL
#
#graf1 <- forestplot(x = OR, labeltext = tabletext, 
#    is.summary = c(rep(TRUE, 1), rep(FALSE, 9), TRUE),
#    clip = c(0.5, 2), 
#    xlog = TRUE,
#    lty.zero  =  4,
#    col = fpColors(box = "royalblue",
#        line = "royalblue",
#        summary = "#be961e"),
#    title = "Period without inmunity: 2020/06/01 - 2020/12/31")
#
############ Final Period
##OR calculation
#OR <- matrix(NA, nrow = 9, ncol = 3, 
#    dimnames = list(names(res_fin_Europe$Data), c("mean", "lower", "upper")))
#for (i in names(res_fin_Europe$Data)){
#    res <- or_gam(data = res_fin_Europe$Data[[i]], 
#        model = res_fin_Europe$Models[[i]], pred = "RH", values = c(60,80))
#    OR[i,1] <- res$oddsratio
#    if(res$`CI_low (2.5%)` < res$`CI_high (97.5%)`){
#        OR[i,2] <- res$`CI_low (2.5%)`
#        OR[i,3] <- res$`CI_high (97.5%)`}
#    else{
#        OR[i,2] <- res$`CI_high (97.5%)`
#        OR[i,3] <- res$`CI_low (2.5%)`}
#}
#yi <- log(OR[,1])
#vi <- (log(OR[,2]) - yi) /1.96
#vi <- vi^2
#effectOR <- rma(yi,vi)
#
#OR <- rbind(c(NA, NA, NA), OR)
#OR <- rbind(c(NA, NA, NA), OR)
#OR <- rbind(OR, c(NA, NA, NA))
#
#OR <- rbind(OR, c(exp(effectOR$beta), exp(effectOR$ci.lb), exp(effectOR$ci.ub)))
#rownames(OR) <-  NULL
#OR <- data.frame(OR)
#OR <- round(OR,3)
#OR <- OR[-c(2,12),]
#
##OR plot
#tabletext <- cbind(c("Region", names(res_ini_Europe$Data), "Summary"), 
#    c("OR", round(OR[-1,1],3)))
#rownames(tabletext) <- NULL
#
#graf2 <- forestplot(x = OR, labeltext = tabletext, 
#    is.summary = c(rep(TRUE, 1), rep(FALSE, 9), TRUE),
#    clip = c(0.2, 1.5), 
#    xlog = TRUE,
#    lty.zero  =  4,
#    col = fpColors(box = "royalblue",
#        line = "royalblue",
#        summary = "#be961e"),
#    title = "Period with inmunity: 2021/06/01 - 2021/12/31")
#
##Arrange both Odds plots
#require(ggplotify)
#require(patchwork)
#graf1 <- grid2grob(print(graf1))
#graf2 <- grid2grob(print(graf2))
#graf_both <- wrap_elements(graf1) * wrap_elements(graf2)
#
#
###########################
##Save OR plot
#setwd("Supplementery_images")
#dev.off()
#jpeg(filename = "Odds_RH_Europe.jpeg", height = 1500, width = 1500)
#graf_both
#dev.off()

#setwd("..")