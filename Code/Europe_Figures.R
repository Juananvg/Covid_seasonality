#################################
## GAM and OR Plots - Europe
#################################
#Abbreviation
#Com -> Communities
#Ini -> Intial
#Fin -> P2
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
require(patchwork)
require(hrbrthemes)
require(forestplot)
require(metafor)


#Load GAM results
load("Data/GAM_Europe_P1.RData")
load("Data/GAM_Europe_P2.RData")

############################
#Prepare Data for plots
############################
r_P1 <- res_P1_Temp_Europe$Data
r_P2 <- res_P2_Temp_Europe$Data

for (i in names(r_P1)){
    r_P1[[i]]$Country = i
    r_P2[[i]]$Country = i
}

r_P1 = do.call("rbind",r_P1)
r_P2 = do.call("rbind",r_P2)

r_P1$Period <- rep("P1", nrow(r_P1))
r_P2$Period <- rep("P2", nrow(r_P2))

res_total <- rbind(r_P1, r_P2)

#Create and Save the plot
setwd("Supplementary_Figures")
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

load("Data/GAM_Europe_P1.RData")
load("Data/GAM_Europe_P2.RData")

######## P1 Period
rang <- c(7.71, 24.94)
#OR calculation
OR <- matrix(NA, nrow = length(res_P1_Temp_Europe$Data), ncol = 3, 
    dimnames = list(names(res_P1_Temp_Europe$Data), c("mean", "lower", "upper")))
for (i in names(res_P1_Temp_Europe$Data)){
    res <- or_gam(data = res_P1_Temp_Europe$Data[[i]], 
        model = res_P1_Temp_Europe$Models[[i]], pred = "Temp", 
        values = c(rang[[1]],rang[[2]]))
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
rownames(OR) <- c(names(res_P1_Temp_Europe$Data), "Meta-Analysis")

#Save OR table
setwd("Tables")
write.csv(OR, file = "Odds_P1_Temp_Europe.csv")
setwd("..")


###### OR Figure P1

resum <- OR[nrow(OR),]
OR <- OR[-nrow(OR),]
OR$Region <- names(res_P2_Temp_Europe$Data)
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
        summary = "#4292c6",
        zero = "#9e9ac8") %>%
    fp_add_header(Region = c("", "Region"),
        OR = c("", "OR")) %>%
    fp_append_row(mean  = resum[[1]],
        lower = resum[[2]],
        upper = resum[[3]],
        Region = "Summary",
        OR = as.character(resum[[1]]),
        is.summary = TRUE)


########### P2 Period
#OR calculation
OR <- matrix(NA, nrow = length(res_P1_Temp_Europe$Data), ncol = 3, 
    dimnames = list(names(res_P2_Temp_Europe$Data), c("mean", "lower", "upper")))
for (i in names(res_P2_Temp_Europe$Data)){
    res <- or_gam(data = res_P2_Temp_Europe$Data[[i]], 
        model = res_P2_Temp_Europe$Models[[i]], pred = "Temp", values = c(rang[[1]],rang[[2]]))
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
rownames(OR) <- c(names(res_P2_Temp_Europe$Data), "Meta-Analysis")

setwd("Tables")
write.csv(OR, file = "Odds_P2_Temp_Europe.csv")
setwd("..")

#### OR Figure P2

resum <- OR[nrow(OR),]
OR <- OR[-nrow(OR),]
OR$Region <- names(res_P2_Temp_Europe$Data)
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
        summary = "#4292c6",
        zero = "#9e9ac8") %>%
    fp_add_header(Region = c("", "Region"),
        OR = c("", "OR")) %>%
    fp_append_row(mean  = resum[[1]],
        lower = resum[[2]],
        upper = resum[[3]],
        Region = "Summary",
        OR = as.character(resum[[1]]),
        is.summary = TRUE)



#Arrange both Odds plots
require(ggplotify)
require(patchwork)
graf1 <- grid2grob(print(graf1))
graf2 <- grid2grob(print(graf2))
graf_both <- wrap_elements(graf1) * wrap_elements(graf2)


##########################
#Save OR plot
setwd("Supplementary_Figures")
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
load("Data/GAM_Europe_P1.RData")
load("Data/GAM_Europe_P2.RData")

############################
#Prepare Data for plots
############################
r_P1 <- res_P1_RH_Europe$Data
r_P2 <- res_P2_RH_Europe$Data

for (i in names(r_P1)){
    r_P1[[i]]$Country = i
    r_P2[[i]]$Country = i
}

r_P1 = do.call("rbind",r_P1)
r_P2 = do.call("rbind",r_P2)

r_P1$Period <- rep("P1", nrow(r_P1))
r_P2$Period <- rep("P2", nrow(r_P2))

res_total <- rbind(r_P1, r_P2)

#Create and Save the plot
setwd("Supplementary_Figures")
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

######## P1 Period
rang <- c(46.6, 86.1)
#OR calculation
OR <- matrix(NA, nrow = length(res_P1_RH_Europe$Data), ncol = 3, 
    dimnames = list(names(res_P1_RH_Europe$Data), c("mean", "lower", "upper")))
for (i in names(res_P1_RH_Europe$Data)){
    res <- or_gam(data = res_P1_RH_Europe$Data[[i]], 
        model = res_P1_RH_Europe$Models[[i]], pred = "RH", values = c(rang[[1]],rang[[2]]))
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
rownames(OR) <- c(names(res_P1_RH_Europe$Data), "Meta-Analysis")

setwd("Tables")
write.csv(OR, file = "Odds_P1_RH_Europe.csv")
setwd("..")

#### OR plot P1
resum <- OR[nrow(OR),]
OR <- OR[-nrow(OR),]
OR$Region <- names(res_P2_RH_Europe$Data)
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
        summary = "#4292c6",
        zero = "#9e9ac8") %>%
    fp_add_header(Region = c("", "Region"),
        OR = c("", "OR")) %>%
    fp_append_row(mean  = resum[[1]],
        lower = resum[[2]],
        upper = resum[[3]],
        Region = "Summary",
        OR = as.character(resum[[1]]),
        is.summary = TRUE)



########### P2 Period
#OR calculation
OR <- matrix(NA, nrow = length(res_P1_RH_Europe$Data), ncol = 3, 
    dimnames = list(names(res_P2_RH_Europe$Data), c("mean", "lower", "upper")))
for (i in names(res_P2_RH_Europe$Data)){
    res <- or_gam(data = res_P2_RH_Europe$Data[[i]], 
        model = res_P2_RH_Europe$Models[[i]], pred = "RH", values = c(rang[[1]],rang[[2]]))
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
rownames(OR) <- c(names(res_P2_RH_Europe$Data), "Meta-Analysis")

setwd("Tables")
write.csv(OR, file = "Odds_P2_RH_Europe.csv")
setwd("..")

### Or plot P2
resum <- OR[nrow(OR),]
OR <- OR[-nrow(OR),]
OR$Region <- names(res_P2_RH_Europe$Data)
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
        summary = "#4292c6",
        zero = "#9e9ac8") %>%
    fp_add_header(Region = c("", "Region"),
        OR = c("", "OR")) %>%
    fp_append_row(mean  = resum[[1]],
        lower = resum[[2]],
        upper = resum[[3]],
        Region = "Summary",
        OR = as.character(resum[[1]]),
        is.summary = TRUE)


#Arrange both Odds plots
require(ggplotify)
require(patchwork)
graf1 <- grid2grob(print(graf1))
graf2 <- grid2grob(print(graf2))
graf_both <- wrap_elements(graf1) * wrap_elements(graf2)


##########################
#Save OR plot
setwd("Supplementary_Figures")
dev.off()
jpeg(filename = "Odds_RH_Europe.jpeg", height = 1500, width = 1500)
graf_both
dev.off()
rm(list = ls())
setwd("..")

rm(list = ls())


######################
## FULL PERIOD PLOTS
######################

load("Data/FullPeriod_Europe.RData")

#################################
# Evolution of pandemic graphics
#################################
setwd("Supplementary_Figures")

#Prepare the data for the plot
dat <- list()
for(i in 1:nrow(newCases)){
    datos <- data.frame(newCases[i,], newDeaths[i,], colnames(newCases))
    names(datos) <- c("Cases", "Deaths", "date")
    datos$date <- as.Date(datos$date)
    dat <- list.append(dat, datos)
}
names(dat) <- rownames(newCases)
data2 <- list(dat, nom = names(dat))
names(data2) <- c("dat", "nom")

#Function for creating the plots
plot2axis <- function(i, dataset){
    coeff <- 0.01
    caseColor <- "#1376b0"
    deathColor <- "#e8282d"
    graf <- ggplot(dataset$dat[[i]], aes(x=date)) +
        theme_bw() +
        geom_bar( aes(y=Cases), stat="identity", linewidth =.1, 
            fill=caseColor, color=caseColor, alpha=.4) + 
        geom_line( aes(y=Deaths / coeff), linewidth=0.5, color=deathColor) +
        scale_y_continuous(
            # Features of the first axis
            name = "Nº Cases",
            # Add a second axis and specify its features
            sec.axis = sec_axis(~.*coeff, name="Nº Deaths")
        ) + 
        theme(
            axis.title.y = element_text(color = caseColor, size=13),
            axis.title.y.right = element_text(color = deathColor, size=13)
        ) +
        xlab("Date") +
        ggtitle(dataset$nom[i])
    return(graf)
}

#Create and save the plots
figures = lapply(1:nrow(newCases),plot2axis, data2)
grid.arrange(figures[[1]], figures[[2]], figures[[3]], figures[[4]],
    figures[[5]], figures[[6]],
    ncol = 3 , nrow = 2)

dev.off()
jpeg(file = "Deaths_vs_cases_Europe.jpeg", height = 800, width = 1200)
grid.arrange(figures[[1]], figures[[2]], figures[[3]], figures[[4]],
    figures[[5]], figures[[6]],
    ncol = 3 , nrow = 2)
dev.off()

##################################
#Evolution of vaccination in Europe
##################################

#Prepare the data for the plot
Vac2 <- Vac
fechas <- colnames(Vac2)
fechas <- fechas[which(fechas =="2021-01-01"):which(fechas =="2021-12-31")]
num <- length(fechas)
paises <- rownames(Vac2)
Vac2 <- Vac2[,fechas]
VacRate <- c(Vac2[1,],Vac2[2,],Vac2[3,],Vac2[4,],Vac2[5,],Vac2[6,])
dates <- rep(fechas,6)
dates <- as.Date(dates)
VacEurope <- data.frame(VacRate, dates)
paisesVac <- c(
    rep(paises[1], num), rep(paises[2], num), rep(paises[3], num), rep(paises[4], num),
    rep(paises[5], num), rep(paises[6], num))

paises <- paisesVac
VacEurope$Country <- paises

#Create and save the plor
jpeg(file = "Vaccination_Europe.jpeg", height = 800, width = 1200)
ggplot(VacEurope, aes(dates)) +
    theme_bw() +
    geom_line(aes(y = VacRate, color = Country), linewidth = 2) +
    xlab("Date") + 
    ylab("Vaccination rate %") +
    scale_colour_brewer(palette="Set2") +
    theme(legend.position = "right", text = element_text(size = 20))
dev.off()


rm(list = ls())
setwd("..")






