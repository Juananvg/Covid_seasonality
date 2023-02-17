#################################
## GAM and OR Plots - Spain
#################################
#Abbreviation
#Com -> Communities
#Ini -> Intial
#Fin -> Final
#Res/r -> Results

#Neccesary packages
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
load("Data/GAM_Spain_P1.RData")
load("Data/GAM_Spain_P2.RData")

############################
#Prepare Data for plots
############################
r_P1 <- res_P1_Temp_Spain$Data
r_P2 <- res_P2_Temp_Spain$Data
names_com <- names(r_P1)
names_com[c(1,2,5,6,7,8,9,12,14,15,16)] <- c("Andalusia", "Aragon",
    "Castille-La Mancha",
    "Catalonia", "Madrid", "Navarre", "C. Valenciana", "The Balearic Islands",
    "Basque Country", "Asturias", "Murcia")
names(r_P1) <- names_com
names(r_P2) <- names_com


com_P1 = r_P1
com_P2 <- r_P2
for (i in names(com_P1)){
    com_P1[[i]]$Country = i
    com_P2[[i]]$Country = i
}

com_P1 = do.call("rbind",com_P1)
com_P2 = do.call("rbind",com_P2)

com_P1$Period <- rep("P1", nrow(com_P1))
com_P2$Period <- rep("P2", nrow(com_P2))

res_total <- rbind(com_P1, com_P2)

#############################
#Create and Save the plot
############################
setwd("Figures")
jpeg(filename = "Gam_Temp_Spain.jpeg", 
    height = 800, width = 1000)
vars <- c("P1"="#4292c6", "P2" ="#53b594")
ggplot() +
    theme_bw() + 
    facet_wrap(~Country,scales = "free") +
    geom_smooth(data = res_total, aes(x = Temp, y = vd, 
        colour = Period), method = "gam", 
        formula = y ~ s(x, bs = "cs"), fill = "#b4b1b1") +
    scale_color_manual(name = "Period:", values = vars) +
    theme(legend.position = "bottom", text = element_text(size = 20)) + 
    geom_hline(yintercept = 1, color = "red", linetype = 2) +
    labs(x = "Temperature ºC",y = bquote(R[e]))
dev.off()
setwd("..")


#######################
## OR Plots
#######################

#Estimation of the range: Percentile 10 and 90
load("Data/FullPeriod_Spain.RData")
rang <- quantile(Temperature, probs = c(0.1,0.9))

######## P1 Period
#OR calculation
OR <- matrix(NA, nrow = 16, ncol = 3, 
    dimnames = list(names(res_P1_Temp_Spain$Data), c("mean", "lower", "upper")))
for (i in names(res_P1_Temp_Spain$Data)){
    res <- or_gam(data = res_P1_Temp_Spain$Data[[i]], 
        model = res_P1_Temp_Spain$Models[[i]], pred = "Temp", values = c(rang[[1]],rang[[2]]))
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
rownames(OR) <- c(names(r_P1), "Meta-Analysis")

#Save OR table
setwd("Tables")
write.csv(OR, file = "Odds_P1_Temp_Spain.csv")
setwd("..")

################
# OR Figure P1
################
resum <- OR[17,]
OR <- OR[-17,]
OR$Region <- names(r_P1)
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
OR <- matrix(NA, nrow = 16, ncol = 3, 
    dimnames = list(names(res_P2_Temp_Spain$Data), c("mean", "lower", "upper")))
for (i in names(res_P2_Temp_Spain$Data)){
    res <- or_gam(data = res_P2_Temp_Spain$Data[[i]], 
        model = res_P2_Temp_Spain$Models[[i]], pred = "Temp", values = c(rang[[1]],rang[[2]]))
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
rownames(OR) <- c(names(r_P2), "Meta-Analysis")

#Save the table
setwd("Tables")
write.csv(OR, file = "Odds_P2_Temp_Spain.csv")
setwd("..")

#### OR Figure P2

resum <- OR[17,]
OR <- OR[-17,]
OR$Region <- names(r_P1)
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
setwd("Figures")
dev.off()
jpeg(filename = "Odds_Temp_Spain.jpeg", height = 1200, width = 1200)
graf_both
dev.off()
rm(list = ls())
setwd("..")



#############################
# Relative Humidity
#############################

#Load GAM results
load("Data/GAM_Spain_P1.RData")
load("Data/GAM_Spain_P2.RData")

############################
#Prepare Data for plots
############################
r_P1 <- res_P1_RH_Spain$Data
r_P2 <- res_P2_RH_Spain$Data
names_com <- names(r_P1)
names_com[c(1,2,5,6,7,8,9,12,14,15,16)] <- c("Andalusia", "Aragon",
    "Castille-La Mancha",
    "Catalonia", "Madrid", "Navarre", "C. Valenciana", "The Balearic Islands",
    "Basque Country", "Asturias", "Murcia")
names(r_P1) <- names_com
names(r_P2) <- names_com

#Comunities that are going to be represented
com_P1 = r_P1
com_P2 <- r_P2
for (i in names(com_P1)){
    com_P1[[i]]$Country = i
    com_P2[[i]]$Country = i
}

com_P1 = do.call("rbind",com_P1)
com_P2 = do.call("rbind",com_P2)

com_P1$Period <- rep("P1", nrow(com_P1))
com_P2$Period <- rep("P2", nrow(com_P2))

res_total <- rbind(com_P1, com_P2)

#Create and Save the plot
setwd("Figures")

jpeg(filename = "Gam_RH_Spain.jpeg", 
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

#Calculation of the range: Percentile 10 and 90
load("Data/FullPeriod_Spain.RData")
rang <- quantile(RH, probs = c(0.1,0.9))

######## Initial Period

#OR calculation
OR <- matrix(NA, nrow = 16, ncol = 3, 
    dimnames = list(names(res_P1_RH_Spain$Data), c("mean", "lower", "upper")))
for (i in names(res_P1_RH_Spain$Data)){
    res <- or_gam(data = res_P1_RH_Spain$Data[[i]], 
        model = res_P1_RH_Spain$Models[[i]], pred = "RH", values = c(rang[[1]],rang[[2]]))
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
rownames(OR) <- c(names(r_P2), "Meta-Analysis")

setwd("Tables")
write.csv(OR, file = "Odds_P1_RH_Spain.csv")
setwd("..")


resum <- OR[17,]
OR <- OR[-17,]
OR$Region <- names(r_P1)
OR$OR <- as.character(OR$mean)

#OR plot
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
OR <- matrix(NA, nrow = 16, ncol = 3, 
    dimnames = list(names(res_P2_RH_Spain$Data), c("mean", "lower", "upper")))
for (i in names(res_P2_RH_Spain$Data)){
    res <- or_gam(data = res_P2_RH_Spain$Data[[i]], 
        model = res_P2_RH_Spain$Models[[i]], pred = "RH", values = c(rang[[1]],rang[[2]]))
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
rownames(OR) <- c(names(r_P2), "Meta-Analysis")

setwd("Tables")
write.csv(OR, file = "Odds_P2_RH_Spain.csv")
setwd("..")


resum <- OR[17,]
OR <- OR[-17,]
OR$Region <- names(r_P1)
OR$OR <- as.character(OR$mean)

#OR plot
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
setwd("Figures")
dev.off()
jpeg(filename = "Odds_RH_Spain.jpeg", height = 1500, width = 1500)
graf_both
dev.off()
rm(list = ls())
setwd("..")

#######################################
###### FULL PERIOD SPAIN PLOTS
#######################################

#Load data
load("Data/FullPeriod_Spain.RData")


#################################
# Evolution of pandemic graphics
#################################
dat <- list()
for(i in 1:nrow(newCases)){
    datos <- data.frame(newCases[i,], newDeaths[i,], colnames(newCases))
    names(datos) <- c("Cases", "Deaths", "date")
    datos$date <- as.Date(datos$date)
    dat <- list.append(dat, datos)
}
names(dat) <- rownames(newCases)
names(dat)[c(1,2,5,6,7,8,9,12,14,15,16)] <- c("Andalusia", "Aragon",
    "Castille-La Mancha",
    "Catalonia", "Madrid", "Navarre", "C. Valenciana", "The Balearic Islands",
    "Basque Country", "Asturias", "Murcia")
data2 <- list(dat, nom = names(dat))
names(data2) <- c("dat", "nom")

#Function to create the plots
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
            text = element_text(size = 20),
            axis.title.y = element_text(color = caseColor, size=13),
            axis.title.y.right = element_text(color = deathColor, size=13)
        ) +
        xlab("Date") +
        ggtitle(dataset$nom[i])
    return(graf)
}

figures = lapply(1:nrow(newCases),plot2axis, data2)

#Save the plot
setwd("Figures")
jpeg(file = "Deaths_vs_cases_communities.jpeg", height = 800, width = 1200)
grid.arrange(figures[[1]], figures[[2]], figures[[3]], figures[[4]],
    figures[[5]], figures[[6]], figures[[7]], figures[[8]],
    figures[[9]], figures[[10]], figures[[11]], figures[[12]],
    figures[[13]], figures[[14]], figures[[15]], figures[[16]],
    ncol = 4 , nrow = 4)
dev.off()
setwd("..")

##################################
#Evolution of vaccination in Spain
##################################
setwd("Supplementary_Figures")
vacSpain <- colMeans(Vac)
for(i in 1:(length(vacSpain)-1)){
    if(vacSpain[i] > vacSpain[i+1]){
        vacSpain[i+1] <- vacSpain[i]}
}
dates <- as.Date(names(vacSpain))
Variants <- factor(variants, labels = c("Original", "Alpha", "Delta", "Omicron"))
datVac <- data.frame(vacSpain, dates, Variants)

#Create the plot
jpeg(file = "Evolution_Vacciantion_Spain.jpeg", height = 800, width = 800)
ggplot(datVac, aes(x = dates)) +
    theme_bw() +
    geom_area(aes(y = vacSpain, color = Variants, fill = Variants), alpha = 0.6) +
    xlab("Date") + 
    ylab("Vaccination rate %") + 
    scale_color_manual("COVID-19 Variant", 
        values=c("#009e73", "#999999", "#E69F00", "#56B4E9")) +
    scale_fill_manual("COVID-19 Variant", 
        values=c("#009e73","#999999", "#E69F00", "#56B4E9")) +
    theme(text = element_text(size = 20))
dev.off()
rm(list = ls())
setwd("..")



