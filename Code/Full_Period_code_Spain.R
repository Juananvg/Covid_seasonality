######################
## Full period code
######################
#Neccessary packages
library(dlnm)
library(mgcv)
library(visreg)
library(corrplot)
library(mctest)
library(rlist)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(oddsratio)
library(dplyr)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(forestplot)
library(metafor)

load("Data/FullPeriod_var_Spain.RData")


#################################
# Evolution of pandemic graphics
#################################
setwd("Paper_images")
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

plot2axis <- function(i, prueba){
    coeff <- 0.01
    caseColor <- "#1376b0"
    deathColor <- "#e8282d"
    graf <- ggplot(prueba$dat[[i]], aes(x=date)) +
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
        #theme_ipsum() +
        theme(
            text = element_text(size = 20),
            axis.title.y = element_text(color = caseColor, size=13),
            axis.title.y.right = element_text(color = deathColor, size=13)
        ) +
        xlab("Date") +
        ggtitle(prueba$nom[i])
    return(graf)
}

#Deaths vs Cases
graficos = lapply(1:nrow(newCases),plot2axis, data2)
grid.arrange(graficos[[1]], graficos[[2]], graficos[[9]], graficos[[10]],
    graficos[[5]], graficos[[6]], graficos[[12]], graficos[[7]],
    graficos[[16]],  
    ncol = 3 , nrow = 3)

dev.off()
jpeg(file = "Deaths_vs_cases_communities.jpeg", height = 800, width = 1200)
grid.arrange(graficos[[1]], graficos[[2]], graficos[[9]], graficos[[10]],
    graficos[[5]], graficos[[6]], graficos[[12]], graficos[[7]],
    graficos[[16]],  
    ncol = 3 , nrow = 3)
dev.off()
setwd("..")

setwd("Supplementary_images")
jpeg(file = "Deaths_vs_cases_communities_Supplementary.jpeg", height = 800, width = 1200)
grid.arrange(graficos[[1]], graficos[[2]], graficos[[3]], graficos[[4]],
    graficos[[5]], graficos[[6]], graficos[[7]], graficos[[8]],
    graficos[[9]], graficos[[10]], graficos[[11]], graficos[[12]],
    graficos[[13]], graficos[[14]], graficos[[15]], graficos[[16]],
    ncol = 4 , nrow = 4)
dev.off()
setwd("..")

##################################
#Evolution of vaccination in Spain
##################################
setwd("Supplementary_images")
vacSpain <- colMeans(Vac)
for(i in 1:(length(vacSpain)-1)){
    if(vacSpain[i] > vacSpain[i+1]){
        vacSpain[i+1] <- vacSpain[i]}
}
dates <- as.Date(names(vacSpain))
Variants <- factor(variants, labels = c("Original", "Alpha", "Delta", "Omicron"))
datVac <- data.frame(vacSpain, dates, Variants)

vacplot <- ggplot(datVac, aes(x = dates)) +
    theme_bw() +
    #geom_line(aes(y = vacSpain, color = variants)) +
    geom_area(aes(y = vacSpain, color = Variants, fill = Variants), alpha = 0.6) +
    xlab("Date") + 
    ylab("Vaccination rate %") + 
    scale_color_manual("COVID-19 Variant", 
        values=c("#009e73", "#999999", "#E69F00", "#56B4E9")) +
    scale_fill_manual("COVID-19 Variant", 
        values=c("#009e73","#999999", "#E69F00", "#56B4E9")) +
    theme(text = element_text(size = 20))
    #scale_color_brewer(palette = "Dark2")

jpeg(file = "Evolution_Vacciantion_Spain.jpeg", height = 800, width = 800)
vacplot
dev.off()
rm(list = ls())
setwd("..")
