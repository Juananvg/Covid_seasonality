#############################
### Initial Period Analysis - Europe Countries
#############################

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


load("Data/FullPeriod_var_Europe.RData")


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
            axis.title.y = element_text(color = caseColor, size=13),
            axis.title.y.right = element_text(color = deathColor, size=13)
        ) +
        xlab("Date") +
        ggtitle(prueba$nom[i])
    return(graf)
}

#Deaths vs Cases
graficos = lapply(1:nrow(newCases),plot2axis, data2)
grid.arrange(graficos[[1]], graficos[[2]], graficos[[3]], graficos[[4]],
    graficos[[5]], graficos[[6]], graficos[[7]], graficos[[8]], graficos[[9]], 
    ncol = 3 , nrow = 3)

dev.off()
jpeg(file = "Deaths_vs_cases_Europe.jpeg", height = 800, width = 1200)
grid.arrange(graficos[[1]], graficos[[2]], graficos[[3]], graficos[[4]],
    graficos[[5]], graficos[[6]], graficos[[7]], graficos[[8]], graficos [[9]],
    ncol = 3 , nrow = 3)
dev.off()

##################################
#Evolution of vaccination in Europe
##################################


Vac2 <- Vac
fechas <- colnames(Vac2)
fechas <- fechas[which(fechas =="2021-01-01"):which(fechas =="2021-12-31")]
num <- length(fechas)
paises <- rownames(Vac2)
Vac2 <- Vac2[,fechas]
VacRate <- c(Vac2[1,],Vac2[2,],Vac2[3,],Vac2[4,],Vac2[5,],Vac2[6,],Vac2[7,], Vac2[8,], Vac2[9,])
dates <- rep(fechas,9)
dates <- as.Date(dates)
VacEurope <- data.frame(VacRate, dates)
paisesVac <- c(
    rep(paises[1], num), rep(paises[2], num), rep(paises[3], num), rep(paises[4], num),
    rep(paises[5], num), rep(paises[6], num), rep(paises[7], num), rep(paises[8], num),
    rep(paises[9], num))

paises <- paisesVac
VacEurope$Country <- paises

jpeg(file = "Vaccination_Europe.jpeg", height = 800, width = 1200)
ggplot(VacEurope, aes(dates)) +
    theme_bw() +
    geom_line(aes(y = VacRate, color = Country), linewidth = 2) +
    xlab("Date") + 
    ylab("Vaccination rate %") + 
    theme(legend.position = "right", text = element_text(size = 20))
dev.off()


rm(list = ls())
setwd("..")


