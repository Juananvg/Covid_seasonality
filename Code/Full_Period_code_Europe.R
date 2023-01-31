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
    graficos[[5]], graficos[[6]], graficos[[7]], graficos[[8]],
    graficos[[9]],  
    ncol = 3 , nrow = 3)

dev.off()
jpeg(file = "Deaths_vs_cases_Europe.jpeg", height = 800, width = 1200)
grid.arrange(graficos[[1]], graficos[[2]], graficos[[3]], graficos[[4]],
    graficos[[5]], graficos[[6]], graficos[[7]], graficos[[8]],
    graficos[[9]],  
    ncol = 3 , nrow = 3)
dev.off()

rm(list = ls())
setwd("..")

##################################
#Evolution of vaccination in Europe
##################################
#vacSpain <- colMeans(Vac)
#for(i in 1:(length(vacSpain)-1)){
#    if(vacSpain[i] > vacSpain[i+1]){
#        vacSpain[i+1] <- vacSpain[i]}
#}
#dates <- as.Date(names(vacSpain))
#variants <- factor(variants, labels = c("Original", "Alpha", "Delta", "Omicron"))
#datVac <- data.frame(vacSpain, dates, variants)
#
#vacplot <- ggplot(datVac, aes(x = dates)) +
#    theme_bw() +
#    geom_line(aes(y = vacSpain, color = variants)) +
#    xlab("Date") + 
#    ylab("Vaccination rate %") + 
#    scale_color_brewer(palette = "Dark2")
#
#jpeg(file = "Evolution_Vacciantion_Spain.jpeg", height = 800, width = 800)
#vacplot
#dev.off()
#rm(list = ls())
#setwd("..")
