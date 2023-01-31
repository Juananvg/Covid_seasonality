##########################
#FUNCIONES
##########################

GamModel <- function(vd, #Variable dependiente
    Temp, Measures, Retail, Work, Residential, Vac, variants){
    require(rlist)
    resultados <- matrix(NA, nrow = nrow(vd), ncol = 10)
    colnames(resultados) <- c("DevianceExplained", "R2Adjusted",
        "Temp", "Measures", "Residential",
        "Retail", "Work", "Vac", "Delta Variant", "Omicron Variant")
    rownames(resultados) <- rownames(vd)
    correlaciones <- matrix(NA, nrow = nrow(vd), ncol = 6)
    colnames(correlaciones) <- c("Temp", "Measures", "Residential",
        "Retail", "Work", "Vac")
    rownames(correlaciones) <- rownames(vd)
    list.models <- list()
    data.models <- list()
    for(i in 1:nrow(vd)){
        datos_mod <- data.frame(vd[i,], Temp[i,], Measures[i,], 
            Retail[i,], Work[i,], Residential[i,], Vac[i,], variants)
        colnames(datos_mod) <- c("vd","Temp", "Measures", "Residential",
            "Retail", "Work", "Vac", "Variants")
        modeloGAM <- modeloGAM <- gam(vd ~ s(Temp) + Measures + Retail + Work + 
            Residential + Vac + Variants, data = datos_mod, method = "REML")
        list.models <- list.append(list.models, modeloGAM)
        data.models <- list.append(data.models, datos_mod)
        modeloGAM <- summary(modeloGAM)
        resultados[i,1] <- modeloGAM$dev.expl * 100
        resultados[i,2] <- modeloGAM$r.sq
        resultados[i,3] <- modeloGAM$s.pv[1] #Temperatura
        resultados[i,4] <- modeloGAM$p.pv[2] #Medidas
        resultados[i,5] <- modeloGAM$p.pv[3] #Desplazaminetos a Retail
        resultados[i,6] <- modeloGAM$p.pv[4] #Desplazamientos al trabajo
        resultados[i,7] <- modeloGAM$p.pv[5] #Desplazamientos al residential
        resultados[i,8] <- modeloGAM$p.pv[6] #Vacunación
        resultados[i,9] <- modeloGAM$p.pv[7] #Variante Delta
        resultados[i,10] <- modeloGAM$p.pv[8] #Variante Omicron
        correlaciones[i,1] <- cor(vd[i,], Temp[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,2] <- cor(vd[i,], Measures[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,3] <- cor(vd[i,], Retail[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,4] <- cor(vd[i,], Work[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,5] <- cor(vd[i,], Residential[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,6] <- cor(vd[i,], Vac[i,], method = "pearson", use = "na.or.complete")
    }
    names(list.models) <- rownames(vd)
    names(data.models) <- rownames(vd)
    models.results <- list(resultados, correlaciones, list.models, data.models)
    names(models.results) <- c("Gam", "Cor", "Models", "Data")
    return(models.results)
}


#FUNCIÓN GAM PARA VARIABLES DE MOVILIDAD Y MEDIDAS COMO CROSSBASICS PARA EVITAR COLINEALIDAD

GamModelCrossbasics <- function(vd, #Variable dependiente
    Temp, Measures, Retail, Work, Residential, Vac, variants){
    require(rlist)
    require(dlnm)
    require(mgcv)
    resultados <- matrix(NA, nrow = nrow(vd), ncol = 10)
    colnames(resultados) <- c("DevianceExplained", "R2Adjusted",
        "Temp", "Measures", "Residential",
        "Retail", "Work", "Vac", "Delta Variant", "Omicron Variant")
    rownames(resultados) <- rownames(vd)
    correlaciones <- matrix(NA, nrow = nrow(vd), ncol = 6)
    colnames(correlaciones) <- c("Temp", "Measures", "Residential",
        "Retail", "Work", "Vac")
    rownames(correlaciones) <- rownames(vd)
    list.models <- list()
    data.models <- list()
    for(i in 1:nrow(vd)){
        datos_mod <- data.frame(vd[i,], Temp[i,], 
            as.numeric(crossbasis(Measures[i,])), 
            as.numeric(crossbasis(Retail[i,])), as.numeric(crossbasis(Work[i,])), 
            as.numeric(crossbasis(Residential[i,])), Vac[i,], variants)
        colnames(datos_mod) <- c("vd","Temp", "Measures", "Residential",
            "Retail", "Work", "Vac", "Variants")
        modeloGAM <- modeloGAM <- gam(vd ~ s(Temp) + Measures + Retail + Work + 
                Residential + Vac + Variants, data = datos_mod, method = "REML")
        list.models <- list.append(list.models, modeloGAM)
        data.models <- list.append(data.models, datos_mod)
        modeloGAM <- summary(modeloGAM)
        resultados[i,1] <- modeloGAM$dev.expl * 100
        resultados[i,2] <- modeloGAM$r.sq
        resultados[i,3] <- modeloGAM$s.pv[1] #Temperatura
        resultados[i,4] <- modeloGAM$p.pv[2] #Medidas
        resultados[i,5] <- modeloGAM$p.pv[3] #Desplazaminetos a Retail
        resultados[i,6] <- modeloGAM$p.pv[4] #Desplazamientos al trabajo
        resultados[i,7] <- modeloGAM$p.pv[5] #Desplazamientos al residential
        resultados[i,8] <- modeloGAM$p.pv[6] #Vacunación
        resultados[i,9] <- modeloGAM$p.pv[7] #Variante Delta
        resultados[i,10] <- modeloGAM$p.pv[8] #Variante Omicron
        correlaciones[i,1] <- cor(vd[i,], Temp[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,2] <- cor(vd[i,], Measures[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,3] <- cor(vd[i,], Retail[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,4] <- cor(vd[i,], Work[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,5] <- cor(vd[i,], Residential[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,6] <- cor(vd[i,], Vac[i,], method = "pearson", use = "na.or.complete")
    }
    names(list.models) <- rownames(vd)
    names(data.models) <- rownames(vd)
    models.results <- list(resultados, correlaciones, list.models, data.models)
    names(models.results) <- c("Gam", "Cor", "Models", "Data")
    return(models.results)
}


GamModelANOVA <- function(vd, #Variable dependiente
    Temp, Measures, Retail, Work, Residential, Vac, variants){
    require(rlist)
    require(dlnm)
    require(mgcv)
    resultados <- matrix(NA, nrow = nrow(vd), ncol = 9)
    colnames(resultados) <- c("DevianceExplained", "R2Adjusted",
        "Temp", "Measures", "Residential",
        "Retail", "Work", "Vac", "Variants")
    rownames(resultados) <- rownames(vd)
    correlaciones <- matrix(NA, nrow = nrow(vd), ncol = 6)
    colnames(correlaciones) <- c("Temp", "Measures", "Residential",
        "Retail", "Work", "Vac")
    rownames(correlaciones) <- rownames(vd)
    list.models <- list()
    data.models <- list()
    for(i in 1:nrow(vd)){
        datos_mod <- data.frame(vd[i,], Temp[i,], 
            as.numeric(crossbasis(Measures[i,])), 
            as.numeric(crossbasis(Retail[i,])), as.numeric(crossbasis(Work[i,])), 
            as.numeric(crossbasis(Residential[i,])), Vac[i,], variants)
        colnames(datos_mod) <- c("vd","Temp", "Measures", "Residential",
            "Retail", "Work", "Vac", "Variants")
        modeloGAM <- modeloGAM <- gam(vd ~ s(Temp) + Measures + Retail + Work + 
                Residential + Vac + Variants, data = datos_mod, method = "REML")
        list.models <- list.append(list.models, modeloGAM)
        data.models <- list.append(data.models, datos_mod)
        modeloANOVA <- anova.gam(modeloGAM)
        modeloGAM <- summary(modeloGAM)
        resultados[i,1] <- modeloGAM$dev.expl * 100
        resultados[i,2] <- modeloGAM$r.sq
        resultados[i,3] <- modeloANOVA$s.table[1,4] #Temperatura
        resultados[i,4] <- modeloANOVA$pTerms.table[1,3] #Medidas
        resultados[i,5] <- modeloANOVA$pTerms.table[2,3] #Desplazaminetos a Retail
        resultados[i,6] <- modeloANOVA$pTerms.table[3,3] #Desplazamientos al trabajo
        resultados[i,7] <- modeloANOVA$pTerms.table[4,3] #Desplazamientos al residential
        resultados[i,8] <- modeloANOVA$pTerms.table[5,3] #Vacunación
        resultados[i,9] <- modeloANOVA$pTerms.table[6,3] #Variantes
        correlaciones[i,1] <- cor(vd[i,], Temp[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,2] <- cor(vd[i,], Measures[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,3] <- cor(vd[i,], Retail[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,4] <- cor(vd[i,], Work[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,5] <- cor(vd[i,], Residential[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,6] <- cor(vd[i,], Vac[i,], method = "pearson", use = "na.or.complete")
    }
    names(list.models) <- rownames(vd)
    names(data.models) <- rownames(vd)
    models.results <- list(resultados, correlaciones, list.models, data.models)
    names(models.results) <- c("Gam", "Cor", "Models", "Data")
    return(models.results)
}

###########################################################


GamModelCrossbasicsSinVac <- function(vd, #Variable dependiente
    Temp, Measures, Retail, Work, Residential, variants){
    require(rlist)
    require(dlnm)
    require(mgcv)
    resultados <- matrix(NA, nrow = nrow(vd), ncol = 8)
    colnames(resultados) <- c("DevianceExplained", "R2Adjusted",
        "Temp", "Measures", "Residential",
        "Retail", "Work", "Variants")
    rownames(resultados) <- rownames(vd)
    correlaciones <- matrix(NA, nrow = nrow(vd), ncol = 5)
    colnames(correlaciones) <- c("Temp", "Measures", "Residential",
        "Retail", "Work")
    rownames(correlaciones) <- rownames(vd)
    list.models <- list()
    data.models <- list()
    for(i in 1:nrow(vd)){
        datos_mod <- data.frame(vd[i,], Temp[i,], 
            as.numeric(crossbasis(Measures[i,])), 
            as.numeric(crossbasis(Retail[i,])), as.numeric(crossbasis(Work[i,])), 
            as.numeric(crossbasis(Residential[i,])), variants)
        colnames(datos_mod) <- c("vd","Temp", "Measures", "Residential",
            "Retail", "Work", "Variants")
        modeloGAM <- modeloGAM <- gam(vd ~ s(Temp) + Measures + Retail + Work + 
                Residential + Variants, data = datos_mod, method = "REML")
        list.models <- list.append(list.models, modeloGAM)
        data.models <- list.append(data.models, datos_mod)
        modeloANOVA <- anova.gam(modeloGAM)
        modeloGAM <- summary(modeloGAM)
        resultados[i,1] <- modeloGAM$dev.expl * 100
        resultados[i,2] <- modeloGAM$r.sq
        resultados[i,3] <- modeloANOVA$s.table[1,4] #Temperatura
        resultados[i,4] <- modeloANOVA$pTerms.table[1,3] #Medidas
        resultados[i,5] <- modeloANOVA$pTerms.table[2,3] #Desplazaminetos a Retail
        resultados[i,6] <- modeloANOVA$pTerms.table[3,3] #Desplazamientos al trabajo
        resultados[i,7] <- modeloANOVA$pTerms.table[4,3] #Desplazamientos al residential
        resultados[i,8] <- modeloANOVA$pTerms.table[5,3] #Variantes
        correlaciones[i,1] <- cor(vd[i,], Temp[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,2] <- cor(vd[i,], Measures[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,3] <- cor(vd[i,], Retail[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,4] <- cor(vd[i,], Work[i,], method = "spearman", use = "na.or.complete")
        correlaciones[i,5] <- cor(vd[i,], Residential[i,], method = "spearman", use = "na.or.complete")
        #correlaciones[i,6] <- cor(vd[i,], Vac[i,], method = "pearson", use = "na.or.complete")
    }
    names(list.models) <- rownames(vd)
    names(data.models) <- rownames(vd)
    models.results <- list(resultados, correlaciones, list.models, data.models)
    names(models.results) <- c("Gam", "Cor", "Models", "Data")
    return(models.results)
}






