#################################
#FUNCTIONS TO APPLY GAM MODELS
#################################

#TEMPERATURE
GamModel <- function(vd,
    Temp, StringencyIndex, Retail, Work, Residential, Vac, variants){
    require(rlist)
    resultados <- matrix(NA, nrow = nrow(vd), ncol = 10)
    colnames(resultados) <- c("DevianceExplained", "R2Adjusted",
        "Temp", "StringencyIndex", "Residential",
        "Retail", "Work", "Vac", "Delta Variant", "Omicron Variant")
    rownames(resultados) <- rownames(vd)
    list.models <- list()
    data.models <- list()
    for(i in 1:nrow(vd)){
        datos_mod <- data.frame(vd[i,], Temp[i,], StringencyIndex[i,], 
            Retail[i,], Work[i,], Residential[i,], Vac[i,], variants)
        colnames(datos_mod) <- c("vd","Temp", "StringencyIndex", "Residential",
            "Retail", "Work", "Vac", "Variants")
        modeloGAM <- modeloGAM <- gam(vd ~ s(Temp) + StringencyIndex + Retail + Work + 
            Residential + Vac + Variants, data = datos_mod, method = "REML")
        list.models <- list.append(list.models, modeloGAM)
        data.models <- list.append(data.models, datos_mod)
        modeloGAM <- summary(modeloGAM)
        resultados[i,1] <- modeloGAM$dev.expl * 100
        resultados[i,2] <- modeloGAM$r.sq
        resultados[i,3] <- modeloGAM$s.pv[1] #Temperature
        resultados[i,4] <- modeloGAM$p.pv[2] #StringencyIndex
        resultados[i,5] <- modeloGAM$p.pv[3] #Retail
        resultados[i,6] <- modeloGAM$p.pv[4] #Work
        resultados[i,7] <- modeloGAM$p.pv[5] #Residential
        resultados[i,8] <- modeloGAM$p.pv[6] #Vac
        resultados[i,9] <- modeloGAM$p.pv[7] #Delta
        resultados[i,10] <- modeloGAM$p.pv[8] #Omicron
    }
    names(list.models) <- rownames(vd)
    names(data.models) <- rownames(vd)
    models.results <- list(resultados, list.models, data.models)
    names(models.results) <- c("Gam", "Models", "Data")
    return(models.results)
}

#GAM FUNCTION FOR MOBILITY VARIABLES AND StringencyIndex AS CROSSBASICS TO AVOID COLINEARITY

GamModelCrossbasics <- function(vd,
    Temp, StringencyIndex, Retail, Work, Residential, Vac, variants){
    require(rlist)
    require(dlnm)
    require(mgcv)
    resultados <- matrix(NA, nrow = nrow(vd), ncol = 10)
    colnames(resultados) <- c("DevianceExplained", "R2Adjusted",
        "Temp", "StringencyIndex", "Residential",
        "Retail", "Work", "Vac", "Delta Variant", "Omicron Variant")
    rownames(resultados) <- rownames(vd)
    list.models <- list()
    data.models <- list()
    for(i in 1:nrow(vd)){
        datos_mod <- data.frame(vd[i,], Temp[i,], 
            as.numeric(crossbasis(StringencyIndex[i,])), 
            as.numeric(crossbasis(Retail[i,])), as.numeric(crossbasis(Work[i,])), 
            as.numeric(crossbasis(Residential[i,])), Vac[i,], variants)
        colnames(datos_mod) <- c("vd","Temp", "StringencyIndex", "Residential",
            "Retail", "Work", "Vac", "Variants")
        modeloGAM <- modeloGAM <- gam(vd ~ s(Temp) + StringencyIndex + Retail + Work + 
                Residential + Vac + Variants, data = datos_mod, method = "REML")
        list.models <- list.append(list.models, modeloGAM)
        data.models <- list.append(data.models, datos_mod)
        modeloGAM <- summary(modeloGAM)
        resultados[i,1] <- modeloGAM$dev.expl * 100
        resultados[i,2] <- modeloGAM$r.sq
        resultados[i,3] <- modeloGAM$s.pv[1] #Temperature
        resultados[i,4] <- modeloGAM$p.pv[2] #StringencyIndex
        resultados[i,5] <- modeloGAM$p.pv[3] #Retail
        resultados[i,6] <- modeloGAM$p.pv[4] #Work
        resultados[i,7] <- modeloGAM$p.pv[5] #Residential
        resultados[i,8] <- modeloGAM$p.pv[6] #Vac
        resultados[i,9] <- modeloGAM$p.pv[7] #Delta
        resultados[i,10] <- modeloGAM$p.pv[8] #Omicron
    }
    names(list.models) <- rownames(vd)
    names(data.models) <- rownames(vd)
    models.results <- list(resultados, list.models, data.models)
    names(models.results) <- c("Gam", "Models", "Data")
    return(models.results)
}


#  RELATIVE HUMIDITY
GamModelCrossbasics_RH <- function(vd,
    RH, StringencyIndex, Retail, Work, Residential, Vac, variants){
    require(rlist)
    require(dlnm)
    require(mgcv)
    resultados <- matrix(NA, nrow = nrow(vd), ncol = 10)
    colnames(resultados) <- c("DevianceExplained", "R2Adjusted",
        "RH", "StringencyIndex", "Residential",
        "Retail", "Work", "Vac", "Delta Variant", "Omicron Variant")
    rownames(resultados) <- rownames(vd)
    list.models <- list()
    data.models <- list()
    for(i in 1:nrow(vd)){
        datos_mod <- data.frame(vd[i,], RH[i,], 
            as.numeric(crossbasis(StringencyIndex[i,])), 
            as.numeric(crossbasis(Retail[i,])), as.numeric(crossbasis(Work[i,])), 
            as.numeric(crossbasis(Residential[i,])), Vac[i,], variants)
        colnames(datos_mod) <- c("vd","RH", "StringencyIndex", "Residential",
            "Retail", "Work", "Vac", "Variants")
        modeloGAM <- modeloGAM <- gam(vd ~ s(RH) + StringencyIndex + Retail + Work + 
                Residential + Vac + Variants, data = datos_mod, method = "REML")
        list.models <- list.append(list.models, modeloGAM)
        data.models <- list.append(data.models, datos_mod)
        modeloGAM <- summary(modeloGAM)
        resultados[i,1] <- modeloGAM$dev.expl * 100
        resultados[i,2] <- modeloGAM$r.sq
        resultados[i,3] <- modeloGAM$s.pv[1] #RH
        resultados[i,4] <- modeloGAM$p.pv[2] #StringencyIndex
        resultados[i,5] <- modeloGAM$p.pv[3] #Retail
        resultados[i,6] <- modeloGAM$p.pv[4] #Work
        resultados[i,7] <- modeloGAM$p.pv[5] #Residential
        resultados[i,8] <- modeloGAM$p.pv[6] #Vac
        resultados[i,9] <- modeloGAM$p.pv[7] #Delta
        resultados[i,10] <- modeloGAM$p.pv[8] #Omicron
    }
    names(list.models) <- rownames(vd)
    names(data.models) <- rownames(vd)
    models.results <- list(resultados, list.models, data.models)
    names(models.results) <- c("Gam", "Models", "Data")
    return(models.results)
}

#Specific humidity
GamModelCrossbasics_SH <- function(vd,
    SH, StringencyIndex, Retail, Work, Residential, Vac, variants){
    require(rlist)
    require(dlnm)
    require(mgcv)
    resultados <- matrix(NA, nrow = nrow(vd), ncol = 10)
    colnames(resultados) <- c("DevianceExplained", "R2Adjusted",
        "SH", "StringencyIndex", "Residential",
        "Retail", "Work", "Vac", "Delta Variant", "Omicron Variant")
    rownames(resultados) <- rownames(vd)
    list.models <- list()
    data.models <- list()
    for(i in 1:nrow(vd)){
        datos_mod <- data.frame(vd[i,], SH[i,], 
            as.numeric(crossbasis(StringencyIndex[i,])), 
            as.numeric(crossbasis(Retail[i,])), as.numeric(crossbasis(Work[i,])), 
            as.numeric(crossbasis(Residential[i,])), Vac[i,], variants)
        colnames(datos_mod) <- c("vd","SH", "StringencyIndex", "Residential",
            "Retail", "Work", "Vac", "Variants")
        modeloGAM <- modeloGAM <- gam(vd ~ s(SH) + StringencyIndex + Retail + Work + 
                Residential + Vac + Variants, data = datos_mod, method = "REML")
        list.models <- list.append(list.models, modeloGAM)
        data.models <- list.append(data.models, datos_mod)
        modeloGAM <- summary(modeloGAM)
        resultados[i,1] <- modeloGAM$dev.expl * 100
        resultados[i,2] <- modeloGAM$r.sq
        resultados[i,3] <- modeloGAM$s.pv[1] #SH
        resultados[i,4] <- modeloGAM$p.pv[2] #StringencyIndex
        resultados[i,5] <- modeloGAM$p.pv[3] #Retail
        resultados[i,6] <- modeloGAM$p.pv[4] #Work
        resultados[i,7] <- modeloGAM$p.pv[5] #Residential
        resultados[i,8] <- modeloGAM$p.pv[6] #Vac
        resultados[i,9] <- modeloGAM$p.pv[7] #Delta
        resultados[i,10] <- modeloGAM$p.pv[8] #Omicron
    }
    names(list.models) <- rownames(vd)
    names(data.models) <- rownames(vd)
    models.results <- list(resultado, list.models, data.models)
    names(models.results) <- c("Gam", "Models", "Data")
    return(models.results)
}



