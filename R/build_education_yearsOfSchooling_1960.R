#' @export

build_education_yearsOfSchooling_1960 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        check_vars <- check_var_existence(CensusData, c("v212", "v213"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        
        # Building age
        check_vars <- check_var_existence(CensusData, c("age"))
        age_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_demographics_age_1960(CensusData)
                age_just_created <- TRUE
        }
        gc()        
        
        
        # Anos de estudo para cada grau: Primario/Elementar
        CensusData[v213 == 2, anosGraus_tmp := 0]
        
        # Anos de estudo para cada grau: Medio 1o Ciclo
        CensusData[v213 == 3, anosGraus_tmp := 4]
        
        # Anos de estudo para cada grau: Medio 2o Ciclo
        CensusData[v213 == 4, anosGraus_tmp := 8]
        
        # Anos de estudo para cada grau: Superior
        CensusData[v213 == 5, anosGraus_tmp := 11]
        
        # Anos de estudo para cada grau: Ignorado
        CensusData[v213 == 6, anosGraus_tmp := NA]
        
        # Anos de estudo para cada grau: Nunca frequentou ou frequentando primeiro ano do elementar
        CensusData[v213 %in% c(0,1), anosGraus_tmp := 0]
        
        # Anos de estudo para cada série
        CensusData[  , anosSeries_tmp := v212 - 3]
        
        # Anos de estudo para cada série: frequentou ou frequentando primeiro ano do elementar
        CensusData[v212 %in% c(0,1) , anosSeries_tmp := 0]
        
        # Anos de estudo para serie: Ignorado
        CensusData[v212 == 2, anosSeries_tmp := NA]
        
        CensusData[, yearsOfSchooling := anosGraus_tmp + anosSeries_tmp]
        
        # Teto para o Primario: 4 anos de estudo
        CensusData[yearsOfSchooling > 4 & v213 == 2, yearsOfSchooling := 4]
        
        # Teto para o Medio 1o Ciclo: 8 anos de estudo
        CensusData[yearsOfSchooling > 8 & v213 == 3, yearsOfSchooling := 8]
        
        # Teto para o Medio 2o Ciclo: 11 anos de estudo
        CensusData[yearsOfSchooling > 11 & v213 == 4, yearsOfSchooling := 11]
        
        # Teto para o Superior: 15 anos de estudo
        CensusData[yearsOfSchooling > 15 & v213 == 5, yearsOfSchooling := 15]
        
        CensusData[ , anosGraus_tmp  := NULL]
        CensusData[ , anosSeries_tmp := NULL]
        
        ##############################
        # Ajuste para idade
        CensusData[age <= 4,  yearsOfSchooling := NA]
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }
        
        gc();Sys.sleep(.5);gc()
        CensusData
}

