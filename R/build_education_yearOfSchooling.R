#' @export

build_education_yearOfSchooling <- function(CensusData){
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        
        year = metadata$year
        
        # Building educationAttainment
        check_vars <- check_var_existence(CensusData, c("educationAttainment"))
        educationAttainment_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- eval(parse(text=paste0("build_education_educationAttainment_",metadata$year,"(CensusData)")))
                educationAttainment_just_created <- TRUE
                gc();Sys.sleep(.5);gc()
        }
        
        if(year != 1960){
                CensusData[educationAttainment==1, yearOfSchooling := 0]
                CensusData[educationAttainment==3, yearOfSchooling := 4]
                CensusData[educationAttainment==5, yearOfSchooling := 8]
                CensusData[educationAttainment==7, yearOfSchooling := 11]
                CensusData[educationAttainment==9, yearOfSchooling := 15]
        }
        
        if(year == 1960){ 
                
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
                
                CensusData[, yearOfSchooling := anosGraus_tmp + anosSeries_tmp]
                
                # Teto para o Primario: 4 anos de estudo
                CensusData[yearOfSchooling > 4 & v213 == 2, yearOfSchooling := 4]
                
                # Teto para o Medio 1o Ciclo: 8 anos de estudo
                CensusData[yearOfSchooling > 8 & v213 == 3, yearOfSchooling := 8]
                
                # Teto para o Medio 2o Ciclo: 11 anos de estudo
                CensusData[yearOfSchooling > 11 & v213 == 4, yearOfSchooling := 11]
                
                # Teto para o Superior: 15 anos de estudo
                CensusData[yearOfSchooling > 15 & v213 == 5, yearOfSchooling := 15]
                
                CensusData[ , anosGraus_tmp  := NULL]
                CensusData[ , anosSeries_tmp := NULL]
                
        }
        
        minCol <- function(...){
                apply(cbind(...), 1, min, na.rm = T)
        }
        
        if(year == 1970 ){ 
                CensusData[educationAttainment==8, yearOfSchooling := minCol(11 + v037 - 1, 14)]
                CensusData[educationAttainment==6, yearOfSchooling := minCol(8 + v037 - 1, 10)]
                CensusData[educationAttainment==4, yearOfSchooling := minCol(4 + v037 - 1, 7)]
                CensusData[educationAttainment==2, yearOfSchooling := minCol(v037 - 1, 3)]
        }
        if(year == 1980 ){ 
                CensusData[educationAttainment==8 & v521==8 & v520<9,                 yearOfSchooling := minCol(11 + v520 - 1, 14)]
                CensusData[educationAttainment==8 & v524==7 & v523<9,                 yearOfSchooling := minCol(11 + v523, 14)]
                CensusData[educationAttainment==6 & v521 %in% c(4,5,7) & v520<9,      yearOfSchooling := minCol(8 + v520 - 1, 10)]
                CensusData[educationAttainment==6 & v524 %in% c(5,6) & v523<9,        yearOfSchooling := minCol(8 + v523, 10)]
                CensusData[educationAttainment==4 & v520 %in% 5:8 & v521 %in% c(3,6), yearOfSchooling := minCol(v520 - 1, 7)]
                CensusData[educationAttainment==4 & v521==2 & v520<9,                 yearOfSchooling := minCol(4 + v520 - 1, 7)]
                CensusData[educationAttainment==4 & v524==4 & v523<9,                 yearOfSchooling := v523]
                CensusData[educationAttainment==4 & v524==3 & v523<4,                 yearOfSchooling := 4 + v523]
                CensusData[educationAttainment==2 & v521 %in% c(1,3,6) & v520<9,      yearOfSchooling := minCol(v520 - 1, 3)]
                CensusData[educationAttainment==2 & v524 %in% c(2,4) & v523<4,        yearOfSchooling := v523]
        }
        if(year == 1991 ){ 
                CensusData[educationAttainment==8 & v0325==3,                            yearOfSchooling := minCol(11 + v0324 -1, 14)]
                CensusData[educationAttainment==8 & v0328==7,                            yearOfSchooling := minCol(11+ v0327, 14)]
                CensusData[educationAttainment==6 & v0325  %in% c(2,5),                  yearOfSchooling := minCol(8 + v0324 - 1, 10)]
                CensusData[educationAttainment==6 & v0328  %in% c(5,6),                  yearOfSchooling := minCol(8 + v0327, 10)]
                CensusData[educationAttainment==4 & v0325  %in% c(1,4) & v0324 %in% 5:8, yearOfSchooling := minCol(v0324 - 1, 7)]
                CensusData[educationAttainment==4 & v0328==4 & v0327 %in% 5:8,           yearOfSchooling := minCol(v0327, 7)]
                CensusData[educationAttainment==4 & v0328==3 & v0327 %in% 1:3,           yearOfSchooling := minCol(4 + v0327, 7)]
                CensusData[educationAttainment==2 & v0325  %in% c(1,4) & v0324<5,        yearOfSchooling := v0324 - 1]
                CensusData[educationAttainment==2 & v0328  %in% c(2,4) & v0327<5,        yearOfSchooling := v0327]
        }
        if(year == 2000 ){ 
                CensusData[educationAttainment==8 & v0430==12,                       yearOfSchooling := minCol(11 + v0431 - 1, 14)]
                CensusData[educationAttainment==8 & v0432==7,                        yearOfSchooling := minCol(11 + v0433, 14)]
                CensusData[educationAttainment==6 & v0430  %in% c(8,9,10) & v0431<9, yearOfSchooling := minCol(8 + v0431 - 1, 10)]
                CensusData[educationAttainment==6 & v0432  %in% c(4,6) & v0433<9,    yearOfSchooling := minCol(8 + v0433, 10)]
                CensusData[educationAttainment==4 & v0430  %in% c(5,6,7) & v0431<9,  yearOfSchooling := minCol(v0431 - 1, 7)]
                CensusData[educationAttainment==4 & v0432==3 & v0433<9,              yearOfSchooling := minCol(4 + v0433, 7)]
                CensusData[educationAttainment==4 & v0432==5 & v0433<9	,          yearOfSchooling := minCol(v0433, 7)]
                CensusData[educationAttainment==2 & v0430  %in% c(5,6,7) & v0431<9,  yearOfSchooling := minCol(v0431 - 1, 3)]
                CensusData[educationAttainment==2 & v0432  %in% c(2,5) & v0433<9,    yearOfSchooling := minCol(v0433, 3)]
        }
        if(year == 2010 ){
                CensusData[educationAttainment==6 & v0629==7 & v0631 %in% 1:4, yearOfSchooling := minCol(8 + v0631 - 1, 10)]
                CensusData[educationAttainment==4 & v0630 %in% 6:9,            yearOfSchooling := minCol(v0630 - 2, 7)]
                CensusData[educationAttainment==2 & v0629==5 & v0630 %in% 2:5, yearOfSchooling := minCol(v0630 - 2, 3)]
                CensusData[educationAttainment==2 & v0629==5 & v0630==1,       yearOfSchooling := 0]
                
        }
        gc();Sys.sleep(.5);gc()
        
        CensusData[educationAttainment==8 & is.na(yearOfSchooling), yearOfSchooling := 12.5]	
        CensusData[educationAttainment==6 & is.na(yearOfSchooling), yearOfSchooling := 9.5]
        CensusData[educationAttainment==4 & is.na(yearOfSchooling), yearOfSchooling := 5.5]
        CensusData[educationAttainment==2 & is.na(yearOfSchooling), yearOfSchooling := 1.5]
        
        if(educationAttainment_just_created == TRUE){
                CensusData[ , educationAttainment := NULL]
                gc();Sys.sleep(.5);gc()
        }
        
        gc();Sys.sleep(.5);gc()
        CensusData
}

