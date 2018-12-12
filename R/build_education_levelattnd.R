#' @export

build_education_levelattnd <- function(CensusData){
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        
        year = metadata$year
        
        # Building age
        check_vars <- check_var_existence(CensusData, c("age"))
        age_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- eval(parse(text=paste0("build_demographics_age_",metadata$year,"(CensusData)")))
                age_just_created <- TRUE
                gc();Sys.sleep(.5);gc()
        }

        # Building educationAttainment
        check_vars <- check_var_existence(CensusData, c("schoolattnd"))
        schoolattnd_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- eval(parse(text=paste0("build_education_schoolattnd_",metadata$year,"(CensusData)")))
                schoolattnd_just_created <- TRUE
                gc();Sys.sleep(.5);gc()
        }
        
        
        # Building educationAttainment
        check_vars <- check_var_existence(CensusData, c("educationAttainment"))
        educationAttainment_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- eval(parse(text=paste0("build_education_educationAttainment_",metadata$year,"(CensusData)")))
                educationAttainment_just_created <- TRUE
                gc();Sys.sleep(.5);gc()
        }
        
        if(year == 1960){
                
                # Building educationAttainment
                check_vars <- check_var_existence(CensusData, c("yearsOfSchooling"))
                yearsOfSchooling_just_created <- FALSE
                if(length(check_vars) > 0) {
                        CensusData <- build_education_yearsOfSchooling(CensusData)
                        yearsOfSchooling_just_created <- TRUE
                        gc();Sys.sleep(.5);gc()
                }
                
                CensusData[schoolattnd == 1 & yearOfSchooling %in% 0:3,   levelattnd := 1]
                CensusData[schoolattnd == 1 & yearOfSchooling %in% 4:7,   levelattnd := 2]
                CensusData[schoolattnd == 1 & yearOfSchooling %in% 8:10,  levelattnd := 3]
                CensusData[schoolattnd == 1 & yearOfSchooling %in% 11:15, levelattnd := 4]
                CensusData[schoolattnd == 1 & is.na(yearOfSchooling),     levelattnd := 99]
                
                if(yearsOfSchooling_just_created == TRUE){
                        CensusData[ , yearsOfSchooling := NULL]
                        gc();Sys.sleep(.5);gc()
                }
        }
        

        if(year == 1970){
                
                check_vars <- check_var_existence(CensusData, c("v036"))
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[v036 != 1,                             levelattnd := 99]
                CensusData[v036 == 1 & educationAttainment == 1,  levelattnd := 99]
                CensusData[v036 == 1 & educationAttainment == 2,  levelattnd := 1]
                CensusData[v036 == 1 & educationAttainment == 3,  levelattnd := 2]
                CensusData[v036 == 1 & educationAttainment == 4,  levelattnd := 2]
                CensusData[v036 == 1 & educationAttainment == 5,  levelattnd := 3]
                CensusData[v036 == 1 & educationAttainment == 6,  levelattnd := 3]
                CensusData[v036 == 1 & educationAttainment == 7,  levelattnd := 4]
                CensusData[v036 == 1 & educationAttainment == 8,  levelattnd := 4]
                
                
                
        }
        
        
        if(year ==1980){
                
                check_vars <- check_var_existence(CensusData, c("v520", "v521"))
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[v521==0 | v520==0 | educationAttainment==1, levelattnd := 99]
                
                CensusData[v521==1 & v520 %in% c(1,2,3,4,9), levelattnd := 1]
                CensusData[v521==3 & v520 %in% c(1,2,3,4,9), levelattnd := 1]
                CensusData[v521==6 & v520 %in% c(1,2,3,4,9), levelattnd := 1]
                CensusData[v521==2 & v520 %in% c(1,2,3,4,9), levelattnd := 2]
                CensusData[v521==3 & v520 %in% c(5,6,7,8),   levelattnd := 2]
                CensusData[v521==6 & v520 %in% c(5,6,7,8),   levelattnd := 2]
                CensusData[v521==4 & v520 %in% c(1,2,3,4,9), levelattnd := 3]
                CensusData[v521==5 & v520 %in% c(1,2,3,4,9), levelattnd := 3]
                CensusData[v521==7 & v520 %in% c(1,2,3,9) ,  levelattnd := 3]
                CensusData[v521==8,                          levelattnd := 4]
        }
        
        
        if(year ==1991){
                
                check_vars <- check_var_existence(CensusData, c("v0324", "v0325", "v0326"))
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[schoolattnd!=1,                      levelattnd := 99] 
                CensusData[v0326 %in% c(1,2),                   levelattnd := 99] 
                CensusData[v0325 %in% c(1,4) & v0324 %in% 1:4,  levelattnd := 1] 
                CensusData[v0326==3,                            levelattnd := 1] 
                CensusData[v0325 %in% c(1,4) & v0324 %in% 5:8,  levelattnd := 2] 
                CensusData[v0325 %in% c(2,5) & v0324 %in% 1:4,  levelattnd := 3] 
                CensusData[v0326 %in% c(4,5),                   levelattnd := 3] 
                CensusData[v0325==3 | v0326==6,                 levelattnd := 4] 
        }	
        
        if(year ==2000){
                
                check_vars <- check_var_existence(CensusData, c("v0430", "v0431", "v4752"))
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[schoolattnd!=1,                        levelattnd := 99] 
                CensusData[v0430 %in% 1:4,                        levelattnd := 99] 
                CensusData[v0430 %in% 5:7 & v0431 %in% 1:4 ,      levelattnd := 1] 
                CensusData[v0430 == 6 & v0431 == 9 & v4752 <= 10, levelattnd := 1] 
                CensusData[v0430 == 7 & v0431 == 9 ,              levelattnd := 1] 
                CensusData[v0430 %in% 5:7 & v0431 %in% 5:8 ,      levelattnd := 2] 
                CensusData[v0430 == 6 & v0431 == 9 & v4752 >= 11, levelattnd := 2] 
                CensusData[v0430 %in% 8:10,                       levelattnd := 3] 
                CensusData[v0430 == 11 ,                          levelattnd := 3] 
                CensusData[v0430 %in% c(12,13),                   levelattnd := 4]
        }
        
        if(year ==2010){
                
                check_vars <- check_var_existence(CensusData, c("v0629", "v0630", "v6036"))
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[schoolattnd!=1,                          levelattnd := 99]
                CensusData[v0629 %in% 1:4,                          levelattnd := 99]
                CensusData[v0629 == 5 & v0630 %in% 1:5,             levelattnd := 1]
                CensusData[v0629 == 6,                              levelattnd := 1]
                CensusData[v0629 == 5 & v0630 == 10 & v6036 <= 10,  levelattnd := 1]
                CensusData[v0629 == 5 & v0630 %in% 6:9,             levelattnd := 2]
                CensusData[v0629 == 5 & v0630 == 10 & v6036 >= 11,  levelattnd := 2]
                CensusData[v0629 %in% 7:8,                          levelattnd := 3]
                CensusData[v0629 %in% 9:12,                         levelattnd := 4]
        }
        
        CensusData[age < 5  ,  levelattnd := as.numeric(NA)]
        
        if(educationAttainment_just_created == TRUE){
                CensusData[ , educationAttainment := NULL]
                gc();Sys.sleep(.5);gc()
        }
        
        if(schoolattnd_just_created == TRUE){
                CensusData[ , schoolattnd := NULL]
                gc();Sys.sleep(.5);gc()
        }
        
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
                gc();Sys.sleep(.5);gc()
        }
        
        # level currently attending (ages 5=+)
        # 1 "Primary school" 
        # 2 "Middle school" 
        # 3 "High school" 
        # 4 "Higher educationAttainment" 
        # 99 "Out of school"

        gc();Sys.sleep(.5);gc()
        CensusData
}
