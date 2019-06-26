#' Harmonize a set of income variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_income_labourIncome2010Values <- function(CensusData){
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        
        year = metadata$year
        
        if(year %in% c(1960,1970)){
                return(CensusData)
        }
        
        age_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("age"))
        if(length(check_vars) > 0){
                CensusData <- eval(parse(text = paste0("build_demographics_age_",metadata$year,"(CensusData)")))
                age_just_created = T
                gc();Sys.sleep(.5);gc()
        }
        
        occupationalStatus_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("occupationalStatus"))
        if(length(check_vars) > 0){
                CensusData <- eval(parse(text = paste0("build_work_occupationalStatus_",metadata$year,"(CensusData)")))
                occupationalStatus_just_created = T
                gc();Sys.sleep(.5);gc()
        }
        
        if(year == 1980){
                
                varlist = c("v607", "v608","v609")
                
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, varlist)
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                identify_NA = function(x){
                        x == 9999999
                }
                
                replace_NA_and_zeros = function(x){
                        x[is.na(x)] <- 0
                        x[x == 9999999] <- NA
                        x
                }
                
                colNumbers = which(names(CensusData) %in% varlist)
                
                CensusData[ , income_NAs := rowSums(
                        CensusData[, lapply(.SD, identify_NA), .SDcols=colNumbers], 
                        na.rm = TRUE)]
                gc(); Sys.sleep(1); gc()
                
                CensusData[ , labourIncome2010Values := rowSums(
                        CensusData[, lapply(.SD, replace_NA_and_zeros), .SDcols=colNumbers], 
                        na.rm = TRUE)/9.0911450843074]
                gc(); Sys.sleep(1); gc()
                
                CensusData[income_NAs == length(colNumbers), labourIncome2010Values := NA ]
                gc(); Sys.sleep(.5); gc()
                
                CensusData[, income_NAs := NULL]
                gc(); Sys.sleep(.5); gc()
        }
        
        if(year == 1991){
                
                varlist = c("v0356", "v0357")
                
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, varlist)
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                identify_NA = function(x){
                        x == 9999999
                }
                
                replace_NA_and_zeros = function(x){
                        x[is.na(x)] <- 0
                        x[x == 9999999] <- NA
                        x
                }
                
                colNumbers = which(names(CensusData) %in% varlist)
                
                CensusData[ , income_NAs := rowSums(
                        CensusData[, lapply(.SD, identify_NA), .SDcols=colNumbers], 
                        na.rm = TRUE)]
                gc(); Sys.sleep(1); gc()
                
                CensusData[ , labourIncome2010Values := rowSums(
                        CensusData[, lapply(.SD, replace_NA_and_zeros), .SDcols=colNumbers], 
                        na.rm = TRUE)/106.362962177897]
                gc(); Sys.sleep(1); gc()
                
                CensusData[income_NAs == length(colNumbers), labourIncome2010Values := NA ]
                gc(); Sys.sleep(.5); gc()
                
                CensusData[, income_NAs := NULL]
                gc(); Sys.sleep(.5); gc()

        }
        
        
        if(year == 2000){
                
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "v4525")
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[ , labourIncome2010Values := v4525/0.512271398527793]
                gc(); Sys.sleep(1); gc()
        }
        
        
        if(year == 2010){
                
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "v6525")
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[ , labourIncome2010Values := v6525]
                gc(); Sys.sleep(1); gc()
        }
        gc(); Sys.sleep(1); gc()
        
        CensusData[age  < 10 , labourIncome2010Values := NA]
        CensusData[is.na(occupationalStatus) | occupationalStatus == 0 , labourIncome2010Values := NA]
        
        if(age_just_created == T){
                CensusData[ , age := NULL]
                gc(); Sys.sleep(1); gc()
        }
        
        if(occupationalStatus_just_created == T){
                CensusData[ , occupationalStatus := NULL]
                gc(); Sys.sleep(1); gc()
        }
        
        gc(); Sys.sleep(1); gc()
        CensusData
}
