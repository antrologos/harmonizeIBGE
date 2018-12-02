#' Harmonize a set of income variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_income_totalIncome2010Values <- function(CensusData){
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        
        year = metadata$year
        
        age_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("age"))
        if(length(check_vars) > 0){
                CensusData <- eval(parse(text = paste0("build_demographics_age_", metadata$year, "(CensusData)")))
                age_just_created = T
                gc();Sys.sleep(.5);gc()
        }
        
        
        if(year == 1960){
                return(CensusData)
        }
        
        if(year == 1970){
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "v041")
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[,             totalIncome2010Values := as.numeric(NA)]
                CensusData[v041 == 9999, totalIncome2010Values := 0]
                CensusData[v041 < 9998,  totalIncome2010Values := v041/0.416904848930426]
                
                gc(); Sys.sleep(1); gc()
        }
        
        if(year == 1980){
                varlist = c("v607", "v608", "v609", "v610", "v611", "v612", "v613")
                
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
                
                CensusData[ , totalIncome2010Values := rowSums(
                        CensusData[, lapply(.SD, replace_NA_and_zeros), .SDcols=colNumbers], 
                        na.rm = TRUE)/9.0911450843074]
                gc(); Sys.sleep(1); gc()
                
                CensusData[income_NAs == length(colNumbers), totalIncome2010Values := NA ]
                gc(); Sys.sleep(.5); gc()
                
                CensusData[, income_NAs := NULL]
                gc(); Sys.sleep(.5); gc()
                
        }
        
        if(year == 1991){
                
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "v3561")
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[                  , totalIncome2010Values := v3561]
                CensusData[v3561 == 99999999 , totalIncome2010Values := NA] 
                CensusData[                  , totalIncome2010Values := totalIncome2010Values/106.362962177897]
                
                gc(); Sys.sleep(1); gc()
        }
        
        
        if(year == 2000){
                
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "v4614")
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[ , totalIncome2010Values := v4614/0.512271398527793]
                gc(); Sys.sleep(1); gc()
        }
        
        
        if(year == 2010){
                
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "v6527")
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[ , totalIncome2010Values := v6527]
                gc(); Sys.sleep(1); gc()
        }
        gc(); Sys.sleep(1); gc()
        
        
        CensusData[age  < 10 , totalIncome2010Values := NA]
        gc(); Sys.sleep(.5); gc()
        
        if(age_just_created == T){
                CensusData[ , age := NULL] 
        }
        
        gc(); Sys.sleep(.5); gc()
        
        CensusData
        
}
