#' Harmonize a set of income variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_income_totalIncome2010Values <- function(CensusData){
        
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        
        year = metadata$year
        
        check_vars <- check_var_existence(CensusData, c("age"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        
        if(year == 1960){
                CensusData[, totalIncome2010Values := as.numeric(NA)]
        }
        
        if(year == 1970){
                check_vars <- check_var_existence(CensusData, "v041")
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[,             totalIncome2010Values := as.numeric(NA)]
                CensusData[v041 == 9999, totalIncome2010Values := 0]
                CensusData[v041 < 9998,  totalIncome2010Values := v041/0.416904849]
                
                gc(); Sys.sleep(1); gc()
        }
        
        if(year == 1980){
                varlist = c("v607", "v608", "v609", "v610", "v611", "v612", "v613")
                
                check_vars <- check_var_existence(CensusData, varlist)
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                for(var in varlist){
                        print(var)
                        CensusData[[var]][is.na(CensusData[[var]])] <- 0
                        CensusData[[var]][CensusData[[var]] == 9999999] <- NA
                }
                gc(); Sys.sleep(1); gc()
                
                income_matrix <-
                        CensusData[, (varlist), with=F] %>%
                        as.matrix()
                gc(); Sys.sleep(1); gc()
                
                missing_values <- apply(income_matrix, 1, function(x) all(is.na(x)))
                income <-  rowSums(income_matrix)
                
                rm(income_matrix)
                gc(); Sys.sleep(1); gc()
                
                income[missing_values] <- NA
                gc(); Sys.sleep(1); gc()
                
                CensusData[ , `:=`("totalIncome2010Values", income/9.091145084)]
                gc(); Sys.sleep(1); gc()
        }
        
        if(year == 1991){
                
                check_vars <- check_var_existence(CensusData, "v3561")
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[                  , totalIncome2010Values := v3561]
                CensusData[v3561 == 99999999 , totalIncome2010Values := NA] 
                CensusData[                  , totalIncome2010Values := totalIncome2010Values/106.3629622]
                
                gc(); Sys.sleep(1); gc()
        }
        
        
        if(year == 2000){
                
                check_vars <- check_var_existence(CensusData, "v4614")
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[ , totalIncome2010Values := v4614/0.512271399]
                gc(); Sys.sleep(1); gc()
        }
        
        
        if(year == 2010){
                
                check_vars <- check_var_existence(CensusData, "v6527")
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[ , totalIncome2010Values := v6527]
                gc(); Sys.sleep(1); gc()
        }
        gc(); Sys.sleep(1); gc()
        
        CensusData[age  < 10 , totalIncome2010Values := NA]
        
        CensusData
        
}
