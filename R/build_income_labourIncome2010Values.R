#' Harmonize a set of income variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_income_labourIncome2010Values <- function(CensusData){
        
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        Data     <- harmonizeIBGE:::check_prepared_to_harmonize(Data)
        metadata <- harmonizeIBGE:::get_metadata(Data)
        
        year == metadata$year
        
        check_vars <- check_var_existence(CensusData, c("age"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        
        if(year == 1960){
                CensusData[, labourIncome2010Values := as.numeric(NA)]
        }
        
        if(year == 1970){
                CensusData[, labourIncome2010Values := as.numeric(NA)]
        }
        
        if(year == 1980){
                
                check_vars <- check_var_existence(CensusData, "v607")
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
        
                CensusData[ ,                labourIncome2010Values := v607]
                CensusData[ is.na(v607),     labourIncome2010Values := 0   ]
                CensusData[ v607 == 9999999, labourIncome2010Values := NA  ]
                
                CensusData[ , labourIncome2010Values := labourIncome2010Values/9.091145084]
                gc(); Sys.sleep(1); gc()
        }
        
        if(year == 1991){
                
                check_vars <- check_var_existence(CensusData, "v0356")
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[                  , labourIncome2010Values := v0356]
                CensusData[v0356 == 99999999 , labourIncome2010Values := NA] 
                CensusData[                  , labourIncome2010Values := labourIncome2010Values/106.3629622]
                gc(); Sys.sleep(1); gc()
        }
        
        
        if(year == 2000){
                
                check_vars <- check_var_existence(CensusData, "v4512")
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[ , labourIncome2010Values := v4512/0.512271399]
                gc(); Sys.sleep(1); gc()
        }
        
        
        if(year == 2010){
                
                check_vars <- check_var_existence(CensusData, "v6513")
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
                
                CensusData[ , labourIncome2010Values := v6513]
                gc(); Sys.sleep(1); gc()
        }
        gc(); Sys.sleep(1); gc()
        
        CensusData[age  < 10 , labourIncome2010Values := NA]
        
        CensusData
}
