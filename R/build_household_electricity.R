#' Builds a synthetic variable for age - 1970
#' @param data.frame
#' @value data.frame
#' @export

build_household_electricity <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizeIBGE:::get_metadata(CensusData)
        
        year = metadata$year
        
        if(year ==1960) { 
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v108"))
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }        
                
                CensusData[, electricity := as.numeric(v108 %in% c(5))]
        }
        
        if(year ==1970) { 
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v014"))
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }  
                CensusData[, electricity := as.numeric(v014 %in% c(1))]
        } 
        
        if(year ==1980){
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v217"))
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }  
                CensusData[, electricity := as.numeric(v217 %in% c(2,4))]
        } 
        
        if(year ==1991){
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v0221"))
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }  
                CensusData[, electricity := as.numeric(v0221 %in% c(1,2))]
        } 
        
        if(year ==2000){
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v0213"))
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }  
                CensusData[, electricity := as.numeric(v0213 %in% c(1))]
        } 
        
        if(year ==2010){
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v0211"))
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }  
                CensusData[, electricity := as.numeric(v0211 %in% c(1, 2))]
        } 
        
        CensusData
        
} 

