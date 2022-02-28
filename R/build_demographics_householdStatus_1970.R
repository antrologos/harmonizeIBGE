#' Builds household dweller status for 1970
#' @param data.frame
#' @value data.frame
#' @export

build_demographics_householdStatus_1970 <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        check_vars <- check_var_existence(CensusData, c("v025"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData[ v025 <= 3 , householdStatus := v025 ]
        CensusData[ v025 >  3 , householdStatus := 4 ]
        
        gc()
        
        CensusData
}

