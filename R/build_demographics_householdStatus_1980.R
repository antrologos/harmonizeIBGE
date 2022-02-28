#' Builds household dweller status for 1980
#' @param data.frame
#' @value data.frame
#' @export

build_demographics_householdStatus_1980 <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        check_vars <- check_var_existence(CensusData, c("v503"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData[ v503 <= 3 , householdStatus := v503 ]
        CensusData[ v503 >  3 , householdStatus := 4 ]
        CensusData[ v503 == 0 , householdStatus := 1 ]
        
        
        gc();Sys.sleep(.5);gc()
        CensusData
}



