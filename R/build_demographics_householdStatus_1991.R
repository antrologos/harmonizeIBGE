#' Builds household dweller status for 1991
#' @param data.frame
#' @value data.frame
#' @export

build_demographics_householdStatus_1991 <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        check_vars <- check_var_existence(CensusData, c("v0302"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData[ v0302 <= 3      , householdStatus := v0302 ]
        CensusData[ v0302 == 20     , householdStatus := 1 ]
        CensusData[ v0302 %in% 4:19 , householdStatus := 4 ]
        
        gc()
        
        CensusData
}


