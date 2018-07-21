#' Builds a synthetic variable for age - 1970
#' @param data.frame
#' @value data.frame
#' @export


build_househould_househouldParticular_1970 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        check_vars <- check_var_existence(CensusData, c("v006", "v007"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData[v007 == 0 ,  househouldParticular := 1]
        CensusData[v007 == 1 ,  househouldParticular := 0]
        CensusData[is.na(v007), househouldParticular := 1]
        gc()
        
        CensusData
        
}

