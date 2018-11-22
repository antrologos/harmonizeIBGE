#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_work_occupationalStatus_1991 <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        check_vars <- check_var_existence(CensusData, c("v0345","age"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData[, occupationalStatus := as.numeric(NA)]
        
        CensusData[ v0345 %in% 1:2,              occupationalStatus := 1]
        CensusData[ v0345 == 3 & v0358 %in% 1:2, occupationalStatus := 0]
        
        CensusData[age < 10, occupationalStatus    := NA]
        gc()
        
        CensusData
}
