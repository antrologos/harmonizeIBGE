#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_work_occupationalStatus_1980 <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        check_vars <- check_var_existence(CensusData, c("v541","age"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData[, occupationalStatus := as.numeric(NA)]
        
        CensusData[ v541 %in% 4   , occupationalStatus := 0]
        CensusData[ v541 %in% 1:3 , occupationalStatus := 1]
        
        CensusData[age < 10, occupationalStatus    := NA]
        gc()
        
        CensusData
}
