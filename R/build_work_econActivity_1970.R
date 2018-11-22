#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_work_econActivity_1970 <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        check_vars <- check_var_existence(CensusData, c("v043","v047", "age"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData[, econActivity := as.numeric(NA)]
        
        CensusData[ v043 %in% 0:6 , econActivity := 0]
        CensusData[ v047 %in% 1:5 , econActivity := 1]
        
        CensusData[age < 10, econActivity := NA]
        gc()
        
        CensusData
}
