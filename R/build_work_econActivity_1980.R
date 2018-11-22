#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_work_econActivity_1980 <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        check_vars <- check_var_existence(CensusData, c("v529","v541", "age"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData[, econActivity := as.numeric(NA)]
        
        CensusData[ v529 %in% 3:9 , econActivity := 0]
        CensusData[ v541 %in% 1:4 , econActivity := 1]
        
        CensusData[age < 10, econ_active := NA]
        gc()
        
        CensusData
}
