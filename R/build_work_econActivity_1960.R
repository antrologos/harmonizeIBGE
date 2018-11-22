#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_work_econActivity_1960 <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        check_vars <- check_var_existence(CensusData, c("v220","v223", "age"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData[, econActivity := as.numeric(NA)]
        
        CensusData[v220 %in% c(0, 1, 4:9), econActivity := 0]     # nao inclui 2 = ignorado e 3 = prejudicado
        CensusData[v223 %in% 2:4    ,      econActivity := 1]
        
        gc()
        
        CensusData[age < 10, econActivity := NA]
        
        CensusData
}
