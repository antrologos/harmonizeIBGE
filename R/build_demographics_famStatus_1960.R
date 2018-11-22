#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_demographics_famStatus_1960 <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        check_vars <- check_var_existence(CensusData, c("v203"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData[v203 %in% 7:9,  famStatus := v203 - 6]
        CensusData[v203 == 6,      famStatus := 1]
        CensusData[v203 %in% 0:4,  famStatus := 4]
        
        gc()
        
        CensusData
}
