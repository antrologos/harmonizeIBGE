#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_demographics_male_2010 <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        check_vars <- check_var_existence(CensusData, c("v0601"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData[, male := as.numeric(v0601 == 1)]
        
        CensusData
        
}