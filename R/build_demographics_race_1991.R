#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_demographics_race_1991 <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        check_vars <- check_var_existence(CensusData, c("v0309"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData[v0309 %in% c(1, 3), race := 1]
        CensusData[v0309 %in% c(4, 5), race := 2]
        CensusData[v0309 %in% c(2),    race := 3]
        gc()
        
        CensusData
        
        
}
