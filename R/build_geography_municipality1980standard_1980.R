#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_geography_municipality1980standard_1980 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        check_vars <- check_var_existence(CensusData, c(state_var_name, "v2", "v5"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        
        CensusData[ , municipality2010 := v2*10^4 + v5]
        
        gc()
        CensusData
}

