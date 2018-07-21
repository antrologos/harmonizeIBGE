#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_geography_municipality2010standard_2010 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        check_vars <- check_var_existence(CensusData, c(state_var_name, "v0001", "v0002"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        CensusData[ , municipality2010standard := v0001*10^5 + v0002]
        
        n_digit <- nchar(min(CensusData[ , municipality2010standard]))
        if(n_digit == 7){
                CensusData[ , municipality2010standard := trunc(municipality2010standard/10)]
        }
        
        gc()
        CensusData
}

