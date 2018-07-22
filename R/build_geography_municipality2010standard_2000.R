#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_geography_municipality2010standard_2000 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        municipalityCurrent_just_created = F
        check_vars <- check_var_existence(CensusData, c("municipalityCurrent"))
        if(length(check_vars) > 0){
                CensusData <- build_geography_municipalityCurrent_2000(CensusData)
                municipalityCurrent_just_created = T
                gc()
        }
        
        CensusData[ , municipality2010standard := municipalityCurrent]
        
        if(municipalityCurrent_just_created == T){
                CensusData[ , municipalityCurrent := NULL]
        }
        
        n_digit <- nchar(min(CensusData[ , municipality2010standard]))
        if(n_digit == 7){
                CensusData[ , municipality2010standard := trunc(municipality2010standard/10)]
        }
        
        gc()
        CensusData
}