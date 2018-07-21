#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_geography_municipality2010standard_1991 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        municipality1991standard_just_created = F
        check_vars <- check_var_existence(CensusData, c("municipality1991standard"))
        if(length(check_vars) > 0){
                CensusData <- build_geography_municipality1991standard_1991(CensusData)
                municipality1991standard_just_created = T
                gc()
        }
        
        CensusData[ , municipality2010standard := municipality1991standard]
        
        n_digit <- nchar(min(CensusData[ , municipality2010standard]))
        if(n_digit == 7){
                CensusData[ , municipality2010standard := trunc(municipality2010standard/10)]
        }
        
        if(municipality1991standard_just_created == T){
                CensusData[ , municipality1991standard := NULL]
        }
        
        gc()
        CensusData
}