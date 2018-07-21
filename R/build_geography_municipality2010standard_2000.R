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
        
        municipality2000standard_just_created = F
        check_vars <- check_var_existence(CensusData, c("municipality2000standard"))
        if(length(check_vars) > 0){
                CensusData <- build_geography_municipality2000standard_2000(CensusData)
                municipality2000standard_just_created = T
                gc()
        }
        
        CensusData[ , municipality2010standard := municipality2000standard]
        
        if(municipality2000standard_just_created == T){
                CensusData[ , municipality2000standard := NULL]
        }
        
        gc()
        CensusData
}