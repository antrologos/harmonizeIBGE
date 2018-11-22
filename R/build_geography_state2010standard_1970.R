#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_geography_state2010standard_1970 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        municipality2010standard_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("municipality2010standard"))
        if(length(check_vars) > 0){
                CensusData <- build_geography_municipality2010standard_1970(CensusData)
                municipality2010standard_just_created = T
                gc()
        }
        
        CensusData[ , state2010standard := trunc(municipality2010standard/10000)]
        
        if(municipality2010standard_just_created == TRUE){
                CensusData[ , municipality2010standard := NULL]
        }
        
        gc()
        CensusData
}
