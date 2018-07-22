#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_geography_stateMiniumComparable_1970 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        stateCurrent_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("stateCurrent"))
        if(length(check_vars) > 0){
                CensusData <- build_geography_stateCurrent_1970(CensusData)
                stateCurrent_just_created = T
                gc()
        }
        
        CensusData[ , stateMiniumComparable := stateCurrent]
        
        CensusData[stateCurrent == 20, stateMiniumComparable := 26]
        CensusData[stateCurrent == 34, stateMiniumComparable := 33]
        CensusData[stateCurrent == 17, stateMiniumComparable := 52]
        CensusData[stateCurrent == 50, stateMiniumComparable := 51]
        
        gc()
        
        if(stateCurrent_just_created == TRUE){
                CensusData[ , stateCurrent := NULL]
        }
        
        gc()
        CensusData
}