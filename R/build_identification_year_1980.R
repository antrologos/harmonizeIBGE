#' Builds a synthetic variable for year - 1980
#' @param data.frame
#' @value data.frame
#' @export

build_identification_year_1980 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        # year
        CensusData[ , year := 1980]
        
        CensusData
}