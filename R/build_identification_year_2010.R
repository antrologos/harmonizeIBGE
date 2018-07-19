#' Builds a synthetic variable for year - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_identification_year_2010 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        # year
        CensusData[ , year := 2010]
        
        CensusData
}