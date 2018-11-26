#' Builds a synthetic variable for year - 1980
#' @param data.frame
#' @value data.frame
#' @export

build_identification_year <- function(CensusData){
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        
        CensusData[ , year := harmonizeIBGE:::get_metadata(CensusData)$year]
        
        gc()
        CensusData
}
