#' Builds a synthetic variable for age - 1970
#' @param data.frame
#' @value data.frame
#' @export

build_household_garbageCollection <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizeIBGE:::get_metadata(CensusData)
        
        year = metadata$year

        if(year ==1991){
                CensusData[, garbageCollection := as.numeric(v0214 %in% c(1,2))]
        } 
        
        if(year ==2000){
                CensusData[, garbageCollection := as.numeric(v0212 %in% c(1,2))]
        } 
        
        if(year ==2010){
                CensusData[, garbageCollection := as.numeric(v0210 %in% c(1, 2))]
        } 
        
        CensusData
        
} 

