#' Builds a synthetic variable for age - 1970
#' @param data.frame
#' @value data.frame
#' @export

build_household_publicWater <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizeIBGE:::get_metadata(CensusData)
        
        year = metadata$year
        
        if(year ==1960) { 
                CensusData[, publicWater := as.numeric(v105 %in% c(1))]
        }
        
        
        if(year ==1970) { 
                CensusData[, publicWater := as.numeric(v012 %in% c(1,2))]
        } 
        
        if(year ==1980){
                CensusData[, publicWater := as.numeric(v206 %in% c(1,6))]
        } 
        
        if(year ==1991){
                CensusData[, publicWater := as.numeric(v0205 %in% c(1,4))]
        } 
        
        if(year ==2000){
                CensusData[, publicWater := as.numeric(v0207 %in% c(1))]
        } 
        
        if(year ==2010){
                CensusData[, publicWater := as.numeric(v0208 %in% c(1))]
        } 
        
        CensusData
        
} 

