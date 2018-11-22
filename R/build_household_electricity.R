#' Builds a synthetic variable for age - 1970
#' @param data.frame
#' @value data.frame
#' @export

build_household_electricity <- function(CensusData){ 
        
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
                CensusData[, electricity := as.numeric(v108 %in% c(5))]
        }
        
        if(year ==1970) { 
                CensusData[, electricity := as.numeric(v014 %in% c(1))]
        } 
        
        if(year ==1980){
                CensusData[, electricity := as.numeric(v217 %in% c(2,4))]
        } 
        
        if(year ==1991){
                CensusData[, electricity := as.numeric(v0221 %in% c(1,2))]
        } 
        
        if(year ==2000){
                CensusData[, electricity := as.numeric(v0213 %in% c(1))]
        } 
        
        if(year ==2010){
                CensusData[, electricity := as.numeric(v0211 %in% c(1, 2))]
        } 
        
        CensusData
        
} 

