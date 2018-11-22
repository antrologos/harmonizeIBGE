#' Builds a synthetic variable for age - 1970
#' @param data.frame
#' @value data.frame
#' @export

build_household_properSewage <- function(CensusData){ 
        
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
                CensusData[, properSewage := as.numeric(v106 %in% c(4,5))]
        }
        
        
        if(year ==1970) { 
                CensusData[, properSewage := as.numeric(v013 %in% c(1,2))]
        } 
        
        if(year ==1980){
                CensusData[, properSewage := as.numeric(v207 %in% c(2,4))]
        } 
        
        if(year ==1991){
                CensusData[, properSewage := as.numeric(v0206 %in% c(1,2,3))]
        } 
        
        if(year ==2000){
                CensusData[, properSewage := as.numeric(v0211 %in% c(1,2))]
        } 
        
        if(year ==2010){
                CensusData[, properSewage := as.numeric(v0207 %in% c(1,2))]
        } 
        
        CensusData
        
} 
