#' Builds a synthetic variable for age - 1970
#' @param data.frame
#' @value data.frame
#' @export

build_household_hhType <- function(CensusData){ 
        
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
                CensusData[v101 %in% c(1,2,4,5) & v102 %in% c(4,5),                hhType := 0]
                CensusData[v101 %in% c(1,2,4,5) & (v102 %in% c(6,7)| is.na(v102)), hhType := 1]
                CensusData[v101 ==3                                              , hhType := 2]
        }
        
        
        if(year ==1970) { 
                CensusData[, hhType := v008]
                
                CensusData[(v007 == 0 & (hhType == 0 | hhType == 1)),   hhType := 0] 
                CensusData[(v007 == 0 & (hhType == 2 | is.na(hhType))), hhType := 1] 
                CensusData[v006 == 0,                                   hhType := 1] 
                CensusData[v007 == 1,                                   hhType := 2] 
        } 
        
        if(year ==1980){
                CensusData[v201 == 1,         hhType := 0]
                CensusData[v201 == 3,         hhType := 1]
                CensusData[v201 %in% c(5,7) , hhType := 2]
        } 
        
        if(year ==1991){
                CensusData[v0201 == 1,  hhType := 0]
                CensusData[v0201 == 2,  hhType := 1]
                CensusData[v0201 == 3,  hhType := 2]
        } 
        
        if(year ==2000){
                CensusData[v0201 == 1,  hhType := 0]
                CensusData[v0201 == 2,  hhType := 1]
                CensusData[v0201 == 3,  hhType := 2]
        } 
        
        if(year ==2010){
                CensusData[v4001 %in% c(1,2),  hhType := 0]
                CensusData[v4001 == 5,  hhType := 1]
                CensusData[v4001 == 6,  hhType := 2]
        } 
        
        
        # 0 "private permanent" 
        # 1 "private improvised" 
        # 2 "collective dwelling"
        
        CensusData
        
} 

