#' @export

build_work_hoursWorkedCategories <- function(CensusData){
        
        CensusData   <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata     <- harmonizeIBGE:::get_metadata(CensusData)

        type = metadata$type
        year = metadata$year
        
        if(type == "census" & year == 1980){
                CensusData[ , hoursWorkedCategories := v535]
                CensusData[hoursWorkedCategories == 9, hoursWorkedCategories := NA]
        }
        
        if(type == "census" & year == 1991){
                CensusData <- CensusData[order(v0354)]
                CensusData[ , hoursWorkedCategories := as.numeric(cut(v0354, breaks = c(1,14,29,39,48,500), include.lowest = T))]
        }
        
        if(type == "census" & year == 2000){
                CensusData <- CensusData[order(v0453)]
                CensusData[ , hoursWorkedCategories := as.numeric(cut(v0453, breaks = c(1,14,29,39,48,500), include.lowest = T))]
        }
        
        if(type == "census" & year == 2010){
                CensusData <- CensusData[order(v0653)]
                CensusData[ , hoursWorkedCategories := as.numeric(cut(v0653, breaks = c(1,14,29,39,48,500), include.lowest = T))]
        }
        
        CensusData <- harmonizeIBGE:::set_metadata(CensusData, metadata = metadata)
        
        gc();Sys.sleep(.5);gc()
               
        CensusData
}


