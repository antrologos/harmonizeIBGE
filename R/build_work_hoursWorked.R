#' @export

build_work_hoursWorked <- function(CensusData){
        
        CensusData   <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata     <- harmonizeIBGE:::get_metadata(CensusData)
        
        type = metadata$type
        year = metadata$year
        
        if(type == "census" & year == 1991){
                CensusData[ , hoursWorked := v0354]
        }
        
        if(type == "census" & year == 2000){
                CensusData[ , hoursWorked := v0453]
        }
        
        if(type == "census" & year == 2010){
                CensusData[ , hoursWorked := v0653]
        }
        
        CensusData <- harmonizeIBGE:::set_metadata(CensusData, metadata = metadata)
        
        gc();Sys.sleep(.5);gc()
        
        CensusData
}


