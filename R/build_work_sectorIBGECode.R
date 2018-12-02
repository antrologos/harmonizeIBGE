#' @export

build_work_sectorIBGECode <- function(CensusData){
        
        CensusData     <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata <- harmonizeIBGE:::get_metadata(CensusData)
        
        varList_location <- system.file("extdata",
                                        "varList_sector.csv",
                                        package = "harmonizeIBGE")
        
        varList   <- read.csv2(varList_location, stringsAsFactors = F)
        
        if(metadata$type != "pnadc"){
                varName <- varList %>%
                        filter(data == metadata$type & year == metadata$year) %>%
                        .$var_sector
        }else{
                varName <- varList %>%
                        filter(data == metadata$type) %>%
                        .$var_sector
        }
        
        harmonizeIBGE:::check_necessary_vars(CensusData, varName)
        
        CensusData[ , sectorIBGECode := CensusData[[varName]] ]
        
        gc();Sys.sleep(.5);gc()
        
        CensusData
}

