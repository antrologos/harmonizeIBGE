#' @export

build_geography_ruralUrban <- function(CensusData){
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        
        varList_location <- system.file("extdata",
                                        "list_of_originalVars.csv",
                                        package = "harmonizeIBGE")
        
        varList   <- read.csv2(varList_location, stringsAsFactors = F) %>%
                filter(year == metadata$year) %>%
                select(geography_ruralUrban) %>%
                as.character() %>%
                str_split(pattern = ";") %>%
                unlist() %>%
                tolower()
        
        harmonizeIBGE:::check_necessary_vars(CensusData, varList)
        CensusData[ , ruralUrban := as.numeric(0)]

        year = metadata$year
        if(year == 1960){
                CensusData[v118_pess %in% c(1,3) , ruralUrban := 1]
        }
        
        if(year == 1970){
                CensusData[v004 %in% c(0,1), ruralUrban := 1]
        }
        
        if(year == 1980){
                CensusData[v598 == 0, ruralUrban := 1]
        }
        
        if(year == 1991){
                CensusData[v1061 %in% 1:3, ruralUrban := 1]
        }
        
        
        if(year == 2000){
                CensusData[v1006 == 1, ruralUrban := 1]
        }
        
        if(year == 2010){
                CensusData[v1006 == 1, ruralUrban := 1]
        }
        
        setDT(CensusData)
        CensusData = harmonizeIBGE:::set_metadata(CensusData, metadata) #recovering metadata...
        
        gc();Sys.sleep(.5);gc()
        
        CensusData
}

