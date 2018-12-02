#' @export

build_education_fieldsIBGECode <- function(CensusData){
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        
        varList_location <- system.file("extdata",
                                        "list_of_originalVars.csv",
                                        package = "harmonizeIBGE")
        
        varList   <- read.csv2(varList_location, stringsAsFactors = F) %>%
                filter(year == metadata$year) %>%
                select(education_fieldsOfStudy) %>%
                as.character() %>%
                str_split(pattern = ";") %>%
                unlist() %>%
                tolower()
        
        if(metadata$year %in% c(1960, 1970)){
                return(CensusData)
        }
        
        harmonizeIBGE:::check_necessary_vars(CensusData, varList)

        
        if(metadata$year == 2010){
                CensusData[ , fieldsIBGECode := CensusData[ , varList[1], with = F ] ]
                CensusData[is.na(fieldsIBGECode), fieldsIBGECode := CensusData[is.na(fieldsIBGECode), varList[2], with = F ] ] 
                CensusData[is.na(fieldsIBGECode), fieldsIBGECode := CensusData[is.na(fieldsIBGECode), varList[3], with = F ] ] 
        }else{
                CensusData[ , fieldsIBGECode := CensusData[[varList]] ]        
        }
        
        gc(); Sys.sleep(.3);gc()
        
        setDT(CensusData)
        CensusData = harmonizeIBGE:::set_metadata(CensusData, metadata) #recovering metadata...
        
        gc();Sys.sleep(.5);gc()
        
        CensusData
}

