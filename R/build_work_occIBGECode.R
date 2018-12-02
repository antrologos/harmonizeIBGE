#' @export

build_work_occIBGECode <- function(CensusData){
        
        CensusData     <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata <- harmonizeIBGE:::get_metadata(CensusData)
        
        varList_location <- system.file("extdata",
                                        "varList_occ.csv",
                                        package = "harmonizeIBGE")
        
        varList   <- read.csv2(varList_location, stringsAsFactors = F)
        
        if(metadata$type != "pnadc"){
                var_ocup <- varList %>%
                        filter(data == metadata$type & year == metadata$year) %>%
                        .$var_ocup %>% 
                        tolower() %>%
                        str_split(pattern = ";") %>%
                        unlist()
        }else{
                var_ocup <- varList %>%
                        filter(data == metadata$type) %>%
                        .$var_ocup %>% 
                        tolower() 
        }
        
        harmonizeIBGE:::check_necessary_vars(CensusData, c(var_ocup))
        
        if(metadata$year != 1980){
                CensusData[ , occIBGECode := CensusData[[var_ocup]] ]        
        }else{
                CensusData[ , occIBGECode := ifelse(!is.na(v542) & v542 != 0, v542, v530)]
                
        }
        
        gc();Sys.sleep(.5);gc()
        
        CensusData
}

