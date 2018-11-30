#' @export

CensusData <- c_1960
aggregated = T

build_education_fieldsOfStudy <- function(CensusData, aggregated = T){
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        
        just_created_vars_list_existedBefore <- exists(x = "just_created_vars", where = .GlobalEnv)
        
        crosswalk_location <- system.file("extdata",
                                          "crosswalk-fields-of-study.xlsx",
                                          package = "harmonizeIBGE")
        #crosswalk_location <- "C:/Users/Rogerio/desktop/crosswalk-fields-of-study.xlsx"
        
        varList_location <- system.file("extdata",
                                        "list_of_originalVars.csv",
                                        package = "harmonizeIBGE")
        #varList_location <- "C:/Users/Rogerio/Google Drive/RCodes/PacotesR/harmonizeIBGE/inst/extdata/list_of_originalVars.csv"
        
        varList   <- read.csv2(varList_location, stringsAsFactors = F) %>%
                filter(year == metadata$year) %>%
                select(education_fieldsOfStudy) %>%
                as.character() %>%
                str_split(pattern = ";") %>%
                unlist() %>%
                tolower()
        
        if(aggregated == T){
                crosswalk <- readxl::read_xlsx(crosswalk_location, sheet = "crosswalk_aggreg_1960_2010") %>%
                        filter(year == metadata$year, !is.na(isced_level3)) %>%
                        select(ibge_code, isced_level3, isced_level3_label_en) %>%
                        as.data.table()
                
        }else{
                
                if(metadata$year %in% c(1960, 1970)){
                        message("It is not possible to built disaggregated fields of study for 1960 and 1970")
                        return(CensusData)
                }
                
                crosswalk <- readxl::read_xlsx(crosswalk_location, sheet = "crosswalk_1980_2010") %>%
                        filter(year == metadata$year, !is.na(isced_level3)) %>%
                        select(ibge_code, isced_level3, isced_level3_label_en) %>%
                        as.data.table()
        }
        
        
        harmonizeIBGE:::check_necessary_vars(CensusData, varList)
        
        # Building educationAttainment
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("educationAttainment"))
        educationAttainment_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- eval(parse(text=paste0("build_education_educationAttainment_",metadata$year,"(CensusData)")))
                educationAttainment_just_created <- TRUE
                gc();Sys.sleep(.5);gc()
        }
        
        
        if(metadata$year == 2010){
                CensusData[ , ibge_code := CensusData[ , varList[1], with = F ] ]
                CensusData[is.na(ibge_code), ibge_code := CensusData[is.na(ibge_code), varList[2], with = F ] ] 
                CensusData[is.na(ibge_code), ibge_code := CensusData[is.na(ibge_code), varList[3], with = F ] ] 
        }else{
                CensusData[ , ibge_code := CensusData[[varList]] ]        
        }
        
        setkey(CensusData, "ibge_code")
        setkey(crosswalk, "ibge_code")
        
        # efficient join using CensusData.table sintax:
        CensusData = CensusData[crosswalk, fieldsOfStudy := isced_level3] # this causes loss of the metadata
        gc(); Sys.sleep(.3);gc()
        
        CensusData = CensusData[crosswalk, label_fieldsOfStudy := isced_level3_label_en] # this causes loss of the metadata
        gc(); Sys.sleep(.3);gc()
        
        CensusData <- CensusData %>%
                select(-ibge_code)
        
        CensusData[educationAttainment != 9, fieldsOfStudy := NA]
        CensusData[educationAttainment == 9 & is.na(fieldsOfStudy), fieldsOfStudy := 999]
        
        
        gc();Sys.sleep(.5);gc()
        
        #if(just_created_vars_list_existedBefore == F){
        #        CensusData <- harmonizeIBGE:::erase_just_created_vars(CensusData)
        #}
        
        if(educationAttainment_just_created == TRUE){
                CensusData[ , educationAttainment := NULL]
                gc();Sys.sleep(.5);gc()
        }
        
        setDT(CensusData)
        
        CensusData = harmonizeIBGE:::set_metadata(CensusData, metadata) #recovering metadata...
        
        gc();Sys.sleep(.5);gc()
        
        CensusData
}

