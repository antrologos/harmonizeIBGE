#' @export

build_education_fieldsSecondary <- function(CensusData){
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        
        if(metadata$year > 1991){
                warning(paste("'fieldsSecondary' is not available for the year", metadata$year))
                return(CensusData)
        }
        
        just_created_vars_list_existedBefore <- exists(x = "just_created_vars", where = .GlobalEnv)
        
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
        
        crosswalk_location <- system.file("extdata",
                                          "crosswalk_fieldsOfStudy_PrimarySecondary_1960_1991.csv",
                                          package = "harmonizeIBGE")
        
        crosswalk <-  read.csv2(crosswalk_location, stringsAsFactors = F) %>%
                filter(year == metadata$year, !is.na(field_code)) %>%
                filter(level == "secondary") %>%
                select(ibge_code, field_code, field_label) %>%
                as.data.table()
        
        harmonizeIBGE:::check_necessary_vars(CensusData, varList)
        
        # Building educationAttainment
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("educationAttainment"))
        educationAttainment_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- eval(parse(text=paste0("build_education_educationAttainment_",metadata$year,"(CensusData)")))
                educationAttainment_just_created <- TRUE
                gc();Sys.sleep(.5);gc()
        }
        
        CensusData[ , ibge_code := CensusData[[varList]] ]
        
        setkey(CensusData, "ibge_code")
        setkey(crosswalk, "ibge_code")
        
        # efficient join using CensusData.table sintax:
        CensusData = CensusData[crosswalk, fieldsSecondary := field_code] # this causes loss of the metadata
        gc(); Sys.sleep(.3);gc()
        
        #gc(); Sys.sleep(.3);gc()
        
        CensusData <- CensusData %>%
                select(-ibge_code)
        
        
        CensusData[educationAttainment %in% c(1,2,9, 99), fieldsSecondary := NA]
        CensusData[educationAttainment %in% 3:8 & is.na(fieldsSecondary), fieldsSecondary := 90]
        
        
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

