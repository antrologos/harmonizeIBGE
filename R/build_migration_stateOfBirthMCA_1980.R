#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_migration_stateOfBirthMCA_1980 <- function(CensusData){
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v512"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
       
        metadata    <- harmonizeIBGE:::get_metadata(CensusData)
        
        bornInBrazil_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("bornInBrazil"))
        if(length(check_vars) > 0){
                CensusData <- build_migration_bornInBrazil_1980(CensusData)
                bornInBrazil_just_created = T
        }
        
        data("crosswalk_states_tmp")
        crosswalk_states_tmp =  crosswalk_states_tmp %>%
                filter(year == 1980 & variable == "state_of_birth") %>%
                select(original_code, semi_harmonized_code) %>%
                rename(v512            = original_code,
                       stateOfBirthMCA = semi_harmonized_code) %>%
                as.data.table() %>%
                setkey("v512")
        
        setkey(CensusData, "v512")
        
        CensusData[crosswalk_states_tmp, stateOfBirthMCA := stateOfBirthMCA]
        
        gc(); Sys.sleep(.5);gc()
        
        
        ## Foreigns  = 999
        CensusData[ bornInBrazil == 0, stateOfBirthMCA := 999]
        
        if(bornInBrazil_just_created == T){
                CensusData[ , bornInBrazil := NULL]
        }
        
        CensusData <- harmonizeIBGE:::set_metadata(Data = CensusData, metadata = metadata) 
        
        gc()
        CensusData
}


