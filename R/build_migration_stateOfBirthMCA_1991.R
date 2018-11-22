#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_migration_stateOfBirthMCA_1991 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v0314", "v0316"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        metadata    <- harmonizeIBGE:::get_metadata(CensusData)
        
        stateCurrent_just_created <- F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("stateCurrent"))
        if(length(check_vars) > 0){
                CensusData <- build_geography_stateCurrent_1991(CensusData)
                stateCurrent_just_created <- T
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        data("crosswalk_states_tmp")
        crosswalk_states_tmp =  crosswalk_states_tmp %>%
                filter(year == 1991 & variable == "state_of_birth") %>%
                select(original_code, semi_harmonized_code) %>%
                rename(v0316            = original_code,
                       stateOfBirthMCA = semi_harmonized_code) 
        
        
        CensusData <- data.table:::merge.data.table(x = CensusData,
                                                    y = crosswalk_states_tmp,
                                                    by = "v0316", 
                                                    all.x = T, 
                                                    all.y = F, 
                                                    sort = F)
        gc()
        
        CensusData[v0314 %in% c(1,2), stateOfBirthMCA := stateCurrent]
        
        CensusData[stateOfBirthMCA == 20, stateOfBirthMCA := 26]
        CensusData[stateOfBirthMCA == 34, stateOfBirthMCA := 33]
        CensusData[stateOfBirthMCA == 17, stateOfBirthMCA := 52]
        CensusData[stateOfBirthMCA == 50, stateOfBirthMCA := 51]
        
        # Brazilian, but unspecified state = 99
        CensusData[v0316 == 29, stateOfBirthMCA := 99] 
        
        ## Foreigns  = 999
        CensusData[v0316 %in% 30:99, stateOfBirthMCA := 999]
        
        gc()
        
        if(stateCurrent_just_created == T){
                CensusData[, stateCurrent := NULL]
        }
        
        CensusData <- harmonizeIBGE:::set_metadata(Data = CensusData, metadata = metadata) 
        
        gc()
        CensusData
}




