#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export 

build_migration_stateOfBirthMCA_2010 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame ")
        }
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v0618", "v0619", "v0622", "v6222"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        metadata    <- harmonizeIBGE:::get_metadata(CensusData)
        
        stateCurrent_just_created <- F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("stateCurrent"))
        if(length(check_vars) > 0){
                CensusData <- build_geography_stateCurrent_2010(CensusData)
                stateCurrent_just_created <- T
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        data("crosswalk_states_tmp")
        crosswalk_states_tmp =  crosswalk_states_tmp %>%
                filter(year == 2010 & variable == "state_of_birth") %>%
                select(original_code, semi_harmonized_code) %>%
                rename(v6222            = original_code,
                       stateOfBirthMCA = semi_harmonized_code) %>%
                as.data.table() %>%
                setkey("v6222")
        
        setkey(CensusData, "v6222")
        CensusData[crosswalk_states_tmp, stateOfBirthMCA := stateOfBirthMCA]
        
        gc(); Sys.sleep(.5);gc()
        
        # Se nasceu no mesmo município em que reside
        CensusData[v0618 %in% c(1,2), stateOfBirthMCA := stateCurrent]
        
        # Se nasceu na mesma uf em que reside
        CensusData[v0619 %in% c(1,2), stateOfBirthMCA := stateCurrent]
        
        CensusData[stateOfBirthMCA == 20, stateOfBirthMCA := 26]
        CensusData[stateOfBirthMCA == 34, stateOfBirthMCA := 33]
        CensusData[stateOfBirthMCA == 17, stateOfBirthMCA := 52]
        CensusData[stateOfBirthMCA == 50, stateOfBirthMCA := 51]
        
        ## Foreigns  = 999
        CensusData[v0622 == 2, stateOfBirthMCA := 999]
        
        gc()
        
        if(stateCurrent_just_created == T){
                CensusData[, stateCurrent := NULL]
        }
        
        CensusData <- harmonizeIBGE:::set_metadata(Data = CensusData, metadata = metadata) 
        
        gc()
        CensusData
}


