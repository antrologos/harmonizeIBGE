#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_migration_stateOfBirthMCA_2000 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v0415", "v0417", "v0418", "v4210"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        metadata    <- harmonizeIBGE:::get_metadata(CensusData)
        
        stateCurrent_just_created <- F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("stateCurrent"))
        if(length(check_vars) > 0){
                CensusData <- build_geography_stateCurrent_2000(CensusData)
                stateCurrent_just_created <- T
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        data("crosswalk_states_tmp")
        crosswalk_states_tmp =  crosswalk_states_tmp %>%
                filter(year == 2000 & variable == "state_of_birth") %>%
                select(original_code, semi_harmonized_code) %>%
                rename(v4210            = original_code,
                       stateOfBirthMCA = semi_harmonized_code) %>%
                as.data.table() %>%
                setkey("v4210")
        

        setkey(CensusData, "v4210")
        CensusData[crosswalk_states_tmp, stateOfBirthMCA := stateOfBirthMCA]
        
        gc(); Sys.sleep(.5);gc()
        

        gc()
        
        # Se mora no mesmo município desde que nasceu
        CensusData[v0415 == 1, stateOfBirthMCA := stateCurrent]
        
        # Se nasceu no mesmo município em que reside
        CensusData[v0417 == 1, stateOfBirthMCA := stateCurrent]
        
        # Se nasceu na mesma uf em que reside
        CensusData[v0418 == 1, stateOfBirthMCA := stateCurrent]
        
        CensusData[stateOfBirthMCA == 20, stateOfBirthMCA := 26]
        CensusData[stateOfBirthMCA == 34, stateOfBirthMCA := 33]
        CensusData[stateOfBirthMCA == 17, stateOfBirthMCA := 52]
        CensusData[stateOfBirthMCA == 50, stateOfBirthMCA := 51]
        
        # Brazilian, but unspecified state = 99
        CensusData[v4210 == 29, stateOfBirthMCA := 99] 
        
        ## Foreigns  = 999
        CensusData[v4210 %in% 30:99, stateOfBirthMCA := 999]
        
        gc()
        
        if(stateCurrent_just_created == T){
                CensusData[, stateCurrent := NULL]
        }
        
        CensusData <- harmonizeIBGE:::set_metadata(Data = CensusData, metadata = metadata) 
        
        gc()
        CensusData
}


