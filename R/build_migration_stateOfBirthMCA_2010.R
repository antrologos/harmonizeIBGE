#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_migration_stateOfBirthMCA_2010 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v0618", "v0619", "v0622", "v6222"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
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
                select(-year, -variable) %>%
                rename(v6222            = original_code,
                       stateOfBirthMCA = semi_harmonized_code) 
        
        
        CensusData <- data.table:::merge.data.table(x = CensusData,
                                                    y = crosswalk_states_tmp,
                                                    by = "v6222", 
                                                    all.x = T, 
                                                    all.y = F, 
                                                    sort = F)
        gc()
        
        # Se nasceu no mesmo município em que reside
        CensusData[v0618 %in% c(1,2), stateOfBirthMCA := stateCurrent]
        
        # Se nasceu na mesma uf em que reside
        CensusData[v0619 %in% c(1,2), stateOfBirthMCA := stateCurrent]
        
        CensusData[stateOfBirthMCA == 20, stateOfBirthMCA := 26]
        CensusData[stateOfBirthMCA == 34, stateOfBirthMCA := 33]
        CensusData[stateOfBirthMCA == 17, stateOfBirthMCA := 52]
        CensusData[stateOfBirthMCA == 50, stateOfBirthMCA := 51]
        
        # Brazilian, but unspecified state = 99
        CensusData[v6222 == 9900000, stateOfBirthMCA := 99] 
        
        ## Foreigns  = 999
        CensusData[v0622 == 2, stateOfBirthMCA := 999]
        
        gc()
        
        if(stateCurrent_just_created == T){
                CensusData[, stateCurrent := NULL]
        }
        
        gc()
        CensusData
}


