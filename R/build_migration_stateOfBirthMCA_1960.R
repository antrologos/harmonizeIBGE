#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_migration_stateOfBirthMCA_1960 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v207"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        data("crosswalk_states_tmp")
        crosswalk_states_tmp =  crosswalk_states_tmp %>%
                filter(year == 1960 & variable == "state_of_birth") %>%
                select(-year, -variable) %>%
                rename(v207            = original_code,
                       stateOfBirthMCA = semi_harmonized_code) 
        
        
        CensusData <- data.table:::merge.data.table(x = CensusData,
                                                    y = crosswalk_states_tmp,
                                                    by = "v207", 
                                                    all.x = T, 
                                                    all.y = F, 
                                                    sort = F)
        CensusData[stateOfBirthMCA == 20, stateOfBirthMCA := 26]
        CensusData[stateOfBirthMCA == 34, stateOfBirthMCA := 33]
        CensusData[stateOfBirthMCA == 17, stateOfBirthMCA := 52]
        CensusData[stateOfBirthMCA == 50, stateOfBirthMCA := 51]
        
        # Brazilian, but unspecified state = 99
        CensusData[v207 == 29, stateOfBirthMCA := 99] 
        
        ## Foreigns  = 999
        CensusData[v207 %in% 30:99, stateOfBirthMCA := 999] 
        
        gc()
        CensusData
}


