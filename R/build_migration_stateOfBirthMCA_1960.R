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
        
        metadata    <- harmonizeIBGE:::get_metadata(CensusData)
        
        bornInBrazil_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("bornInBrazil"))
        if(length(check_vars) > 0){
                CensusData <- build_migration_bornInBrazil_1960(CensusData)
                bornInBrazil_just_created = T
        }
        
        data("crosswalk_states_tmp")
        crosswalk_states_tmp =  crosswalk_states_tmp %>%
                filter(year == 1960 & variable == "state_of_birth") %>%
                select(original_code, semi_harmonized_code) %>%
                rename(v207            = original_code,
                       stateOfBirthMCA = semi_harmonized_code) 
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("stateOfBirthMCA"))
        
        if(length(check_vars) == 0){
                
                if(bornInBrazil_just_created == T){
                        CensusData[ , bornInBrazil := NULL]
                }
                
                CensusData <- harmonizeIBGE:::set_metadata(Data = CensusData, metadata = metadata)
                return(CensusData)
        
        }else{
                
                CensusData <- data.table:::merge.data.table(x = CensusData,
                                                            y = crosswalk_states_tmp,
                                                            by = "v207", 
                                                            all.x = T, 
                                                            all.y = F, 
                                                            sort = F)
                
                gc()
                
                ## Foreigns  = 999
                CensusData[ bornInBrazil == 0, stateOfBirthMCA := 999]
                
                if(bornInBrazil_just_created == T){
                        CensusData[ , bornInBrazil := NULL]
                }
                
                CensusData <- harmonizeIBGE:::set_metadata(Data = CensusData, metadata = metadata) 
                gc()
                return(CensusData)
        }
}


