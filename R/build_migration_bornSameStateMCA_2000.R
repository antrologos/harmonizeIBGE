#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_migration_bornSameStateMCA_2000 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        stateMiniumComparable_just_created <- F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("stateMiniumComparable"))
        if(length(check_vars) > 0){
                CensusData <- build_geography_stateMiniumComparable_2000(CensusData)
                stateMiniumComparable_just_created <- T
                gc()
        }
        
        stateOfBirthMCA_just_created <- F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("stateOfBirthMCA"))
        if(length(check_vars) > 0){
                CensusData <- build_migration_stateOfBirthMCA_2000(CensusData)
                stateOfBirthMCA_just_created <- T
                gc()
        }
        
        CensusData[ , bornSameStateMCA := as.numeric(stateMiniumComparable == stateOfBirthMCA)]
        
        # Unknown state will be NA
        CensusData[stateOfBirthMCA == 99, bornSameStateMCA := NA]
        
        # Foreigns will be zero
        CensusData[stateOfBirthMCA == 999, bornSameStateMCA := 0]
        
        gc()
        
        if(stateMiniumComparable_just_created == T){
                CensusData[, stateCurrent := NULL]
        }
        
        gc() 
        
        if(stateOfBirthMCA_just_created == T){
                CensusData[, stateOfBirthMCA := NULL]
        }
        
        
        gc()
        CensusData
}






