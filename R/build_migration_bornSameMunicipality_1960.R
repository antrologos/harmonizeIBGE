#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_migration_bornSameMunicipality_1960 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        metadata    <- harmonizeIBGE:::get_metadata(CensusData)
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v209"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        # Building dweller
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("dweller"))
        dweller_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_identification_dweller_1960(CensusData)
                dweller_just_created <- TRUE
        }
        gc()
        
        # Building stateMiniumComparable
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("stateOfBirthMCA"))
        stateOfBirthMCA_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_migration_stateOfBirthMCA_1960(CensusData)
                stateOfBirthMCA_just_created <- TRUE
        }
        gc()
        
        CensusData[ , bornSameMunicipality := ifelse(v209 == 2, 1, 0)]
        CensusData[is.na(v209), bornSameMunicipality := NA]
        
        CensusData[is.na(dweller),                  bornSameMunicipality := NA]
        CensusData[dweller == 0,                    bornSameMunicipality := NA] #the code 2 in v209 codifies all non-dwellers (regardless of where they were born)
        
        CensusData[is.na(stateOfBirthMCA), bornSameMunicipality := NA]
        CensusData[stateOfBirthMCA == 99,  bornSameMunicipality := NA] #brazilians, unknown state
        CensusData[stateOfBirthMCA == 999, bornSameMunicipality := 0] #foreigners
        
        
        if(dweller_just_created == T){
                CensusData[ , dweller := NULL]
        }
        
        if(stateOfBirthMCA_just_created == T){
                CensusData[ , stateOfBirthMCA := NULL]
        }
        
        warning("This result is still wrong. There inconsistency between the variables bornSameMunicipality and bornSameStateMCA")
        
        CensusData <- harmonizeIBGE:::set_metadata(Data = CensusData, metadata = metadata)
        
        gc()
        CensusData
}






