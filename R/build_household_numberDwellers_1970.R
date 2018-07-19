#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_household_numberDwellers_1970 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        # Building idhh
        check_vars <- check_var_existence(CensusData, c("idhh"))
        idhh_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_identification_idhh_1970(CensusData)
                idhh_just_created <- TRUE
        }
        
        # Building dweller
        check_vars <- check_var_existence(CensusData, c("dweller"))
        dweller_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_identification_dweller_1970(CensusData)
                dweller_just_created <- TRUE
        }
        
        
        CensusData[ , numberDwellers := sum(dweller), by = idhh]
        
        if(dweller_just_created == T){
                CensusData[, dweller := NULL]
        }
        
        if(idhh_just_created == T){
                CensusData[, idhh := NULL]
        }
        
        gc()
        CensusData
}