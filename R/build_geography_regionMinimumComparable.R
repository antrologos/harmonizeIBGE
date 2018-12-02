#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_geography_regionMinimumComparable <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizeIBGE:::get_metadata(CensusData)
        
        stateCurrent_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("stateCurrent"))
        if(length(check_vars) > 0){
                CensusData <- eval(parse(text = paste0("build_geography_stateCurrent_",metadata$year,"(CensusData)")))
                stateCurrent_just_created = T
                gc()
        }
        gc();Sys.sleep(.5);gc()
        
        
        stateMinimumComparable_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("stateMinimumComparable"))
        if(length(check_vars) > 0){
                CensusData <- eval(parse(text = paste0("build_geography_stateMinimumComparable_",metadata$year,"(CensusData)")))
                stateMinimumComparable_just_created = T
                gc()
        }
        gc();Sys.sleep(.5);gc()
        
        
        CensusData[ , regionMinimumComparable := trunc(stateMinimumComparable/10)]
        
        
        
        if(stateCurrent_just_created == TRUE){
                CensusData[ , stateCurrent := NULL]
        }
        
        
        if(stateMinimumComparable_just_created == TRUE){
                CensusData[ , stateMinimumComparable := NULL]
        }
        
        
        gc()
        CensusData <- harmonizeIBGE:::set_metadata(Data = CensusData, metadata = metadata)
        
        CensusData
}
