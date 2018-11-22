#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_geography_regionMiniumComparable <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        # Selecting the appropriate crosswalk for the current year
        metadata    <- harmonizeIBGE:::get_metadata(CensusData)
        
        stateMiniumComparable_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("stateMiniumComparable"))
        if(length(check_vars) > 0){
                CensusData <- eval(parse(text = paste0("build_geography_stateMiniumComparable_",metadata$year,"(CensusData)")))
                stateMiniumComparable_just_created = T
                gc()
        }
        
        CensusData[ , regionMiniumComparable := trunc(stateMiniumComparable/10)]
        
        if(stateMiniumComparable_just_created == TRUE){
                CensusData[ , stateMiniumComparable := NULL]
        }
        
        
        gc()
        CensusData
}