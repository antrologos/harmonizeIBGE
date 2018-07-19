#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_demographics_numberRelatives_1970 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        check_vars <- check_var_existence(CensusData, c("v007"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
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

        # Identifying members of the main family of the household
        check_vars <- check_var_existence(CensusData, c("relative"))
        relative_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_demographics_relative_1970(CensusData)
                relative_just_created <- TRUE
        }


        CensusData[ , numberRelatives := sum(relative), by = idhh]
        CensusData[ v007 == 1, numberRelatives := NA]
        
        gc()
        
        if(idhh_just_created == TRUE){
                CensusData[ , idhh := NULL]
        }
        
        if(relative_just_created == TRUE){
                CensusData[ , relative := NULL]
        }
        

        gc()
        CensusData
}
