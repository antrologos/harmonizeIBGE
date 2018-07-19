#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_demographics_numberRelatives_1970 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        # Building idhh
        check_vars <- check_var_existence(CensusData, c("idhh"))
        hhid_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_identification_idhh_1970(CensusData)
                hhid_just_created <- TRUE
        }

        # Identifying members of the main family of the household
        check_vars <- check_var_existence(CensusData, c("relative"))
        relative_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_demographics_relative_1970(CensusData)
                relative_just_created <- TRUE
        }


        CensusData[ , numberRelatives := sum(relative), by = idhh]
        CensusData[ relative != 1, numberRelatives := NA]

        gc()
        CensusData
}
