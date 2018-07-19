#' Builds a synthetic variable for education attainment - 1970
#' @param data.frame
#' @value data.frame
#' @export

build_education_schoolattnd_1970 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v036"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        # Building age
        check_vars <- check_var_existence(CensusData, c("age"))
        age_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_demographics_age_1970(CensusData)
                age_just_created <- TRUE
        }
        gc()

        # School Attendance
        CensusData[, schoolattnd := as.numeric(NA)]
        CensusData[v036 == 2, schoolattnd := 0]
        CensusData[v036 == 1, schoolattnd := 1]

        # Ajuste para idade
        CensusData[age <= 4,  schoolattnd := NA]
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }

        CensusData

}
