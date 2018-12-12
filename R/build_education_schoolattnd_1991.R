#' Builds a synthetic variable for education attainment - 1991
#' @param data.frame
#' @value data.frame
#' @export

build_education_schoolattnd_1991 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v0325", "v0326"))
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
                CensusData <- build_demographics_age_1991(CensusData)
                age_just_created <- TRUE
        }
        gc()

        # School Attendance
        CensusData[, schoolattnd := 0]
        CensusData[v0325 != 0, schoolattnd := 1]
        CensusData[v0326 %in% c(2:4, 6), schoolattnd := 1] # esta excluindo vestibular (opcao 5)

        CensusData[is.na(v0325) | is.na(v0326), schoolattnd := NA]

        # Ajuste para idade
        CensusData[age <= 4,  schoolattnd := NA]
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }

        CensusData

}
