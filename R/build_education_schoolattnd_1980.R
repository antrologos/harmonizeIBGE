#' Builds a synthetic variable for education attainment - 1980
#' @param data.frame
#' @value data.frame
#' @export

build_education_schoolattnd_1980 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v521", "v522"))
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
                CensusData <- build_demographics_age_1980(CensusData)
                age_just_created <- TRUE
        }
        gc()

        # School Attendance
        CensusData[, schoolattnd := 0]
        CensusData[v521 != 0, schoolattnd := 1]
        CensusData[v522 %in% c(2:4, 8), schoolattnd := 1] # esta excluindo vestibular (opcao 7)

        CensusData[is.na(v521) | is.na(v522), schoolattnd := NA]

        # Ajuste para idade
        CensusData[age <= 4,  schoolattnd := NA]
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }

        CensusData

}
