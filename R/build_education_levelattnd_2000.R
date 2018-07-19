#' Builds a synthetic variable for education attainment - 2000
#' @param data.frame
#' @value data.frame
#' @export

build_education_levelattnd_2000 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v0430"))
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
                CensusData <- build_demographics_age_2000(CensusData)
                age_just_created <- TRUE
        }
        gc()

        # Level of attendance
        CensusData[v0430 %in% c(5),               levelattnd := 1]
        CensusData[v0430 %in% c(8),               levelattnd := 2]
        CensusData[v0430 %in% c(12, 13),          levelattnd := 3]
        CensusData[v0430 %in% c(1:4, 6, 7, 9:11), levelattnd := 9]

        # Ajuste para idade
        CensusData[age <= 4,  levelattnd := NA]
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }

        CensusData
}
