#' Builds a synthetic variable for education attainment - 1991
#' @param data.frame
#' @value data.frame
#' @export

build_education_levelattnd_1991 <- function(CensusData){

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

        # Level of attendance
        CensusData[v0325 %in% c(1),    levelattnd := 1]
        CensusData[v0325 %in% c(2),    levelattnd := 2]
        CensusData[v0325 %in% c(3),    levelattnd := 3]
        CensusData[v0325 %in% c(4, 5), levelattnd := 9]

        CensusData[v0326 %in% c(6),   levelattnd := 3]
        CensusData[v0326 %in% c(1:5), levelattnd := 9]

        # Ajuste para idade
        CensusData[age <= 4,  levelattnd := NA]
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }

        CensusData
}
