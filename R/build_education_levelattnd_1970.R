#' Builds a synthetic variable for education attainment - 1970
#' @param data.frame
#' @value data.frame
#' @export

build_education_levelattnd_1970 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v037", "v038"))
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

        # Level of attendance
        CensusData[v038 %in% c(1, 2), levelattnd := 1]
        CensusData[v038 %in% c(3),    levelattnd := 2]
        CensusData[v038 %in% c(4),    levelattnd := 3]
        CensusData[v038 %in% c(9),    levelattnd := 9]
        CensusData[v038 %in% c(5),    levelattnd := NA]
        CensusData[v037 %in% c(8 ,9), levelattnd := 9]

        # Ajuste para idade
        CensusData[age <= 4,  levelattnd := NA]
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }

        CensusData

}
