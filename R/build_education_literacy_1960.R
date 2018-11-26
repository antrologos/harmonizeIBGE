#' Builds a synthetic variable for education attainment - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_education_literacy_1960 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v211"))
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
                CensusData <- build_demographics_age_1960(CensusData)
                age_just_created <- TRUE
        }
        gc()

        # Literacy
        CensusData[, literacy := as.numeric(NA)]
        CensusData[v211 %in% c(0, 1), literacy := 1]
        CensusData[v211 %in% c(2, 3), literacy := 0]

        # Ajuste para idade
        CensusData[age <= 4,  literacy := NA]
        
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
                gc();Sys.sleep(.5);gc()
        }

        gc()
        CensusData

}
