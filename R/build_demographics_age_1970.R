#' Builds a synthetic variable for age - 1970
#' @param data.frame
#' @value data.frame
#' @export

build_demographics_age_1970 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v026","v027"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        #age
        CensusData[, age := as.double(NA)]
        CensusData[v026 %in% c(1,2), age := 0]
        CensusData[v026 %in% c(3,4), age := as.numeric(v027)]
        gc()


        CensusData

}
