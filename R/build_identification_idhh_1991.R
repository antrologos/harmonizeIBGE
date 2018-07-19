#' Builds a synthetic variable for age - 1980
#' @param data.frame
#' @value data.frame
#' @export

build_identification_idhh_1991 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v0102"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        CensusData[, idhh := v0102]

        CensusData

}

