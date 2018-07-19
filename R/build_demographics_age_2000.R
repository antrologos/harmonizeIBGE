#' Builds a synthetic variable for age - 2000
#' @param data.frame
#' @value data.frame
#' @export

build_demographics_age_2000 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v4752"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        #age
        CensusData[ , age := v4752]
        gc()


        CensusData

}
