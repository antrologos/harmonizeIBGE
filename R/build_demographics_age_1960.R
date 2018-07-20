#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_demographics_age_1960 <- function(CensusData){ 

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }

        check_vars <- check_var_existence(CensusData, c("v204","v204b"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        #age
        CensusData[, age := as.numeric(v204b)]
        CensusData[v204 == 0,   age := 0]         # para pessoas com idade em meses
        CensusData[v204 == 5,   age := age + 100] # para pessoas com mais de 100 anos
        CensusData[v204 == 9,   age := NA]        # para pessoas com idade ignorada
        CensusData[is.na(v204), age := NA]        # para pessoas com registro corrompido


        CensusData

}
