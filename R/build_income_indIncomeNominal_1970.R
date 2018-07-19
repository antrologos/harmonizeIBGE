#' Harmonize a set of income variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_income_indIncomeNominal_1970 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v041"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        age_just_created = F
        check_vars <- check_var_existence(CensusData, c("age"))
        if(length(check_vars) > 0){
                CensusData <- build_demographics_age_1970(CensusData)
                age_just_created = T
        }

        CensusData[v041 > 0 & v041 < 9999,  indIncomeNominal := v041]
        CensusData[v041 == 9999, indIncomeNominal := 0]    # o valor 9999 representa sem rendimento
        CensusData[v041 == 0,    indIncomeNominal := NA]   # o valor 0 significa sem declaracao

        CensusData[age <= 9, indIncomeNominal := NA]

        if(age_just_created == T){
                CensusData[ , age := NULL]
        }

        gc()
        CensusData
}
