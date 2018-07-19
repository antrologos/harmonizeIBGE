#' Harmonize a set of income variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_income_indIncomeReal_1970 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        indIncomeNominal_just_created = F
        check_vars <- check_var_existence(CensusData, c("indIncomeNominal"))
        if(length(check_vars) > 0){
                CensusData <- build_income_indIncomeNominal_1970(CensusData)
                indIncomeNominal_just_created = T
        }
        gc()
        
        CensusData[, indIncomeReal := indIncomeNominal/0.416904849]
        
        if(indIncomeNominal_just_created == T){
                CensusData[, indIncomeNominal := NULL]
        }

        CensusData
}
