#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export



build_income_hhIncomePerCap2010Values <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        check_vars <- check_var_existence(CensusData, c("idhh", "hhIncome2010Values", "nonrelative"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        gc()

        CensusData[                                                         , numberRelatives_tmp := as.numeric(!nonrelative)]
        CensusData[ is.na(numberRelatives_tmp) | is.nan(numberRelatives_tmp), numberRelatives_tmp := 0]
        
        CensusData[ , numberRelatives_tmp := sum(numberRelatives_tmp), by = idhh]
        
        
        CensusData[ , hhIncomePerCap2010Values := hhIncome2010Values/numberRelatives_tmp]
        gc()

        CensusData[is.na(hhIncomePerCap2010Values),      hhIncomePerCap2010Values := NA]
        CensusData[!is.finite(hhIncomePerCap2010Values), hhIncomePerCap2010Values := NA]
        CensusData[is.nan(hhIncomePerCap2010Values),     hhIncomePerCap2010Values := NA]


        CensusData[ , numberRelatives_tmp := NULL]
        

        gc()
        CensusData
}
