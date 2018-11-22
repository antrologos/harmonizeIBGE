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
        
        metadata = harmonizeIBGE:::get_metadata(CensusData)
        
        idhh_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("idhh"))
        if(length(check_vars) > 0){
                CensusData <- eval(parse(text = paste0("build_indentification_idhh", metadata$year, "(CensusData)")))
                idhh_just_created = T
        }
        
        nonrelative_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("nonrelative"))
        if(length(check_vars) > 0){
                CensusData <- eval(parse(text = paste0("build_demographics_nonrelative", metadata$year, "(CensusData)")))
                nonrelative_just_created = T
        }
        
        hhIncome2010Values_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("hhIncome2010Values"))
        if(length(check_vars) > 0){
                CensusData <- build_income_hhIncome2010Values(CensusData)
                hhIncome2010Values_just_created = T
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
        
        
        if(metadata$year == 1960){
                CensusData[, hhIncomePerCap2010Values := NA]
        }
        
        

        if(idhh_just_created == T){
                CensusData[, idhh := NULL] 
        }

        if(nonrelative_just_created == T){
                CensusData[, nonrelative := NULL] 
        }
       
        
        if(hhIncome2010Values_just_created == T){
                CensusData[, hhIncome2010Values := NULL] 
        }
        
        gc()
        CensusData
}
