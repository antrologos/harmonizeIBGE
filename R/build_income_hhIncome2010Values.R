#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_income_hhIncome2010Values <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        idhh_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("idhh"))
        if(length(check_vars) > 0){
                CensusData <- eval(parse(text = paste0("build_indentification_idhh", metadata$year, "(CensusData)")))
                idhh_just_created = T
        }
        
        hhType_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("hhType"))
        if(length(check_vars) > 0){
                CensusData <- eval(parse(text = paste0("build_household_hhType", metadata$year, "(CensusData)")))
                hhType_just_created = T
        }
        
        nonrelative_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("nonrelative"))
        if(length(check_vars) > 0){
                CensusData <- eval(parse(text = paste0("build_demographics_nonrelative", metadata$year, "(CensusData)")))
                nonrelative_just_created = T
        }
        
        totalIncome2010Values_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("totalIncome2010Values"))
        if(length(check_vars) > 0){
                CensusData <- build_income_totalIncome2010Values(CensusData)
                hhIncome2010Values_just_created = T
        }

        # Copying the income information
        CensusData[ , totalIncome2010Values_tmp := totalIncome2010Values]

        # Just members of the household will have valid values
        CensusData[, totalIncome2010Values_tmp := totalIncome2010Values_tmp*as.numeric(!nonrelative)]
        
        # Once the incomes will be summed inside the household, we need to replace NAs by zero
        CensusData[is.na(totalIncome2010Values), totalIncome2010Values_tmp := 0]

        # Calculating the total household income
        CensusData[, hhIncome2010Values := sum(totalIncome2010Values_tmp), by=idhh]

        # Non-relatives will be NAs:
        CensusData[nonrelative == 1, hhIncome2010Values := NA]

        # Collective households will be NAs:
        # 0 "private permanent" 
        # 1 "private improvised" 
        # 2 "collective dwelling"
        CensusData[hhType == 2, hhIncome2010Values := NA]

        gc()
        
        CensusData[, totalIncome2010Values_tmp := NULL]
        
        metadata = harmonizeIBGE:::get_metadata(CensusData)
        if(metadata$year == 1960){
                CensusData[, hhIncome2010Values := NA]
        }
        
        if(idhh_just_created == T){
                CensusData[, idhh := NULL] 
        }
        
        if(hhType_just_created == T){
                CensusData[, hhType := NULL] 
        }
        
        if(nonrelative_just_created == T){
                CensusData[, nonrelative := NULL] 
        }
        
        if(totalIncome2010Values_just_created == T){
                CensusData[, totalIncome2010Values := NULL] 
        }
        
        
        gc()
        CensusData
}
