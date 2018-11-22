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

        check_vars <- check_var_existence(CensusData, c("idhh", "totalIncome2010Values", "hhType", "nonrelative"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
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
        CensusData[relative != 1, hhIncome2010Values := NA]

        # Collective households will be NAs:
        # 0 "private permanent" 
        # 1 "private improvised" 
        # 2 "collective dwelling"
        CensusData[hhType == 2, hhIncome2010Values := NA]

        gc()
        
        CensusData[, totalIncome2010Values_tmp := NULL]
        
        gc()
        CensusData
}
