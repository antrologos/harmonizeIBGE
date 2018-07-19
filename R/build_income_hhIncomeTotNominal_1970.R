#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_income_hhIncomeTotNominal_1970 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        # Building Total individual income (nominal)
        check_vars <- check_var_existence(CensusData, c("indIncomeNominal"))
        indIncomeNominal_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_income_indIncomeNominal_1970(CensusData)
                indIncomeNominal_just_created <- TRUE
        }

        # Building idhh
        check_vars <- check_var_existence(CensusData, c("idhh"))
        idhh_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_identification_idhh_1970(CensusData)
                idhh_just_created <- TRUE
        }

        # Building househouldParticular
        check_vars <- check_var_existence(CensusData, c("househouldParticular"))
        househouldParticular_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_identification_househouldParticular_1970(CensusData)
                househouldParticular_just_created <- TRUE
        }

        # Identifying members of the main family of the household
        check_vars <- check_var_existence(CensusData, c("relative"))
        relative_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_demographics_relative_1970(CensusData)
                relative_just_created <- TRUE
        }

        # Copying the income information
        CensusData[ , indIncomeNominal_tmp := indIncomeNominal]

        # Once the incomes will be summed inside the household, we need to replace NAs by zero
        CensusData[is.na(indIncomeNominal), indIncomeNominal_tmp := 0]

        # Just members of the household will have valid values
        CensusData[, indIncomeNominal_tmp := indIncomeNominal_tmp*relative]

        # Calculating the total household income
        CensusData[, hhIncomeTotNominal := sum(indIncomeNominal_tmp), by=idhh]

        # Non-relatives will be NAs:
        CensusData[relative != 1, hhIncomeTotNominal := NA]

        # Collective households will be NAs:
        CensusData[househouldParticular != 1, hhIncomeTotNominal := NA]

        gc()

        CensusData[, indIncomeNominal_tmp := NULL]

        if(indIncomeNominal_just_created == TRUE){
                CensusData[ , indIncomeNominal := NULL]
        }

        if(idhh_just_created == TRUE){
                CensusData[ , idhh := NULL]
        }

        if(househouldParticular_just_created == TRUE){
                CensusData[ , househouldParticular := NULL]
        }

        if(relative_just_created == TRUE){
                CensusData[ , relative := NULL]
        }

        gc()
        CensusData
}
