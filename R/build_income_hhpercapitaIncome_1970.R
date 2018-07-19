#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export



build_income_hhIncomePerCapNominal_1970 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        # Building idhh
        check_vars <- check_var_existence(CensusData, c("idhh"))
        idhh_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_identification_idhh_1970(CensusData)
                idhh_just_created <- TRUE
        }

        # Building hhIncomeTotNominal
        check_vars <- check_var_existence(CensusData, c("hhIncomeTotNominal"))
        hhIncomeTotNominal_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_identification_idhh_1970(CensusData)
                hhIncomeTotNominal_just_created <- TRUE
        }

        # Building numberRelatives
        check_vars <- check_var_existence(CensusData, c("numberRelatives"))
        numberRelatives_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_demographics_numberRelatives_1970(CensusData)
                numberRelatives_just_created <- TRUE
        }

        CensusData[ , hhIncomePerCapNominal := hhIncomeTotNominal/numberRelatives]

        CensusData[!is.finite(hhIncomePerCapNominal), hhIncomePerCapNominal := NA]
        CensusData[is.nan(hhIncomePerCapNominal), hhIncomePerCapNominal := NA]


        if(idhh_just_created == TRUE){
                CensusData[ , idhh := NULL]
        }

        if(hhIncomeTotNominal_just_created == TRUE){
                CensusData[ , hhIncomeTotNominal := NULL]
        }

        if(numberRelatives_just_created == TRUE){
                CensusData[ , numberRelatives := NULL]
        }

        gc()
        CensusData
}
