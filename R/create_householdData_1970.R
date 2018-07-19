
#CensusData <- prepare_to_harmonize(CensusData)
#state_var_name = "cem005"

create_householdData_1970 <- function(CensusData,
                                      state_var_name = "uf"){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        househould_vars = c("v001", "v002", "v003", "v004",
                            "v006", "v007",
                            "v008", "v009", "v010", "v011", "v012",
                            "v013", "v014", "v015", "v016", "v017",
                            "v018", "v019", "v020", "v021", "v054",
                            "v024", "v025", "v041")

        check_vars <- check_var_existence(CensusData, c(state_var_name,househould_vars))

        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        check_vars <- check_var_existence(CensusData, "municipality1970standard")
        if(length(check_vars) > 0){
                CensusData <- build_geography_municipality1970standard_1970(CensusData,
                                                                            state_var_name = state_var_name)
        }

        check_vars <- check_var_existence(CensusData, "municipality2010standard")
        if(length(check_vars) > 0){
                CensusData <- build_geography_municipality2010standard_1970(CensusData,
                                                                            state_var_name = state_var_name)
        }

        CensusData[ , state := CensusData[[state_var_name]] ]

        check_vars <- check_var_existence(CensusData, "idhh")
        if(length(check_vars) > 0){
                CensusData <- build_identification_idhh_1970(CensusData)
        }

        check_vars <- check_var_existence(CensusData, "relative")
        if(length(check_vars) > 0){
                CensusData <- build_demographics_relative_1970(CensusData)
        }

        check_vars <- check_var_existence(CensusData, "numberRelatives")
        if(length(check_vars) > 0){
                CensusData <- build_demographics_numberRelatives_1970(CensusData)
        }

        check_vars <- check_var_existence(CensusData, "hhIncomeTotNominal")
        if(length(check_vars) > 0){
                CensusData <- build_income_hhIncomeTotNominal_1970(CensusData)
        }

        check_vars <- check_var_existence(CensusData, "hhIncomePerCapNominal")
        if(length(check_vars) > 0){
                CensusData <- build_income_hhIncomePerCapNominal_1970(CensusData)
        }






        househould_vars

        CensusData %>%
                group_by(idhh) %>%
                select(state,
                       v001, v002, v003, v004, v005, v006, v007,
                       v008, v009, v010, v011, v012, v013, v014,
                       v015, v016, v017, v018, v019, v020, v021,
                       v054, hhIncomeTotNominal, hhIncomePerCapNominal,
                       numberRelatives,
                       municipality1970standard, municipality2010standard,
                       househould_vars) %>%
                summarise_all()



        v001
        v002
        v003
        v004
        v007
        v008
        v009
        v010
        v011
        v012
        v013
        v014
        v015
        v016
        v017
        v018
        v019
        v020
        v021
        v054
        cem001
        cem002
        cem003
        cem004
        cem005
        municcode1970
        municcode2010


        householdData
}
