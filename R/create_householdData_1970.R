#' Builds a synthetic variable for age - 1970
#' @param data.frame
#' @value data.frame
#' @import data.table
#' @import dplyr
#' @import zoo
#' @export

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

        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c(state_var_name,househould_vars))

        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "municipality1970standard")
        if(length(check_vars) > 0){
                CensusData <- build_geography_municipality1970standard_1970(CensusData,
                                                                            state_var_name = state_var_name)
        }

        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "municipality2010standard")
        if(length(check_vars) > 0){
                CensusData <- build_geography_municipality2010standard_1970(CensusData,
                                                                            state_var_name = state_var_name)
        }

        CensusData[ , state := CensusData[[state_var_name]] ]

        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "idhh")
        if(length(check_vars) > 0){
                CensusData <- build_identification_idhh_1970(CensusData)
        }

        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "relative")
        if(length(check_vars) > 0){
                CensusData <- build_demographics_relative_1970(CensusData)
        }
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "numberRelatives")
        if(length(check_vars) > 0){
                CensusData <- build_demographics_numberRelatives_1970(CensusData)
        }

        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "numberDwellers")
        if(length(check_vars) > 0){
                CensusData <- build_household_numberDwellers_1970(CensusData)
        }
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "hhIncomeTotNominal")
        if(length(check_vars) > 0){
                CensusData <- build_income_hhIncomeTotNominal_1970(CensusData)
        }

        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "hhIncomePerCapNominal")
        if(length(check_vars) > 0){
                CensusData <- build_income_hhIncomePerCapNominal_1970(CensusData)
        }
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "wgthh")
        if(length(check_vars) > 0){
                CensusData <- build_identification_wgthh_1970(CensusData)
        }
        
        gc()
        
        householdData <- CensusData %>%
                filter(relative == 1) %>% #excludes guests and domestic servants
                select(idhh, 
                       v001, v002, v003, v004, v006, v007,
                       v008, v009, v010, v011, v012, v013, v014,
                       v015, v016, v017, v018, v019, v020, v021,
                       wgthh, 
                       numberDwellers, numberRelatives, 
                       hhIncomeTotNominal, hhIncomePerCapNominal,
                       state,
                       municipality1970standard, municipality2010standard) %>%
                group_by(idhh) %>% 
                summarise_all(first)
        
        householdData = data.table(householdData)
        
        gc()
        householdData
}
