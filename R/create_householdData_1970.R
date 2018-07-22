#' Builds a synthetic variable for age - 1970
#' @param data.frame
#' @value data.frame
#' @import zoo
#' @export

create_householdData_1970 <- function(CensusData,
                                      state_var_name = "uf"){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!(is.character(state_var_name) & (length(state_var_name)==1) )){
                stop("'state_var_name' must be a single-valued character vector")
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
        gc()
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "idhh")
        if(length(check_vars) > 0){
                cat("The variable 'idhh' was missing from your data. Building it now...\n")
                CensusData <- build_identification_idhh_1970(CensusData)
        }
        gc()
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "municipalityCurrent")
        if(length(check_vars) > 0){
                cat("The variable 'municipalityCurrent' was missing from your data. Building it now...\n")
                CensusData <- build_geography_municipalityCurrent_1970(CensusData,
                                                                       state_var_name = state_var_name)
        }
        gc()
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "municipality2010standard")
        if(length(check_vars) > 0){
                cat("The variable 'municipality2010standard' was missing from your data. Building it now...\n")
                CensusData <- build_geography_municipality2010standard_1970(CensusData,
                                                                            state_var_name = state_var_name)
        }
        gc()
        
        CensusData[ , state := CensusData[[state_var_name]] ]
        
        gc()
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "relative")
        if(length(check_vars) > 0){
                cat("The variable 'relative' was missing from your data. Building it now...\n")
                CensusData <- build_demographics_relative_1970(CensusData)
        }
        gc()
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "numberRelatives")
        if(length(check_vars) > 0){
                cat("The variable 'numberRelatives' was missing from your data. Building it now...\n")
                CensusData <- build_demographics_numberRelatives_1970(CensusData)
        }
        gc()
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "numberDwellers")
        if(length(check_vars) > 0){
                cat("The variable 'numberDwellers' was missing from your data. Building it now...\n")
                CensusData <- build_household_numberDwellers_1970(CensusData)
        }
        gc()
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "hhIncomeTotNominal")
        if(length(check_vars) > 0){
                cat("The variable 'hhIncomeTotNominal' was missing from your data. Building it now...\n")
                CensusData <- build_income_hhIncomeTotNominal_1970(CensusData)
        }
        gc()
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "hhIncomePerCapNominal")
        if(length(check_vars) > 0){
                cat("The variable 'hhIncomePerCapNominal' was missing from your data. Building it now...\n")
                CensusData <- build_income_hhIncomePerCapNominal_1970(CensusData)
        }
        gc()
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "wgthh")
        if(length(check_vars) > 0){
                cat("The variable 'wgthh' was missing from your data. Building it now...\n")
                CensusData <- build_identification_wgthh_1970(CensusData)
        }
        gc()
        
        cat("householdData: Selecting just relevant variables ...\n")
        householdData <- CensusData %>%
                select(idhh, 
                       v001, v002, v003, v004, v006, v007,
                       v008, v009, v010, v011, v012, v013, v014,
                       v015, v016, v017, v018, v019, v020, v021,
                       wgthh, 
                       numberDwellers, numberRelatives, 
                       hhIncomeTotNominal, hhIncomePerCapNominal,
                       state,
                       municipalityCurrent, municipality2010standard) 
        
        rm(CensusData)
        gc();Sys.sleep(1);gc()
        
        cat("householdData: Aggregating persons into household registries ...\n")
        householdData <- householdData[, list(v001 = first(v001),
                                              v002 = first(v002),
                                              v003 = first(v003),
                                              v004 = first(v004),
                                              v006 = first(v006),
                                              v007 = first(v007),
                                              v008 = first(v008),
                                              v009 = first(v009),
                                              v010 = first(v010),
                                              v011 = first(v011),
                                              v012 = first(v012),
                                              v013 = first(v013),
                                              v014 = first(v014),
                                              v015 = first(v015),
                                              v016 = first(v016),
                                              v017 = first(v017),
                                              v018 = first(v018),
                                              v019 = first(v019),
                                              v020 = first(v020),
                                              v021 = first(v021),
                                              wgthh                    = first(wgthh),
                                              numberDwellers           = first(numberDwellers),
                                              numberRelatives          = first(numberRelatives),
                                              hhIncomeTotNominal       = first(hhIncomeTotNominal),
                                              hhIncomePerCapNominal    = first(hhIncomePerCapNominal),
                                              state                    = first(state),
                                              municipalityCurrent      = first(municipalityCurrent),
                                              municipality2010standard = first(municipality2010standard)),
                                       by = idhh]
        
        gc()
        householdData
}
