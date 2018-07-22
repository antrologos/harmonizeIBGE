#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_geography_municipality2010standard_1970 <- function(CensusData,
                                                          state_var_name = "uf"){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!(is.character(state_var_name) & (length(state_var_name)==1) )){
                stop("'state_var_name' must be a single-valued character vector")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        municipalityCurrent_just_created = F
        check_vars <- check_var_existence(CensusData, c("municipalityCurrent"))
        if(length(check_vars) > 0){
                CensusData <- build_geography_municipalityCurrent_1970(CensusData,
                                                                       state_var_name = state_var_name)
                municipalityCurrent_just_created = T
                gc()
        }

        data(crosswalk_munic_1970_to_2010)

        crosswalk_munic_1970_to_2010 = data.table(crosswalk_munic_1970_to_2010 %>%
                                                          select(municipality1970standard,
                                                                 municipality2010standard)) %>%
                rename(municipalityCurrent = municipality1970standard)

        CensusData = data.table:::merge.data.table(x     = CensusData,
                                                   y     = crosswalk_munic_1970_to_2010,
                                                   by    = "municipalityCurrent",
                                                   all.x = T,
                                                   all.y = F,
                                                   sort  = F)
        gc();Sys.sleep(1);gc()
        
        
        n_digit <- nchar(min(CensusData[ , municipality2010standard]))
        if(n_digit == 7){
                CensusData[ , municipality2010standard := trunc(municipality2010standard/10)]
        }

        if(municipalityCurrent_just_created == T){
                CensusData[ , municipalityCurrent := NULL]
        }

        gc()

        CensusData
}

