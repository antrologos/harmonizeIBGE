#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_geography_municipalityCurrent_1970 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        metadata <- harmonizeIBGE:::get_metadata(CensusData)
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c(metadata$state_var_name, "v001", "v002"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData$state_tmp_for_coding <- CensusData[[metadata$state_var_name]]
        
        # Catches a list of the state codes
        # The Census od 1970 is lacking a variable for states.
        # Often, users attribute to the state code the number which comes in the data file name
        # But those numbers are not the official codes
        state_values <- unique(CensusData$state_tmp_for_coding) %>% sort()
        
        # Builds up a list of the official codes
        census1970_official_stateCode = c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 13, 14, 15, 16, 17,
                                          19, 21, 23, 24, 25, 27, 29, 31, 32, 34, 35, 36)
        
        # If the official codes are all contained in the list of state codes, the
        # empirical variable is considered to be alright; else we proceed a recodification
        test_state_code <- all(census1970_official_stateCode %in% state_values)
        
        if(test_state_code == T){
                CensusData[ , state_tmp_for_coding2 := state_tmp_for_coding]
        }else{
                CensusData[ state_tmp_for_coding == 2 , state_tmp_for_coding2 := 2]
                CensusData[ state_tmp_for_coding == 13, state_tmp_for_coding2 := 15]
                CensusData[ state_tmp_for_coding == 3 , state_tmp_for_coding2 := 3]
                CensusData[ state_tmp_for_coding == 6 , state_tmp_for_coding2 := 6]
                CensusData[ state_tmp_for_coding == 16, state_tmp_for_coding2 := 19]
                CensusData[ state_tmp_for_coding == 9 , state_tmp_for_coding2 := 10]
                CensusData[ state_tmp_for_coding == 27, state_tmp_for_coding2 := 36]
                CensusData[ state_tmp_for_coding == 18, state_tmp_for_coding2 := 23]
                CensusData[ state_tmp_for_coding == 14, state_tmp_for_coding2 := 16]
                CensusData[ state_tmp_for_coding == 20, state_tmp_for_coding2 := 25]
                CensusData[ state_tmp_for_coding == 26, state_tmp_for_coding2 := 35]
                CensusData[ state_tmp_for_coding == 7 , state_tmp_for_coding2 := 8]
                CensusData[ state_tmp_for_coding == 17, state_tmp_for_coding2 := 21]
                CensusData[ state_tmp_for_coding == 25, state_tmp_for_coding2 := 34]
                CensusData[ state_tmp_for_coding == 5 , state_tmp_for_coding2 := 5]
                CensusData[ state_tmp_for_coding == 11, state_tmp_for_coding2 := 13]
                CensusData[ state_tmp_for_coding == 12, state_tmp_for_coding2 := 14]
                CensusData[ state_tmp_for_coding == 8 , state_tmp_for_coding2 := 9]
                CensusData[ state_tmp_for_coding == 22, state_tmp_for_coding2 := 29]
                CensusData[ state_tmp_for_coding == 19, state_tmp_for_coding2 := 24]
                CensusData[ state_tmp_for_coding == 10, state_tmp_for_coding2 := 11]
                CensusData[ state_tmp_for_coding == 1 , state_tmp_for_coding2 := 1]
                CensusData[ state_tmp_for_coding == 4 , state_tmp_for_coding2 := 4]
                CensusData[ state_tmp_for_coding == 24, state_tmp_for_coding2 := 32]
                CensusData[ state_tmp_for_coding == 23, state_tmp_for_coding2 := 31]
                CensusData[ state_tmp_for_coding == 15, state_tmp_for_coding2 := 17]
                CensusData[ state_tmp_for_coding == 21, state_tmp_for_coding2 := 27]
        }
        gc();Sys.sleep(1);gc()
        
        # 2-digit microrregion.
        CensusData[ , micro_2digit := v001%%100]
        
        # municipality1970standard
        CensusData[ , municipalityCurrent := (state_tmp_for_coding2*100000)+(micro_2digit*1000)+v002]
        
        # Correction for Brasilia
        CensusData[ state_tmp_for_coding2 == 36, municipalityCurrent := 3600000]
        
        # Correction for Rio de Janeiro
        CensusData[ state_tmp_for_coding2 == 25, municipalityCurrent := 2531000]
        
        CensusData[ , c("state_tmp_for_coding", "state_tmp_for_coding2",
                        "micro_2digit") := NULL]
        gc()
        
        CensusData
}

