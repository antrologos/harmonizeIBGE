#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_geography_stateMiniumComparable_1960 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        metadata    <- harmonizeIBGE:::get_metadata(CensusData)
        
        stateCurrent_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("stateCurrent"))
        if(length(check_vars) > 0){
                CensusData <- build_geography_stateCurrent_1960(CensusData)
                stateCurrent_just_created = T
                gc()
        }
        
        CensusData[ , stateMiniumComparable := NULL]
        
        data("crosswalk_states_tmp")
        
        crosswalk_states_tmp <- crosswalk_states_tmp %>%
                filter(year == 1960 & variable == "state") %>%
                select(original_code, semi_harmonized_code) %>%
                rename(stateCurrent          = original_code,
                       stateMiniumComparable = semi_harmonized_code) %>%
                as.data.table() %>%
                setkey("stateCurrent")
        
        setkey(CensusData, "stateCurrent")
        gc(); Sys.sleep(.5); gc()
        
        CensusData[crosswalk_states_tmp, stateMiniumComparable:= stateMiniumComparable]
        
        Sys.sleep(.5);gc()
        
        if(stateCurrent_just_created == TRUE){
                CensusData[ , stateCurrent := NULL]
        }
        
        CensusData <- harmonizeIBGE:::set_metadata(Data = CensusData, metadata = metadata) 
        gc()
        CensusData
       
}
