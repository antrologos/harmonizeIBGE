#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_geography_stateMinimumComparable_1970 <- function(CensusData){
        
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
                CensusData <- build_geography_stateCurrent_1970(CensusData)
                stateCurrent_just_created = T
                gc()
        }
        
        data("crosswalk_states_tmp")
        
        crosswalk_states_tmp <- crosswalk_states_tmp %>%
                filter(year == 1970 & variable == "state") %>%
                select(original_code, semi_harmonized_code) %>%
                rename(stateCurrent          = original_code,
                       stateMinimumComparable = semi_harmonized_code)%>%
                as.data.table() %>%
                setkey("stateCurrent")
        
        setkey(CensusData, "stateCurrent")
        gc(); Sys.sleep(.5); gc()
        
        CensusData[crosswalk_states_tmp, stateMinimumComparable:= stateMinimumComparable]

        Sys.sleep(.5);gc()
        
        
        if(stateCurrent_just_created == TRUE){
                CensusData[ , stateCurrent := NULL]
        }
        
        CensusData <- harmonizeIBGE:::set_metadata(Data = CensusData, metadata = metadata) 
        
        gc()
        CensusData
}
