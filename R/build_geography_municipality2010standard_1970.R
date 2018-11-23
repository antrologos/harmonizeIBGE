#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_geography_municipality2010standard_1970 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        metadata <- harmonizeIBGE:::get_metadata(CensusData)
        
        municipalityCurrent_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("municipalityCurrent"))
        if(length(check_vars) > 0){
                CensusData <- build_geography_municipalityCurrent_1970(CensusData)
                municipalityCurrent_just_created = T
                gc();Sys.sleep(1);gc()
        }

        data(crosswalk_munic_1970_to_2010)

        crosswalk_munic_1970_to_2010 = data.table(crosswalk_munic_1970_to_2010 %>%
                                                          select(municipality1970standard,
                                                                 municipality2010standard)) %>%
                rename(municipalityCurrent = municipality1970standard) %>%
                as.data.table() %>%
                setkey("municipalityCurrent")

        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("municipality2010standard"))
        if(length(check_vars) == 0){
                CensusData[ , municipality2010standard := NULL]
        }
        
       
        setkey(CensusData, "municipalityCurrent")
        gc(); Sys.sleep(1); gc()
        CensusData[crosswalk_munic_1970_to_2010, municipality2010standard:= municipality2010standard]
        
        gc();Sys.sleep(2);gc()
        
        n_digit <- nchar(min(CensusData$municipality2010standard))
        if(n_digit == 7){
                CensusData[ , municipality2010standard := trunc(municipality2010standard/10)]
        }

        if(municipalityCurrent_just_created == T){
                CensusData[ , municipalityCurrent := NULL]
        }

        gc()
        
        CensusData <- harmonizeIBGE:::set_metadata(CensusData, metadata = metadata)

        CensusData
}

