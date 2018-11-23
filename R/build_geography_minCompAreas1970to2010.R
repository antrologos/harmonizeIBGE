#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_geography_minCompAreas1970to2010 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        metadata <- harmonizeIBGE:::get_metadata(CensusData)
        
        if(metadata$year == 1960){
                return(CensusData)
        }
        
        municipality2010standard_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("municipality2010standard"))
        if(length(check_vars) > 0){
                CensusData <- eval(parse(text = paste0("build_geography_municipality2010standard_",metadata$year,"(CensusData)")))
                municipality2010standard_just_created = T
                gc(); Sys.sleep(2); gc()
        }
        
        data(ehrl_mca_1970_2010)
        
        ehrl_mca_1970_2010 <- ehrl_mca_1970_2010 %>% 
                select(municipality2010_6d, mca) %>%
                rename(municipality2010standard = municipality2010_6d) %>%
                as.data.table() %>%
                setkey("municipality2010standard")
        
        setkey(CensusData, "municipality2010standard")
        gc(); Sys.sleep(.5); gc()
        CensusData[ehrl_mca_1970_2010, mca:= mca]

        gc();Sys.sleep(.5);gc()
        
        CensusData <- harmonizeIBGE:::set_metadata(CensusData, metadata = metadata)
        
        gc()
        CensusData
}
