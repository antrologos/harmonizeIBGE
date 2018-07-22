#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_geography_minCompAreas1970to2010_1991 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        municipality2010standard_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("municipality2010standard"))
        if(length(check_vars) > 0){
                CensusData <- build_geography_municipality2010standard_1991(CensusData)
                municipality2010standard_just_created = T
                gc()
        }
        
        data(ehrl_mca_1970_2010)
        
        ehrl_mca_1970_2010 <- ehrl_mca_1970_2010 %>% 
                select(municipality2010_6d, mca) %>%
                rename(municipality2010standard = municipality2010_6d)
        
        CensusData <- data.table:::merge.data.table(x     = CensusData,
                                                    y     = ehrl_mca_1970_2010, 
                                                    by    = "municipality2010standard",
                                                    all.x = T, 
                                                    all.y = F, 
                                                    sort  = F)
        
        
        setnames(CensusData, old = "mca", new = "minCompAreas1970to2010")
        
        gc()
        CensusData
}