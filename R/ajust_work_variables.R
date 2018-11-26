#' Implements a correction for the variable "class worker"
#' @param data.frame
#' @value data.frame
#' @export


ajust_work_variables <- function(CensusData){

        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        
        type <- metadata$type
        year <- metadata$year
        
        if(type == "census" & year == 1980){
                var_hours = "v535"
        }
        
        if(type == "census" & year == 1991){
                var_hours = "v0354"
        }
        
        if(type == "census" & year == 2000){
                var_hours = "v0453" 
        }
        
        if(type == "census" & year == 2010){
                var_hours = "v0653" 
        }
        
        if(year %in% c(1960, 1970)){
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("classWorker","occupationalStatus", "econActivity"))
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }
        }else{
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("classWorker","occupationalStatus", "econActivity", var_hours))
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }        
        }
        
        
        if(type == "census" & year == 1980){
                CensusData[v535 %in% 2:5, at_least_15hours_work := 1]
        }
        
        if(type == "census" & year == 1991){
                CensusData[v0354 >= 15, at_least_15hours_work := 1]
        }
        
        if(type == "census" & year == 2000){
                CensusData[v0453 >= 15, at_least_15hours_work := 1]
        }
        
        if(type == "census" & year == 2010){
                CensusData[v0653 >= 15, at_least_15hours_work := 1]
        }
       
        
        if(year %in% c(1960, 1970)){
                CensusData[ is.na(occupationalStatus) | occupationalStatus == 0, classWorker := NA]
        }else{
                for(i in 1:2){ # repete duas vezes a operacao porque as variaveis se modificam
                        CensusData[ is.na(occupationalStatus) | occupationalStatus == 0,                                      classWorker := NA]
                        CensusData[ is.na(at_least_15hours_work) &  classWorker %in% 3:4 & econActivity == 1,                 econActivity := 0]
                        CensusData[ is.na(at_least_15hours_work) &  classWorker %in% 3:4 & occupationalStatus == 1, occupationalStatus    := NA]
                        gc();Sys.sleep(.5);gc()
                }        
        }
        
        CensusData[ is.na(occupationalStatus) | occupationalStatus ==0,    isco88     := NA]
        CensusData[ is.na(occupationalStatus) | occupationalStatus ==0,    sectorISIC3 := NA]
        
        if(!(year %in% c(1960, 1970))){
                CensusData[ , at_least_15hours_work := NULL]        
                gc();Sys.sleep(.5);gc()
        }
        
        gc();Sys.sleep(.5);gc()
        return(CensusData)

}
