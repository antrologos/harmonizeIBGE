#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_identification_wgthh_1970 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v054"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        idhh_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, "idhh")
        if(length(check_vars) > 0){
                CensusData <- build_identification_idhh_1970(CensusData)
                idhh_just_created = T
        }
        
        #Weight
        # For unique or main families
        CensusData[v006 %in% c(1, 2) & v025 == 1, wgthh := as.numeric(v054)]
        
        # For persons living alone
        CensusData[v006 == 0, wgthh := as.numeric(v054)]
        
        # For collective households - the weight will be equal to the average weight 
        CensusData[v007 == 1, wgthh := mean(v054), by = idhh]
        
        # All the others
        CensusData[, wgthh := zoo::na.locf(wgthh)]

        gc()
        
        if(idhh_just_created == T){
                CensusData[ , idhh := NULL]
        }
        
        gc()
        CensusData

}
