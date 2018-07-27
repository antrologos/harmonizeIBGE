#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_migration_bornSameMunicipality_1980 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v513"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        CensusData[ , bornSameMunicipality := ifelse(v513 == 1, 1, 0)]
        CensusData[is.na(v513), bornSameMunicipality := NA] #non-dwellers are missing
        
        gc()
        CensusData
}






