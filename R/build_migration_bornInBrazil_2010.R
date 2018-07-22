#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_migration_bornInBrazil_2010 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v0618","v0619","v0620"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        # For persons who were born in the same municipality
        CensusData[ , bornInBrazil := ifelse(v0618 %in% c(1, 2), 1, 0)]
        
        # For persons who were born in the same state
        CensusData[v0619 %in% c(1, 2), bornInBrazil := 1]
        
        # For persons who were born in Brazil
        CensusData[v0620 == 1, bornInBrazil := 1]
        
        gc()
        CensusData
}






