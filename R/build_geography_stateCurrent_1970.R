#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_geography_stateCurrent_1970 <- function(CensusData, 
                                              state_var_name = "uf"){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c(state_var_name))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        CensusData[ , stateCurrent := CensusData[[state_var_name]] ]
        
        gc()
        CensusData
}