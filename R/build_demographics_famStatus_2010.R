#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_demographics_famStatus_2010 <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        check_vars <- check_var_existence(CensusData, c("v0502"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData[ v0502 == 1     , famStatus := 1 ]
        CensusData[ v0502 %in% 2:3 , famStatus := 2 ]
        CensusData[ v0502 %in% 4:6 , famStatus := 3 ]
        CensusData[ v0502 %in% 7:20, famStatus := 4 ]
        
        
        gc()
        
        CensusData
}

