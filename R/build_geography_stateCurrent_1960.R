#' Builds a synthetic variable for education attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_geography_stateCurrent_1960 <- function(CensusData, persons = T){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        warning("===================================================================================\nThis function assumes you are working with the 1.27% sample of the 1960 Census,\nas prepared and consisted by the Center for Metropolitan Studies (Centro de Estudos\nda Metropole - USP/Brazil).\n\nEvery individual will be given the same sample weight, equal to the inverse of the\nsample fraction: 1/0.0127\n\nDownload available at: http://200.144.244.241:3003/\nMore information at: http://web.fflch.usp.br/centrodametropole/\n===================================================================================")
        
        if(persons == T){
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("uf_pess"))
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }        
                
                CensusData[ , stateCurrent := uf_pess]
                
        }else{
                check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("uf_dom"))
                if(length(check_vars) > 0){
                        stop("The following variables are missing from the data: ",
                             paste(check_vars, collapse = ", "))
                }        
                CensusData[ , stateCurrent := uf_dom]
                
        }

        warning(paste("Despite all efforts made by the Center for Metropolitan Studies in order to make
the 1960 Census (1.27% Sample) consistent, that database still has some unsolved
issues.  Persons and household data have disprepancies in a few registries. That's
why there is two different variables for 'state': uf_pess and uf_dom. So:

-- Setting the argument 'persons' to TRUE in this function will produce an harmonized 
   variable based on 'uf_pess'.
-- Setting it to FALSE will produce an harmonized variable based on 'uf_dom'.

In this case, it was:",persons))
        
        gc()
        CensusData
}