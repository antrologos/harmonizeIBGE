#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_identification_wgtperson_1960 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        warning(paste("===================================================================================",
                       "This function assumes you are working with the Compiled Sample of the 1960 Census, as",
                       "prepared and consisted by the Center for Metropolitan Studies (Centro de Estudos da", 
                       "Metropole - USP/Brazil).",
                       "More information at: http://web.fflch.usp.br/centrodametropole/",
                       "===================================================================================", 
                      sep = "\n"))

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        check_vars <- check_var_existence(CensusData, c("cem_wgt"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        #Weight
        CensusData[ , wgtperson := cem_wgt]

        gc
        CensusData

}
