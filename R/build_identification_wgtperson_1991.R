#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_identification_wgtperson_1991 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v7301"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        #Weight
        CensusData[ , wgtperson := v7301]
        
        if( CensusData[ , mean(wgtperson, na.rm = T)] > 1000){
                CensusData[ , wgtperson := wgtperson/(10^8)]
        }

        gc()
        CensusData

}
