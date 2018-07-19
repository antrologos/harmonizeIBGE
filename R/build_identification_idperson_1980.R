#' Builds a synthetic variable for age - 1980
#' @param data.frame
#' @value data.frame
#' @export

build_identification_idperson_1980 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        # Person ID
        CensusData[ , idperson := 1980*10^8 + 1:nrow(CensusData)]

        CensusData
}
