#' Builds a synthetic variable for age - 2000
#' @param data.frame
#' @value data.frame
#' @export

build_identification_idperson_2000 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        # Person ID
        CensusData[ , idperson := 2000*10^9 + V0300*10^2 + V0400]

        CensusData
}



