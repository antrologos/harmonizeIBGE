#' Builds a synthetic variable for age - 1991
#' @param data.frame
#' @value data.frame
#' @export

build_identification_idperson_1991 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        
        # Person ID
        CensusData[ , idperson := 1991*10^11 + V0102*10^2 + V0098]

        CensusData
}

