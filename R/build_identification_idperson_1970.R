#' Builds a synthetic variable for age - 1970
#' @param data.frame
#' @value data.frame
#' @export

build_identification_idperson_1970 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        warning("This function the data has never been resorted before. The order of the lines must be\nexactly the same as in the original IBGE files.")
        
        # Person ID
        CensusData[ , idperson := 1970*10^8 + 1:nrow(CensusData)]
        
        CensusData
}

