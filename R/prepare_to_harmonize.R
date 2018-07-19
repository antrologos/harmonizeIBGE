#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

prepare_to_harmonize <- function(CensusData){

        # Cheking if it is a data.frame
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        # Converting to data.table
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        # Variable names in lower case
        setnames(x = CensusData, old = names(CensusData), new = tolower(names(CensusData)))

        CensusData
}
