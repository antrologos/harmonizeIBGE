#' @export

drop_originalVariables <- function(CensusData){
        
        CensusData %>%
                select(names(CensusData)[ names(CensusData) %in% list_harmonizedVariables()[, 2]])
        
}

