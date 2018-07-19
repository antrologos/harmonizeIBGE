#' Builds a synthetic variable for age - 1970
#' @param data.frame
#' @value data.frame
#' @import data.table
#' @export


build_identification_idhh_1970 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v006", "v007", "v025"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        warning("This function requires:\n1) the data to be never resorted before. The order of the lines must be\n   exactly the same as in the original IBGE files.\n2) The data is complete: no registry were excluded. It has 24.793.358 lines.")

        CensusData[ , flag := as.numeric(NA)]
        CensusData[ , ind_collective := v007]
        CensusData[is.na(ind_collective), ind_collective := 0]
        
        # (1) In private households, a person who is head of a family starts a new household 
        # (except if her family is a secondary family)
        CensusData[ind_collective == 0 & v025 == 1 & (v006 %in% c(1,2)), flag := 1]

        # (2) The first person living collective household starts a new household. We will assume every adjacent persons
        # also marked as living in a collective househould inhabit that same house househould. In other words, a collective
        # household just ends when a particular household starts.
        CensusData[ , diff_collective := c(0,diff(ind_collective))]
        CensusData[diff_collective == 1, flag := 1]

        # Persons living alone start a new household
        CensusData[v006 == 0, flag := 1]

        # Adjusting: stating clearly that heads os secondary families do not start a household
        # CensusData[v025 == 1 & (v006 %in% c(3,4)), flag := NA]
        
        #(3) All the other registries will receive the same household id given to the previous line
        n_househoulds <- nrow(CensusData[flag == 1])
        CensusData[flag == 1, idhh := 1:n_househoulds]
        CensusData[, idhh := zoo::na.locf(idhh)]

        CensusData[ , c("flag", "ind_collective", "diff_collective") := NULL ]

        CensusData

}

