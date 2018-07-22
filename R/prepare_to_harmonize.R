#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

prepare_to_harmonize <- function(CensusData, year,
                                 state_var_name = NULL){

        # Cheking if it is a data.frame
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        # Converting to data.table
        if((length(year) != 1) &  !(year %in% c(1960, 1970, 1980, 1991, 2000, 2010))){
                stop("'year' must be: 1960, 1970, 1980, 1991, 2000, or 2010")
        }

        # Converting to data.table
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        # Variable names to lower case
        setnames(x = CensusData, old = names(CensusData), new = tolower(names(CensusData)))
        warning("All variable names were set to lowercase")
        
        
        call <- paste0("build_identification_year_",year,"(CensusData)")
        CensusData <- eval(parse(text = call))
        
        if(year == 1970){
                
                if(is.null(state_var_name)){
                        stop("\n1970 Census: For the year 1970, you have to specify 'state_var_name'. The original\ndatabase produced by IBGE do not contains an state variable. So each user may have\ncreated a different name for it. This function will rename it to 'uf'.")
                }else{
                        
                        # Converting to data.table
                        if((length(state_var_name) != 1) | !is.character(state_var_name)){
                                stop("\n'state_var_name' must be a single-valued character vector informing the name of the\nvariable representing the Brazilian states in the 1970 Census.")
                        }
                        
                        setnames(CensusData, old = state_var_name, new = "uf")
                        warning("1970 Census: The variable for states was renamed to 'uf'.")
                }
        }
        
        gc()
        CensusData
}
