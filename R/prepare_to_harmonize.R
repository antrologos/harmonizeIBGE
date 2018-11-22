#' Builds a synthetic variable for age - 1960
#' @param Data
#' @param type
#' @param year
#' @param pnadc_freq
#' @param quarter
#' @param state_var_name
#' @value data.frame
#' @export

prepare_to_harmonize <- function(Data,
                                 type,
                                 year,
                                 pnadc_freq = NULL,
                                 quarter = NULL,
                                 state_var_name = NULL){
        
        if(!(type %in% c("pnad", "pnadc", "census"))){
                stop("'type' must be 'pnad', 'pnadc' or 'census'.")
        }
        
        if(type == "pnad"){
                time_frame = c(1973, 1976:1979, 1981:1990, 1992, 1993, 1995:1999, 2001:2009, 2011:2015)
                
                if((length(year) != 1) | !(year %in% time_frame)){
                        stop(paste("'year' must be one of the following:", paste(time_frame, collapse = ", ")))
                }
        }
        
        if(type == "pnadc"){
                time_frame = 2012:2018
                
                if((length(year) != 1) | !(year %in% time_frame)){
                        stop(paste("'year' must be one of the following:", paste(time_frame, collapse = ", ")))
                }
                
                if(is.null(pnadc_freq) | !(pnadc_freq %in% c("annual", "quarterly"))){
                        stop(paste("'pnadc_freq' must be 'annual' or 'quarterly'."))
                }
                
                if(pnadc_freq == "annual" & !is.null("quarter")){
                        warning("The data was defined as 'annual'. The argument 'quarter' will be ignored")
                }
                
                if(pnadc_freq == "quarterly" & !(quarter %in% 1:4)){
                        stop("The data was defined as 'quarterly'. The argument 'quarter' must be 1, 2, 3, or 4.")
                }
                
        }
        
        
        if(type == "census"){
                time_frame = c(1960, 1970, 1980, 1991, 2000, 2010)
                
                if((length(year) != 1) | !(year %in% time_frame)){
                        stop(paste("'year' must be one of the following:", paste(time_frame, collapse = ", ")))
                }
                
        }
        
        # Cheking if it is a data.frame and converting to data.table
        Data <- harmonizeIBGE:::check_Data_frame_convert_to_data_table(Data)
        
        attr(Data, which = "readyToHarmonize") <- FALSE
        
        attr(Data, which = "type") <- type
        attr(Data, which = "year") <- year
        
        if(type == "pnadc"){
                attr(Data, which = "pnadc_freq") <- pnadc_freq
                
                if(pnadc_freq == "quarterly") {
                        attr(Data, which = "quarter") <- quarter
                }
        }
        
        # Variable names to lower case
        
        varNames <- names(Data)
        to_remove_VAR <- varNames[grep(x = varNames, pattern = "^var[[:digit:]]{4,6}", ignore.case = T)]
        VAR_removed   <- gsub(x = to_remove_VAR, pattern = "^var", replacement = "v", ignore.case = T)
        setnames(x = Data, old = to_remove_VAR, new = VAR_removed)
        
        var_tolower <- names(Data)
        var_tolower <- var_tolower[grep(x = var_tolower, pattern = "[Vv][Dd]?[[:digit:]]{4,6}")]
        setnames(x = Data, old = var_tolower, new = tolower(var_tolower))
        
        var_tolower2 <- names(Data)
        var_tolower2 <- var_tolower2[grep(x = var_tolower2, pattern = "[P][p]?[[:digit:]]{4,6}")]
        setnames(x = Data, old = var_tolower2, new = tolower(var_tolower2))
        
        
        if(type == "pnad"){
                
                vars_to_fill <- names(Data)
                vars_to_fill <- vars_to_fill[grep(x = vars_to_fill, pattern = "[vp][d]?[[:digit:]]{4,6}")]
                
                list_digits <- stringr::str_locate_all(vars_to_fill, "[[:digit:]]")
                place_to_split <- as.numeric(sapply(list_digits, function(x) x[1,1]))
                
                var_start <- str_sub(vars_to_fill, start = 1, end = (place_to_split-1))
                var_end   <- str_sub(vars_to_fill, start = place_to_split, end = nchar(vars_to_fill))
                
                var_end[nchar(var_end) == 1] = paste("000", var_end[nchar(var_end) == 1])
                var_end[nchar(var_end) == 2] = paste("00",  var_end[nchar(var_end) == 2])
                var_end[nchar(var_end) == 3] = paste("0",   var_end[nchar(var_end) == 3])
                
                vars_filled <- paste0(var_start, var_end)
                
                setnames(x = Data, old = vars_to_fill, new = vars_filled)
        }
        
        
        
        if(type == "census" & year == 1970){
                
                if(is.null(state_var_name)){
                        warning("\n1970 Census: For the year 1970, you have to specify 'state_var_name'. The original\ndatabase produced by IBGE do not contains an state variable. So each user may have\ncreated a different name for it. If you do not want to use this variable, inform 'none'")
                }else{
                        
                        # Converting to data.table
                        if((length(state_var_name) != 1) | !is.character(state_var_name) | is.na(state_var_name)){
                                stop("\n'state_var_name' must be a single-valued character vector informing the name of the\nvariable representing the Brazilian states in the 1970 Census (or simply 'none').")
                        }
                        
                        if(!(state_var_name == 'none')){
                                attr(Data, which = "state_var_name") <- state_var_name
                        }else{
                                attr(Data, which = "state_var_name") <- NA
                        }
                }
        }
        
        attr(Data, which = "readyToHarmonize") <- TRUE
        
        gc()
        Data
}
