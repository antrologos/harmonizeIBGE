check_Data_frame_convert_to_data_table <- function(Data){
        if(!is.data.frame(Data)){
                stop("'Data' is not a data.frame")
        }
        
        if(!is.data.table(Data)){
                setDT(Data)
        }else{
                Data
        }
}


check_prepared_to_harmonize <- function(Data){
        test <- attributes(Data)$readyToHarmonize
        
        if(is.null(test)){
                stop("The data was not prepared to be harmonized. Use 'prepare_to_harmonize()' to make it ready to use with harmonizePNAD")
        }
        
        if(test == FALSE){
                stop("The data could not prepared to be harmonized. There was some kind of error when your ran 'prepare_to_harmonize()'")
        }
        
        harmonizePNAD:::check_Data_frame_convert_to_data_table(Data)
}


get_metadata <- function(Data){
        
        metadata = list(type = attributes(Data)$type,
                        year = attributes(Data)$year)
        
        if(metadata$type == "pnadc"){
                metadata$pnadc_freq <- attributes(Data)$pnadc_freq
                if(metadata$pnadc_freq == "quarterly"){
                        metadata$quarter <- attributes(Data)$quarter
                }
        }
        
        if(metadata$type == "census" & metadata$year == 1970){
                metadata$state_var_name <- attributes(Data)$state_var_name
        }
        
        metadata
}


find_sulfix <- function(Data, general_or_specific){
        
        if(length(general_or_specific) != 1 | !(general_or_specific %in% c("general", "specific"))){
                
                stop("'general_or_specific' must be equal to 'general' or 'specific'")
        }
        
        metadata <- harmonizePNAD:::get_metadata(Data)
        
        if(general_or_specific == "general"){
                
                sulfix <- metadata$type
                
        }else{
                
                if(metadata$type == "pnad"){
                        
                        if(metadata$year %in% 1973){
                                sulfix = "pnad1973"
                        }
                        
                        if(metadata$year %in% 1976:1979){
                                sulfix = "pnad1970s"
                        }
                        
                        if(metadata$year %in% 1981:1990){
                                sulfix = "pnad1980s"
                        }
                        
                        if(metadata$year %in% 1992:2015){
                                sulfix = "pnad1990s"
                        }
                        
                }
                
                if(metadata$type == "pnadc"){
                        sulfix = "pnadc"
                }
                
                if(metadata$type == "census"){
                        sulfix = paste0("census",metadata$year)
                }
                
        }
        
        sulfix
}


find_sulfixforOccSectors <- function(Data){
        
        metadata <- harmonizePNAD:::get_metadata(Data)
        
        if(metadata$type == "census" & metadata$year == 1960){
                sulfix = "ibge60"
        }
        
        if((metadata$type == "census" & metadata$year == 1970)|(metadata$type == "pnad" & metadata$year == 1973)){
                sulfix = "ibge70"
        }
        
        if(metadata$type == "pnad" & metadata$year %in% 1976:1979){
                sulfix = "pnads76_79"
        }
        
        if((metadata$type == "census" & metadata$year == 1980)|(metadata$type == "pnad" & metadata$year %in% 1981:1990)){
                sulfix = "ibge80"
        }
        
        if((metadata$type == "census" & metadata$year == 1991)|(metadata$type == "pnad" & metadata$year %in% 1992:2001)){
                sulfix = "ibge91"
        }
        
        if((metadata$type == "census" & metadata$year == 2000)|(metadata$type == "pnad" & metadata$year %in% 2002:2015)){
                sulfix = "cboDom"
        }
        
        if((metadata$type == "census" & metadata$year == 2010)|(metadata$type == "pnadc")){
                sulfix = "cod2010"
        }
        
        
        
        sulfix
}





find_function <- function(Data, pattern, general_or_specific){
        
        existing_functions <- unclass(lsf.str(envir = asNamespace("harmonizePNAD"), all = T))
        relevant_functions <- existing_functions[grep(pattern = pattern, x = existing_functions)]
        
        f_parts <- strsplit(relevant_functions, split = "_")
        f_parts <- f_parts[sapply(f_parts, function(x) length(x) >= 3)]
        
        f_parts <- f_parts[sapply(f_parts, function(x) x[3] == pattern)]
        
        if(!is.na(general_or_specific)){
                sulfix  <- harmonizePNAD:::find_sulfix(Data, general_or_specific = general_or_specific)
                f_parts <- f_parts[sapply(f_parts, function(x) x[4] == sulfix)]
                f_parts <- f_parts[!sapply(f_parts, is.null)]
                
        }
        
        if(length(f_parts) == 0){
                stop("The function you looked for does not exist")
        }
        
        if(length(f_parts) >= 2){
                stop("There is more than one function for doing the job you want...")
        }
        
        f <- paste(unlist(f_parts), collapse = "_")
        
        f
}


check_var_existence <- function(Data, var_names){
        
        test <- sapply(var_names, function(x) is.null(Data[[x]]))
        
        problematic <- NULL
        problematic <- names(test)[which(test)]
        
        problematic
}


check_necessary_vars <-function(Data, var_names){
        
        check_vars <- harmonizePNAD:::check_var_existence(Data, var_names)
        
        if(length(check_vars) > 0){
                stop("The following variables are missing from the Data: ",
                     paste(check_vars, collapse = ", "))
        }
}


just_created_vars_list = function(){
        if(!exists("just_created_vars", envir = parent.frame())){
                just_created_vars <<- NULL
        }
}




build_onTheFly <- function(Data, var_name, general_or_specific){
        
        if(!is.character(var_name) | length(var_name) != 1){
                stop("'var_name' must be a one-valued character vector")
        }
        
        harmonizePNAD:::just_created_vars_list()
        
        f    <- harmonizePNAD:::find_function(Data, var_name, general_or_specific)
        
        call <- paste0(f,"(Data)")
        Data <- eval(parse(text = call))
        gc(); Sys.sleep(.5); gc()
        f
        just_created_vars <<- c(just_created_vars, var_name)
        
        Data
}

check_and_build_onTheFly <- function(Data, var_name, general_or_specific){
        
        test <- harmonizePNAD:::check_var_existence(Data = Data, var_names = var_name)
        
        if(length(test) == 1){
                Data <- harmonizePNAD:::build_onTheFly(Data = Data,
                                                       var_name = var_name,
                                                       general_or_specific = general_or_specific)
        }
        
        Data
}



erase_just_created_vars <- function(Data){
        if(exists("just_created_vars", envir = parent.frame())){
                if(is.character(just_created_vars)){
                        just_created_vars <- just_created_vars[just_created_vars %in% names(Data)]
                        Data[ , c(just_created_vars) := NULL]
                }
                
                rm(just_created_vars, envir = .GlobalEnv)
        }
        Data
}


list_available_harmonizations <- function(x){
        objects <- ls("package:harmonizePNAD")
        objects <- objects[grep(x = objects, pattern = x)]
        objects <- objects[!(objects == paste0("harmonize_",x))]
        
        objects
}



set_metadata <- function(Data, metadata){
        
        attr(Data, which = "type") <- metadata$type
        attr(Data, which = "year") <- metadata$year
        
        if(metadata$type == "pnadc"){
                attr(Data, which = "pnadc_freq") <- metadata$pnadc_freq
                
                if(pnadc_freq == "quarterly") {
                        attr(Data, which = "quarter") <- metadata$quarter
                }
        }
        
        if(metadata$type == "census" & metadata$year == 1970){
                attr(Data, which = "state_var_name") <- metadata$state_var_name
        }
        
        attr(Data, which = "readyToHarmonize") <- TRUE
        
        Data
}



list_originalVariables_to_drop <- function(year, themes = "all"){
        
        existing_themes <- harmonizeIBGE:::get_themes()
        
        if(themes=="all"){
                themes = get_themes()
        }
        
        if(any(!(themes %in% existing_themes))){
                stop("This theme does not exist")
        }
        
        
        df_themes_vars <- get_original_variables_by_theme(year = year, themes = themes) %>%
                setDT() %>%
                setkey("theme", "varName")
        
        priorityList_location <- system.file("extdata",
                                             "list_functions_priority.csv",
                                             package = "harmonizeIBGE")
        
        #priorityList_location <- "E:/Google Drive/RCodes/PacotesR/harmonizeIBGE/inst/extdata/list_functions_priority.csv"
        priorityList <- fread(priorityList_location)
        priorityList <- priorityList[ ,list(theme, varName, ordem)] %>%
                setkey("theme", "varName")
        
        df_themes_vars <- df_themes_vars[priorityList , ordem := ordem][order(ordem)]
        
        chosen_themes_ordered <- unique(df_themes_vars$theme)
        
        vars_to_delete <-list()
        for(chosen_theme_i in chosen_themes_ordered){
                vars_to_delete[[chosen_theme_i]] <- unique(df_themes_vars[theme == chosen_theme_i, variable])
        }
        
        num_themes <- length(vars_to_delete)
        
        for(i in 1:(num_themes-1)){
                for(j in (i+1):num_themes){
                        vars_to_delete[[i]] <- setdiff(vars_to_delete[[i]] , vars_to_delete[[j]])
                }
        }
        
        vars_to_delete
        
}



get_original_variables_by_theme <- function(year, themes = "all"){
        
        existing_themes <- harmonizeIBGE:::get_themes()
        
        if(length(themes) == 1){
                
                if(themes=="all"){
                        themes = get_themes()
                }
        }
        
        if(any(!(themes %in% existing_themes))){
                stop("This theme does not exist")
        }
        
        
        env <- environment()
        #listOriginalVars_location <- "E:/Google Drive/RCodes/PacotesR/harmonizeIBGE/inst/extdata/list_of_originalVars.csv"
        
        listOriginalVars_location <- system.file("extdata",
                                                 "list_of_originalVars.csv",
                                                 package = "harmonizeIBGE")
        
        listOriginalVars <- fread(listOriginalVars_location)
        listOriginalVars <- listOriginalVars[year == get("year", envir = env)]
        listOriginalVars[ , year := NULL]
        
        df_themes_vars <- data.table(themes_vars = names(listOriginalVars)) %>% 
                separate(col = themes_vars, sep = "_", into = c("theme", "varName")) %>%
                .[!is.na(varName)]
        
        df_themes_vars[ , originalVars := as.character(listOriginalVars)]
        df_themes_vars <- df_themes_vars[nchar(originalVars)>1]
        
        df_themes_vars <- df_themes_vars[theme %in% themes]
        
        
        ncols <- max(str_count(df_themes_vars$originalVars, ";")) + 1
        
        df_themes_vars <- df_themes_vars %>% 
                separate(col = originalVars, into = paste0("var_", 1:ncols), sep = ";") %>%
                gather(key = "originalVar", value = "variable", paste0("var_", 1:ncols)) %>%
                select(-originalVar) %>%
                filter(complete.cases(.)) %>%
                mutate(variable = tolower(variable)) %>%
                arrange(theme, varName)
        
        df_themes_vars
}
