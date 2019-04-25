#' @export

#CensusData = censo
#dropOriginalVariables = T

harmonize_themes <- function(CensusData, themes = "all", dropOriginalVariables = F, ajustWorkVariables = T){

        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        
        existing_themes <- harmonizeIBGE:::get_themes()
        
        if(length(themes) == 1){
                
                if(themes=="all"){
                        themes = harmonizeIBGE:::get_themes()
                }
        }
        
        if(any(!(themes %in% existing_themes))){
                stop("This theme does not exist")
        }
        
        functions <- lsf.str("package:harmonizeIBGE") %>%
                as.character()
        function_list   <- str_split(functions, "_")
        function_matrix <- matrix(NA, 
                                  nrow = length(function_list), 
                                  ncol = max(sapply(function_list, length)))
        
        for(i in 1:length(function_list)){
                ncols <- length(function_list[[i]])
                function_matrix[i, 1:ncols] <- function_list[[i]]
        }
        
        function_df <- function_matrix %>%
                data.frame() %>%
                setNames(c("funcType", "theme", "varName", "year")) %>%
                filter(funcType == "build")

        priorityList_location <- system.file("extdata",
                                        "list_functions_priority.csv",
                                        package = "harmonizeIBGE")
        
        #priorityList <- fread("c:/users/rogerio/Google Drive/PacotesR/harmonizeIBGE/inst/extdata/list_functions_priority.csv")
        priorityList <- fread(priorityList_location)
        priorityList <- priorityList[ ,list(funcType, theme, varName, ordem)] %>%
                setkey("funcType", "theme", "varName")
        priorityList[ , test := paste(theme, varName, sep = "_")]
        
        function_df <- function_df %>% as.data.table() %>% setkey("funcType", "theme", "varName")
        function_df[ , test := paste(theme, varName, sep = "_")]
        
        if(any(!(unique(priorityList$test) %in% unique(function_df$test)))){
                stop("There are nonexistent functions listed in the 'priority list'")
        }
        
        if(any(!(unique(function_df$test) %in% unique(priorityList$test)))){
                stop("There are functions not listed in the 'priority list'")
        }
        
        function_df <- function_df[priorityList, ordem := ordem][order(ordem)]
        
        function_df <- function_df %>% 
                filter(theme %in% themes)
        
        function_df <- function_df %>% 
                filter(is.na(year) |  year == metadata$year) %>%
                arrange(ordem) %>%
                select(-ordem, -test)
        
        function_matrix <- function_df %>% as.matrix()
        
        function_df$functions_to_execute <- as.character(NA)
        for(i in 1:nrow(function_matrix)){
                
                function_i <- function_matrix[i, ]
                function_i <- function_i[!is.na(function_i)]
                
                function_df$functions_to_execute[i] <- paste0(paste(function_i, collapse = "_"), "(CensusData)")
        }
        
        if(dropOriginalVariables == T){
                vars_to_drop <- harmonizeIBGE:::list_originalVariables_to_drop(year = metadata$year, themes = themes)
        }
        
        function_df <- function_df %>%
                mutate(ordem = 1:n()) %>%
                group_by(theme) %>%
                mutate(count_by_theme = as.numeric(theme))
        
        function_df$diff_count = ifelse(c(diff(function_df$count_by_theme), 1)==0,0,1)
        
        for(i in 1:nrow(function_df)){
                   
                function_to_execute_i <- function_df$functions_to_execute[i]
                print(paste("Executing", function_to_execute_i))
                
                CensusData <- eval(parse(text = function_to_execute_i))
                gc();Sys.sleep(.3);gc()
                
                if(ajustWorkVariables == T & "work" %in% themes & function_df$theme[i] == "work" & function_df$diff_count[i] == 1){
                        message("Ajusting work variables...")
                        CensusData <- ajust_work_variables(CensusData)
                        gc();Sys.sleep(.5);gc()
                }
                
                if(dropOriginalVariables == T & function_df$diff_count[i] == 1){
                        theme_i = as.character(function_df$theme[i])
                        
                        vars_to_drop_i <- vars_to_drop[[theme_i]]
                        vars_to_drop_i <- vars_to_drop_i[vars_to_drop_i %in% names(CensusData)]
                        
                        if(!is.null(vars_to_drop_i)){
                                CensusData <- CensusData %>%
                                        select(-vars_to_drop_i)
                                gc();Sys.sleep(.3);gc()        
                        }
                }
              
        }
        
        gc();Sys.sleep(.5);gc()
        CensusData
}
