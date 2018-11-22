#' @export

harmonize_themes <- function(CensusData, themes = "all"){

        functions <- lsf.str("package:harmonizeIBGE") %>%
                as.character()
        function_list <- str_split(functions, "_")
        function_matrix <- matrix(NA, 
                                  nrow = length(function_list), 
                                  ncol = max(sapply(function_list, length)))
        
        for(i in 1:length(function_list)){
                ncols <- length(function_list[[i]])
                function_matrix[i, 1:ncols] <- function_list[[i]]
        }
        
        function_df <- function_matrix %>%
                data.frame() %>%
                setNames(c("funcType", "theme", "varName", "year", "other")) %>%
                filter(funcType == "build")
        
        if(themes != "all"){
                function_df <- function_df %>% 
                        filter(theme %in% themes)
        }
        
        metadata <- harmonizeIBGE:::get_metadata(CensusData)
        
        function_df <- function_df %>% 
                filter(is.na(year) |  year == metadata$year)
        
        
        function_matrix <- function_df %>% as.matrix()
        
        functions_to_execute <- NULL
        for(i in 1:nrow(function_matrix)){
                
                function_i <- function_matrix[i, ]
                function_i <- function_i[!is.na(function_i)]
                
                functions_to_execute <- c(functions_to_execute, 
                                          paste0(paste(function_i, collapse = "_"), "(CensusData)"))
        }
        
        for(function_to_execute_i in functions_to_execute){
                print(paste("Executing", function_to_execute_i))
                
                CensusData <- eval(parse(text = functions_to_execute))
        }
             
        CensusData
}
