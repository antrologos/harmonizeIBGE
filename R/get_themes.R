#' @export

get_themes <- function(CensusData, themes = "all"){
        
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
                data.table()
        
        names(function_df)[1:4] <- c("funcType", "theme", "varName", "year")
        
        function_df %>%
                filter(funcType == "build") %>%
                .$theme %>%
                unique()
}
