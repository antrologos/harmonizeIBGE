check_var_existence <- function(data, var_names){

        test <- sapply(var_names, function(x) is.null(data[[x]]))

        problematic <- NULL
        problematic <- names(test)[which(test)]

        problematic
}


