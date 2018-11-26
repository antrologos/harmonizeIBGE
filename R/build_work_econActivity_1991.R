#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_work_econActivity_1991 <- function(CensusData){ 
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        
        check_vars <- check_var_existence(CensusData, c("v0345", "v0358"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        # Building age
        age_just_created <- FALSE
        check_vars <- check_var_existence(CensusData, c("age"))
        if(length(check_vars) > 0) {
                CensusData <- eval(parse(text=paste0("build_demographics_age_", metadata$year, "(CensusData)")))
                age_just_created <- TRUE
                gc();Sys.sleep(.5);gc()
        }
        
        CensusData[, econActivity := as.numeric(NA)]
        
        CensusData[ v0358 %in% 3:9, econActivity := 0]
        CensusData[ (v0345 %in% 1:2 | v0358 %in% 1:2), econActivity := 1]
        
        CensusData[age < 10, econActivity := NA]
        gc()
        
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
                gc();Sys.sleep(.5);gc()
        }
        
        CensusData
}
