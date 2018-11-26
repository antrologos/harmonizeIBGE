#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_work_occupationalStatus_1960 <- function(CensusData){ 
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        
        check_vars <- check_var_existence(CensusData, c("v223"))
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
        
        CensusData[, occupationalStatus := as.numeric(NA)]
        
        CensusData[v223 %in% 4,   occupationalStatus := 0]     # nao inclui 2 = ignorado e 3 = prejudicado
        CensusData[v223 %in% 2:3, occupationalStatus := 1]
        gc()
        
        CensusData[age < 10, occupationalStatus    := NA]
        
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
                gc();Sys.sleep(.5);gc()
        }
        
        CensusData
}
