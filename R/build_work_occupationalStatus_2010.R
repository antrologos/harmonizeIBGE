#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_work_occupationalStatus_2010 <- function(CensusData){ 
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        
        check_vars <- check_var_existence(CensusData, c("v0641", "v0642", "v0643", "v0644", "v0654", "v0655"))
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
        
        CensusData[ v0641 == 1, occupationalStatus := 1]
        CensusData[ v0642 == 1, occupationalStatus := 1]
        CensusData[ v0643 == 1, occupationalStatus := 1]
        CensusData[ v0644 == 1, occupationalStatus := 1]
        CensusData[ v0654 == 1 & v0655 == 1 & (
                (is.na(v0641) | v0641 != 1) &
                        (is.na(v0642) | v0642 != 1) &
                        (is.na(v0643) | v0643 != 1) &
                        (is.na(v0644) | v0644 != 1)
        ),
        occupationalStatus := 0]
        CensusData[ is.na(v0641), occupationalStatus := NA]
        
        CensusData[age < 10, occupationalStatus    := NA]
        
        
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
                gc();Sys.sleep(.5);gc()
        }
        
        gc()
        CensusData
}

