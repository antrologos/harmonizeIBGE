#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_work_occupationalStatus_2010 <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        check_vars <- check_var_existence(CensusData, c("v0641", "v0642", "v0643", "v0644", "v0654", "v0655", "age"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
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
        gc()
        
        CensusData
}

