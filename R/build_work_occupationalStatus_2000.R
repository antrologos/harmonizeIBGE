#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_work_occupationalStatus_2000 <- function(CensusData){ 
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame") 
        }
        
        check_vars <- check_var_existence(CensusData, c("v0439", "v0440", "v0441", "v0442", "v0443", "v0455", "age"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        CensusData[ v0439 == 1, occupationalStatus := 1]
        CensusData[ v0440 == 1, occupationalStatus := 1]
        CensusData[ v0441 == 1, occupationalStatus := 1]
        CensusData[ v0442 == 1, occupationalStatus := 1]
        CensusData[ v0443 == 1, occupationalStatus := 1]
        CensusData[ v0455 == 1 & (
                (is.na(v0439) | v0439 != 1) &
                        (is.na(v0440) | v0440 != 1) &
                        (is.na(v0441) | v0441 != 1) &
                        (is.na(v0442) | v0442 != 1) &
                        (is.na(v0443) | v0443 != 1)
        ),
        occupationalStatus := 0]
        CensusData[ is.na(v0439), occupationalStatus := NA]

        CensusData[age < 10, occupationalStatus    := NA]
        gc()
        
        CensusData
}
