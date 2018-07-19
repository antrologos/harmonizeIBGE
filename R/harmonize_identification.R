#' Harmonize a set of identification variables all at once
#' @param data.frame
#' @value data.frame
#' @export

harmonize_identification <- function(CensusData,
                                     year,
                                     delete_originals = T){


        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        if(!is.numeric(year)){
                stop("'year' must be a numeric value")
        }

        gc(); Sys.sleep(1); gc()


        call <- paste0("build_identification_year_",year,"(CensusData)")
        CensusData <- eval(parse(text = call))
        gc(); Sys.sleep(1); gc()

        call <- paste0("build_identification_wgt_",year,"(CensusData)")
        CensusData <- eval(parse(text = call))
        gc(); Sys.sleep(1); gc()

        call <- paste0("build_identification_idperson_",year,"(CensusData)")
        CensusData <- eval(parse(text = call))
        gc(); Sys.sleep(1); gc()

        call <- paste0("build_identification_idhh_",year,"(CensusData)")
        CensusData <- eval(parse(text = call))
        gc(); Sys.sleep(1); gc()


        if(delete_originals == T){

                if(year == 1960){
                        var_to_exclude <- c("cem_wgt","cem_idindividuo","cem_iddomicilio")
                }

                if(year == 1970){
                        var_to_exclude <- c("v054")
                }

                if(year == 1980){
                        var_to_exclude <- c("v604", "v601")
                }

                if(year == 1991){
                        var_to_exclude <- c("v0102", "v7301")
                }

                if(year == 2000){
                        var_to_exclude <- c("p001", "v0300")
                }

                if(year == 2010){
                        var_to_exclude <- c("v0010", "v0300")
                }

                CensusData[, (var_to_exclude) := NULL]

                gc();Sys.sleep(1);gc()
        }

        CensusData
}

