#' Implements a correction for the variable "class worker"
#' @param data.frame
#' @value data.frame
#' @export


correct_classWorker_econActivity <- function(CensusData,
                                        year,
                                        type = "census"){

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        gc(); Sys.sleep(1); gc()

        if(type == "census" & year %in% c(1960,1970)){
                CensusData[ is.na(occupied) | occupied == 0, class_worker := NA]
        }


        if(type == "census" & year %in% c(1980,1991,2000,2010)){

                for(i in 1:2){ # repete duas vezes a operacao porque as variaveis se modificam
                        CensusData[ is.na(occupied) | occupied == 0, class_worker := NA]
                        CensusData[ is.na(at_least_15hours_work) &  class_worker %in% 3:4 & econ_active == 1, econ_active := 0]
                        CensusData[ is.na(at_least_15hours_work) &  class_worker %in% 3:4 & occupied == 1, occupied    := NA]
                        gc()
                }

        }

        return(CensusData)

}

