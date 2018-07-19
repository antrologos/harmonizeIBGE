#' Harmonize a set of economic activity variables all at once
#' @param data.frame
#' @value data.frame
#' @export


harmonize_economicActivity <- function(CensusData,
                                       year,
                                       type = "census",
                                       correctForHarmonizedAge = T,
                                       delete_originals = T){

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        gc(); Sys.sleep(1); gc()

        CensusData[, econ_active := as.numeric(NA)]
        CensusData[, occupied := as.numeric(NA)]

        if(type == "census" & year == 1960){
                CensusData[v220 %in% c(0, 1, 4:9), econ_active := 0]     # nao inclui 2 = ignorado e 3 = prejudicado
                CensusData[v223 %in% 2:4    ,      econ_active := 1]

                CensusData[v223 %in% 4,   occupied := 0]     # nao inclui 2 = ignorado e 3 = prejudicado
                CensusData[v223 %in% 2:3, occupied := 1]

                if(delete_originals == T){
                        CensusData[ , v220 := NULL]
                        CensusData[ , v223 := NULL]
                }

        }

        if(type == "census" & year == 1970){
                CensusData[ v043 %in% 0:6 , econ_active := 0]
                CensusData[ v047 %in% 1:5 , econ_active := 1]

                CensusData[ v047 %in% 4:5 , occupied := 0]
                CensusData[ v047 %in% 1:3 , occupied := 1]

                if(delete_originals == T){
                        CensusData[ , v043 := NULL]
                        CensusData[ , v047 := NULL]
                }
        }

        if(type == "census" & year == 1980){
                CensusData[ v529 %in% 3:9 , econ_active := 0]
                CensusData[ v541 %in% 1:4 , econ_active := 1]

                CensusData[ v541 %in% 4   , occupied := 0]
                CensusData[ v541 %in% 1:3 , occupied := 1]

                if(delete_originals == T){
                        CensusData[ , v529 := NULL]
                        CensusData[ , v541 := NULL]
                }

        }

        if(type == "census" & year == 1991){
                CensusData[ v0358 %in% 3:9, econ_active := 0]
                CensusData[ (v0345 %in% 1:2 | v0358 %in% 1:2), econ_active := 1]

                CensusData[ v0345 %in% 1:2,              occupied := 1]
                CensusData[ v0345 == 3 & v0358 %in% 1:2, occupied := 0]

                if(delete_originals == T){
                        CensusData[ , v0358 := NULL]
                        CensusData[ , v0345 := NULL]
                }

        }

        if(type == "census" & year == 2000){
                CensusData[ , econ_active := 0]
                CensusData[ v0439 == 1, econ_active := 1]
                CensusData[ v0440 == 1, econ_active := 1]
                CensusData[ v0441 == 1, econ_active := 1]
                CensusData[ v0442 == 1, econ_active := 1]
                CensusData[ v0443 == 1, econ_active := 1]
                CensusData[ v0455 == 1, econ_active := 1]
                CensusData[ is.na(v0439), econ_active := NA]

                CensusData[ v0439 == 1, occupied := 1]
                CensusData[ v0440 == 1, occupied := 1]
                CensusData[ v0441 == 1, occupied := 1]
                CensusData[ v0442 == 1, occupied := 1]
                CensusData[ v0443 == 1, occupied := 1]
                CensusData[ v0455 == 1 & (
                                      (is.na(v0439) | v0439 != 1) &
                                      (is.na(v0440) | v0440 != 1) &
                                      (is.na(v0441) | v0441 != 1) &
                                      (is.na(v0442) | v0442 != 1) &
                                      (is.na(v0443) | v0443 != 1)
                                      ),
                      occupied := 0]
                CensusData[ is.na(v0439), occupied := NA]

                if(delete_originals == T){
                        CensusData[ , v0439 := NULL]
                        CensusData[ , v0440 := NULL]
                        CensusData[ , v0441 := NULL]
                        CensusData[ , v0442 := NULL]
                        CensusData[ , v0443 := NULL]
                        CensusData[ , v0455 := NULL]
                }
        }

        if(type == "census" & year == 2010){
                CensusData[ , econ_active := 0]
                CensusData[ v0641 == 1, econ_active := 1]
                CensusData[ v0642 == 1, econ_active := 1]
                CensusData[ v0643 == 1, econ_active := 1]
                CensusData[ v0644 == 1, econ_active := 1]
                CensusData[ v0654 == 1 & v0655 == 1, econ_active := 1]
                CensusData[ is.na(v0641), econ_active := NA]

                CensusData[ v0641 == 1, occupied := 1]
                CensusData[ v0642 == 1, occupied := 1]
                CensusData[ v0643 == 1, occupied := 1]
                CensusData[ v0644 == 1, occupied := 1]
                CensusData[ v0654 == 1 & v0655 == 1 & (
                        (is.na(v0641) | v0641 != 1) &
                                (is.na(v0642) | v0642 != 1) &
                                (is.na(v0643) | v0643 != 1) &
                                (is.na(v0644) | v0644 != 1)
                ),
                occupied := 0]
                CensusData[ is.na(v0641), occupied := NA]

                if(delete_originals == T){
                        CensusData[ , v0641 := NULL]
                        CensusData[ , v0642 := NULL]
                        CensusData[ , v0643 := NULL]
                        CensusData[ , v0644 := NULL]
                        CensusData[ , v0654 := NULL]
                        CensusData[ , v0655 := NULL]
                }
        }
        gc(); Sys.sleep(1);gc()

        if(correctForHarmonizedAge == T){
                CensusData[age < 10, econ_active := NA]
                CensusData[age < 10, occupied    := NA]
        }


        return(CensusData)

}

