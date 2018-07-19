#' Harmonize a set of demographic variables all at once
#' @param data.frame
#' @value data.frame
#' @export


harmonize_demographics <- function(CensusData,
                                   year,
                                   harmonize_identification_first = F,
                                   scripts_folder = "E:/Dropbox-Ro/Dropbox/Rogerio/Artigos/1 - Em faccao/000 - Padroniza??o de Ocupa??es e Setores/Rogerio/Harmonization Scripts - INCOMPLETE",
                                   delete_originals = T){

        library(data.table)
        library(tidyverse)

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        gc(); Sys.sleep(1); gc()


        if(harmonize_identification_first == T){
                source(paste0(scripts_folder,"/_FUNCTION_harmonize_identification.R"))
                CensusData <- CensusData %>%
                        harmonize_identification(year = year) %>%
                        as.data.table()
                gc(); Sys.sleep(1); gc()
        }


        if(year == 1960){

                #non-relatives
                CensusData[ , nonrelative := as.numeric( v203 %in% c(4,5) )]


                #gender
                CensusData[v202 %in% c(1,3,5), male:=1]
                CensusData[v202 %in% c(2,4,6), male:=0]
                # 0 - Mulheres
                # 1 - Homens

                #race
                CensusData[ v206 %in% c(4, 6),    race := 1]
                CensusData[ v206 %in% c(7, 8),    race := 2]
                CensusData[ v206 %in% c(5),       race := 3]
                # 1 - Brancos e amarelos
                # 2 - Pardos e indigenas
                # 3 - Pretos



                # residence_status (condicao de presenca)
                CensusData[v202 %in% c(1,2), residence_status := 1]
                CensusData[v202 %in% c(3,4), residence_status := 2]
                CensusData[v202 %in% c(5,6), residence_status := 3]

                # 1 - Morador presente
                # 2 - Morador ausente
                # 3 - Nao-morador presente

                if(delete_originals == T){
                        CensusData[ , v202 := NULL]
                        CensusData[ , v203 := NULL]
                        CensusData[ , v204 := NULL]
                        CensusData[ , v206 := NULL]
                        gc()
                }


        }

        if(year == 1970){

                CensusData[ , nonrelative := as.numeric( v025==0|v025>=7) ]
                CensusData[, male := as.numeric(v023 == 0)]
                CensusData[, race := as.numeric(NA)]
                gc()


                # residence_status (condicao de presenca)
                CensusData[v024 == 0, residence_status := 1]
                CensusData[v024 == 1, residence_status := 2]
                CensusData[v024 == 2, residence_status := 3]

                # 1 - Morador presente
                # 2 - Morador ausente
                # 3 - Nao-morador presente
                gc()

                if(delete_originals == T){
                        CensusData[ , v023 := NULL]
                        CensusData[ , v024 := NULL]
                        CensusData[ , v025 := NULL]
                        CensusData[ , v026 := NULL]
                        CensusData[ , v027 := NULL]
                        gc()
                }
        }


        if(year == 1980){
                CensusData[ , nonrelative := as.numeric( v503>=7) ]
                CensusData[, male := as.numeric(v501 == 1)]
                gc()

                CensusData[v509 %in% c(2, 6), race := 1]
                CensusData[v509 %in% c(8),    race := 2]
                CensusData[v509 %in% c(4),    race := 3]
                gc()



                # residence_status (condicao de presenca)
                CensusData[, residence_status := 1]
                gc()

                if(delete_originals == T){
                        CensusData[ , v501 := NULL]
                        CensusData[ , v503 := NULL]
                        CensusData[ , v509 := NULL]
                        CensusData[ , v606 := NULL]
                        gc()
                }

        }




        if(year == 1991){
                CensusData[ , nonrelative := as.numeric( v0302>=14 ) ]
                CensusData[, male := as.numeric(v0301 == 1)]
                gc()

                CensusData[v0309 %in% c(1, 3), race := 1]
                CensusData[v0309 %in% c(4, 5), race := 2]
                CensusData[v0309 %in% c(2),    race := 3]
                gc()


                # residence_status (condicao de presenca)
                CensusData[, residence_status := 1]
                gc()

                if(delete_originals == T){
                        CensusData[ , v0301 := NULL]
                        CensusData[ , v0302 := NULL]
                        CensusData[ , v0309 := NULL]
                        CensusData[ , v3072 := NULL]
                        gc()
                }

        }


        if(year == 2000){
                CensusData[ , nonrelative := as.numeric( v0402>=9 ) ]
                CensusData[, male := as.numeric(v0401 == 1)]
                gc()

                CensusData[v0408 %in% c(1, 3), race := 1]
                CensusData[v0408 %in% c(4, 5), race := 2]
                CensusData[v0408 %in% c(2),    race := 3]
                gc()



                # residence_status (condicao de presenca)
                CensusData[, residence_status := 1]
                gc()

                if(delete_originals == T){
                        CensusData[ , v0401 := NULL]
                        CensusData[ , v0402 := NULL]
                        CensusData[ , v0408 := NULL]
                        CensusData[ , v4752 := NULL]
                        gc()
                }
        }



        if(year == 2010){
                CensusData[ , nonrelative := as.numeric(v0502>=17) ]
                gc()

                CensusData[, male := as.numeric(v0601 == 1)]
                gc()

                CensusData[v0606 %in% c(1, 3), race := 1]
                CensusData[v0606 %in% c(4, 5), race := 2]
                CensusData[v0606 %in% c(2),    race := 3]
                gc()



                # residence_status (condicao de presenca)
                CensusData[, residence_status := 1]
                gc()

                if(delete_originals == T){
                        CensusData[ , v0502 := NULL]
                        CensusData[ , v0601 := NULL]
                        CensusData[ , v0606 := NULL]
                        CensusData[ , v6036 := NULL]
                        gc()
                }
        }

        gc()
        CensusData[ , dweller := 0]
        CensusData[residence_status %in% c(1,2), dweller := 1]
        gc()


        CensusData[ , dweller_relative := 0]
        CensusData[nonrelative == 0 & dweller == 1, dweller_relative := 1]
        gc()


        CensusData[ , num_dwellers           := sum(dweller), by = hh_id]
        CensusData[ , num_dwellers_relatives := sum(dweller_relative), by = hh_id]
        gc()

        gc(); Sys.sleep(1); gc()
        CensusData
}
