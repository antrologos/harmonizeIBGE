#' Harmonize a set of income variables all at once
#' @param data.frame
#' @value data.frame
#' @export

#scripts_folder = "c:/Dropbox/Rogerio/Artigos/1 - Em faccao/000 - Padroniza??o de Ocupa??es e Setores/Rogerio/Harmonization Scripts - INCOMPLETE"


harmonize_income <- function(CensusData,
                             year,
                             scripts_folder = "E:/Dropbox-Ro/Dropbox/Rogerio/Artigos/1 - Em faccao/000 - Padroniza??o de Ocupa??es e Setores/Rogerio/Harmonization Scripts - INCOMPLETE",
                             harmonize_demographics_first = F,
                             harmonize_identification_first = F){

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


        if(harmonize_demographics_first == T){
                source(paste0(scripts_folder,"/_FUNCTION_harmonize_demographics.R"))
                CensusData <- CensusData %>%
                        harmonize_demographics(year = year) %>%
                        as.data.table()
                gc(); Sys.sleep(1); gc()
        }



        if(year == 1960){
                CensusData[, total_income := as.numeric(NA)]
                gc(); Sys.sleep(1); gc()
        }

        if(year == 1970){
                CensusData[,             total_income := as.numeric(NA)]
                CensusData[v041 == 9999, total_income := 0]
                CensusData[v041 < 9998,  total_income := v041/0.416904849]

                gc(); Sys.sleep(1); gc()
        }

        if(year == 1980){
                varlist = c("v607", "v608", "v609", "v610", "v611", "v612", "v613")

                for(var in varlist){
                        print(var)
                        CensusData[[var]][is.na(CensusData[[var]])] <- 0
                        CensusData[[var]][CensusData[[var]] == 9999999] <- NA
                }
                gc(); Sys.sleep(1); gc()

                income_matrix <-
                        CensusData[, (varlist), with=F] %>%
                        as.matrix()
                gc(); Sys.sleep(1); gc()

                missing_values <- apply(income_matrix, 1, function(x) all(is.na(x)))
                income <-  rowSums(income_matrix)

                rm(income_matrix)
                gc(); Sys.sleep(1); gc()

                income[missing_values] <- NA
                gc(); Sys.sleep(1); gc()

                CensusData[ , `:=`("total_income", income/9.091145084)]
                gc(); Sys.sleep(1); gc()
        }

        if(year == 1991){
                CensusData[v3561 < 99999999, total_income := v3561/106.3629622]
                gc(); Sys.sleep(1); gc()
        }


        if(year == 2000){
                CensusData[ , total_income := v4614/0.512271399]
                gc(); Sys.sleep(1); gc()
        }


        if(year == 2010){
                CensusData[ , total_income := v6527]
                gc(); Sys.sleep(1); gc()
        }
        gc(); Sys.sleep(1); gc()


        missing_income = CensusData[ , is.na(total_income)]

        CensusData[age < 10 & missing_income == F, total_income := 0]
        gc()

        CensusData[,                 total_income_tmp := total_income]
        CensusData[missing_income,   total_income_tmp := 0]
        CensusData[nonrelative == 1, total_income_tmp := 0]
        CensusData[dweller == 0,     total_income_tmp := 0]
        gc(); Sys.sleep(1); gc()

        CensusData[ , household_income  := sum(total_income_tmp, na.rm = T), by=hh_id]
        CensusData[ , income_per_capita := household_income/num_dwellers_relatives, by=hh_id]

        CensusData[dweller_relative == 0, household_income  := NA]
        CensusData[dweller_relative == 0, income_per_capita := NA]

        if(year == 1960){
                CensusData[, household_income  := NA]
                CensusData[, income_per_capita := NA]
        }

        CensusData[, total_income_tmp := NULL]

        gc(); Sys.sleep(1); gc()

        CensusData

}




