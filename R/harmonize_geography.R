#' Harmonize a set of geographic variables all at once
#' @param data.frame
#' @value data.frame
#' @export

#CensusData = c_1960
#year = 1960
#delete_originals = T

harmonize_geography <- function(CensusData,
                                year,
                                delete_originals = T){

        library(tidyverse)
        library(data.table)

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        gc(); Sys.sleep(1); gc()

        data("crosswalk_states_tmp")
        crosswalk =  crosswalk_states_tmp %>%
                filter(year==year) %>%
                as.data.table()

        if(year %in% c(1960, 1970) ){
                crosswalk_state <- crosswalk %>%
                        filter(variable == "state") %>%
                        select(year, original_code, semi_harmonized_code) %>%
                        setnames(old = "semi_harmonized_code", new = "state_curr")
        }

        crosswalk_state_of_birth <- crosswalk %>%
                filter(variable == "state_of_birth") %>%
                select(year, original_code, semi_harmonized_code) %>%
                setnames(old = "semi_harmonized_code", new = "state_of_birth")

        y = year
        if(year == 1960){

                crosswalk_state <- crosswalk_state %>%
                        filter(year == y) %>%
                        select(-year) %>%
                        setnames(old = "original_code", new = "uf_pess")

                crosswalk_state_of_birth <- crosswalk_state_of_birth %>%
                        filter(year == y) %>%
                        select(-year) %>%
                        setnames(old = "original_code", new = "v207")

                CensusData <- left_join(x = CensusData,
                                        y = crosswalk_state,
                                        by = "uf_pess")

                CensusData <- left_join(x = CensusData,
                                        y = crosswalk_state_of_birth,
                                        by = "v207")

                CensusData = data.table(CensusData)

                CensusData[state_curr == 20, state_harm := 26]
                CensusData[state_curr == 34, state_harm := 33]
                CensusData[state_curr == 17, state_harm := 52]
                CensusData[state_curr == 50, state_harm := 51]

                CensusData[ , born_same_state := as.numeric(state_of_birth == state_harm)]

                CensusData[ , born_same_municipality := as.numeric(v209==2)]

                CensusData[ , municipality2010 := NA]


                if(delete_originals == T){
                        CensusData[ , uf_pess := NULL]
                        CensusData[ , uf_dom := NULL]
                        CensusData[ , v207 := NULL]
                        CensusData[ , v209 := NULL]
                        gc()
                }


        }

        if(year == 1970){

                crosswalk_state <- crosswalk_state %>%
                        filter(year == y) %>%
                        select(-year) %>%
                        setnames(old = "original_code", new = "cem005") %>%
                        as.data.table() %>%
                        setkey(cem005)

                crosswalk_state_of_birth <- crosswalk_state_of_birth %>%
                        filter(year == y) %>%
                        select(-year) %>%
                        setnames(old = "original_code", new = "v030") %>%
                        as.data.table() %>%
                        setkey(v030)

                for(i in 1:nrow(crosswalk_state)){
                        old_state = crosswalk_state$cem005[i]
                        new_state = crosswalk_state$state_curr[i]
                        CensusData[cem005 == old_state, state_curr := new_state]
                        gc()
                }
                gc();Sys.sleep(1);gc()


                for(i in 1:nrow(crosswalk_state_of_birth)){
                        old_state = crosswalk_state_of_birth$v030[i]
                        new_state = crosswalk_state_of_birth$state_of_birth[i]
                        CensusData[v030 == old_state, state_of_birth := new_state]
                        gc()
                }
                gc();Sys.sleep(1);gc()


                CensusData[state_of_birth %in% 30:98, state_of_birth := 99]
                gc()

                values_state_of_birth <- crosswalk_state_of_birth$state_of_birth %>%
                        unique() %>%
                        as.numeric()

                CensusData[is.na(state_of_birth) | !(state_of_birth %in% values_state_of_birth),
                           state_of_birth := 999]
                gc()

                CensusData[state_curr == 20, state_harm := 26]
                CensusData[state_curr == 34, state_harm := 33]
                CensusData[state_curr == 17, state_harm := 52]
                CensusData[state_curr == 50, state_harm := 51]

                gc()

                CensusData[ , born_same_state := as.numeric(state_of_birth == state_harm)]
                CensusData[ , born_same_municipality := as.numeric(is.na(v031))]
                gc()

                if(delete_originals == T){
                        CensusData[ , cem005 := NULL]
                        CensusData[ , v030 := NULL]
                        CensusData[ , v031 := NULL]
                        gc()
                }
        }


        if(year == 1980){
                crosswalk_state_of_birth <- crosswalk_state_of_birth %>%
                        filter(year == y) %>%
                        select(-year) %>%
                        setnames(old = "original_code", new = "v512") %>%
                        as.data.table() %>%
                        setkey(v512)


                for(i in 1:nrow(crosswalk_state_of_birth)){
                        old_state = crosswalk_state_of_birth$v512[i]
                        new_state = crosswalk_state_of_birth$state_of_birth[i]
                        CensusData[v512 == old_state, state_of_birth := new_state]
                        gc()
                }
                gc();Sys.sleep(1);gc()


                CensusData[state_of_birth %in% 30:98, state_of_birth := 99]
                gc()

                values_state_of_birth <- crosswalk_state_of_birth$state_of_birth %>%
                        unique() %>%
                        as.numeric()

                CensusData[is.na(state_of_birth) | !(state_of_birth %in% values_state_of_birth),
                           state_of_birth := 999]
                gc()

                CensusData[ ,  state_curr := v2]

                CensusData[state_curr == 20, state_harm := 26]
                CensusData[state_curr == 34, state_harm := 33]
                CensusData[state_curr == 17, state_harm := 52]
                CensusData[state_curr == 50, state_harm := 51]

                gc()

                CensusData[ , born_same_state := as.numeric(state_of_birth == state_harm)]
                CensusData[ , born_same_municipality := ifelse( v513 == 1, 1, 0)]
                CensusData[born_same_municipality == 0, born_same_state := 0]
                gc()


                if(delete_originals == T){
                        CensusData[ , v512 := NULL]
                        CensusData[ , v2 := NULL]
                        CensusData[ , v5 := NULL]
                        CensusData[ , v513 := NULL]
                        gc()
                }


        }


        if(year == 1991){


                crosswalk_state_of_birth <- crosswalk_state_of_birth %>%
                        filter(year == y) %>%
                        select(-year) %>%
                        setnames(old = "original_code", new = "v0316") %>%
                        as.data.table() %>%
                        setkey(v0316)

                for(i in 1:nrow(crosswalk_state_of_birth)){
                        old_state = crosswalk_state_of_birth$v0316[i]
                        new_state = crosswalk_state_of_birth$state_of_birth[i]
                        CensusData[v0316 == old_state, state_of_birth := new_state]
                        gc()
                }
                gc();Sys.sleep(1);gc()


                CensusData[state_of_birth %in% 30:98, state_of_birth := 99]

                values_state_of_birth <- crosswalk_state_of_birth$state_of_birth %>%
                        unique() %>%
                        as.numeric()

                CensusData[is.na(state_of_birth) | !(state_of_birth %in% values_state_of_birth),
                           state_of_birth := 999]

                CensusData[ ,  state_curr := v1101]

                CensusData[state_curr == 20, state_harm := 26]
                CensusData[state_curr == 34, state_harm := 33]
                CensusData[state_curr == 17, state_harm := 52]
                CensusData[state_curr == 50, state_harm := 51]


                CensusData[ , born_same_state := as.numeric(state_of_birth == state_harm)]
                CensusData[ , born_same_municipality := ifelse( v0314 %in% c(1,2), 1, 0)]
                CensusData[born_same_municipality == 1, born_same_state := 1]
                CensusData[born_same_municipality == 1, state_of_birth := state_harm]


                if(delete_originals == T){
                        CensusData[ , v1102 := NULL]
                        CensusData[ , v1101 := NULL]
                        CensusData[ , v0314 := NULL]
                        CensusData[ , v0316 := NULL]
                        gc()
                }
        }


        if(year == 2000){
                crosswalk_state_of_birth <- crosswalk_state_of_birth %>%
                        filter(year == y) %>%
                        select(-year) %>%
                        setnames(old = "original_code", new = "v4210") %>%
                        as.data.table() %>%
                        setkey(v4210)


                for(i in 1:nrow(crosswalk_state_of_birth)){
                        old_state = crosswalk_state_of_birth$v4210[i]
                        new_state = crosswalk_state_of_birth$state_of_birth[i]
                        CensusData[v4210 == old_state, state_of_birth := new_state]
                        gc()
                }
                gc();Sys.sleep(1);gc()


                CensusData[state_of_birth %in% 30:98, state_of_birth := 99]

                values_state_of_birth <- crosswalk_state_of_birth$state_of_birth %>%
                        unique() %>%
                        as.numeric()

                CensusData[is.na(state_of_birth) | !(state_of_birth %in% values_state_of_birth),
                           state_of_birth := 999]

                CensusData[ ,  state_curr := v0102]

                CensusData[state_curr == 20, state_harm := 26]
                CensusData[state_curr == 34, state_harm := 33]
                CensusData[state_curr == 17, state_harm := 52]
                CensusData[state_curr == 50, state_harm := 51]


                CensusData[ , born_same_state := as.numeric( v0415==1 | v0417==1 | v0418==1)]
                CensusData[ , born_same_municipality := as.numeric( v0415==1 | v0417==1)]
                CensusData[ born_same_state == 1, state_of_birth := state_harm]
                CensusData[ state_of_birth == state_harm, born_same_state := 1]

                
                if(delete_originals == T){
                        CensusData[ , v0102 := NULL]
                        CensusData[ , v0103 := NULL]
                        CensusData[ , v4210 := NULL]
                        CensusData[ , v0415 := NULL]
                        CensusData[ , v0417 := NULL]
                        CensusData[ , v0418 := NULL]
                        gc()
                }
        }


        if(year == 2010){
                crosswalk_state_of_birth <- crosswalk_state_of_birth %>%
                        filter(year == y) %>%
                        select(-year) %>%
                        setnames(old = "original_code", new = "v6222") %>%
                        as.data.table() %>%
                        setkey(v6222)

                for(i in 1:nrow(crosswalk_state_of_birth)){
                        old_state = crosswalk_state_of_birth$v6222[i]
                        new_state = crosswalk_state_of_birth$state_of_birth[i]
                        CensusData[v6222 == old_state, state_of_birth := new_state]
                        gc()
                }
                gc();Sys.sleep(1);gc()

                CensusData[ ,  state_curr := v0001]

                CensusData[ , state_harm := state_curr]
                CensusData[state_curr == 20, state_harm := 26]
                CensusData[state_curr == 34, state_harm := 33]
                CensusData[state_curr == 17, state_harm := 52]
                CensusData[state_curr == 50, state_harm := 51]

                CensusData[v0622==2, state_of_birth := 99]

                CensusData[ , born_same_municipality := ifelse(v0618 %in% c(1,2), 1, 0)  ]
                CensusData[ , born_same_state := as.numeric(born_same_municipality==1 | ifelse(v0619 %in% c(1,2) , T, F))]
                CensusData[born_same_state == 1, state_of_birth := state_harm ]
                CensusData[ state_of_birth == state_harm, born_same_state := 1]

                if(delete_originals == T){
                        CensusData[ , v0001 := NULL]
                        CensusData[ , v0002 := NULL]
                        CensusData[ , v0622 := NULL]
                        CensusData[ , v6222 := NULL]
                        CensusData[ , v0618 := NULL]
                        CensusData[ , v0619 := NULL]
                        gc()
                }
        }


        # Convertendo codigo dos municipios para 6 digitos
        if( year != 1960 & CensusData[ , max(municipality2010, na.rm = T)] > 999999) {
                CensusData[ , municipality2010 := trunc(municipality2010/10)]
                CensusData[ , state_2010 := trunc(municipality2010/10000)]
        }

        if(year == 1960){
                CensusData[ , state_2010 := NA]
        }

        CensusData[ , region     := trunc(state_harm/10)]


        CensusData
}


