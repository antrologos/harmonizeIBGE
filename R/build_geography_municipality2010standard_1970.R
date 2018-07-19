#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_geography_municipality2010standard_1970 <- function(CensusData,
                                                          state_var_name = "uf"){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c(state_var_name, "v001", "v002"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }


        municipality1970standard_just_created = F
        check_vars <- check_var_existence(CensusData, c("municipality1970standard"))
        if(length(check_vars) > 0){
                CensusData <- build_geography_municipality1970standard_1970(CensusData,
                                                                            state_var_name = state_var_name)
                municipality1970standard_just_created = T
                gc()
        }

        #data(crosswalk_munic_1970_to_2010)
        load("C:\\Users\\Rogerio\\Google Drive\\PacotesR\\harmonizeIBGE\\data\\crosswalk_munic_1970_to_2010.rda")

        crosswalk_munic_1970_to_2010 = data.table(crosswalk_munic_1970_to_2010 %>%
                                                          select(municipality1970standard,
                                                                 municipality2010standard))

        CensusData = merge(x = CensusData,
                           y = crosswalk_munic_1970_to_2010,
                           all.x = T,
                           all.y = F,
                           by = "municipality1970standard")
        gc();Sys.sleep(1);gc()


        if(municipality1970standard_just_created == T){
                CensusData[ , municipality1970standard := NULL]
        }

        gc()

        CensusData
}

