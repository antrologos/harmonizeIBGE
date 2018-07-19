#' Identifies people who worked at least 15 hours per week
#' @param data.frame
#' @value data.frame
#' @export


dummy_at_least_15hours_work <- function(CensusData,
                                       year,
                                       type = "census"){


        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        if(type == "census" & year == 1980){
                CensusData[v535 %in% 2:5, at_least_15hours_work := 1]
        }

        if(type == "census" & year == 1991){
                CensusData[v0354 >= 15, at_least_15hours_work := 1]
        }

        if(type == "census" & year == 2000){
                CensusData[v0453 >= 15, at_least_15hours_work := 1]
        }

        if(type == "census" & year == 2010){
                CensusData[v0653 >= 15, at_least_15hours_work := 1]
        }

        return(CensusData)

}

