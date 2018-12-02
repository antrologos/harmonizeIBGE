#' Harmonize a set of income variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_income_totalIncome1960brackets <- function(CensusData){
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        
        year = metadata$year
        
        age_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("age"))
        if(length(check_vars) > 0){
                CensusData <- eval(parse(text = paste0("build_demographics_age_", metadata$year, "(CensusData)")))
                age_just_created = T
                gc();Sys.sleep(.5);gc()
        }
        
        totalIncome2010Values_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("totalIncome2010Values"))
        if(length(check_vars) > 0){
                CensusData <- build_income_totalIncome2010Values(CensusData)
                totalIncome2010Values_just_created = T
                gc();Sys.sleep(.5);gc()
        }
        
        # lower and upper bound of each bracket
        limits = matrix(c(0,     0,
                           1,     2100,
                           2101,  3300,
                           3301,  4500,
                           4501,  6000,
                           6001,  10000,
                           10001, 20000,
                           20001, 50000,
                           50001, NA), byrow= T, ncol =2)
        
        # Correcting for inflation
        limits = limits/10.8613840297639  # values of August/2010
        
        
        if(year == 1960){
                #Reordenando as categorias da renda de 1960
                CensusData[ , totalIncome1960brackets := v219]
                CensusData[totalIncome1960brackets == 4, totalIncome1960brackets := NA]
                CensusData[totalIncome1960brackets == 3, totalIncome1960brackets := 4]
                CensusData[totalIncome1960brackets %in% c(0,1,2), totalIncome1960brackets := totalIncome1960brackets + 10]
                CensusData[, totalIncome1960brackets := totalIncome1960brackets - 3]
               
        }else{
                # Categorizando a renda de 1970 a 2010
                bracket_limits    <- rowMeans(cbind(limits[-1,1],limits[-nrow(limits),2]))
                bracket_limits[1] <- 0
                
                CensusData[totalIncome2010Values == bracket_limits[1], totalIncome1960brackets := 1]
                CensusData[totalIncome2010Values > bracket_limits[1] & totalIncome2010Values <= bracket_limits[2] , totalIncome1960brackets := 2]
                CensusData[totalIncome2010Values > bracket_limits[2] & totalIncome2010Values <= bracket_limits[3] , totalIncome1960brackets := 3]
                gc();Sys.sleep(.1);gc()
                
                CensusData[totalIncome2010Values > bracket_limits[3] & totalIncome2010Values <= bracket_limits[4] , totalIncome1960brackets := 4]
                CensusData[totalIncome2010Values > bracket_limits[4] & totalIncome2010Values <= bracket_limits[5] , totalIncome1960brackets := 5]
                CensusData[totalIncome2010Values > bracket_limits[5] & totalIncome2010Values <= bracket_limits[6] , totalIncome1960brackets := 6]
                gc();Sys.sleep(.1);gc()
                
                CensusData[totalIncome2010Values > bracket_limits[6] & totalIncome2010Values <= bracket_limits[7] , totalIncome1960brackets := 7]
                CensusData[totalIncome2010Values > bracket_limits[7] & totalIncome2010Values <= bracket_limits[8] , totalIncome1960brackets := 8]
                CensusData[totalIncome2010Values > bracket_limits[8], totalIncome1960brackets := 9]
                gc();Sys.sleep(.1);gc()
                
        }
        
        gc(); Sys.sleep(1); gc()
        
        CensusData[age  < 10 , totalIncome1960brackets := NA]
        gc(); Sys.sleep(.5); gc()
        
        
        if(age_just_created == T){
                CensusData[ , age := NULL] 
                gc(); Sys.sleep(.5); gc()
        }
        
        if(totalIncome2010Values_just_created == T){
                CensusData[ , totalIncome2010Values := NULL] 
                gc(); Sys.sleep(.5); gc()
        }
        
        CensusData <- harmonizeIBGE:::set_metadata(CensusData, metadata = metadata)
        
        gc(); Sys.sleep(.5); gc()
        
        CensusData
        
}





