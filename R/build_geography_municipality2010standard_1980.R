#' Harmonize a set of education variables all at once
#' @param data.frame
#' @value data.frame
#' @export

build_geography_municipality2010standard_1980 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }
        
        municipality1980standard_just_created = F
        check_vars <- check_var_existence(CensusData, c("municipality1980standard"))
        if(length(check_vars) > 0){
                CensusData <- build_geography_municipality1980standard_1980(CensusData)
                municipality1980standard_just_created = T
                gc()
        }
        
        CensusData[ , municipality2010standard := municipality1980standard]
        
        
        # Fernando de Noronha
        CensusData[municipality2010standard == 200010, municipality2010standard := 260545]
        gc()
        
        # De Goias para Tocantins
        CensusData[municipality2010standard == 520040, municipality2010standard := 170040]
        CensusData[municipality2010standard == 520070, municipality2010standard := 170070]
        CensusData[municipality2010standard == 520100, municipality2010standard := 170100]
        CensusData[municipality2010standard == 520190, municipality2010standard := 170190]
        CensusData[municipality2010standard == 520200, municipality2010standard := 170200]
        CensusData[municipality2010standard == 520210, municipality2010standard := 170210]
        CensusData[municipality2010standard == 520220, municipality2010standard := 170220]
        CensusData[municipality2010standard == 520230, municipality2010standard := 170230]
        CensusData[municipality2010standard == 520240, municipality2010standard := 170240]
        CensusData[municipality2010standard == 520270, municipality2010standard := 170270]
        CensusData[municipality2010standard == 520290, municipality2010standard := 170290]
        gc()
        CensusData[municipality2010standard == 520300, municipality2010standard := 170300]
        CensusData[municipality2010standard == 520370, municipality2010standard := 170370]
        CensusData[municipality2010standard == 520550, municipality2010standard := 170550]
        CensusData[municipality2010standard == 520560, municipality2010standard := 170560]
        CensusData[municipality2010standard == 520600, municipality2010standard := 170600]
        CensusData[municipality2010standard == 520610, municipality2010standard := 170610]
        CensusData[municipality2010standard == 520700, municipality2010standard := 170700]
        CensusData[municipality2010standard == 520720, municipality2010standard := 170720]
        CensusData[municipality2010standard == 520730, municipality2010standard := 170730]
        CensusData[municipality2010standard == 520770, municipality2010standard := 170770]
        CensusData[municipality2010standard == 520820, municipality2010standard := 170820]
        gc()
        CensusData[municipality2010standard == 520900, municipality2010standard := 170900]
        CensusData[municipality2010standard == 520930, municipality2010standard := 170930]
        CensusData[municipality2010standard == 520950, municipality2010standard := 170950]
        CensusData[municipality2010standard == 521050, municipality2010standard := 171050]
        CensusData[municipality2010standard == 521070, municipality2010standard := 171070]
        CensusData[municipality2010standard == 521110, municipality2010standard := 171110]
        CensusData[municipality2010standard == 521240, municipality2010standard := 171240]
        CensusData[municipality2010standard == 521320, municipality2010standard := 171320]
        CensusData[municipality2010standard == 521330, municipality2010standard := 171330]
        CensusData[municipality2010standard == 521360, municipality2010standard := 171360]
        CensusData[municipality2010standard == 521420, municipality2010standard := 171420]
        CensusData[municipality2010standard == 521430, municipality2010standard := 171430]
        gc()
        CensusData[municipality2010standard == 521510, municipality2010standard := 171510]
        CensusData[municipality2010standard == 521610, municipality2010standard := 171610]
        CensusData[municipality2010standard == 521620, municipality2010standard := 171620]
        CensusData[municipality2010standard == 521650, municipality2010standard := 171650]
        CensusData[municipality2010standard == 521660, municipality2010standard := 171660]
        CensusData[municipality2010standard == 521670, municipality2010standard := 171665]
        CensusData[municipality2010standard == 521700, municipality2010standard := 171700]
        CensusData[municipality2010standard == 521750, municipality2010standard := 171750]
        CensusData[municipality2010standard == 521780, municipality2010standard := 171780]
        CensusData[municipality2010standard == 521790, municipality2010standard := 171790]
        CensusData[municipality2010standard == 521820, municipality2010standard := 171820]
        CensusData[municipality2010standard == 521840, municipality2010standard := 171840]
        CensusData[municipality2010standard == 522030, municipality2010standard := 172030]
        gc()
        CensusData[municipality2010standard == 522080, municipality2010standard := 172080]
        CensusData[municipality2010standard == 522090, municipality2010standard := 172090]
        CensusData[municipality2010standard == 522110, municipality2010standard := 172110]
        CensusData[municipality2010standard == 522120, municipality2010standard := 172120]
        CensusData[municipality2010standard == 522210, municipality2010standard := 172210]

        
        n_digit <- nchar(min(CensusData[ , municipality2010standard]))
        if(n_digit == 7){
                CensusData[ , municipality2010standard := trunc(municipality2010standard/10)]
        }
        
        if(municipality1980standard_just_created == T){
                CensusData[ , municipality1980standard := NULL]
        }        
        
        gc()
        CensusData
}


