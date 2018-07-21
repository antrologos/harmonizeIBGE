#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_identification_wgtperson_1960 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        warning("===================================================================================\nThis function assumes you are working with the 1.27% sample of the 1960 Census,\nas prepared and consisted by the Center for Metropolitan Studies (Centro de Estudos\nda Metropole - USP/Brazil).\n\nEvery individual will be given the same sample weight, equal to the inverse of the\nsample fraction: 1/0.0127\n\nDownload available at: http://200.144.244.241:3003/\nMore information at: http://web.fflch.usp.br/centrodametropole/\n===================================================================================")

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        #Weight
        CensusData[ , wgtperson := 1/0.0127]

        gc
        CensusData

}
