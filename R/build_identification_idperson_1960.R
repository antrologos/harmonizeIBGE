#' Builds a synthetic variable for age - 1960
#' @param data.frame
#' @value data.frame
#' @export

build_identification_idperson_1960 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        check_vars <- check_var_existence(CensusData, c("cem_idindividuo"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        warning("===================================================================================\nThis function assumes you are working with the 1.27% sample of the 1960 Census,\nas prepared and consisted by the Center for Metropolitan Studies (Centro de Estudos\nda Metropole - USP/Brazil).\n\nEvery individual will be given the same sample weight, equal to the inverse of the\nsample fraction: 1/0.0127\n\nDownload available at: http://200.144.244.241:3003/\nMore information at: http://web.fflch.usp.br/centrodametropole/\n===================================================================================")

        # Person ID
        CensusData[ , idperson := 1960*10^8 + cem_idindividuo]

        CensusData
}

