#' Creates a synthetic variable for position in occupation
#' @param data.frame
#' @value data.frame
#' @export


build_work_classWorker <- function(CensusData){

        CensusData <- check_prepared_to_harmonize(CensusData)
        
        metadata    <- get_metadata(CensusData)
        year        <- metadata$year

        if(year == 1960){
                
                var_setor   = CensusData$v223b
                var_posOcup = CensusData$v224
                
                setor_primario <- rep(0, length(var_posOcup))
                setor_primario[var_setor %in% c(111:219,291,292)] <- 1
                setor_primario[is.na(var_setor)] <- NA
                
                posOcup <- rep(NA, length(var_posOcup))
                
                posOcup[var_posOcup==1] <- NA
                posOcup[var_posOcup %in% 5:6] <- 1
                posOcup[var_posOcup==9] <- 2
                posOcup[var_posOcup==7 &  setor_primario == 0] <- 3
                posOcup[var_posOcup==7 &  setor_primario == 1] <- 4
                posOcup[var_posOcup==8] <- 4
                posOcup[var_posOcup==0 & setor_primario == 0] <- 5
                posOcup[var_posOcup==0 & setor_primario == 1] <- 6
                
                message("Esta variavel de posicao na ocupacao refere-se ao trabalho 'habitual' do individuo")
        }
        
        if(year == 1970){
                
                var_setor   = CensusData$v045
                var_posOcup = CensusData$v046
                
                setor_primario <- rep(0, length(var_posOcup))
                setor_primario[var_setor %in% 111:223] <- 1
                setor_primario[is.na(var_setor)] <- NA
                
                
                posOcup <- rep(NA, length(var_posOcup))
                
                posOcup[var_posOcup == 0] <- NA
                posOcup[var_posOcup %in% 1:2] <- 1
                posOcup[var_posOcup == 5] <- 2
                posOcup[var_posOcup == 3 & setor_primario == 0] <- 3
                posOcup[var_posOcup == 3 & setor_primario == 1] <- 4
                posOcup[var_posOcup == 4] <- 4
                posOcup[var_posOcup == 6 & setor_primario == 0] <- 5
                posOcup[var_posOcup == 6 & setor_primario == 1] <- 6
                
                message("Esta variavel de posicao na ocupacao refere-se ao trabalho 'habitual' do indiv??duo")
        }
        
        if(year == 1980){
                
                var_setor   = CensusData$v532
                var_posOcup_1 = CensusData$v533
                var_posOcup_2 = CensusData$v545
                
                var_posOcup = ifelse(!is.na(var_posOcup_2) & var_posOcup_2 != 0, var_posOcup_2, var_posOcup_1)
                
                setor_primario <- rep(0, length(var_posOcup))
                setor_primario[var_setor %in% 11:42] <- 1
                setor_primario[is.na(var_setor)] <- NA
                
                posOcup <- rep(NA, length(var_posOcup))
                
                posOcup[var_posOcup == 9] <- NA
                
                posOcup[var_posOcup %in% c(1:2,6)] <- 1
                posOcup[var_posOcup %in% c(4,7) ] <- 2
                posOcup[var_posOcup == 8 & setor_primario == 0] <- 3
                posOcup[var_posOcup == 8 & setor_primario == 1] <- 4
                posOcup[var_posOcup %in% c(3,5)] <- 4
                posOcup[var_posOcup == 0 & setor_primario == 0] <- 5
                posOcup[var_posOcup == 0 & setor_primario == 1] <- 6
                
                
        }
        
        if(year == 1991){
                
                var_setor   = CensusData$v0347
                var_posOcup = CensusData$v0349
                
                setor_primario <- rep(0, length(var_posOcup))
                setor_primario[var_setor %in% 11:42] <- 1
                setor_primario[is.na(var_setor)] <- NA
                
                posOcup <- rep(NA, length(var_posOcup))
                
                posOcup[var_posOcup %in% c(1,4,6:8)] <- 1
                posOcup[var_posOcup == 10] <- 2
                posOcup[var_posOcup == 5] <- 3
                posOcup[var_posOcup == 9 & setor_primario == 0] <- 3
                posOcup[var_posOcup == 9 & setor_primario == 1] <- 4
                posOcup[var_posOcup %in% c(2,3)] <- 4
                posOcup[var_posOcup == 11 & setor_primario == 0] <- 5
                posOcup[var_posOcup == 11 & setor_primario == 1] <- 6
                
        }
        
        if(year == 2000){
                
                var_setor   = CensusData$v4462
                var_posOcup = CensusData$v0447
                
                setor_primario <- rep(0, length(var_posOcup))
                setor_primario[var_setor %in% 1101:5002] <- 1
                setor_primario[is.na(var_setor)] <- NA
                
                posOcup <- rep(NA, length(var_posOcup))
                
                posOcup[var_posOcup %in% 1:4] <- 1
                posOcup[var_posOcup == 5] <- 2
                posOcup[var_posOcup == 6 & setor_primario == 0] <- 3
                posOcup[var_posOcup == 6 & setor_primario == 1] <- 4
                posOcup[var_posOcup %in% c(7,8) & setor_primario == 0] <- 5
                posOcup[var_posOcup %in% c(7,8) & setor_primario == 1] <- 6
                posOcup[var_posOcup == 9] <- 6
        }
        
        if(year == 2010){
                
                var_setor   = CensusData$v6471
                var_posOcup = CensusData$v6930
                
                setor_primario <- rep(0, length(var_posOcup))
                setor_primario[var_setor %in% 1101:3002] <- 1
                setor_primario[is.na(var_setor)] <- NA
                
                posOcup <- rep(NA, length(var_posOcup))
                
                posOcup[var_posOcup %in% 1:3] <- 1
                posOcup[var_posOcup == 5] <- 2
                posOcup[var_posOcup == 4 & setor_primario == 0] <- 3
                posOcup[var_posOcup == 4 & setor_primario == 1] <- 4
                posOcup[var_posOcup == 6 & setor_primario == 0] <- 5
                posOcup[var_posOcup == 6 & setor_primario == 1] <- 6
                posOcup[var_posOcup == 7] <- 6
        }
        gc()
        
        # 1 "employee"
        # 2 "employer"
        # 3 "self-employed, urban"
        # 4 "self-employed/unpaid, rural"
        # 5 "unpaid, urban"
        
        posOcup[posOcup == 6] <- 4
        
        CensusData[ , classWorker := posOcup]
        gc()
        
        return(CensusData)
        
}

