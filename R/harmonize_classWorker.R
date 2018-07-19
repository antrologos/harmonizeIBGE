#' Creates a synthetic variable for position in occupation
#' @param data.frame
#' @value data.frame
#' @export


harmonize_classWorker <- function(var_posOcup,
                                  var_setor,           #variavel original (nao o ISIC)
                                  year,
                                  type = "censo"
                                  ){ #apenas para 1980 - para ocupacao nao habitual


        if(type == "censo" & year == 1960){

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

        if(type == "censo" & year == 1970){

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

        if(type == "censo" & year == 1980){


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

        if(type == "censo" & year == 1991){

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

        if(type == "censo" & year == 2000){

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

        if(type == "censo" & year == 2010){

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

        # 1 "employee"
        # 2 "employer"
        # 3 "self-employed, urban"
        # 4 "self-employed/unpaid, rural"
        # 5 "unpaid, urban"

        posOcup[posOcup == 6] <- 4


        return(posOcup)

}

