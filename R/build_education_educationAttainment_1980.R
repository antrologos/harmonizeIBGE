#' Builds a synthetic variable for educationAttainment attainment - 1980
#' @param data.frame
#' @value data.frame
#' @export


build_education_educationAttainment_1980 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v520", "v521", "v522", "v523", "v524", "v525"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        gc()

        # Censo de 1980 ==============================================================================================================
        # ============================================================================================================================

        # Building age
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("age"))
        age_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_demographics_age_1980(CensusData)
                age_just_created <- TRUE
        }
        gc()
        ##############################
        # Frequenta Seriado
        ##############################

        # Nao frequenta
        CensusData[v521 == 0, educationAttainment_tmp1 := 1]

        # Frequenta primario - 1a a 4a serie
        CensusData[v521 == 1 & v520 %in% c(1:4), educationAttainment_tmp1 := 2]

        # Frequenta primario - serie indefinida
        CensusData[v521 == 1 & v520 %in% c(9), educationAttainment_tmp1 := 2]

        # Frequenta ginasio - 1a a 4a serie
        CensusData[v521 == 2 & v520 %in% c(1:4), educationAttainment_tmp1 := 4]

        # Frequenta ginasio - Serie indefinida
        CensusData[v521 == 2 & v520 %in% c(9), educationAttainment_tmp1 := 4]

        # Frequenta 1o grau - 1a a 4a serie
        CensusData[v521 == 3 & v520 %in% c(1:4), educationAttainment_tmp1 := 2]

        # Frequenta 1o grau - 5a a 8a serie
        CensusData[v521 == 3 & v520 %in% c(5:8), educationAttainment_tmp1 := 4]

        # Frequenta 1o grau - serie indefinida
        CensusData[v521 == 3 & v520 %in% c(9), educationAttainment_tmp1 := 3] ######## DECISAO AD HOC: PRIMARIO COMPLETO

        # Frequenta Medio 2o Ciclo ou 2o grau - 1a a 4a serie
        CensusData[v521 %in% c(4) & v520 %in% c(1:4), educationAttainment_tmp1 := 6]
        CensusData[v521 %in% c(5) & v520 %in% c(1:4), educationAttainment_tmp1 := 6]

        # Frequenta Medio 2o Ciclo ou 2o grau - serie indefinida
        CensusData[v520 %in% c(9) & v521 %in% c(4,5), educationAttainment_tmp1 := 6]

        # Frequenta supletivo 1o grau (seriado) - 1a a 4a serie
        CensusData[v521 == 6 & v520 %in% c(1:4), educationAttainment_tmp1 := 2]

        # Frequenta supletivo 1o grau (seriado) - 5a a 8a serie
        CensusData[v521 == 6 & v520 %in% c(5:8), educationAttainment_tmp1 := 4]

        # Frequenta supletivo 1o grau (seriado) - serie indefinida
        CensusData[v521 == 6 & v520 %in% c(9), educationAttainment_tmp1 := 3] ######## DECISAO AD HOC: PRIMARIO COMPLETO

        # Frequenta supletivo 2o grau (seriado) - 1a a 3a serie
        CensusData[v521 == 7 & v520 %in% c(1:3), educationAttainment_tmp1 := 6]

        # Frequenta supletivo 2o grau (seriado) - serie indefinida
        CensusData[v521 == 7 & v520 %in% c(9), educationAttainment_tmp1 := 6]

        # Frequenta superior
        CensusData[v521 == 8, educationAttainment_tmp1 := 8]


        gc()
        ##############################
        # Frequenta nao seriado
        ##############################

        # Nao Frequenta
        CensusData[v522 == 0, educationAttainment_tmp2 := 1]

        # Frequenta pre-escola
        CensusData[v522 == 1, educationAttainment_tmp2 := 1]

        # Frequenta Alfabetizacao de adultos
        CensusData[v522 == 2, educationAttainment_tmp2 := 1]

        # Frequenta Supletivo 1o grau (nao seriado)
        CensusData[v522 == 3, educationAttainment_tmp2 := 3] ######## DECISAO AD HOC: PRIMARIO COMPLETO

        # Frequenta Supletivo 2o grau (nao seriado)
        CensusData[v522 == 4, educationAttainment_tmp2 := 6]

        # Frequenta Supletivo 1o grau via Tv ou radio (nao seriado)
        CensusData[v522 == 5, educationAttainment_tmp2 := 3] ######## DECISAO AD HOC: PRIMARIO COMPLETO

        # Frequenta Supletivo 2o grau via Tv ou radio (nao seriado)
        CensusData[v522 == 6, educationAttainment_tmp2 := 6]

        # Frequenta pre-vestibular
        CensusData[v522 == 7, educationAttainment_tmp2 := 7]

        # Frequenta mestrado ou doutorado
        CensusData[v522 == 8, educationAttainment_tmp2 := 9]


        gc()

        ##############################
        # Nao frequenta
        ##############################

        # Nao concluiu nenhum curso
        CensusData[v524 == 0, educationAttainment_tmp3 := 1]

        # Concluiu alfabetizacao de adultos
        CensusData[v524 == 1, educationAttainment_tmp3 := 1]

        # Concluiu alguma serie do Primario - 1a a 3a serie
        CensusData[v524 == 2 & v523 %in% c(1:3), educationAttainment_tmp3 := 2]

        # Concluiu alguma serie do Primario - 4a ou 5a serie
        CensusData[v524 == 2 & v523 %in% c(4:5), educationAttainment_tmp3 := 3]

        # Concluiu alguma serie do Primario - serie indefinida
        CensusData[v524 == 2 & v523 %in% c(9), educationAttainment_tmp3 := 2]  ######## DECISAO AD HOC: PRIMARIO INCOMPLETO

        # Concluiu alguma serie do ginasial - 1a a 3a serie
        CensusData[v524 == 3 & v523 %in% c(1:3), educationAttainment_tmp3 := 4]

        # Concluiu alguma serie do ginasial - 4a ou 5a serie
        CensusData[v524 == 3 & v523 %in% c(4:5), educationAttainment_tmp3 := 5] # Poucas pessoas fazem 5o serie do ginasio. Considero que concluir ao menos a 4a serie ja implica em conclusao desta etapa

        # Concluiu alguma serie do ginasial - serie indefinida
        CensusData[v524 == 3 & v523 %in% c(9), educationAttainment_tmp3 := 6]  ######## DECISAO AD HOC: MEDIO INCOMPLETO

        # Concluiu 1o grau - 1a a 3a serie
        CensusData[v524 == 4 & v523 %in% c(1:3), educationAttainment_tmp3 := 2]

        # Concluiu alguma serie do 1o grau - 4a serie
        CensusData[v524 == 4 & v523 %in% c(4), educationAttainment_tmp3 := 3]

        # Concluiu alguma serie do 1o grau - 5a a 7a serie
        CensusData[v524 == 4 & v523 %in% c(5:7), educationAttainment_tmp3 := 4]

        # Concluiu alguma serie do 1o grau - 8a serie
        CensusData[v524 == 4 & v523 %in% c(8), educationAttainment_tmp3 := 5]

        # Concluiu alguma serie do 1o grau - serie indefinida
        CensusData[v524 == 4 & v523 %in% c(9), educationAttainment_tmp3 := 2]   ######## DECISAO AD HOC: PRIMARIO INCOMPLETO

        # Concluiu alguma serie do Medio 1o ciclo ou 2o grau - 1a a 2a serie
        CensusData[v524 %in% c(5) & v523 %in% c(1:2), educationAttainment_tmp3 := 6]
        CensusData[v524 %in% c(6) & v523 %in% c(1:2), educationAttainment_tmp3 := 6]

        # Concluiu alguma serie do Medio 2o ciclo ou 2o grau - 3a a 4a serie
        CensusData[v524 %in% c(5) & v523 %in% c(3:4), educationAttainment_tmp3 := 7] # Poucas pessoas fazem 4o ano do 2o grau. Considero que concluir ao menos o 3o ano ja implica em conclusao desta etapa
        CensusData[v524 %in% c(6) & v523 %in% c(3:4), educationAttainment_tmp3 := 7]

        # Concluiu alguma serie do Medio 2o ciclo ou 2o grau - serie indefinida
        CensusData[v523 %in% c(9) & v524 %in% c(5,6), educationAttainment_tmp3 := 6]  ######## DECISAO AD HOC: MEDIO INCOMPLETO

        # Concluiu alguma serie do superior - qualquer serie
        CensusData[v524 == 7, educationAttainment_tmp3 := 8] # vai receber ajuste MAIS ABAIXO (a partir da informacao sobre cursos)

        # Concluiu alguma ANO do mestrado ou doutorado
        CensusData[v524 == 8, educationAttainment_tmp3 := 9]

        gc()

        ##############################
        # Classificacao a partir da variavel de cursos
        # (espero que isso resolva parte das indecisoes acima)
        ##############################
        # houve problema aqui: pessoas que nao concluiram nenhum grau (v524=0)
        # respondem cursos. por isso adicionei a condicao de que v524 deve ser
        # compativel com o curso declarado


        # Nao Concluiu curso algum
        CensusData[v525 == 0, educationAttainment_tmp4 := 1]

        # Concluiu primario/elementar
        CensusData[v524 %in% c(2) & v525 %in% c(1:8), educationAttainment_tmp4 := 3]

        # Concluiu 1o grau/medio 1o ciclo
        CensusData[v524 %in% c(3) & v525 %in% c(10:23), educationAttainment_tmp4 := 5]
        CensusData[v524 %in% c(4) & v525 %in% c(10:23), educationAttainment_tmp4 := 5]

        # Concluiu colegial/medio 2o ciclo/2o grau
        CensusData[v524 %in% c(5) & v525 %in% c(24:42), educationAttainment_tmp4 := 7]
        CensusData[v524 %in% c(6) & v525 %in% c(24:42), educationAttainment_tmp4 := 7]

        # Concluiu superior
        CensusData[v524 %in% c(7) & v525 %in% c(43:99), educationAttainment_tmp4 := 9]
        CensusData[v524 %in% c(8) & v525 %in% c(43:99), educationAttainment_tmp4 := 9]


        gc()
        ############################################################
        ############################################################
        # variavel final

        # Substituindo missings por -1 -- para fins computacionais
        CensusData[is.na(educationAttainment_tmp1), educationAttainment_tmp1 := -1]
        CensusData[is.na(educationAttainment_tmp2), educationAttainment_tmp2 := -1]
        CensusData[is.na(educationAttainment_tmp3), educationAttainment_tmp3 := -1]
        CensusData[is.na(educationAttainment_tmp4), educationAttainment_tmp4 := -1]
        gc(); Sys.sleep(1); gc()
        
        educVars <- which(names(CensusData) %in% c("educationAttainment_tmp1", "educationAttainment_tmp2",
                                                   "educationAttainment_tmp3", "educationAttainment_tmp4"))
        
        # Regra de desambiguacao: o sujeito tera o maior nivel de ensino
        # dentre os captados pelas 4 variaveis auxiliares.
        CensusData[, educationAttainment := do.call(pmax,.SD) , .SDcols=educVars]
        gc(); Sys.sleep(1); gc()
        
        
        # Levando os valores -1 de volta para NA
        CensusData[educationAttainment < 1,  educationAttainment := NA]

        # Ajuste para idade
        CensusData[age <= 4,  educationAttainment := NA]
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }

        CensusData[ , educationAttainment_tmp1 := NULL]
        CensusData[ , educationAttainment_tmp2 := NULL]
        CensusData[ , educationAttainment_tmp3 := NULL]
        CensusData[ , educationAttainment_tmp4 := NULL]


        gc(); Sys.sleep(1); gc()

        # ============================================================================================================================


        #educationAttainment
        # 1 - Nenhum
        # 2 - Primario incompleto
        # 3 - Primario completo
        # 4 - Fundamental incompleto
        # 5 - Fundamental completo
        # 6 - Medio incompleto
        # 7 - Medio completo
        # 8 - Superior incompleto
        # 9 - Superior completo

        CensusData
}

