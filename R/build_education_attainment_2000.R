#' Builds a synthetic variable for education attainment - 2000
#' @param data.frame
#' @value data.frame
#' @export


build_education_attainment_2000 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("age", "v0429", "v0430", "v0431", "v0432", "v0433", "v0434"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }


        # Censo de 2000 ==============================================================================================================
        # ============================================================================================================================


        # Building age
        check_vars <- check_var_existence(CensusData, c("age"))
        age_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_demographics_age_2000(CensusData)
                age_just_created <- TRUE
        }
        gc()
        ##############################
        # Nao Frequenta e nao frequentou
        ##############################

        CensusData[v0429 == 4, education := 1]


        ##############################
        # Frequenta
        ##############################

        # creche, pre-escolar, classe de alfabetizacao, alfabetizacao de adultos
        CensusData[v0430 %in% 1:4, education := 1]

        # 1o grau regular (seriado ou nao) e supletivo -- e com informacao de serie ou equivalente na v0431
        # 1a a 4a serie
        CensusData[v0430 %in% 5 & v0431 %in% 1:4, education := 2]
        CensusData[v0430 %in% 6 & v0431 %in% 1:4, education := 2]
        CensusData[v0430 %in% 7 & v0431 %in% 1:4, education := 2]

        # 1o grau regular (seriado ou nao) e supletivo -- e com informacao de serie ou equivalente na v0431
        # 5a a 8a serie
        CensusData[v0430 %in% 5 & v0431 %in% 5:8, education := 4]
        CensusData[v0430 %in% 6 & v0431 %in% 5:8, education := 4]
        CensusData[v0430 %in% 7 & v0431 %in% 5:8, education := 4]



        gc()



        # 1o grau regular nao-seriado SEM informacao de serie equivalente ############################################# (ha pessoas adultas e ate idosas cursando fundamental)
        # -- Usando da informacao sobre a idade para gerar classificacao educacional
        # -- menos do que 10 anos = primario incompleto
        CensusData[v0430 %in% 6 & v0431 %in% 9 & age <= 10, education := 2]

        # 1o grau regular nao-seriado SEM informacao de serie equivalente ############################################# (ha pessoas adultas e ate idosas cursando fundamental)
        # -- Usando da informacao sobre a idade para gerar classificacao educacional
        # -- 11 ou mais = fundamental incompleto
        CensusData[v0430 %in% 6 & v0431 %in% 9 & age >= 11, education := 4]

        # 1o grau supletivo SEM informacao de serie ou equivalente na v0431
        CensusData[v0430 %in% 7 & v0431 %in% 9, education := 3] ######## decisao ad hoc: PRIMARIO COMPLETO

        # 2o grau regular (seriado ou nao) e supletivo -- e com informacao de serie ou equivalente na v0431
        # Independentemente da serie = medio incompleto
        CensusData[v0430 %in% 8:10, education := 6]

        # Pre-vestibular
        CensusData[v0430 %in% 11, education := 7]

        # Superior
        # Independentemente do ano = superior incompleto
        CensusData[v0430 %in% 12, education := 8]

        # Mestrado ou doutorado
        CensusData[v0430 %in% 13, education := 9]

        gc()
        ##############################
        # Nao Frequenta
        ##############################

        # Nenhum curso
        CensusData[v0432 %in% 9, education := 1]

        # Alfabetizacao de adultos
        CensusData[v0432 %in% 1, education := 1]

        # Antigo primario & Nao concluiu
        CensusData[v0432 %in% 2 & v0434 == 2, education := 2]

        # Antigo primario & Concluiu
        CensusData[v0432 %in% 2 & v0434 == 1, education := 3]

        # Antigo ginasio & Nao concluiu
        CensusData[v0432 %in% 3 & v0434 == 2, education := 4]

        # Antigo ginasio & Concluiu
        CensusData[v0432 %in% 3 & v0434 == 1, education := 5]

        # Antigo classico/colegial/cientifico & Nao concluiu
        CensusData[v0432 %in% 4 & v0434 == 2, education := 6]



        gc()

        # Antigo classico/colegial/cientifico & Concluiu
        CensusData[v0432 %in% 4 & v0434 == 1, education := 7]

        # Fundamental / 1o grau - 1a a 3a series
        CensusData[v0432 %in% 5 & v0433 %in% 1:3, education := 2]

        # Fundamental / 1o grau - 4a serie
        CensusData[v0432 %in% 5 & v0433 %in% 4, education := 3]

        # Fundamental / 1o grau - 5a a 7a series
        CensusData[v0432 %in% 5 & v0433 %in% 5:7, education := 4]

        # Fundamental / 1o grau - 8a serie
        CensusData[v0432 %in% 5 & v0433 %in% 8, education := 5]

        # Fundamental / 1o grau NAO SERIADO - e nao concluiu
        CensusData[v0432 %in% 5 & v0433 %in% 9 & v0434 == 2, education := 3] ######## DECISAO AD HOC: PRIMARIO COMPLETO

        # Fundamental / 1o grau NAO SERIADO - mas concluiu
        CensusData[v0432 %in% 5 & v0433 %in% 9 & v0434 == 1, education := 5]

        # Medio / 2o grau - nao concluiu
        CensusData[v0432 %in% 6 & v0434 == 2, education := 6]

        # Medio / 2o grau - concluiu
        CensusData[v0432 %in% 6 & v0434 == 1, education := 7]

        # Superior - nao concluiu
        CensusData[v0432 %in% 7 & v0434 == 2, education := 8]

        # Superior - concluiu
        CensusData[v0432 %in% 7 & v0434 == 1, education := 9]

        # Mestrado / Doutorado
        CensusData[v0432 %in% 8, education := 9]

        gc()

        # Ajuste para idade
        CensusData[age <= 4,  education := NA]
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }


        #education
        # 1 - Nenhum
        # 2 - Primario incompleto
        # 3 - Primario completo
        # 4 - Fundamental incompleto
        # 5 - Fundamental completo
        # 6 - Medio incompleto
        # 7 - Medio completo
        # 8 - Superior incompleto
        # 9 - Superior completo

        # ============================================================================================================================

        # OBSERvACAO:
        # Ha 12.988 registros que na variavel anos de estudo estao marcados como 8 anos de estudo
        # mas que na nossa classificacao estao marcados como Medio Completo.
        # Todos esses casos, no entanto, dizem respeito a pessoas que afirmaram nao frequentar escola,
        # (v0429 = 3), que frequentaram anteriormente o medio (v0432 = 6) e que concluiram esse nivel
        # de ensino (v0434 = 1). Alem disso, a idade dessas pessoas ? compativel com a idade de
        # conclusao do m?dio (n?o h? casos de menores do que 17 anos).
        # Ou seja: acredito mais na nossa classifica??o. O fato de terem sido marcados como
        # 8 anos de estudo ? estranho -- e n?o deve invalidar nossa categorizacao como Medio completo.


        CensusData
}

