#' Builds a synthetic variable for educationAttainment attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_education_educationAttainment_2010 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v0628", "v0629", "v0630", "v0631", "v0632", "v0633", "v0634"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        # Censo de 2010 ==============================================================================================================
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
        gc()

        CensusData[v0628 == 4, educationAttainment := 1]


        ##############################
        # Frequenta
        ##############################

        # creche, pre-escolar, classe de alfabetizacao, alfabetizacao de adultos
        CensusData[v0629 %in% 1:4, educationAttainment := 1]

        # 1o grau regular - 1a a 4a serie (1o ao 5o ano como equivalentes de frequencia ao primario)
        CensusData[v0629 %in% 5 & v0630 %in% 1:5, educationAttainment := 2]

        # 1o grau regular - 5a a 8a serie (6o ao 9o ano como equivalentes de frequencia ao ginasio/fundamental 2o ciclo)
        CensusData[v0629 %in% 5 & v0630 %in% 6:9, educationAttainment := 4]

        # 1o grau regular nao seriado - AJUSTE POR IDADE  ############################################# (ha pessoas adultas e ate idosas cursando fundamental)
        CensusData[v0629 %in% 5 & v0630 %in% 10 & age <= 10, educationAttainment := 2]

        # 1o grau regular nao seriado - AJUSTE POR IDADE  ############################################# (ha pessoas adultas e ate idosas cursando fundamental)
        CensusData[v0629 %in% 5 & v0630 %in% 10 & age >= 11, educationAttainment := 4]


        gc()


        # 1o grau SUPLETIvO
        CensusData[v0629 %in% 6, educationAttainment := 2] ######## DECISAO AD HOC: PRIMARIO COMPLETO

        # 2o grau regular OU supletivo
        CensusData[v0629 %in% 7:8, educationAttainment := 6]

        # superior
        CensusData[v0629 %in% 9, educationAttainment := 8]

        # pos-graduacao: especializacao, mestrado ou doutorado
        CensusData[v0629 %in% 10:12, educationAttainment := 9]

        # JA CONCLUIU outra graducao
        CensusData[v0632 %in% 1, educationAttainment := 9]


        gc()
        ##############################
        # NAO  Frequenta
        ##############################

        # creche/pre-escolar/Classe de alfabetizacao OU alfabetizacao de adultos
        CensusData[v0633 %in% 1:2, educationAttainment := 1]

        # primario nao concluido
        CensusData[v0633 %in% 3 & v0634 == 2, educationAttainment := 2]

        # primario concluido
        CensusData[v0633 %in% 3 & v0634 == 1, educationAttainment := 3]

        # ginasio nao concluido
        CensusData[v0633 %in% 4 & v0634 == 2, educationAttainment := 4]

        # ginasio nao concluido
        CensusData[v0633 %in% 4 & v0634 == 1, educationAttainment := 5]

        gc()


        # 1o grau - 1a a 3a serie
        CensusData[v0633 %in% 5, educationAttainment := 2]

        # 1o grau - 4a serie / 5o ano
        CensusData[v0633 %in% 6, educationAttainment := 3]

        # 1o grau - 5a a 8a serie (6o ao 9o ano) - nao concluido
        CensusData[v0633 %in% 7 & v0634 == 2, educationAttainment := 4]

        # 1o grau - 5a a 8a serie (6o ao 9o ano) - concluido
        CensusData[v0633 %in% 7 & v0634 == 1, educationAttainment := 5]

        # 1o grau SUPLETIvO - nao concluido
        CensusData[v0633 %in% 8 & v0634 == 2, educationAttainment := 2]  ######## DECISAO AD HOC: PRIMARIO COMPLETO


        gc()


        # 1o grau SUPLETIvO - concluido
        CensusData[v0633 %in% 8 & v0634 == 1, educationAttainment := 5]

        # colegial/cientifico etc / 2o grau (regular ou supletivo) - nao concluido
        CensusData[v0634 == 2 & v0633 %in% 9:10, educationAttainment := 6]

        # colegial/cientifico etc / 2o grau (regular ou supletivo) - concluido
        CensusData[v0634 == 1 & v0633 %in% 9:10, educationAttainment := 7]

        # superior - nao concluido
        CensusData[v0633 %in% 11 & v0634 == 2, educationAttainment := 8]

        # superior - concluido
        CensusData[v0633 %in% 11 & v0634 == 1, educationAttainment := 9]

        # pos-graduacao: especializacao, mestrado ou doutorado
        CensusData[v0633 %in% 12:14, educationAttainment := 9]

        gc()

        ##############################
        # Ajuste para idade
        CensusData[age <= 4,  educationAttainment := NA]
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }

        ##############################

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

        # Observacoes:
        # 1 - Nossa variavel esta discrepante com a variavel "N?vEL DE INSTRU??O" (v6400) original
        # do Censo 2010. 43.574 registros estao classificados como "3- M?dio completo e superior incompleto"
        # nessa variavel, enquanto na nossa estao marcados como "6 - MEDIO INCOMPLETO".
        # Mas esses sao registros de pessoas que disseram frequentar escola (v0628=1 ou 2), e que o nivel
        # frequentado ? o m?dio regular (v0629 = 7). Ou seja, estao todos ainda na escola
        # No entanto, a serie frequentada por todos eles ? a quarta. Se entendermos que a duracao
        # regulamentar ? de apenas 3 anos, ent?o essas pessoas devem ser marcadas como medio completo.
        # Mas se concedermos que certas modalidades de ensino medio profissionalizante tem 4 anos,
        # entao eles tem medio incompleto
        # Pelo que sei (mas tenho que verificar):
        # --- ha casos de ensino medio profissional que tem duracao de 3 anos + 1 ano e meio ou 2 anos de apenas vocacional (modalidade subsequente. ver LDB: artigo 36B)
        # ------- Nesse caso, o matriculado no ensino tecnico ja concluiu o ensino medio. E os 3 ou 4 semestres posteriores
        # ------- sao cursados com uma matricula separada. O IBGE parece assumir que todos listados no quadro acima se enquadram
        # ------- nessa categoria. Eu acho que isso nao esta correto. Afinal, se ele tivesse terminado o medio ja teria declarado isso
        # ------- na variavel v0634 (e nao teria declarado que esta frequentando o ensino medio regular)
        # --- ha casos de ensino medio profissional que tem duracao de 4 anos (modalidade integrada/articulada -  ver LDB: artigo 36B). Nesse caso, o individuo
        # ------- Nesse caso, o individuo que esta no quarto ano ainda nao terminou o medio. (ver LDB, art 36-C, I)


        # 2 - Outra discrepancia com respeito ? v6400. 91.530 apontados nessa variavel como "5- N?o determinado", estao
        # marcados na nossa como medio incompleto
        # Todas essas pessoas frequentam escola (v0628 = 1 ou 2), o nivel ? o medio regular (v0629 = 7) e a modalidade ?
        # nao seriada (v0631 = 5). Acredito que nesse caso a opcao escolhida na nossa variavel seja a correta.

        CensusData
}

