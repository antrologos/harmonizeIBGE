#' Builds a synthetic variable for education attainment - 1960 
#' @param data.frame
#' @value data.frame
#' @export


build_education_attainment_1960 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }


        check_vars <- check_var_existence(CensusData, c("v211", "v212", "v213", "v214"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        gc()


        # Censo de 1960 ==============================================================================================================
        # ============================================================================================================================

        # Building schoolattnd
        check_vars <- check_var_existence(CensusData, c("schoolattnd"))
        schoolattnd_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_education_schoolattnd_1960(CensusData)
                schoolattnd_just_created <- TRUE
        }

        # Building age
        check_vars <- check_var_existence(CensusData, c("age"))
        age_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_demographics_age_1960(CensusData)
                age_just_created <- TRUE
        }
        gc()

        ################################################
        # recodificacoes para simplificar as operacoes posteriores


        # As alternativas e codigos do censo de 1960 nao estao ordenados
        # no banco de dados. Por esta razao, as variaveis sobre series e graus
        # serao recodificadas

        #  series
        CensusData[, series := as.numeric(NA)]
        CensusData[v212 == 0, series := 0]
        CensusData[v212 == 2, series := 99]
        CensusData[v212 == 1, series := 98]
        CensusData[v212 == 4, series := 1]
        CensusData[v212 == 6, series := 3]
        CensusData[v212 == 5, series := 2]
        CensusData[v212 == 8, series := 5]
        CensusData[v212 == 7, series := 4]
        CensusData[v212 == 9, series := 6]

        # Categorias da nova variavel: series
        # ===================================
        # 0 - cursando 1a serie do elementar
        # 1 - 1a serie
        # 2 - 2a serie
        # 3 - 3a serie
        # 4 - 4a serie
        # 5 - 5a serie
        # 6 - 6a serie
        # 98 - nunca frequentou
        # 99 - ignorado


        # graus
        CensusData[, graus := as.numeric(NA)]
        CensusData[v213 == 0, graus := 0]
        CensusData[v213 == 2, graus := 1]
        CensusData[v213 == 3, graus := 2]
        CensusData[v213 == 1, graus := 98]
        CensusData[v213 == 5, graus := 4]
        CensusData[v213 == 4, graus := 3]
        CensusData[v213 == 6, graus := 99]

        # Categorias da nova variavel: graus
        # ===================================
        # 0 - cursando 1a serie do elementar
        # 1 - Primario
        # 2 - Medio 1o ciclo
        # 3 - Medio 2o ciclo
        # 4 - Superior
        # 98 - Nunca frequentou escola
        # 99 - Ignorado

        gc()

        ################################################
        # Ajustes adicionais de consistencia:

        # saber a serie sem saber o grau nao ajuda. vai tudo pra missing
        CensusData[graus == 99, series := 99]
        CensusData[is.na(series) & !is.na(graus), series := 99]  # (1 registro -- justamente um que foi corrompido no arquivo de dados original)

        # nao existe 4o ano do Medio 2o Ciclo
        CensusData[graus == 3 & series == 4, series := 99] # (acao arbitraria. Afeta apenas 1 registro - que nao estava corrompido)


        ################################################
        # Ajustes por idade

        CensusData[age <= 4, graus := NA]
        CensusData[age <= 4, series := NA]

        CensusData[age >= 5 & is.na(graus), graus := 99]
        CensusData[age >= 5 & is.na(series), series := 99]

        gc()
        ################################################
        # Utilizando a informacao sobre ultimo grau e serie concluidos com aprovacao

        # nunca frequentou escola
        CensusData[graus == 98, education_tmp1 := 1]

        # Cursa a primeira serie do elementar
        CensusData[graus == 0, education_tmp1 := 1]

        # Cursou PRIMARIO, 1a a 3a serie
        CensusData[graus == 1 & series %in% 1:3, education_tmp1 := 2]

        # Cursou PRIMARIO, concluiu 4a, 5a ou 6a serie
        # O primario regulamentar possuia 4 anos. A quinta serie era o
        # complementar (opcional ou nao ofertado em todas as localidades)
        # Em alguns lugares era classe preparatoria para o exame de admissao
        # do medio 1o ciclo
        # O sexto ano (registrado apenas para dois individuos) poderia ser
        # tambem um complementar ou classe preparatoria
        CensusData[graus == 1 & series %in% 4:6, education_tmp1 := 3]

        # Cursou MEDIO 1o Ciclo, 1a a 4a serie (sem informacao sobre conclusao)
        CensusData[graus == 2 & series %in% 1:4, education_tmp1 := 4] # vai receber ajuste mais abaixo

        # Cursou MEDIO 1o Ciclo, 5a serie
        CensusData[graus == 2 & series %in% 5, education_tmp1 := 5]

        # Cursou MEDIO 2o Ciclo, 1o ou 2o ano (sem informacao sobre conclusao)
        CensusData[graus == 3 & series %in% 1:2, education_tmp1 := 6]

        # Cursou MEDIO 2o Ciclo, 3o ano
        CensusData[graus == 3 & series %in% 3, education_tmp1 := 7]

        # Cursou Superior
        CensusData[graus == 4, education_tmp1 := 8] # superior completo sera ajustado a partir da informacao sobre curso completo


        gc()
        ################################################
        # Apenas o grau -- para aqueles sem informacao sobre serie concluida

        # FREQUENTAM ESCOLA
        # ====================

        # Frequentam primario - serie indefinida
        CensusData[graus == 1 & series == 99 & schoolattnd == 1, education_tmp2 := 2]

        # Frequentam medio 1o ciclo - serie indefinida
        CensusData[graus == 2 & series == 99 & schoolattnd == 1, education_tmp2 := 4]

        # Frequentam medio 2o ciclo - serie indefinida
        CensusData[graus == 3 & series == 99 & schoolattnd == 1, education_tmp2 := 6]

        # Frequentam superior - serie indefinida
        CensusData[graus == 4 & series == 99 & schoolattnd == 1, education_tmp2 := 8]

        gc()

        # NAO FREQUENTAM ESCOLA
        # ====================

        # Frequentou primario - serie indefinida
        CensusData[graus == 1 & series == 99 & schoolattnd == 0, education_tmp2 := 2]

        # Frequentou medio 1o ciclo - serie indefinida
        CensusData[graus == 2 & series == 99 & schoolattnd == 0, education_tmp2 := 4]

        # Frequentou medio 2o ciclo - serie indefinida
        CensusData[graus == 3 & series == 99 & schoolattnd == 0, education_tmp2 := 6]

        # Frequentou superior - serie indefinida
        CensusData[graus == 4 & series == 99 & schoolattnd == 0, education_tmp2 := 8]


        gc()
        ################################################
        # Considerando o curso concluido

        # Cursou primario (e outros elementares) e concluiu
        CensusData[v214 %in% 0, education_tmp3 := 1]

        # Cursou primario (e outros elementares) e concluiu
        CensusData[v214 %in% 10:19, education_tmp3 := 3]

        # Cursou medio 1o ciclo e concluiu
        CensusData[v214 %in% 20:39, education_tmp3 := 5]

        # Cursou medio 2o ciclo e concluiu
        CensusData[v214 %in% 40:49, education_tmp3 := 7]

        # Cursou superior e concluiu
        CensusData[v214 %in% 50:79, education_tmp3 := 9]



        gc()
        ############################################################
        # variavel final

        # Substituindo missings por -1 -- para fins computacionais
        CensusData[is.na(education_tmp1), education_tmp1 := -1]
        CensusData[is.na(education_tmp2), education_tmp2 := -1]
        CensusData[is.na(education_tmp3), education_tmp3 := -1]

        CensusData[, education := as.numeric(NA)]

        # Regra de desambiguacao: o sujeito tera o maior nivel de ensino
        # dentre os captados pelas 3 variaveis auxiliares.
        CensusData[, education := apply(cbind(education_tmp1,
                                              education_tmp2,
                                              education_tmp3),
                                        1, max)]


        # Levando os valores -1 de volta para NA
        CensusData[education < 1,  education := NA]

        # Ajuste para idade
        CensusData[age <= 4,  education := NA]
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }


        CensusData[ , education_tmp1 := NULL]
        CensusData[ , education_tmp2 := NULL]
        CensusData[ , education_tmp3 := NULL]

        CensusData[ , graus  := NULL]
        CensusData[ , series := NULL]
        gc()

        if(schoolattnd_just_created == TRUE){
                CensusData[ , schoolattnd := NULL]
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

        ####################################################################################################################

        # Observacao:
        # O Numero de pessoas cursando o ensino superior (809 registros) esta muito baixo. Mas isso pode fazer sentido...
        # a falta de vagas era mesmo uma das maiores questoes do periodo. Assumindo que esta amostra ? de 1,27% e que todos
        # os individuos possuem o mesmo peso, ent?o podemos estimar o total populacional desse grupo como:
        # 809 * (1/0.0127) = 63701
        #
        # O que faz algum sentido...

        CensusData
}
