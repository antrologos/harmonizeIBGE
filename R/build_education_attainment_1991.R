#' Builds a synthetic variable for education attainment - 1991
#' @param data.frame
#' @value data.frame
#' @export


build_education_attainment_1991 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v520", "v521", "v522", "v523", "v524", "v525"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }


        # Censo de 1991 ==============================================================================================================
        # ============================================================================================================================
        gc()


        # Building age
        check_vars <- check_var_existence(CensusData, c("age"))
        age_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_demographics_age_1991(CensusData)
                age_just_created <- TRUE
        }
        gc()


        ##############################
        # Frequenta Seriado
        ##############################

        # Nao frequenta
        CensusData[v0325 == 0, education_tmp1 := 1]

        # Frequenta 1o grau regular ou supletivo - 1a a 4a serie
        CensusData[v0325 %in% c(1) & v0324 %in% c(1:4), education_tmp1 := 2]
        CensusData[v0325 %in% c(4) & v0324 %in% c(1:4), education_tmp1 := 2]

        # Frequenta 1o grau regular ou supletivo - 5a a 8a serie
        CensusData[v0325 %in% c(1) & v0324 %in% c(5:8), education_tmp1 := 4]
        CensusData[v0325 %in% c(4) & v0324 %in% c(5:8), education_tmp1 := 4]

        # Frequenta 2o grau regular ou supletivo - 5a a 8a serie
        CensusData[v0325 %in% c(2) & v0324 %in% c(1:4), education_tmp1 := 6]
        CensusData[v0325 %in% c(5) & v0324 %in% c(1:4), education_tmp1 := 6]

        # Frequenta superior - qualquer ano
        CensusData[v0325 %in% c(3), education_tmp1 := 8]

        gc()
        ##############################
        # Frequenta nao seriado
        ##############################

        # Nao Frequenta
        CensusData[v0326 == 0, education_tmp2 := 1]

        # Frequenta pre-escola
        CensusData[v0326 == 1, education_tmp2 := 1]

        # Frequenta alfabetizacao de adultos
        CensusData[v0326 == 2, education_tmp2 := 1]

        # Frequenta supletivo nao seriado de 1o grau
        CensusData[v0326 == 3, education_tmp2 := 3] ######## DECISAO AD HOC: PRIMARIO COMPLETO

        # Frequenta supletivo nao seriado de 2o grau
        CensusData[v0326 == 4, education_tmp2 := 6]

        # Frequenta pre-vestibular
        CensusData[v0326 == 5, education_tmp2 := 7]

        # Frequenta pre-vestibular
        CensusData[v0326 == 6, education_tmp2 := 9]

        gc()
        ##############################
        # Nao frequenta
        ##############################

        # Nao concluiu nenhum curso
        CensusData[v0328 == 0, education_tmp3 := 1]

        # Concluiu alfabetizacao de adultos
        CensusData[v0328 == 1, education_tmp3 := 1]

        # Concluiu alguma serie do primario - 1a a 3a serie
        CensusData[v0328 == 2 & v0327 %in% c(1:3), education_tmp3 := 2]

        # Concluiu alguma serie do primario - 4a a 6a serie
        CensusData[v0328 == 2 & v0327 %in% c(4:6), education_tmp3 := 3]  # Assumindo que o primario finda na 4a serie. 5a e 6a sao geralmente complementares ou classes de admissao

        # Concluiu alguma serie do ginasio - 1a a 3a serie
        CensusData[v0328 == 3 & v0327 %in% c(1:3), education_tmp3 := 4]

        # Concluiu alguma serie do ginasio - 4a e 5a serie
        CensusData[v0328 == 3 & v0327 %in% c(4:5), education_tmp3 := 5]  # Assumindo que o ginasio finda na 4a serie. 5a serie ? pouco usual (e talvez possa ser classe de admissao para o colegial)

        # Concluiu alguma serie do 1o grau - 1a a 3a serie
        CensusData[v0328 == 4 & v0327 %in% c(1:3), education_tmp3 := 2]

        # Concluiu alguma serie do 1o grau - 4a serie
        CensusData[v0328 == 4 & v0327 %in% c(4), education_tmp3 := 3]

        # Concluiu alguma serie do 1o grau - 5a a 7a serie
        CensusData[v0328 == 4 & v0327 %in% c(5:7), education_tmp3 := 4]

        # Concluiu alguma serie do 1o grau - 8a serie
        CensusData[v0328 == 4 & v0327 %in% c(8), education_tmp3 := 5]

        # Concluiu alguma serie do 2o grau ou colegial - 1a a 2a serie
        CensusData[v0328 %in% c(5) & v0327 %in% c(1:2), education_tmp3 := 6]
        CensusData[v0328 %in% c(6) & v0327 %in% c(1:2), education_tmp3 := 6]

        # Concluiu alguma serie do 2o grau ou colegial - 3a a 4a serie
        # Assumindo que o 2o grau finda no 3o ano. O 4o ano pode ser vocacional, cursado de forma subsequente
        CensusData[v0328 %in% c(5) & v0327 %in% c(3:4), education_tmp3 := 7]
        CensusData[v0328 %in% c(6) & v0327 %in% c(3:4), education_tmp3 := 7]

        # Concluiu alguma serie do superior
        CensusData[v0328 == 7, education_tmp3 := 8] #vai ser ajustado

        # Concluiu alguma serie do mestrado/doutorado
        CensusData[v0328 == 8, education_tmp3 := 9]

        gc()
        ##############################
        # Classificacao a partir da variavel de cursos
        # (espero que isso resolva parte das indecisoes acima)
        ##############################

        # Nao Concluiu curso algum
        CensusData[v0329 == 0, education_tmp4 := 1]

        # Concluiu algum curso primario
        CensusData[v0328 == 2 & v0329 %in% c(1:8), education_tmp4 := 3]

        # Concluiu algum curso de 1o grau/ginasio
        CensusData[v0328 %in% c(3) & v0329 %in% c(10:23), education_tmp4 := 5]
        CensusData[v0328 %in% c(4) & v0329 %in% c(10:23), education_tmp4 := 5]

        # Concluiu algum curso de 2o grau/colegial
        CensusData[v0328 %in% c(5) & v0329 %in% c(24:42), education_tmp4 := 7]
        CensusData[v0328 %in% c(6) & v0329 %in% c(24:42), education_tmp4 := 7]

        # Concluiu algum curso superior
        CensusData[v0328 %in% c(7) & v0329 %in% c(43:97), education_tmp4 := 9]

        gc()
        ############################################################
        ############################################################
        # variavel final

        # Substituindo missings por -1 -- para fins computacionais
        CensusData[is.na(education_tmp1), education_tmp1 := -1]
        CensusData[is.na(education_tmp2), education_tmp2 := -1]
        CensusData[is.na(education_tmp3), education_tmp3 := -1]
        CensusData[is.na(education_tmp4), education_tmp4 := -1]
        gc()

        CensusData[, education := as.numeric(NA)]

        # Regra de desambiguacao: o sujeito tera o maior nivel de ensino
        # dentre os captados pelas 4 variaveis auxiliares.
        education_tmp <- cbind(CensusData[ , education_tmp1],
                               CensusData[ , education_tmp2],
                               CensusData[ , education_tmp3],
                               CensusData[ , education_tmp4])

        education_tmp <-  apply(education_tmp, 1, max)

        CensusData[, education := education_tmp]
        gc()


        # Levando os valores -1 de volta para NA
        CensusData[education < 1,  education := NA]
        gc()

        # Ajuste para idade
        CensusData[age <= 4,  education := NA]
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }

        CensusData[ , education_tmp1 := NULL]
        CensusData[ , education_tmp2 := NULL]
        CensusData[ , education_tmp3 := NULL]
        CensusData[ , education_tmp4 := NULL]

        gc()
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

        #===========================================================================================
        CensusData
}
