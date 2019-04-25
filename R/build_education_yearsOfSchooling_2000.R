#' Builds a synthetic variable for yearsOfSchooling yearsOfSchoolingAttainment - 2000
#' @param data.frame
#' @value data.frame
#' @export


build_education_yearsOfSchooling_2000 <- function(CensusData){
        
        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }
        
        check_vars <- check_var_existence(CensusData, c("v0429", "v0430", "v0431", "v0432", "v0433", "v0434"))
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
        
        #= Attending school
        # Curso 'Pre escolar'
        CensusData[v0430==2, yearsOfSchooling := 0]
        # Curso 'alfabetizacao'	
        CensusData[v0430==3, yearsOfSchooling := 0]
        # Curso 'alfabetizacao de adultos'
        CensusData[v0430==4, yearsOfSchooling := 0]	
        # Curso 'Ensino Fundamental seriado'
        CensusData[v0430==5 & v0431 %in% c(1:8), yearsOfSchooling := v0431 - 1]
        # Curso 'Ensino Fundamental nao seriado'
        CensusData[v0430==6 & v0431 %in% c(1:8), yearsOfSchooling := v0431 - 1]
        # Curso 'Supletivo 1o grau'
        CensusData[v0430==7 & v0431 %in% c(1:8), yearsOfSchooling := v0431 - 1]
        # Curso 'Ensino medio seriado'
        CensusData[v0430==8 & v0431 %in% c(1:4), yearsOfSchooling := apply(cbind(8 + v0431 - 1, 10), 1, min)]
        # Curso 'Ensino medio nao seriado'
        CensusData[v0430==9 & v0431 %in% c(1:4), yearsOfSchooling := apply(cbind(8 + v0431 - 1, 10), 1, min)]
        # Curso 'Supletivo 2o grau'
        CensusData[v0430==10 & v0431 %in% c(1:4), yearsOfSchooling := apply(cbind(8 + v0431 - 1, 10), 1, min)]
        # Curso 'Pre vestibular'
        CensusData[v0430==11, yearsOfSchooling := 11]
        # Curso 'Superior - Graduacao '
        CensusData[v0430==12 & v0431 %in% c(1:6), yearsOfSchooling := apply(cbind(11 + v0431 - 1, 14), 1, min)]
        # Curso 'Superior - Mestrado/Doutorado'
        CensusData[v0430==13, yearsOfSchooling := 15]
        # Special cases (residual, guesstimated)
        # Serie 'curso nao seriado': same as 1980-1991
        CensusData[v0430==6  & v0431==9 & age<=14, yearsOfSchooling := 0] # fundamental nao seriado
        CensusData[v0430==6  & v0431==9 & age>14,  yearsOfSchooling := 4] # ditto
        
        CensusData[v0430==7  & v0431==9 & age<=14, yearsOfSchooling := 0] # supletivo 1o grau
        CensusData[v0430==7  & v0431==9 & age>14,  yearsOfSchooling := 4] # ditto
        
        CensusData[v0430==9  & v0431==9,  yearsOfSchooling := 8] # ensino medio nao seriado
        CensusData[v0430==10 & v0431==9,  yearsOfSchooling := 8]  # supletivo 2o grau
        
        #= Out of school
        # Never attended school
        CensusData[v0432==9 | v0429==4, yearsOfSchooling := 0]
        
        # Curso 'alfabetizacao de adultos'	
        CensusData[v0432==1, yearsOfSchooling := 0]	
        
        # Curso 'antigo primario' e series 1-6
        CensusData[v0432==2 & v0433 %in% c(1:6), yearsOfSchooling := apply(cbind(v0433, 3), 1, min)]
        CensusData[v0432==2 & v0433 %in% c(4:6) & v0434==1, yearsOfSchooling := 4]
        
        # Curso 'antigo ginasio' e series 1-5
        CensusData[v0432==3 & v0433 %in% c(1:5), yearsOfSchooling := apply(cbind(4 + v0433, 7), 1, min)]
        CensusData[v0432==3 & v0433 %in% c(4:5) & v0434==1, yearsOfSchooling := 8 ]		
        
        # Curso 'antigo classico, cientifico etc'
        CensusData[v0432==4 & v0433 %in% c(1:4), yearsOfSchooling := apply(cbind(8 + v0433, 10), 1, min)]
        CensusData[v0432==4 & v0433 %in% c(3:4) & v0434==1, yearsOfSchooling := 11]
        
        # Curso 'Ensino Fundamental/1o grau'
        CensusData[v0432==5 & v0433 %in% c(1:8), yearsOfSchooling := apply(cbind(v0433, 7), 1, min)]
        CensusData[v0432==5 & v0433 == 8 & v0434==1, yearsOfSchooling := 8]
        
        # Curso 'Ensino Medio/2o grau'
        CensusData[v0432==6 & v0433 %in% c(1:4), yearsOfSchooling := apply(cbind(8 + v0433, 10), 1, min) ]
        CensusData[v0432==6 & v0433 %in% c(3:4) & v0434==1, yearsOfSchooling := 11]
        
        # Curso 'Superior - Graduacao'
        CensusData[v0432==7 & v0433 %in% c(1:6), yearsOfSchooling := apply(cbind(11 + v0433, 14), 1, min)]
        CensusData[v0432==7 & v0433 %in% c(3:6) & v0434==1, yearsOfSchooling := 15]		
        
        # Curso 'Superior - mestrado/doutorado'
        CensusData[v0432==8, yearsOfSchooling := 15]
                                         
                                         
        # Special cases (residual, guesstimates)
        # Serie 'curso nao seriado': same as 1980-1991
        CensusData[v0432==5 & v0433==9 & v0434==1,           yearsOfSchooling := 8] # Fundamental
        CensusData[v0432==5 & v0433==9 & v0434==2 & age<=14, yearsOfSchooling := 0] # ditto
        CensusData[v0432==5 & v0433==9 & v0434==2 & age>14,  yearsOfSchooling := 4] # ditto
        
        CensusData[v0432==6 & v0433==9 & v0434==1, yearsOfSchooling := 11] # Medio
        CensusData[v0432==6 & v0433==9 & v0434==2, yearsOfSchooling := 8]  # ditto	
        
        
        
        # Ajuste para idade
        CensusData[age <= 4,  yearsOfSchooling := NA]
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }
        
        
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

