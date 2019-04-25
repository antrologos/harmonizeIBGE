#' Builds a synthetic variable for yearsOfSchooling attainment - 1980
#' @param data.frame
#' @value data.frame
#' @export


build_education_yearsOfSchooling_1980 <- function(CensusData){
        
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
        
        # = Attending school 
        #  Grau 'nenhum' e serie 'nenhuma'
        CensusData[v521==0 & v520==0, yearsOfSchooling_tmp1 := 0]
        
        #  Grau 'primario' e serie 1-4
        CensusData[v521==1 & v520 %in% 1:4, yearsOfSchooling_tmp1 := v520 - 1]
        
        #  Grau 'ginasial medio' e serie 1-4
        CensusData[v521==2 & v520 %in% 1:4, yearsOfSchooling_tmp1 := 4 + v520 - 1]
        
        #  Grau '1o grau' e serie 1-8
        CensusData[v521==3 & v520 %in% 1:8, yearsOfSchooling_tmp1 := v520 - 1]
        
        #  Grau '2o grau' e serie 1-4
        CensusData[v521==4 & v520 %in% 1:4, yearsOfSchooling_tmp1 := apply(cbind(8 + v520 - 1, 10), 1, min, na.rm = T)]
        
        #  Grau 'colegial medio' e serie 1-4
        CensusData[v521==5 & v520 %in% 1:4, yearsOfSchooling_tmp1 := apply(cbind(8 + v520 - 1, 10), 1, min, na.rm = T)]
        
        #  Grau 'supletivo 1o grau' e serie 1-8
        CensusData[v521==6 & v520 %in% 1:8, yearsOfSchooling_tmp1 := v520 - 1]
        
        #  Grau 'supletivo 2o grau' e serie 1-4
        CensusData[v521==7 & v520 %in% 1:4, yearsOfSchooling_tmp1 := apply(cbind(8 + v520 - 1, 10), 1, min, na.rm = T)]
        
        #  Grau 'Superior' e series 1-6
        CensusData[v521==8 & v520 %in% 1:6, yearsOfSchooling_tmp1 := apply(cbind(11 + v520 - 1, 14), 1, min, na.rm = T)]
        
        #  Nao Seriado - Curso 'pre-escolar', 'alfabetizacao de adultos'
        CensusData[v522 %in% c(1,2), yearsOfSchooling_tmp1 := 0]
        
        #  Nao Seriado - Curso 'vestibular'
        CensusData[v522 %in% c(7), yearsOfSchooling_tmp1 := 11]
        
        #  Nao Seriado - Curso 'mestrado', 'doutorado'
        CensusData[v522 %in% c(8), yearsOfSchooling_tmp1 := 15]
        
        
        #  Special cases (residual, guesstimated)
        #  Serie 'sem declaracao': assumes minimum yrs of schooling for each level except 1o grau and supletivo 1o
        #  grau. In these cases, it's zero for <= 14 yrs old and 4 otherwise.
			#  attending supletivo 1o grau and/or 1o grau which get 4 
        CensusData[v521==1 & v520==9, yearsOfSchooling_tmp1 := 0] # primario
        CensusData[v521==2 & v520==9, yearsOfSchooling_tmp1 := 4] # ginasio medio
        
        CensusData[v521==3 & v520==9 & age <= 14, yearsOfSchooling_tmp1 := 0] # 1o grau
        CensusData[v521==3 & v520==9 & age > 14, yearsOfSchooling_tmp1 := 4] # 1o grau
        
        CensusData[v521==4 & v520==9, yearsOfSchooling_tmp1 := 8] # 2o grau
        CensusData[v521==5 & v520==9, yearsOfSchooling_tmp1 := 8] # colegial medio
        
        CensusData[v521==6 & v520==9 & age <= 14, yearsOfSchooling_tmp1 := 0] # supletivo 1o grau
        CensusData[v521==6 & v520==9 & age >  14, yearsOfSchooling_tmp1 := 4] # supletivo 1o grau
        
        CensusData[v521==7 & v520==9, yearsOfSchooling_tmp1 := 8]  # supletivo 2o grau
        CensusData[v521==8 & v520==9, yearsOfSchooling_tmp1 := 11] # superior
        
        #  Nao Seriado - Supletivos: same as above
        CensusData[v522 == 3 & age <= 14, yearsOfSchooling_tmp1 := 0]  # supletivo 1o grau
        CensusData[v522 == 3 & age > 14,  yearsOfSchooling_tmp1 := 4]  # supletivo 1o grau
        
        CensusData[v522 == 4, yearsOfSchooling_tmp1 := 8]  # supletivo 2o grau
        
        CensusData[v522 == 5 & age <= 14, yearsOfSchooling_tmp1 := 0]  # supletivo 1o grau TV
        CensusData[v522 == 5 & age > 14,  yearsOfSchooling_tmp1 := 4]  # supletivo 1o grau TV
        
        CensusData[v522 == 6,  yearsOfSchooling_tmp1 := 8]  # supletivo 2o grau TV
        
        
				
	# = Out of school
		#  Grau 'nenhum' e serie 'nenhuma'
                CensusData[v524==0 & v523==0,  yearsOfSchooling_tmp2 := 0]
		
		#  Grau 'alfabetizacao de adultos'
                CensusData[v524==1,  yearsOfSchooling_tmp2 := 0]
                
		#  Grau 'primario/elementar' e series 1-5
                CensusData[v524==2 & v523 %in% c(1:5),  yearsOfSchooling_tmp2 := apply(cbind(v523, 4), 1, min, na.rm = T)]
                
		#  Grau 'ginasial medio' e series 1-5
                CensusData[v524==3 & v523 %in% c(1:5),  yearsOfSchooling_tmp2 := apply(cbind(v523 + 4, 8), 1, min, na.rm = T)]
                
		#  Grau '1o grau' e series 1-8
                CensusData[v524==4 & v523 %in% c(1:8),  yearsOfSchooling_tmp2 := v523]
			
		#  Grau '2o grau' e series 1-4
                CensusData[v524==5 & v523 %in% c(1:4),  yearsOfSchooling_tmp2 := apply(cbind(v523 + 8, 11), 1, min, na.rm = T)]
                
		#  Grau 'colegial medio' e series 1-4 
                CensusData[v524==6 & v523 %in% c(1:4),  yearsOfSchooling_tmp2 := apply(cbind(v523 + 8, 11), 1, min, na.rm = T)]
			
		#  Grau 'superior' e series 1-6
                CensusData[v524==7 & v523 %in% c(1:6),  yearsOfSchooling_tmp2 := apply(cbind(v523 + 11, 14), 1, min, na.rm = T)]
                
		#  Grau 'mestrado/doutorado'
                CensusData[v524==8,  yearsOfSchooling_tmp2 := 15]
		
		#  Special cases (residual, guesstimated)
			#  Serie 'sem declaracao': same as above
                CensusData[v524==2 & v523 == 9,  yearsOfSchooling_tmp2 := 0] # primario
                CensusData[v524==3 & v523 == 9,  yearsOfSchooling_tmp2 := 4] # ginasial medio
                
                CensusData[v524==4 & v523 == 9 & age <= 14,  yearsOfSchooling_tmp2 := 0] # 1o grau
                CensusData[v524==4 & v523 == 9 & age >  14,  yearsOfSchooling_tmp2 := 4] # 1o grau
		
                CensusData[v524==5 & v523 == 9,  yearsOfSchooling_tmp2 := 8]  # 2o grau
                CensusData[v524==6 & v523 == 9,  yearsOfSchooling_tmp2 := 8]  # colegial medio
                CensusData[v524==7 & v523 == 9,  yearsOfSchooling_tmp2 := 11] # superior
                
				
	# = 'Tipo do ultimo curso concluido' (reconciled with v524 to minimize reporting mistakes)
                CensusData[,  yearsOfSchooling_tmp3 := 0] # sets everyone to zero
                
                CensusData[v524 == 2 & v525 %in% 1:8,  yearsOfSchooling_tmp3 := 4] #  concluiu primario/elementar
                
                CensusData[v524 == 3 & v525 %in% 10:23,  yearsOfSchooling_tmp3 := 8] # concluiu 1o grau/medio 1o ciclo
                CensusData[v524 == 4 & v525 %in% 10:23,  yearsOfSchooling_tmp3 := 8] # ditto
                
                CensusData[v524 == 5 & v525 %in% 24:42,  yearsOfSchooling_tmp3 := 11] # concluiu colegial/2o grau
                CensusData[v524 == 6 & v525 %in% 24:42,  yearsOfSchooling_tmp3 := 11] # ditto
                
                CensusData[v524 == 5 & v525 %in% 43:99,  yearsOfSchooling_tmp3 := 15] # concluiu superior
                CensusData[v524 == 6 & v525 %in% 43:99,  yearsOfSchooling_tmp3 := 15] # ditto
                
                
	# = Final outcome variable: assigns highest level among the three variables
	# Substituindo missings por -1 -- para fins computacionais
        CensusData[is.na(yearsOfSchooling_tmp1), yearsOfSchooling_tmp1 := -1]
        CensusData[is.na(yearsOfSchooling_tmp2), yearsOfSchooling_tmp2 := -1]
        CensusData[is.na(yearsOfSchooling_tmp3), yearsOfSchooling_tmp3 := -1]
        gc(); Sys.sleep(1); gc()
        
        educVars <- which(names(CensusData) %in% c("yearsOfSchooling_tmp1", 
                                                   "yearsOfSchooling_tmp2", 
                                                   "yearsOfSchooling_tmp3"))
        
        # Regra de desambiguacao: o sujeito tera o maior nivel de ensino
        # dentre os captados pelas 4 variaveis auxiliares.
        CensusData[, yearsOfSchooling := do.call(pmax,.SD) , .SDcols=educVars]
        gc(); Sys.sleep(1); gc()
        
        
        # Levando os valores -1 de volta para NA
        CensusData[yearsOfSchooling == -1,  yearsOfSchooling := NA]
        
        # Ajuste para idade
        CensusData[age <= 4,  yearsOfSchooling := NA]
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }
        
        CensusData[ , yearsOfSchooling_tmp1 := NULL]
        CensusData[ , yearsOfSchooling_tmp2 := NULL]
        CensusData[ , yearsOfSchooling_tmp3 := NULL]
        
        
        gc(); Sys.sleep(1); gc()
        
        # ============================================================================================================================
        
        
        CensusData
}

