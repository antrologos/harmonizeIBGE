#' Builds a synthetic variable for yearsOfSchooling attainment - 1991
#' @param data.frame 
#' @value data.frame
#' @export

#CensusData = censo

build_education_yearsOfSchooling_1991 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("v0324", "v0325", "v0326", "v0327", "v0328", "v0329"))
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
        check_vars <-  harmonizeIBGE:::check_var_existence(CensusData, c("age"))
        age_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_demographics_age_1991(CensusData)
                age_just_created <- TRUE
        }
        gc()

        
        #= Attending school
        # Grau 'nenhum' e serie 'nenhuma'
        CensusData[v0325==0 & v0324==0, yearsOfSchooling_tmp1 := as.numeric(0)]
        
        # Grau '1o' e serie 1-8
        CensusData[v0325==1 & v0324 %in% c(1:8), yearsOfSchooling_tmp1 := v0324 - 1]
        # Grau '2o' e serie 1-4
        CensusData[v0325==2 & v0324 %in% c(1:4), yearsOfSchooling_tmp1 := apply(cbind(8 + v0324 - 1, 10), 1, min)]
        # Grau 'Superior' e serie 1-6
        CensusData[v0325==3 & v0324 %in% c(1:6), yearsOfSchooling_tmp1 := apply(cbind(11 + v0324 - 1, 14), 1, min)]
        # Grau 'Supletivo 1o grau' e serie 1-8
        CensusData[v0325==4 & v0324 %in% c(1:8), yearsOfSchooling_tmp1 := v0324 - 1]
        # Grau 'Supletivo 2o grau' e serie 1-4
        CensusData[v0325==5 & v0324 %in% c(1:4), yearsOfSchooling_tmp1 := apply(cbind(8 + v0324 - 1, 10), 1, min)]
        # Nao seriado - pre escolar
        CensusData[v0326==1, yearsOfSchooling_tmp1 := 0]
        # Nao seriado - alfabetizacao de adults
        CensusData[v0326==2, yearsOfSchooling_tmp1 := 0]
        # Nao seriado - pre vestibular
        CensusData[v0326==5, yearsOfSchooling_tmp1 := 11]	
        # Nao seriado - mestrado/doutorado
        CensusData[v0326==6, yearsOfSchooling_tmp1 := 15]	
        # Special cases (residual, guesstimated)
        # Nao Seriado - Supletivos: same as in 1980
        CensusData[v0326==3 & age<=14, yearsOfSchooling_tmp1 := 0]		# supletivo 1o grau
        CensusData[v0326==3 & age>14,  yearsOfSchooling_tmp1 := 4]		# supletivo 1o grau
        CensusData[v0326==4,	       yearsOfSchooling_tmp1 := 8]              # supletivo 2o grau
        
                                               
        #= Out of school
                # Grau 'nenhum' e serie 'nenhuma'
                CensusData[v0328==0 & v0327==0, yearsOfSchooling_tmp2 := 0]
                # Grau 'nenhum' e serie 'nunca frequentou
                CensusData[v0328==0 & v0327==9, yearsOfSchooling_tmp2 := 0]
	        # Grau 'alfabetizacao de adultos'
                CensusData[v0328==1, yearsOfSchooling_tmp2 := 0]
		# Grau 'primario/elementar' e series 1-6
		CensusData[v0328==2 & v0327 %in% c(1:6), yearsOfSchooling_tmp2 := apply(cbind(v0327, 4), 1, min)]
		# Grau 'ginasial/medio 1o ciclo' e series 1-5
		CensusData[v0328==3 & v0327 %in% c(1:5), yearsOfSchooling_tmp2 := apply(cbind(4 + v0327, 8), 1, min)]
		# Grau '1o grau' e series 1-8
		CensusData[v0328==4 & v0327 %in% c(1:8), yearsOfSchooling_tmp2 := v0327]
		# Grau '2o grau' e series 1-4
		CensusData[v0328==5 & v0327 %in% c(1:4), yearsOfSchooling_tmp2 := apply(cbind(8 + v0327, 11), 1, min)]
		# Grau 'colegial/medio 2o ciclo' e series 1-4
		CensusData[v0328==6 & v0327 %in% c(1:4), yearsOfSchooling_tmp2 := apply(cbind(8 + v0327, 11), 1, min)]
		# Grau 'superior' e series 1-6
		CensusData[v0328==7 & v0327 %in% c(1:6), yearsOfSchooling_tmp2 := apply(cbind(11 + v0327, 14), 1, min)]
		# Grau 'mestrado/doutorado'
		CensusData[v0328==8, yearsOfSchooling_tmp2 := 15]
		           
	#= 'Curso concluido' (reconciled with v0328 to minimize reporting mistakes)
	        CensusData[, yearsOfSchooling_tmp3 := 0]		# sets everyone to zero
                CensusData[v0328==2 & v0329 %in% c(1:8),   yearsOfSchooling_tmp3 := 4]		# concluiu primario
                
	        CensusData[v0328==3 & v0329 %in% c(10:23), yearsOfSchooling_tmp3 := 8]		# conclui 1o grau/ginasio
	        CensusData[v0328==4 & v0329 %in% c(10:23), yearsOfSchooling_tmp3 := 8] 		# ditto
	        
	        CensusData[v0328==5 & v0329 %in% c(24:42), yearsOfSchooling_tmp3 := 11]		# concluiu 2o grau/colegial
	        CensusData[v0328==6 & v0329 %in% c(24:42), yearsOfSchooling_tmp3 := 11]		# ditto
	        
	        CensusData[v0328==7 & v0329 %in% c(43:97), yearsOfSchooling_tmp3 := 15]		# concluiu superior		
	        
	
	        
	#= Final outcome variable: assigns highest level among the three variables
	
        # Substituindo missings por -1 -- para fins computacionais
        CensusData[is.na(yearsOfSchooling_tmp1), yearsOfSchooling_tmp1 := -1]
        CensusData[is.na(yearsOfSchooling_tmp2), yearsOfSchooling_tmp2 := -1]
        CensusData[is.na(yearsOfSchooling_tmp3), yearsOfSchooling_tmp3 := -1]
        gc()

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

        gc()
        
        #===========================================================================================
        
        CensusData
}
