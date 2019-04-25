#' Builds a synthetic variable for educationAttainment attainment - 1970
#' @param data.frame
#' @value data.frame
#' @export

build_education_yearsOfSchooling_1970 <- function(CensusData){

        if(!is.data.frame(CensusData)){
                stop("'CensusData' is not a data.frame")
        }

        check_vars <- check_var_existence(CensusData, c("v037", "v038", "v039"))
        if(length(check_vars) > 0){
                stop("The following variables are missing from the data: ",
                     paste(check_vars, collapse = ", "))
        }

        if(!is.data.table(CensusData)){
                CensusData = as.data.table(CensusData)
        }

        gc()

        # Censo de 1970 ==============================================================================================================
        # ============================================================================================================================

        # Building age
        check_vars <- check_var_existence(CensusData, c("age"))
        age_just_created <- FALSE
        if(length(check_vars) > 0) {
                CensusData <- build_demographics_age_1970(CensusData)
                age_just_created <- TRUE
        }
        gc()

        #######################
        # Zero years 
        #######################
 
	# Nunca frequentou escola ou grau nao declarado
	# (todos os casos de serie sem declaracao sao nessas duas, nao precis usar v307)
	CensusData[v038 %in% c(0, 5), yearsOfSchooling := 0]
	
	# Cursou so alfabetizacao de adultos
	CensusData[v037 == 9, yearsOfSchooling := 0]
	
			
        #######################
        # 1-3 years 
        #######################
	
	# Concluiu 1a a 3a serie do primario
	CensusData[v037 == 1, yearsOfSchooling := 1]
	CensusData[v037 %in% c(2,3,4) & v038 == 1, yearsOfSchooling := v037 - 1]
		
			
	    #######################
        # 4 years 
        #######################
		
		# Concluiu 4a/5a/6a serie do primario
		CensusData[v037 %in% c(5,6) & v038 == 1, yearsOfSchooling := 4]			
			
		# Concluiu classe de admissao do primario para o medio
		CensusData[v037 == 7 & v038 == 1, yearsOfSchooling := 4]
					
		# Cursando modalidade de ensino a distancia ou estudando para exames de 
		# madureza para obtencao do medio 1o ciclo (artigo 99)
		CensusData[v037 == 8 & v038 == 2, yearsOfSchooling := 4]
				
		
		#######################
        # 5-7 years
        #######################
	
		# Concluiu 1a a 3a serie do Medio 1o ciclo
		CensusData[v037 %in% c(2,3,4) & v038 == 2, yearsOfSchooling := 4 + v037 - 1]		
						
	#######################
	# 8 years
	#######################
	
		# Concluiu 4a/5a/6a serie do medio 1o ciclo
		CensusData[v037 %in% c(5,6) & v038 == 2, yearsOfSchooling := 8]
					
		# Cursando modalidade de ensino à distancia ou estudando para os exames de 
		# madureza para obtencao do medio 2o ciclo
		CensusData[v037 == 8 & v038 == 3, yearsOfSchooling := 8]
					
	
	#######################
	# 9-10 years
	#######################
	
		# Concluiu 1a ou 2a serie do medio 2o ciclo
		CensusData[v037 %in% c(2,3) & v038 == 3, yearsOfSchooling := 8 + v037 - 1]		
			
	
	#######################
	# 11 years
	#######################
	
		# Concluiu 3a serie do medio 2o ciclo
		CensusData[v037 == 4 & v038 == 3, yearsOfSchooling := 11]
					
		# Cursando admissao/vestibular
		CensusData[v037 == 7 & v038 == 3, yearsOfSchooling := 11]			
	
	#######################	
	# 12-14 years 
	#######################
	
		# Concluiu da 1a a 6a serie do superior (alguns serao recodificados abaixo)		
		CensusData[v037 %in% 2:6 & v038 == 4, yearsOfSchooling := apply(cbind(11 + v037 - 1, 14), 1, min)]
				
	
	#######################
	# Superior completo ou mais- 15 anos + (truncado)
	#######################
	# Cursou superior e informou curso concluido com codigos 70 a 97 na v039 	
	CensusData[v039 %in% 70:97 & v038 == 4, yearsOfSchooling := 15]


        # Ajuste para idade
        CensusData[age <= 4,  yearsOfSchooling := NA]

        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }
		
        gc()

        CensusData
}
