#' Builds a synthetic variable for yearsOfSchooling attainment - 2010
#' @param data.frame
#' @value data.frame
#' @export

build_education_yearsOfSchooling_2010 <- function(CensusData){
        
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
                CensusData <- build_demographics_age_2010(CensusData)
                age_just_created <- TRUE
        }
        gc()
        
        
        ###################
        #= Attending school
        ###################
        
        # Curso 'creche', 'pre-escolar', 'alfabetizacao' ou 'alfabetizacao de jovens e adultos'
        CensusData[v0629 %in% c(1:4), yearsOfSchooling := 0]
        
        # Curso 'Regular do Ensino Fundamental' e series 1-9
        CensusData[v0629==5 & v0630 %in% c(1:2), yearsOfSchooling := 0]	 	# 'primeiro ano' e 'primeira serie'
        CensusData[v0629==5 & v0630 %in% c(3:9), yearsOfSchooling := v0630 - 2]	 	# 3o-9o ano (2a-8a series)
        
        # Curso 'Regular do Ensino Medio' e series 1-4
        CensusData[v0629==7 & v0631 %in% c(1:4), yearsOfSchooling := apply(cbind(8 + v0631 - 1, 10), 1, min)]
        
        # Curso 'especializacao de nivel superior':
        CensusData[v0629==10, yearsOfSchooling := 15]
        
        # Curso 'mestrado/ doutorado'
        CensusData[v0629 %in% c(11,12), yearsOfSchooling := 15]
        
        # Special cases (not residual, guesstimates)
        # Curso 'Ensino Fundamental' e serie nao seriado: same as 1980-2000
        CensusData[v0629==5 & v0630==10 & age<=14, yearsOfSchooling := 0]
        CensusData[v0629==5 & v0630==10 & age>14, yearsOfSchooling := 4]
        
        # Curso 'EJA ou supletivo do fundamental': same as 1980-2000
        CensusData[v0629==6 & age<=14, yearsOfSchooling := 0]
        CensusData[v0629==6 & age>14, yearsOfSchooling := 4]
        
        # Curso 'Ensino Medio' e serie nao seriado: same as 1980-2000
        CensusData[v0629==7 & v0631==5, yearsOfSchooling := 8]
        
        # Curso 'EJA ou supletivo do medio': same as 1980-2000
        CensusData[v0629==8, yearsOfSchooling := 8]
        
        # Curso 'Superior de graduacao': not an issue in other years
        CensusData[v0629==9 & v0632==1, yearsOfSchooling := 15]	#  already hold a higher ed degree
        CensusData[v0629==9 & v0632 !=1 & age %in% c(15:20), yearsOfSchooling := 11 + apply(cbind(age - 18, 0), 1, max)]
        CensusData[v0629==9 & v0632 !=1 & age>20, yearsOfSchooling := 12.5]
        
        
        ################
        #= Out of school
        ################
        
        # Never attended
        CensusData[v0628==4, yearsOfSchooling := 0]
        
        # Cursos 'creche', 'alfabetizacao de jovens e adultos'
        CensusData[v0633 %in% c( 1,2), yearsOfSchooling := 0]
        
        # Curso 'antigo primario' concluido
        CensusData[v0633==3 & v0634==1, yearsOfSchooling := 4]
        
        # Curso 'antigo ginasio' concluido
        CensusData[v0633==4 & v0634==1, yearsOfSchooling := 8]
        
        # Curso 'ensino fundamental 5-8a series' concluido
        CensusData[v0633==7 & v0634==1, yearsOfSchooling := 8]
        
        # Curso 'supletivo 1o grau' concluido
        CensusData[v0633==8 & v0634==1, yearsOfSchooling := 8]
        
        # Curso 'antigo cientifico' concluido
        CensusData[v0633==9 & v0634==1, yearsOfSchooling := 11]
        
        # Curso 'ensino medio' concluido
        CensusData[v0633==10 & v0634==1, yearsOfSchooling := 11]
        
        # Curso 'superior de graduacao' concluido
        CensusData[v0633==11 & v0634==1, yearsOfSchooling := 15]
        
        # Curso 'especializacao de nivel superior' concluido ou nao
        CensusData[v0633==12, yearsOfSchooling := 15]
        
        # Cursos 'mestrado', 'doutorado' 
        CensusData[v0633 %in% c(13,14), yearsOfSchooling := 15]
        
        # Special cases (millions of obs, guesstimates)
        # Cursou 'antigo primario' but did not complete: not an issue in other years, assigns mid-point between 
        # min and max possible values regardless of age
        CensusData[v0633==3 & v0634==2, yearsOfSchooling := 1.5]
        
        # Cursou 'antigo ginasio' but did not complete: not an issue in other years, assigns mid-point between 
        # min and max possible values regardless of age
        CensusData[v0633==4 & v0634==2, yearsOfSchooling := 5.5]
        
        # Cursou 'fundamental 1-3a series': not an issue in other years, assigns mid-point between 
        # min and max possible values except for individuals in specific ages
        CensusData[v0633==5 & age<=10, yearsOfSchooling := 0]
        CensusData[v0633==5 & age>10, yearsOfSchooling := 1.5]
        
        # Cursou 'fundamental 4a serie':  not an issue in other years, assigns mid-point between 
        # min and max possible values regardless of age
        CensusData[v0633==6 , yearsOfSchooling := 3.5]
        
        # Cursou 'fundamental 5-8a series' but did not complete: not an issue in other years, assigns mid-point between 
        # min and max possible values regardless of age
        CensusData[v0633==7 & v0634==2, yearsOfSchooling := 4]
        CensusData[v0633==7 & v0634==2, yearsOfSchooling := 5.5]
        
        # Cursou 'supletivo 1o grau' but did not complete: same as in 1980-2000
        CensusData[v0633==8	& v0634==2 & age<=14, yearsOfSchooling := 0]
        CensusData[v0633==8	& v0634==2 & age>14, yearsOfSchooling := 4]
        
        # Cursou 'antigo cientifico' but did not complete:  not an issue in other years, assigns mid-point between 
        # min and max possible values for children in specific ages
        CensusData[v0633==9 & v0634==2 & age<=18, yearsOfSchooling := 8]
        CensusData[v0633==9 & v0634==2 & age>18, yearsOfSchooling := 9]
        
        # Cursou 'ensino medio' but did not complete:  not an issue in other years, assigns mid-point between 
        # min and max possible values for individuals in specific ages
        CensusData[v0633==10 & v0634==2 & age<=18, yearsOfSchooling := 8]
        CensusData[v0633==10 & v0634==2 & age>18, yearsOfSchooling := 9]
        
        # Cursou 'superior' but did not complete: not an issue in other years, assigns mid-point between 
        # min and max possible values for individuals in specific ages
        CensusData[v0633==11 & v0634==2 & age<=22, yearsOfSchooling := 11]
        CensusData[v0633==11 & v0634==2 & age>22, yearsOfSchooling := 12.5]
        
        
        gc()
        
        ##############################
        # Ajuste para idade
        CensusData[age <= 4,  yearsOfSchooling := NA]
        if(age_just_created == TRUE){
                CensusData[ , age := NULL]
        }
        
        ##############################
        
        CensusData
}

