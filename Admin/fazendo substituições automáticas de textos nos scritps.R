library(stringr)


setwd("E:/Google Drive/RCodes/PacotesR/harmonizeIBGE/R")

lista_scripts <- list.files(pattern = "*.R")

#script_i <- lista_scripts[1]
for(script_i in lista_scripts){
        print(script_i)
        script_text <- readLines(script_i)
        
        location <- str_locate_all(script_text, pattern = "V[[:digit:]]{2,}") 
        
        i=12
        matrix_location <- NULL
        for(i in 1:length(location)){
                
                if( nrow(location[[i]]) == 0){
                        next
                }
                
                matrix_location_i <- cbind(linha = i, location[[i]])
                matrix_location = rbind(matrix_location, matrix_location_i)
        }
        
        if(!is.matrix(matrix_location)){
                next
        }
        
        for(j in 1:nrow(matrix_location)){
                str_sub(script_text[matrix_location[j,1]], start = matrix_location[j,2], end = matrix_location[j,2]) <- "v"
        }
        
        writeLines(text = script_text, con = paste0("teste/", script_i))
        
}
