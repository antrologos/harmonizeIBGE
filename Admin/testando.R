rm(list=ls());gc();Sys.sleep(.5);gc()
options(scipen=999)
library(tidyverse)
library(stringr)
library(Hmisc)
library(data.table)
library(harmonizeIBGE)
library(fst)
#install.packages("fst")

setwd("E:/Dropbox-Ro/Dropbox/Rogerio/Bancos_Dados/Censos")

variaveis <- fread("E:/Google Drive/RCodes/PacotesR/harmonizeIBGE/Admin/variaveis_CENSOS.csv")

getVarNames <- function(y){
        x <- variaveis %>% 
                filter(year == y) %>%
                select(-file_household, -file_person, - year) %>%
                as.matrix() %>%
                str_split(pattern = ";") %>%
                unlist() %>%
                .[nchar(.) > 1] %>%
                .[!duplicated(.)]
        
        c(x, tolower(x), toupper(x)) %>%
                .[!duplicated(.)]
}

#i= 6

#n = 2000000
n = 30000000

for(i in 1:6){
        
        ano = variaveis$year[i]
        print(paste("===================================================================================", ano))
        
        if(ano != 2000){
                censo <- fread(paste0("Censo ", ano, "/", variaveis$file_person[i]), 
                               select = getVarNames(ano),
                               nrows = n) %>%
                        prepare_to_harmonize(type = "census", year = ano, state_var_name = ifelse(ano == 1970, "CEM005", ""))
                Sys.sleep(.5);gc()
        }else{
                
                c00_pes <- fread(paste0("Censo ", ano, "/", variaveis$file_person[i]), 
                                 select = getVarNames(ano),
                                 nrows = n) 
                c00_dom <- fread(paste0("Censo ", ano, "/", variaveis$file_household[i]), 
                                 select = getVarNames(ano),
                                 nrows = n) 
                repeated_vars <- tolower(names(c00_dom)) %in% tolower(names(c00_pes))
                repeated_vars[which(names(c00_dom) == "V0300")] = F
                c00_dom <- c00_dom %>% select(names(c00_dom)[!repeated_vars])
                censo <- data.table:::merge.data.table(x = c00_pes,
                                                     y = c00_dom,
                                                     by = "V0300",
                                                     all.x = TRUE, 
                                                     all.y = FALSE, 
                                                     sort = FALSE) %>%
                        prepare_to_harmonize(type = "census", year = 2000)
                rm(c00_pes, c00_dom);gc();Sys.sleep(.5);gc()
        }
        
        censo <- censo %>%
                        harmonize_themes(themes = c("identification", "demographics",
                                                    "household", "geography", "migration"), 
                                         dropOriginalVariables = T) 
        gc();Sys.sleep(.5);gc()
        
        censo <- censo %>%
                harmonize_themes(themes = c("education", "work","income"), 
                                 dropOriginalVariables = T) 
        
        
        write_fst(censo, path = paste0("e:/censos_tmp/censo_harmonizado_", ano, ".fst"))
        
        rm(censo)
        gc();Sys.sleep(.5);gc()
}

#===============================================================================================
rm(list=ls());gc();Sys.sleep(.5);gc()
anos = c(1960, 1970, 1980, 1991, 2000, 2010)

for(ano in anos){
        
        print(ano)
        
        assign(paste0("c_",ano), 
               value = read_fst(path = paste0("e:/censos_tmp/censo_harmonizado_", ano, ".fst"),
                                as.data.table = T
                                ))
}


summary(c_1960)
summary(c_1970)
summary(c_1980)
summary(c_1991)
summary(c_2000)
summary(c_2010)

