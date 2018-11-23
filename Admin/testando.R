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

i= 3

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
                harmonize_themes(themes = "all") %>%
                drop_originalVariables()
        Sys.sleep(.5);gc()
        
        write_fst(censo, path = paste0("e:/censos_tmp/censo_harmonizado_", ano, ".fst"))
        
        rm(censo)
        gc();Sys.sleep(.5);gc()
}        




censo <- build_demographics_age_1970(censo)
censo <- build_demographics_famStatus_1970(censo)
censo <- build_demographics_male_1970(censo)
censo <- build_demographics_nonrelative_1970(censo)
censo <- build_demographics_residenceStatus_1970(censo)
censo <- build_education_attainment_1970(censo)
censo <- build_education_levelattnd_1970(censo)
censo <- build_education_literacy_1970(censo)
censo <- build_education_schoolattnd_1970(censo)
censo <- build_geography_minCompAreas1970to2010(censo)

