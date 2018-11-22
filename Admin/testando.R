rm(list=ls());gc()
options(scipen=999)
library(tidyverse)
library(stringr)
library(Hmisc)
library(data.table)
library(harmonizeIBGE)

setwd("E:/Dropbox-Ro/Dropbox/Rogerio/Bancos_Dados/Censos/Censo 1960")

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



c60 <- fread("Censo.1960.brasil.pessoas.amostra.1.27porcento.csv", 
             select = getVarNames(1960),
             nrows = 30000000) %>%
        prepare_to_harmonize(type = "census", year = 1960)


CensusData <- harmonize_themes(c60,  themes = "identification")
CensusData <- harmonize_themes(CensusData, themes = "household")
CensusData <- harmonize_themes(CensusData, themes = "demographics")
CensusData <- harmonize_themes(CensusData, themes = "geography") 
CensusData <- harmonize_themes(CensusData, themes = "migration") 
CensusData <- harmonize_themes(CensusData, themes = "education")
CensusData <- harmonize_themes(CensusData, themes = "work")
CensusData <- harmonize_themes(CensusData, themes = "income")



