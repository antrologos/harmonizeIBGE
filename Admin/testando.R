rm(list=ls());gc()
options(scipen=999)
library(tidyverse)
library(stringr)
library(Hmisc)
library(data.table)
library(harmonizeIBGE)

setwd("E:/Dropbox-Ro/Dropbox/Rogerio/Bancos_Dados/Censos/Censo 2010")

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


#c60 <- fread("Censo.1960.brasil.pessoas.amostra.1.27porcento.csv", 
#             select = getVarNames(1960),
#             nrows = 30000000) %>%
#        prepare_to_harmonize(type = "census", year = 1960)

#c70 <- fread("Censo.1970.brasil.pessoas.amostra.25porcento.csv", 
#             select = getVarNames(1970),
#             nrows = 100000) %>%
#        prepare_to_harmonize(type = "census", year = 1970, state_var_name = "CEM005")

#c80 <- fread("Censo.1980.brasil.pessoas.amostra.25porcento.csv", 
#             select = getVarNames(1980),
#             nrows = 100000) %>%
#        prepare_to_harmonize(type = "census", year = 1980)


#c91 <- fread("Censo.1991.brasil.pessoas.amostra.10porcento.csv", 
#             select = getVarNames(1991),
#             nrows = 100000) %>%
#        prepare_to_harmonize(type = "census", year = 1991)


#c00_pes <- fread("Censo.2000.brasil.pessoas.amostra.10porcento.csv", 
#             select = getVarNames(2000),
#             nrows = 100000) 
#c00_dom <- fread("Censo.2000.brasil.domicilios.amostra.10porcento.csv", 
#             select = getVarNames(2000),
#             nrows = 100000)
#repeated_vars <- tolower(names(c00_dom)) %in% tolower(names(c00_pes))
#repeated_vars[which(names(c00_dom) == "V0300")] = F
#c00_dom <- c00_dom %>% select(names(c00_dom)[!repeated_vars])
#c00 <- data.table:::merge.data.table(x = c00_pes,
#                                     y = c00_dom,
#                                     by = "V0300",
#                                     all.x = TRUE, 
#                                     all.y = FALSE, 
#                                     sort = FALSE) %>%
#        prepare_to_harmonize(type = "census", year = 2000)
#rm(c00_pes, c00_dom);gc()


c10 <- fread("Censo.2010.brasil.pessoas.amostra.10porcento.csv", 
             select = getVarNames(2010),
             nrows = 100000) %>%
        prepare_to_harmonize(type = "census", year = 2010)

CensusData <- harmonize_themes(c10,  themes = "identification")
CensusData <- harmonize_themes(CensusData, themes = "household")
CensusData <- harmonize_themes(CensusData, themes = "demographics")
CensusData <- harmonize_themes(CensusData, themes = "geography") 
CensusData <- harmonize_themes(CensusData, themes = "migration") 
CensusData <- harmonize_themes(CensusData, themes = "education")
CensusData <- harmonize_themes(CensusData, themes = "work")
CensusData <- harmonize_themes(CensusData, themes = "income")



