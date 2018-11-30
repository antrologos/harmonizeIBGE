rm(list=ls());gc();Sys.sleep(.5);gc()
options(scipen=999)
library(harmonizeIBGE)
library(Hmisc)
library(descr)
library(fst)
#======================================================================================================

setwd("E:/Dropbox-Ro/Dropbox/Rogerio/Bancos_Dados/Censos")
variaveis <- fread("E:/Google Drive/RCodes/PacotesR/harmonizeIBGE/Admin/variaveis_CENSOS.csv")

anos <- c(1960, 1970, 1980, 1991, 2000, 2010)
themes_to_open <- c("identification", "demographics", "education")
n = 30000000

read_harmonize_and_save = F

if(read_harmonize_and_save == T){
        for(i in 1:6){
                
                ano = variaveis$year[i]
                print(paste("===================================================================================", ano))
                
                vars_to_open <- harmonizeIBGE:::list_originalVariables_to_drop(ano, themes = themes_to_open) %>% 
                        unlist() %>%
                        c(., toupper(.), tolower(.)) %>%
                        unique()
                
                vars_to_drop <- harmonizeIBGE:::list_originalVariables_to_drop(ano, themes = c("identification", "demographics")) %>% 
                        unlist() %>%
                        c(tolower(.)) %>%
                        unique()
                
                if(ano == 1970){
                        vars_to_open <- c(vars_to_open, "CEM005")
                        vars_to_drop <- c(vars_to_drop, "CEM005")
                }
                
                assign(x = paste0("c_",ano), 
                       value = fread(paste0("Censo ", ano, "/", variaveis$file_person[i]), 
                                     select = vars_to_open,
                                     nrows = n) %>%
                               prepare_to_harmonize(type = "census", year = ano, state_var_name = ifelse(ano == 1970, "CEM005", ""))
                )
                
                Sys.sleep(.5);gc()
                
                assign(x     = paste0("c_",ano), 
                       value = get(paste0("c_",ano)) %>%
                               harmonize_themes(themes = c("identification", "demographics")) %>%
                               filter(age >= 17) %>%
                               select(-vars_to_drop))
                
                gc();Sys.sleep(.5);gc()
                
                assign(x     = paste0("c_",ano), 
                       value = get(paste0("c_",ano)) %>%
                               harmonize_themes(themes = "education") %>%
                               filter(!is.na(educationAttainment)) %>%
                               setDT())
                gc();Sys.sleep(.5);gc()
        }
        
        setwd("e:/censos_tmp")
        
        for(ano in anos){
                print(ano)
                write_fst(x = get(paste0("c_",ano)), path = paste0("censo_",ano,"_fieldsOfStudy.csv"))
                gc()
        }        
        
}else{
        setwd("e:/censos_tmp")
        for(ano in anos){
                print(ano)
                assign(x = paste0("c_",ano), 
                       value = read_fst(path = paste0("censo_",ano,"_fieldsOfStudy.csv"),as.data.table = T) %>%
                               prepare_to_harmonize(type = "census", year = ano, state_var_name = "CEM005")
                       )
                gc()
        }        
}

c_1991[, wgtperson := wgtperson/(10^8)]

#======================================================================================================

labels_isced <- readxl::read_xlsx(crosswalk_location, sheet = "Fields_Codes_labels") %>%
        select(isced_code_level3, isced_label_level3_en) %>%
        rename(isced = isced_code_level3,
               label = isced_label_level3_en) %>%
        setDT(key = "isced")


ano = 1960
freq_isced_aggreg = NULL
for(ano in anos){
        print(ano)
        assign(x = paste0("c_",ano), 
               value = get(paste0("c_",ano)) %>%
                       build_education_fieldsOfStudy(aggregated = T)
        )
        gc();Sys.sleep(.3);gc()

        table = get(paste0("c_",ano))[, freq(label_fieldsOfStudy, w = wgtperson)]        
        
        freq_isced_aggreg_i    = tibble(ano = ano,
                                 isced    = attr(table, "dimnames")[[1]],
                                 freq_abs = round(table[,1],digits = 0),
                                 freq_rel = round(table[,3], digits =3)) %>%
                filter(complete.cases(.))
        
        freq_isced_aggreg <- bind_rows(freq_isced_aggreg, freq_isced_aggreg_i)
        gc();Sys.sleep(.1);gc()
}
   
freq_isced = NULL
for(ano in c(1980,1991,2000,2010)){
        print(ano)
        assign(x = paste0("c_",ano), 
               value = get(paste0("c_",ano)) %>%
                       build_education_fieldsOfStudy(aggregated = F)
        )
        gc();Sys.sleep(.3);gc()
        
        table = get(paste0("c_",ano))[, freq(label_fieldsOfStudy, w = wgtperson)]        
        
        freq_isced_i    = tibble(ano = ano,
                                        isced    = attr(table, "dimnames")[[1]],
                                        freq_abs = round(table[,1],digits = 0),
                                        freq_rel = round(table[,3], digits =3)) %>%
                filter(complete.cases(.))
        
        freq_isced <- bind_rows(freq_isced, freq_isced_i)
        gc();Sys.sleep(.1);gc()
}




freq_abs_isced_wide_aggreg <- freq_isced_aggreg %>%
        select(-freq_rel) %>%
        filter(!(isced=="Total")) %>%
        spread(key = ano, value = freq_abs) %>%
        setDT() 

freq_rel_isced_wide_aggreg  <- freq_isced_aggreg %>%
        select(-freq_abs) %>%
        mutate(freq_rel = round(freq_rel, 3)) %>%
        filter(!(isced=="Total")) %>%
        spread(key = ano, value = freq_rel) %>%
        setDT() 


freq_abs_isced_wide <- freq_isced %>%
        select(-freq_rel) %>%
        filter(!(isced=="Total")) %>%
        spread(key = ano, value = freq_abs) %>%
        setDT() 

freq_rel_isced_wide <- freq_isced %>%
        select(-freq_abs) %>%
        filter(!(isced=="Total")) %>%
        mutate(freq_rel = round(freq_rel, 3)) %>%
        spread(key = ano, value = freq_rel) %>%
        setDT() 



