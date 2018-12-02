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
themes_to_open <- c("identification", "demographics", "work")
n = 30000000

isco4dig_labels <- fread("E:/Google Drive/RCodes/PacotesR/harmonizeIBGE/inst/extdata/IscoLabels.csv") %>%
        rename(isco88 = isco88_4digit) %>%
        select(isco88, isco88_4digit_label) %>%
        setDT(key = "isco88")

isco3dig_labels <- fread("E:/Google Drive/RCodes/PacotesR/harmonizeIBGE/inst/extdata/IscoLabels.csv") %>%
        select(isco88_3digit, isco88_3digit_label) %>%
        filter(!duplicated(.)) %>%
        setDT(key = "isco88_3digit")



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
                               filter(age >= 10) %>%
                               select(-vars_to_drop, -idhh, -idperson, -famStatus, -nonrelative))
                
                gc();Sys.sleep(.5);gc()
                
                assign(x     = paste0("c_",ano), 
                       value = get(paste0("c_",ano)) %>%
                               harmonize_themes(themes = "work") %>%
                               filter(age >= 10, occupationalStatus == 1) %>%
                               setDT())
                gc();Sys.sleep(.5);gc()
        }
        
        setwd("e:/censos_tmp")
        
        for(ano in anos){
                print(ano)
                write_fst(x = get(paste0("c_",ano)), path = paste0("censo_",ano,"_isco88.csv"))
                gc()
        }        
        
}else{
        setwd("e:/censos_tmp")
        for(ano in anos){
                print(ano)
                assign(x = paste0("c_",ano), 
                       value = read_fst(path = paste0("censo_",ano,"_isco88.csv"),as.data.table = T) %>%
                               prepare_to_harmonize(type = "census", year = ano, state_var_name = "CEM005")
                       
                )
                gc();Sys.sleep(.2);gc()
        }        
}



#======================================================================================================

ano = 1960
freq_isco88 = NULL
for(ano in anos){
        print(ano)
        assign(x = paste0("c_",ano), 
               value = get(paste0("c_",ano)) %>%
                       build_work_isco88()
        )
        gc();Sys.sleep(.3);gc()
        
        table = get(paste0("c_",ano))[!is.na(isco88), freq(isco88, w = wgtperson, plot = F)]        
        
        freq_isco88_i    = tibble(ano = ano,
                                        isco88   = attr(table, "dimnames")[[1]],
                                        freq_abs = round(table[,1], digits = 0),
                                        freq_rel = round(table[,2], digits = 4)) %>%
                filter(complete.cases(.))
        
        freq_isco88 <- bind_rows(freq_isco88, freq_isco88_i)
        gc();Sys.sleep(.1);gc()
}

freq_abs_isco88_wide <- freq_isco88 %>%
        select(-freq_rel) %>%
        filter(!(isco88=="Total")) %>%
        mutate(isco88 = as.numeric(isco88)) %>%
        spread(key = ano, value = freq_abs) %>%
        setDT(key = "isco88") %>%
        merge(y = isco_labels) %>%
        select(isco88, isco88_4digit_label, everything())

freq_abs_isco88_3dig_wide <- freq_abs_isco88_wide %>%
        mutate(isco88_3digit = trunc(isco88/10)) %>%
        select(isco88_3digit, `1960`:`2010`) %>%
        group_by(isco88_3digit) %>%
        summarise_all(sum, na.rm=T) %>%
        
        setDT(key = "isco88_3digit") %>%
        merge(y = isco3dig_labels) %>%
        select(isco88_3digit, isco88_3digit_label, everything())


freq_rel_isco88_wide <- freq_isco88 %>%
        select(-freq_abs) %>%
        filter(!(isco88=="Total")) %>%
        mutate(isco88 = as.numeric(isco88)) %>%
        spread(key = ano, value = freq_rel) %>%
        setDT(key = "isco88") %>%
        merge(y = isco_labels) %>%
        select(isco88, isco88_4digit_label, everything())


freq_rel_isco88_3dig_wide <- freq_rel_isco88_wide %>%
        mutate(isco88_3digit = trunc(isco88/10)) %>%
        select(isco88_3digit, `1960`:`2010`) %>%
        group_by(isco88_3digit) %>%
        summarise_all(sum, na.rm=T) %>%
        setDT(key = "isco88_3digit") %>%
        merge(y = isco3dig_labels) %>%
        select(isco88_3digit, isco88_3digit_label, everything())



freq_rel_isco88_long <- freq_rel_isco88_wide %>%
        gather(key = year, value = freq, `1960`:`2010`) %>%
        mutate(isco88_3dig = trunc(isco88/10))



isco_i = unique(freq_rel_isco88_long$isco88_3dig)[3]

for(isco_i in unique(freq_rel_isco88_long$isco88_3dig)){
        print(isco_i)
        
        data_isco_i <- freq_rel_isco88_long %>%
                filter(isco88_3dig == isco_i) %>%
                mutate(isco_label = paste(isco88, isco88_4digit_label, sep = " - "))
        
        nrows <- length(unique(data_isco_i$isco_label))
        
        
        plot_i <- data_isco_i %>%
                ggplot(aes(x = as.numeric(year), y = as.numeric(freq), color = as.factor(isco_label))) +
                geom_line(lwd = 2) + 
                theme(legend.position="bottom",
                      legend.title=element_blank()) + 
                guides(color=guide_legend(nrow=nrows,byrow=TRUE)) +
                ggtitle(paste("ISCO - 3 digit =",isco_i))
        
        print(plot_i)
        Sys.sleep(.6)
}




