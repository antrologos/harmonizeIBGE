#' Converts IBGE sector codes to the ISIC 3.1 standard - ILO-UN
#' @param data.frame
#' @value data.frame
#' @export


#library(data.table)
#setwd(dir.pnads)
#p15 <- fread("pnad 2015/pnad2015_pes_dom.csv")

#setores = p15$V9907
#tipo = "pnad"
#ano = 2015
#endereco_matriz = "E:\\Dropbox-Ro\\Dropbox\\Artigos\\1 - Em faccao\\000 - Padroniza??o de Ocupa??es e Setores\\Harmonizacoes"

setoresToISIC <- function(setores,  # nome da variavel que contem a classificacao das atividades economicas
                          tipo,     # character: "censo", "pnad" ou "pnadc"
                          ano= NULL,
                          endereco_matriz = ""){ # numeric: nao eh preciso especificar ano para a pnadc {

        teste_excel <- require(readxl)
        teste_dplyr <- require(dplyr)

        if(!teste_excel)
                stop("Erro: o pacote readxl nao esta instalado")

        if(!teste_dplyr)
                stop("Erro: o pacote dplyr nao esta instalado")


        if(!dir.exists(endereco_matriz))
                stop("a pasta indicada nao existe")

        endereco_matriz = paste0(endereco_matriz,"/crosswalk_surveys_isic3.xlsx")

        if(!file.exists(endereco_matriz))
                stop("a pasta indicada nao contem o arquivo com a matriz de conversao dos setores")


        banco_tmp <- data.frame(sector_code = setores)

        setores_conversao = read_excel(path = endereco_matriz,
                                       sheet = 1) %>% as.data.table()


        if((tipo == "censo" & ano == 2010)| (tipo == "pnadc")){
                message("Sistema de Setores de atividade: CNAE-Dom 2010")
                setores_conversao = setores_conversao[year == 2010]

        }


        if((tipo == "censo" & ano == 2000) | (tipo == "pnad" & ano >= 2002 & ano <= 2015)){
                message("Sistema de Setores de atividade: CNAE-Dom 2000")
                setores_conversao = setores_conversao[year == 2000]
        }


        if((tipo == "censo" & ano == 1991) | (tipo == "pnad" & (ano >= 1992 & ano <= 2001))){
                message("Sistema de Setores de atividade: IBGE-91")
                setores_conversao = setores_conversao[year == 1991]
        }


        if((tipo == "censo" & ano == 1980) | (tipo == "pnad" & (ano >= 1981 & ano <= 1990))){
                message("Sistema de Setores de atividade: IBGE-80")
                setores_conversao = setores_conversao[year == 1980]
        }


        #if(tipo == "pnad" & ano >= 1976 & ano <= 1979 ){
        #        message("Sistema de Setores de atividade: IBGE-76")
        #        setores_conversao = setores_conversao[year == 1980]
        #}

        if( (tipo == "censo" & ano == 1970) | (tipo == "pnad" & ano == 1973 ) ){
                message("Sistema de Setores de atividade: IBGE-70")
                setores_conversao = setores_conversao[year == 1970]
        }


        if(tipo == "censo" & ano == 1960){
                message("Sistema de Setores de atividade: IBGE-60")
                setores_conversao = setores_conversao[year == 1960]
        }

        if(length(ls(pattern = "setores_conversao")) == 0)
                stop("os parametros 'tipo' ou 'ano' contem valores invalidos")


        banco_tmp <- left_join(banco_tmp, setores_conversao, by = "sector_code", all.x = T)
        banco_tmp$isic3_major[banco_tmp$isic3_major == 0] <- NA


        #0 'NIU - not in universe'
        #10 'Agriculture, fishing, and forestry'
        #20 'Mining'
        #30 'Manufacturing'
        #40 'Electricity, gas and water'
        #50 'Construction'
        #60 'Wholesale and retail trade'
        #70 'Hotels and restaurants'
        #80 'Transportation and communications'
        #90 'Financial services and insurance'
        #100 'Public administration and defense'
        #111 'Real estate and business services'
        #112 'Education'
        #113 'Health and social work'
        #114 'Other services'
        #120 'Private household services'
        #999 'Unknown' .


        banco_tmp$isic3_major
}

