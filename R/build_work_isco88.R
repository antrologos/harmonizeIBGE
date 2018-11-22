#' @export


build_work_isco88 <- function(CensusData){
        
        CensusData <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        metadata   <- harmonizeIBGE:::get_metadata(CensusData)
        sulfix     <- harmonizeIBGE:::find_sulfixforOccSectors(CensusData)
        
        just_created_vars_list_existedBefore <- exists(x = "just_created_vars", where = .GlobalEnv)
        
        
        basicCrossWalk_location <- system.file("extdata",
                                               "crosswalk_occ_basicCrossWalk.csv",
                                               package = "harmonizeIBGE")
        
        SectoralAjustments_location <- system.file("extdata",
                                                   "crosswalk_occ_SectoralAjustments.csv",
                                                   package = "harmonizeIBGE")
        
        ClassWorkerAdjustments_location <- system.file("extdata",
                                                       "crosswalk_occ_ClassWorkerAdjustments.csv",
                                                       package = "harmonizeIBGE")
        
        FurtherAdjustments_location <- system.file("extdata",
                                                   "crosswalk_occ_FurtherAdjustments.csv",
                                                   package = "harmonizeIBGE")
        
        varList_location <- system.file("extdata",
                                        "varList_occ.csv",
                                        package = "harmonizeIBGE")
        
        
        occ_conversao            <- read.csv2(basicCrossWalk_location, stringsAsFactors = F)
        sectoral_adjustments     <- read.csv2(SectoralAjustments_location, stringsAsFactors = F) 
        class_worker_adjustments <- read.csv2(ClassWorkerAdjustments_location, stringsAsFactors = F) 
        further_adjustments      <- read.csv2(FurtherAdjustments_location, stringsAsFactors = F) 
        varList                  <- read.csv2(varList_location, stringsAsFactors = F)

        
        if(metadata$type != "pnadc"){
                var_sector <- varList %>%
                        filter(data == metadata$type & year == metadata$year) %>%
                        .$var_sector %>% 
                        tolower()
                
                var_ocup <- varList %>%
                        filter(data == metadata$type & year == metadata$year) %>%
                        .$var_ocup %>% 
                        tolower() %>%
                        str_split(pattern = ";") %>%
                        unlist()
        }else{
                var_sector <- varList %>%
                        filter(data == metadata$type) %>%
                        .$var_sector %>% 
                        tolower()
                
                var_ocup <- varList %>%
                        filter(data == metadata$type) %>%
                        .$var_ocup %>% 
                        tolower() 
        }
        
        harmonizeIBGE:::check_necessary_vars(CensusData, c(var_ocup, var_sector)) 
        
        sectorISIC3_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("sectorISIC3"))
        if(length(check_vars) > 0){
                CensusData <- build_work_sectorISIC3(CensusData)
                sectorISIC3_just_created = T
        }
        
        classWorker_just_created = F
        check_vars <- harmonizeIBGE:::check_var_existence(CensusData, c("classWorker"))
        if(length(check_vars) > 0){
                CensusData <- build_work_classWorker(CensusData)
                classWorker_just_created = T
        }
        
        if(metadata$type == "census" & metadata$year == 1980){
                banco_tmp <- CensusData %>% 
                        select(v530, v542, var_sector, "sectorISIC3", "classWorker") %>%
                        mutate(ibge_code = ifelse(!is.na(v542) & v542 != 0, v542, v530)) %>%
                        select(-v530, -v542) %>%
                        select(ibge_code, everything())
        }else{
                banco_tmp <- CensusData %>% 
                        select(var_ocup, var_sector, "sectorISIC3", "classWorker")
        }
        
        setnames(banco_tmp, old = names(banco_tmp), new = c("ibge_code", "sector", "isic", "class_worker"))
        
        type <- metadata$type
        year <- metadata$year
        
        if(type == "census"){
                y = year
        }
        
        if(type == "pnadc"){
                y = 2010
        }
        
        if(type == "pnad" & year >= 2002 & year <= 2015){
                y = 2000
        }
        
        if(type == "pnad" & year >= 1992 & year <= 2001){
                y = 1991
        }
        
        if(type == "pnad" & year >= 1981 & year <= 1990){
                y = 1980
        }
        
        if(type == "pnad" & year >= 1976 & year <= 1979 ){
                y = 1976
        }
        
        if(type == "pnad" & year == 1973){
                y = 1970
        }

        # Selecionando o ano 
        occ_conversao = occ_conversao %>% filter(year == y)
        
        # Importando informacoes sobre o ano especifico
        banco_tmp <- left_join(banco_tmp, 
                               occ_conversao, 
                               by = "ibge_code", 
                               all.x = T)
        
        
        banco_tmp$isco88_4digit[banco_tmp$isco88_4digit == 0]     <- NA
        banco_tmp$isco88_4digit[banco_tmp$isco88_4digit == -9999] <- NA
        
        
        ######################################################
        # Ajustes por setor e posicao na ocupacao
        
        sectoral_adjustments1 <- sectoral_adjustments %>%
                select(year, isic, ibge_code, newISCO_sector) %>%   # por ISIC
                rename(newISCO_sector1 = newISCO_sector) %>%
                filter(year == y & !is.na(isic))
                
        sectoral_adjustments2 <- sectoral_adjustments %>%
                select(year, sector, ibge_code, newISCO_sector) %>% # por setores na classificacao original
                rename(newISCO_sector2 = newISCO_sector) %>%
                filter(year == y & !is.na(sector))
        
        class_worker_adjustments1 <- class_worker_adjustments %>%
                select(year, class_worker, ibge_code, newISCO_classWorker) %>% # gera um ISCO a partir dos c?digos de ocupacao do IBGE
                rename(newISCO_classWorker1 = newISCO_classWorker) %>%
                filter(year == y & !is.na(ibge_code))
        
        class_worker_adjustments2 <- class_worker_adjustments %>%
                select(year, class_worker, oldISCO, newISCO_classWorker) %>% # corrige/gera um novo ISCO, corrigindo a conversao anterior
                rename(newISCO_classWorker2 = newISCO_classWorker) %>%
                filter(year == y & !is.na(oldISCO))
        
        
        if(nrow(sectoral_adjustments1) > 0){
                banco_tmp <- left_join(x = banco_tmp, 
                                       y = sectoral_adjustments1 %>% select(-year), 
                                       by = c("isic", "ibge_code"))
        }
        gc()
        
        if(nrow(sectoral_adjustments2) > 0){
                banco_tmp <- left_join(x = banco_tmp, 
                                       y = sectoral_adjustments2 %>% select(-year), 
                                       by = c("sector", "ibge_code"))
        }
        gc()
        
        if(nrow(class_worker_adjustments1) > 0){
                banco_tmp <- left_join(x = banco_tmp, 
                                       y = class_worker_adjustments1 %>% select(-year), 
                                       by = c("class_worker", "ibge_code"))
        }
        gc()
        
        if(nrow(class_worker_adjustments2) > 0){
                banco_tmp <- left_join(x = banco_tmp, 
                                       y = class_worker_adjustments2 %>% select(-year), 
                                       by = c("class_worker", "isco88_4digit" = "oldISCO"))
        }
        gc()
        
        
        if(is.null(banco_tmp$newISCO_sector1)){
                banco_tmp$newISCO_sector1 = as.numeric(NA)
        }
        
        if(is.null(banco_tmp$newISCO_sector2)){
                banco_tmp$newISCO_sector2 = as.numeric(NA)
        }
        
        if(is.null(banco_tmp$newISCO_classWorker1)){
                banco_tmp$newISCO_classWorker1 = as.numeric(NA)
        }
        
        if(is.null(banco_tmp$newISCO_classWorker2)){
                banco_tmp$newISCO_classWorker2 = as.numeric(NA)
        }
        
        banco_tmp = data.table(banco_tmp)
        gc(); Sys.sleep(1); gc()
        
        banco_tmp[!is.na(newISCO_sector1),      isco88_4digit := newISCO_sector1]
        banco_tmp[!is.na(newISCO_sector2),      isco88_4digit := newISCO_sector2]
        banco_tmp[!is.na(newISCO_classWorker1), isco88_4digit := newISCO_classWorker1]
        banco_tmp[!is.na(newISCO_classWorker2), isco88_4digit := newISCO_classWorker2]
        
        ######################################################
        # Ajustes adicionais
      
        further_adjustments <- further_adjustments %>%
                filter(!is.na(newISCO)) %>%
                rename(newISCO_further = newISCO)
                
        banco_tmp <- left_join(x = banco_tmp, 
                               y = further_adjustments, 
                               by = c("isco88_4digit" = "oldISCO")) %>%
                as.data.table()
        
        banco_tmp[!is.na(newISCO_further), isco88_4digit := newISCO_further]
        
        ######################################################
        
        CensusData[, isco88 := banco_tmp$isco88_4digit]
        gc()
        
        if(sectorISIC3_just_created == T){
                CensusData[, sectorISIC3 := NULL]
        }
        
        if(classWorker_just_created == T){
                CensusData[, classWorker := NULL]
        }
        
        CensusData  <- harmonizeIBGE:::set_metadata(CensusData, metadata = metadata)
        CensusData
}



