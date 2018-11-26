#' Creates a synthetic variable for position in occupation
#' @param data.frame
#' @value data.frame
#' @export

#CensusData <- censo

build_work_classWorker <- function(CensusData){
        
        CensusData  <- harmonizeIBGE:::check_prepared_to_harmonize(CensusData)
        
        metadata    <- harmonizeIBGE:::get_metadata(CensusData)
        year        <- metadata$year
        
        if(year == 1960){
                
                setnames(CensusData, old = c("v223b", "v224"), new = c("var_setor", "var_posOcup"))
                
                CensusData[ ,setor_primario := rep(0, length(var_posOcup))]
                CensusData[ var_setor %in% c(111:219,291,292), setor_primario := 1]
                CensusData[ is.na(var_setor),                  setor_primario := NA]
                
                CensusData[ var_posOcup==1, classWorker := as.numeric(NA)]
                
                CensusData[ var_posOcup %in% 5:6, classWorker := 1]
                CensusData[ var_posOcup==9, classWorker := 2]
                CensusData[ var_posOcup==7 &  setor_primario == 0, classWorker := 3]
                CensusData[ var_posOcup==7 &  setor_primario == 1, classWorker := 4]
                CensusData[ var_posOcup==8, classWorker := 4]
                CensusData[ var_posOcup==0 & setor_primario == 0, classWorker := 5]
                CensusData[ var_posOcup==0 & setor_primario == 1, classWorker := 6]
                
                message("Esta variavel de posicao na ocupacao refere-se ao trabalho 'habitual' do indivíduo")
                setnames(CensusData, new = c("v223b", "v224"), old = c("var_setor", "var_posOcup"))
                CensusData[, setor_primario := NULL]
        }
        
        if(year == 1970){
                
                setnames(CensusData, old = c("v045", "v046"), new = c("var_setor", "var_posOcup"))
                
                CensusData[ ,setor_primario := rep(0, length(var_posOcup))]
                CensusData[var_setor %in% 111:223 ,setor_primario := 1]
                CensusData[is.na(var_setor) ,setor_primario := NA]
                
                
                CensusData[var_posOcup == 0 ,classWorker := as.numeric(NA)]
                CensusData[var_posOcup %in% 1:2 ,classWorker := 1]
                CensusData[var_posOcup == 5 ,classWorker := 2]
                CensusData[var_posOcup == 3 & setor_primario == 0 ,classWorker := 3]
                CensusData[var_posOcup == 3 & setor_primario == 1 ,classWorker := 4]
                CensusData[var_posOcup == 4 ,classWorker := 4]
                CensusData[var_posOcup == 6 & setor_primario == 0 ,classWorker := 5]
                CensusData[var_posOcup == 6 & setor_primario == 1 ,classWorker := 6]
                
                message("Esta variavel de posicao na ocupacao refere-se ao trabalho 'habitual' do indivíduo")
                setnames(CensusData, new = c("v045", "v046"), old = c("var_setor", "var_posOcup"))
                CensusData[, setor_primario := NULL]
        }
        
        if(year == 1980){
                
                setnames(CensusData, old = c("v532"), new = c("var_setor"))
                CensusData[ ,var_posOcup := ifelse(!is.na(v545) & v545 != 0, v545, v533)]
                
                CensusData[var_posOcup == 9 , classWorker := as.numeric(NA)]
                
                CensusData[var_posOcup %in% c(1:2,6) ,classWorker := 1]
                CensusData[var_posOcup %in% c(4,7)  ,classWorker := 2]
                CensusData[var_posOcup == 8 & !(var_setor %in% 11:42) ,classWorker := 3]
                CensusData[var_posOcup == 8 & (var_setor %in% 11:42),classWorker := 4]
                CensusData[var_posOcup %in% c(3,5) ,classWorker := 4]
                CensusData[var_posOcup == 0 & !(var_setor %in% 11:42) ,classWorker := 5]
                CensusData[var_posOcup == 0 & (var_setor %in% 11:42) ,classWorker := 6]
                
                CensusData[ ,var_posOcup := NULL]
                gc(); Sys.sleep(.5);gc()
                
                setnames(CensusData, new = c("v532"), old = c("var_setor"))
        }
        
        if(year == 1991){
                
                setnames(CensusData, old = c("v0347", "v0349"), new = c("var_setor", "var_posOcup"))
      
                CensusData[ ,setor_primario := rep(0, length(var_posOcup))]
                CensusData[var_setor %in% 11:42 ,setor_primario := 1]
                CensusData[is.na(var_setor) ,setor_primario := NA]
                
                
                CensusData[var_posOcup %in% c(1,4,6:8) ,classWorker := 1]
                CensusData[var_posOcup == 10 ,classWorker := 2]
                CensusData[var_posOcup == 5 ,classWorker := 3]
                CensusData[var_posOcup == 9 & setor_primario == 0 ,classWorker := 3]
                CensusData[var_posOcup == 9 & setor_primario == 1 ,classWorker := 4]
                CensusData[var_posOcup %in% c(2,3) ,classWorker := 4]
                CensusData[var_posOcup == 11 & setor_primario == 0 ,classWorker := 5]
                CensusData[var_posOcup == 11 & setor_primario == 1 ,classWorker := 6]
                
                setnames(CensusData, new = c("v0347", "v0349"), old = c("var_setor", "var_posOcup"))
                CensusData[, setor_primario := NULL]
        }
        
        if(year == 2000){
                
                setnames(CensusData, old = c("v4462", "v0447"), new = c("var_setor", "var_posOcup"))
                
                CensusData[ ,setor_primario := rep(0, length(var_posOcup))]
                CensusData[var_setor %in% 1101:5002 ,setor_primario := 1]
                CensusData[is.na(var_setor) ,setor_primario := NA]
                
                CensusData[var_posOcup %in% 1:4 ,classWorker := 1]
                CensusData[var_posOcup == 5 ,classWorker := 2]
                CensusData[var_posOcup == 6 & setor_primario == 0 ,classWorker := 3]
                CensusData[var_posOcup == 6 & setor_primario == 1 ,classWorker := 4]
                CensusData[var_posOcup %in% c(7,8) & setor_primario == 0 ,classWorker := 5]
                CensusData[var_posOcup %in% c(7,8) & setor_primario == 1 ,classWorker := 6]
                CensusData[var_posOcup == 9 ,classWorker := 6]
                
                setnames(CensusData, new = c("v4462", "v0447"),  old = c("var_setor", "var_posOcup"))
                CensusData[, setor_primario := NULL]
                
        }
        
        if(year == 2010){
                
                setnames(CensusData, old = c("v6471", "v6930"), new = c("var_setor", "var_posOcup"))

                CensusData[ ,setor_primario := rep(0, length(var_posOcup))]
                CensusData[var_setor %in% 1101:3002 ,setor_primario := 1]
                CensusData[is.na(var_setor) ,setor_primario := NA]
                
                CensusData[var_posOcup %in% 1:3 ,classWorker := 1]
                CensusData[var_posOcup == 5 ,classWorker := 2]
                CensusData[var_posOcup == 4 & setor_primario == 0 ,classWorker := 3]
                CensusData[var_posOcup == 4 & setor_primario == 1 ,classWorker := 4]
                CensusData[var_posOcup == 6 & setor_primario == 0 ,classWorker := 5]
                CensusData[var_posOcup == 6 & setor_primario == 1 ,classWorker := 6]
                CensusData[var_posOcup == 7 ,classWorker := 6]
                
                setnames(CensusData,  new = c("v6471", "v6930"), old = c("var_setor", "var_posOcup"))
                CensusData[, setor_primario := NULL]
        }
        gc(); Sys.sleep(.5);gc()

        # 1 "employee"
        # 2 "employer"
        # 3 "self-employed, urban"
        # 4 "self-employed/unpaid, rural"
        # 5 "unpaid, urban"
        
        CensusData[classWorker == 6, classWorker := 4]
        
        gc(); Sys.sleep(.5);gc()
        
        return(CensusData)
        
}

