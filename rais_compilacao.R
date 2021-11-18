#   rm(list=ls())
library(tidyverse)
library(lubridate)
options(editor = 'notepad')

#____________________________________________________________________________


# CARREGAMENTO RAIS 2010 a 2017 ---------------------------------------------
UF <- 
  c("AC","AL","AM","AP","BA","CE","DF","ES","GO","MA","MG","MS","MT","PA",
    "PB","PE","PI","PR","RJ","RN","RO","RR","RS","SC","SE","SP","TO")

ANOS <- 2010:2017
# ---------------------------------------------------------------------------

arquivos <- list()
for (i in ANOS) {
  arquivos[[i]] <-
    paste(UF, i, ".Rda", sep = "")
}

# Ciclo de importação 
for (ano in ANOS) {
  for (uf in 1:length(UF)) {
  load(paste(
    "./CSV_Data/RAIS_CAGED/mcdRAIS/", arquivos[[ano]][uf], sep = ""))
  }}
    


#_____________________________________________________________________

RAIS_2010_VINC <- 
  rbind(
  AC2010,AL2010,AM2010,AP2010,BA2010,CE2010,DF2010,ES2010,GO2010,MA2010,MG2010,MS2010,MT2010,PA2010,PB2010,PE2010,PI2010,PR2010,RJ2010,RN2010,RO2010,RR2010,RS2010,SC2010,SE2010,SP2010,TO2010)
RAIS_2010_VINC$ano <- 2010

RAIS_2011_VINC <- 
  rbind(
  AC2011,AL2011,AM2011,AP2011,BA2011,CE2011,DF2011,ES2011,GO2011,MA2011,MG2011,MS2011,MT2011,PA2011,PB2011,PE2011,PI2011,PR2011,RJ2011,RN2011,RO2011,RR2011,RS2011,SC2011,SE2011,SP2011,TO2011)
RAIS_2011_VINC$ano <- 2011

RAIS_2012_VINC <- 
  rbind(
  AC2012,AL2012,AM2012,AP2012,BA2012,CE2012,DF2012,ES2012,GO2012,MA2012,MG2012,MS2012,MT2012,PA2012,PB2012,PE2012,PI2012,PR2012,RJ2012,RN2012,RO2012,RR2012,RS2012,SC2012,SE2012,SP2012,TO2012)
RAIS_2012_VINC$ano <- 2012

RAIS_2013_VINC <- 
  rbind(
  AC2013,AL2013,AM2013,AP2013,BA2013,CE2013,DF2013,ES2013,GO2013,MA2013,MG2013,MS2013,MT2013,PA2013,PB2013,PE2013,PI2013,PR2013,RJ2013,RN2013,RO2013,RR2013,RS2013,SC2013,SE2013,SP2013,TO2013)
RAIS_2013_VINC$ano <- 2013

RAIS_2014_VINC <- 
  rbind(
  AC2014,AL2014,AM2014,AP2014,BA2014,CE2014,DF2014,ES2014,GO2014,MA2014,MG2014,MS2014,MT2014,PA2014,PB2014,PE2014,PI2014,PR2014,RJ2014,RN2014,RO2014,RR2014,RS2014,SC2014,SE2014,SP2014,TO2014)
RAIS_2014_VINC$ano <- 2014

RAIS_2015_VINC <- 
  rbind(
  AC2015,AL2015,AM2015,AP2015,BA2015,CE2015,DF2015,ES2015,GO2015,MA2015,MG2015,MS2015,MT2015,PA2015,PB2015,PE2015,PI2015,PR2015,RJ2015,RN2015,RO2015,RR2015,RS2015,SC2015,SE2015,SP2015,TO2015)
RAIS_2015_VINC$ano <- 2015

RAIS_2016_VINC <- 
  rbind(
  AC2016,AL2016,AM2016,AP2016,BA2016,CE2016,DF2016,ES2016,GO2016,MA2016,MG2016,MS2016,MT2016,PA2016,PB2016,PE2016,PI2016,PR2016,RJ2016,RN2016,RO2016,RR2016,RS2016,SC2016,SE2016,SP2016,TO2016)
RAIS_2016_VINC$ano <- 2016

RAIS_2017_VINC <- 
  rbind(
  AC2017,AL2017,AM2017,AP2017,BA2017,CE2017,DF2017,ES2017,GO2017,MA2017,MG2017,MS2017,MT2017,PA2017,PB2017,PE2017,PI2017,PR2017,RJ2017,RN2017,RO2017,RR2017,RS2017,SC2017,SE2017,SP2017,TO2017)
RAIS_2017_VINC$ano <- 2017  

rm(list = ls(pattern = "[A-Z]{2}20[0-9]{2}"))


# CARREGAMENTO RAIS 2018 a 2019 ---------------------------------------------

# 2018 ----
    RAIS_VINC_PUB_CENTRO_OESTE_2018 <- readRDS("./CSV_Data/RAIS_CAGED/mcdRAIS/RAIS_VINC_PUB_CENTRO_OESTE_2018.Rda")
    RAIS_VINC_PUB_MG_ES_RJ_2018 <- readRDS("./CSV_Data/RAIS_CAGED/mcdRAIS/RAIS_VINC_PUB_MG_ES_RJ_2018.Rda")
    RAIS_VINC_PUB_NORDESTE_2018 <- readRDS("./CSV_Data/RAIS_CAGED/mcdRAIS/RAIS_VINC_PUB_NORDESTE_2018.Rda")
    RAIS_VINC_PUB_NORTE_2018 <- readRDS("./CSV_Data/RAIS_CAGED/mcdRAIS/RAIS_VINC_PUB_NORTE_2018.Rda")
    RAIS_VINC_PUB_SP_2018 <- readRDS("./CSV_Data/RAIS_CAGED/mcdRAIS/RAIS_VINC_PUB_SP_2018.Rda")
    RAIS_VINC_PUB_SUL_2018 <- readRDS("./CSV_Data/RAIS_CAGED/mcdRAIS/RAIS_VINC_PUB_SUL_2018.Rda")
  
    
    RAIS_2018_VINC <- 
      rbind(RAIS_VINC_PUB_CENTRO_OESTE_2018,RAIS_VINC_PUB_MG_ES_RJ_2018,RAIS_VINC_PUB_NORDESTE_2018,RAIS_VINC_PUB_NORTE_2018,RAIS_VINC_PUB_SP_2018,RAIS_VINC_PUB_SUL_2018)
    RAIS_2018_VINC$ano <- 2018
    
    colnames(RAIS_2018_VINC) <- colnames(RAIS_2010_VINC)

# 2019 ----    
    RAIS_VINC_PUB_CENTRO_OESTE_2019 <- readRDS("./CSV_Data/RAIS_CAGED/mcdRAIS/RAIS_VINC_PUB_CENTRO_OESTE_2019.Rda")
    RAIS_VINC_PUB_MG_ES_RJ_2019 <- readRDS("./CSV_Data/RAIS_CAGED/mcdRAIS/RAIS_VINC_PUB_MG_ES_RJ_2019.Rda")    
    RAIS_VINC_PUB_NORDESTE_2019 <- readRDS("./CSV_Data/RAIS_CAGED/mcdRAIS/RAIS_VINC_PUB_NORDESTE_2019.Rda")
    RAIS_VINC_PUB_NORTE_2019 <- readRDS("./CSV_Data/RAIS_CAGED/mcdRAIS/RAIS_VINC_PUB_NORTE_2019.Rda")    
    RAIS_VINC_PUB_SP_2019 <- readRDS("./CSV_Data/RAIS_CAGED/mcdRAIS/RAIS_VINC_PUB_SP_2019.Rda")    
    RAIS_VINC_PUB_SUL_2019 <- readRDS("./CSV_Data/RAIS_CAGED/mcdRAIS/RAIS_VINC_PUB_SUL_2019.Rda")

    RAIS_2019_VINC <- 
      rbind(RAIS_VINC_PUB_CENTRO_OESTE_2019,RAIS_VINC_PUB_MG_ES_RJ_2019,RAIS_VINC_PUB_NORDESTE_2019,RAIS_VINC_PUB_NORTE_2019,RAIS_VINC_PUB_SP_2019,RAIS_VINC_PUB_SUL_2019)
    RAIS_2019_VINC$ano <- 2019
    
    colnames(RAIS_2019_VINC) <- colnames(RAIS_2010_VINC)

    RAIS_2010_2019_VINC <-
      rbind(
        # RAIS_2010_VINC,
        # RAIS_2011_VINC,
        # RAIS_2012_VINC,
        # RAIS_2013_VINC,
        # RAIS_2014_VINC,
        # RAIS_2015_VINC,
        # RAIS_2017_VINC,
        # RAIS_2017_VINC,
        RAIS_2018_VINC,
        RAIS_2019_VINC
      )
    
    
rm(list = ls(pattern = "RAIS_VINC_PUB|RAIS_...._VINC"))

RAIS_2010_2019_VINC$Vínculo.Ativo.31.12 <-
  as.integer(RAIS_2010_2019_VINC$Vínculo.Ativo.31.12)

# Unindo Geocod ----

geocod <-
  read.table(
    file = "./CSV_Data/GeoCodigos_IBGE.csv",
    header = TRUE,
    sep = ";",
    stringsAsFactors = FALSE,
    fill = TRUE,
    quote = ""
  )

geocod$GEOCOD <- 
  stringr::str_extract(string = geocod$GEOCOD, pattern = "^......")


# Município de trabalho ----
RAIS_2010_2019_VINC <- 
  left_join(RAIS_2010_2019_VINC, geocod[,c("GEOCOD","Município","UF_sigla")], by = c("Mun.Trab" = "GEOCOD"))

colnames(RAIS_2010_2019_VINC)[c(3,4,8,9)] <- c("cod_Mun_Trab", "cod_Mun_Sede", "Mun_Trab", "UF_Trab")

# Município da Sede ----
RAIS_2010_2019_VINC <- 
  left_join(RAIS_2010_2019_VINC, geocod[,c("GEOCOD","Município","UF_sigla")], by = c("cod_Mun_Sede" = "GEOCOD"))

colnames(RAIS_2010_2019_VINC)[c(11,10)] <- c("Mun_Sede", "UF_Sede")


# Unindo CNAE ----

# CNAE 2.3 ----
source('D:/Users/humberto.serna/Documents/CSV_Data/RAIS_CAGED/CNAE_2_3.R')

RAIS_2010_2019_VINC$CNAE.2.0.Subclasse <- 
  as.character(RAIS_2010_2019_VINC$CNAE.2.0.Subclasse)


RAIS_2010_2019_VINC <-
  left_join(RAIS_2010_2019_VINC,
            CNAE_Subclasses_2_0[, c("subclasse", "subclasse.descrição")],
            by = c("CNAE.2.0.Subclasse" = "subclasse"))



saveRDS(object = RAIS_2010_2019_VINC, file = "./CSV_DATA/RAIS_CAGED/RAIS_2010_2019_VINC.Rda")




