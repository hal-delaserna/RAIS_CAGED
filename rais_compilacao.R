rm(list=ls())
library(dplyr)
library(stringr)


SP2018 <- 'D:/Users/humberto.serna/Desktop/CSV_Data/RAIS/RAIS_microdados/Metadados RAIS/RAIS_VINC_PUB_SP/RAIS_VINC_PUB_SP.txt'

rais_SP2018 <- 
  read.table(file = SP2018, header = TRUE,sep = ";",dec = ",", 
  stringsAsFactors = FALSE,encoding = "ANSI")[,c("CNAE.2.0.Classe", "CNAE.95.Classe", "Vínculo.Ativo.31.12", 
                        "Escolaridade.após.2005", "Mun.Trab", "Município", "Natureza.Jurídica", 
                        "CNAE.2.0.Subclasse", "Tamanho.Estabelecimento", 
                        "Tipo.Vínculo", "IBGE.Subsetor", "Ind.Trab.Parcial")]

rais_SP2018$`Mun Trab` <- as.integer(AC2006$`Mun Trab`)
rais <- filter(select(rais_SP2018, `Vínculo Ativo 31/12`, `Escolaridade após 2005`, `Mun Trab`,`ano`), `Mun Trab` %in%   c(352620,350270,352120,354260,350925,352240,354325,353282,354995,351020,351760,353620,352460,354300,350990,350715,352215,353760,352210,355350,352265,350540,351480,354280,352990,352030,350535,352320,352610,355180,352330,353720,352042))

save(rais_SP2018, file = 'rais_SP2018.Rda')