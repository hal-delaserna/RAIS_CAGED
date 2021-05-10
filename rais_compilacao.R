#   rm(list=ls())
library(tidyverse)
library(lubridate)
#____________________________________________________________________________


# CARREGAMENTO --------------------------------------------------------------
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
    


# CNAE 
source('./CSV_Data/RAIS_CAGED/CNAE_2_3.R')

#_____________________________________________________________________


# delimitação por subclasses ----

colunas <- c(
  #     "Bairros SP",
  #     "Bairros Fortaleza",
  #     "Bairros RJ",
  #     "Causa Afastamento 1",
  #     "Causa Afastamento 2",
  #     "Causa Afastamento 3",
  #     "Motivo Desligamento",
       "CBO Ocupação 2002",
  #     "CNAE 2.0 Classe",
  #     "CNAE 95 Classe",
  #     "Distritos SP",
       "Vínculo Ativo 31/12",
  #     "Faixa Etária",
  #     "Faixa Hora Contrat",
  #     "Faixa Remun Dezem (SM)",
  #     "Faixa Remun Média (SM)",
  #     "Faixa Tempo Emprego",
  #     "Escolaridade após 2005",
  #     "Qtd Hora Contr",
  #     "Idade",
  #     "Ind CEI Vinculado",
  #     "Ind Simples",
  #     "Mês Admissão",
  #     "Mês Desligamento",
       "Mun Trab",
       "Município",
  #     "Nacionalidade",
  #     "Natureza Jurídica",
  #     "Ind Portador Defic",
  #     "Qtd Dias Afastamento",
  #     "Raça Cor",
  #     "Regiões Adm DF",
  #     "Vl Remun Dezembro Nom",
  #     "Vl Remun Dezembro (SM)",
       "Vl Remun Média Nom",
  #     "Vl Remun Média (SM)",
       "CNAE 2.0 Subclasse"#,
  #     "Sexo Trabalhador",
  #     "Tamanho Estabelecimento",
  #     "Tempo Emprego",
  #     "Tipo Admissão",
  #     "Tipo Estab",
  #     "Tipo Estab",
  #     "Tipo Defic",
  #     "Tipo Vínculo",
  #     "IBGE Subsetor",
  #     "Vl Rem Janeiro CC",
  #     "Vl Rem Fevereiro CC",
  #     "Vl Rem Março CC",
  #     "Vl Rem Abril CC",
  #     "Vl Rem Maio CC",
  #     "Vl Rem Junho CC",
  #     "Vl Rem Julho CC",
  #     "Vl Rem Agosto CC",
  #     "Vl Rem Setembro CC",
  #     "Vl Rem Outubro CC",
  #     "Vl Rem Novembro CC",
  #     "Ano Chegada Brasil",
  #     "Ind Trab Intermitente",4
  #     "Ind Trab Parcial"
)



rais <- 
  rbind(AC2010, AC2011, AC2012, AC2013, AC2014, AC2015, AC2016, AC2017, AL2010, AL2011, AL2012, AL2013, AL2014, AL2015, AL2016, AL2017, AM2010, AM2011, AM2012, AM2013, AM2014, AM2015, AM2016, AM2017, AP2010, AP2011, AP2012, AP2013, AP2014, AP2015, AP2016, AP2017, BA2010, BA2011, BA2012, BA2013, BA2014, BA2015, BA2016, BA2017, CE2010, CE2011, CE2012, CE2013, CE2014, CE2015, CE2016, CE2017, DF2010, DF2011, DF2012, DF2013, DF2014, DF2015, DF2016, DF2017, 
      ES2010, ES2011, ES2012, ES2013, ES2014, ES2015, ES2016, ES2017, GO2010, GO2011, GO2012, GO2013, GO2014, GO2015, GO2016, GO2017, MA2010, MA2011, MA2012, MA2013, MA2014, MA2015, MA2016, MA2017, MG2010, MG2011, MG2012, MG2013, MG2014, MG2015, MG2016, MG2017, MS2010, MS2011, MS2012, MS2013, MS2014, MS2015, MS2016, MS2017, MT2010, MT2011, MT2012, MT2013, MT2014, MT2015, MT2016, MT2017, PA2010, PA2011, PA2012, PA2013, PA2014, PA2015, PA2016, PA2017, 
      PB2010, PB2011, PB2012, PB2013, PB2014, PB2015, PB2016, PB2017, PE2010, PE2011, PE2012, PE2013, PE2014, PE2015, PE2016, PE2017, PI2010, PI2011, PI2012, PI2013, PI2014, PI2015, PI2016, PI2017, PR2010, PR2011, PR2012, PR2013, PR2014, PR2015, PR2016, PR2017, RJ2010, RJ2011, RJ2012, RJ2013, RJ2014, RJ2015, RJ2016, RJ2017, RN2010, RN2011, RN2012, RN2013, RN2014, RN2015, RN2016, RN2017, RO2010, RO2011, RO2012, RO2013, RO2014, RO2015, RO2016, RO2017, 
      RR2010, RR2011, RR2012, RR2013, RR2014, RR2015, RR2016, RR2017, RS2010, RS2011, RS2012, RS2013, RS2014, RS2015, RS2016, RS2017, SC2010, SC2011, SC2012, SC2013, SC2014, SC2015, SC2016, SC2017, SE2010, SE2011, SE2012, SE2013, SE2014, SE2015, SE2016, SE2017, SP2010, SP2011, SP2012, SP2013, SP2014, SP2015, SP2016, SP2017, TO2010, TO2011, TO2012, TO2013, TO2014, TO2015, TO2016, TO2017)


rm(list = ls(pattern = "[A-Z]{2}20[0-9]{2}"))


