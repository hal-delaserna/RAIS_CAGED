# rm(list = ls())
options(editor = 'notepad')
library(tidyverse)
library(lubridate)

if (!require(svDialogs)) {
  install.packages('svDialogs')
  library(svDialogs)
}


#  Geocod | CNAE | CBO ----
source('./R/CNAE.R', encoding = "UTF-8")
CNAE_Subclasses_2_0$subclasse <- as.integer(CNAE_Subclasses_2_0$subclasse)
source("./R/geocod.R", encoding = "UTF-8")
geocod$GEOCOD <- 
  str_extract(geocod$GEOCOD, "......")
# Intervalo de Datas
data_inicial <- ymd("2020-01-01")
data_final <- floor_date(Sys.Date(), "year") - 1
intervalo_datas <- seq(data_inicial, data_final, by = "year")
intervalo_datas <- format(intervalo_datas, "%Y")



# source("./R/CBO.R", encoding = "UTF-8")
# cbo$CO_CBO <- as.integer(cbo$CO_CBO)


# ___________________________________________________________________________________________________________ ----
#  CARREGAMENTO MICRODADOS E PREPARA��O DA BASE ----
# . ----

# _______________________________________________________________________
#   O n�mero de estabelecimentos que apresentam declara��es � Rais       | 
#   difere ano a ano, o que dificulta discriminar se a varia��o do       |
#   emprego se deve a um real aumento ou redu��o decorrente da situa��o  | 
#   do mercado de trabalho e/ou a um melhor desempenho na declara��o.    |
#   Opte por uma amostra longitudinal de empresas que declaram os dados. |
# _______________________________________________________________________|


res <- 
  dlg_message(c("Selecionar CNAE?", '\n (Escolha "N�o" p/ as CNAEs do INFORME)'),
              "yesno")$res

if (res == "yes") {
  dlgMessage(
    c('CNAE: Selecione Grupamento(s)', '\n (use CTRL)')
  )
########### FUTURAMENTE USAR C�DIGOS cnae NO LUGAR DE NOMES POR EXTENSO: O ALGORITIMO N�O ENCONTRA, 
########## SEMPRE CORRESPOND�NCIA NA cnae 2.0 (DISCRETAS DIFEREN�AS DE GRAFIA)
  grupamento <-
    dlgList(
      unique(CNAE_Subclasses_2_0$grupamento),
      multiple = TRUE, 
      preselect = c(
        "Ind�stria geral"
      ),
      title = 'GRUPAMENTOS CNAE'
    )$res
  
  # Se��o
  dlgMessage(
    c('CNAE: Selecione SECAO')
  )
  secao <-
    dlgList(
      unique(
        CNAE_Subclasses_2_0[CNAE_Subclasses_2_0$grupamento %in% grupamento,]$se��o.descri��o),
      multiple = TRUE, 
      preselect = c("Ind�strias Extrativas", "Ind�strias de Transforma��o"),
      title = 'SE��O CNAE'
    )$res
  
  # Divis�o
  
  dlgMessage(
    c('CNAE: Selecione DIVISAO')
  )
  divisao <-
    dlgList(
      unique(
        CNAE_Subclasses_2_0[CNAE_Subclasses_2_0$se��o.descri��o %in% secao,]$divis�o.descri��o),
      multiple = TRUE, 
      preselect = NULL,
      title = 'DIVIS�ES CNAE'
    )$res
  
  # Grupo
  dlgMessage(
    c('CNAE: Selecione GRUPO(S)')
  )
  grupo <-
    dlgList(
      unique(
        CNAE_Subclasses_2_0[CNAE_Subclasses_2_0$divis�o.descri��o %in% divisao,]$grupo.descri��o),
      multiple = TRUE,
      preselect = NULL,
      title = 'GRUPOS CNAE'
    )$res
  
  # Classe
  dlgMessage(
    c('CNAE: Selecione CLASSE(S)')
  )
  classe <-
    dlgList(
      unique(
        CNAE_Subclasses_2_0[CNAE_Subclasses_2_0$grupo.descri��o %in% grupo,]$classe.descri��o),
      multiple = TRUE,
      preselect = NULL,
      title = 'CLASSES CNAE'
    )$res
  
  # subclasse
  dlgMessage(
    c('CNAE: Selecione SUBCLASSE(S)')
  )
  subclasse <- 
    dlgList(
      unique(
        CNAE_Subclasses_2_0[CNAE_Subclasses_2_0$classe.descri��o %in% classe,]$subclasse.descri��o),
      multiple = TRUE,
      preselect = NULL,
      title = 'SUBCLASSES CNAE'
    )$res
  
  
  subclasses_alvo <- 
    CNAE_Subclasses_2_0[CNAE_Subclasses_2_0$subclasse.descri��o %in% subclasse,]$subclasse
  
  
} else { 
  subclasses_alvo <- # c(subclasses_alvo_SECAO_B, subclasses_alvo_SECAO_C)
    c(500301, 500302, 710301, 710302, 721901, 721902, 722701, 722702, 723501, 723502, 724301, 724302, 725100, 729401, 729402, 729403, 729404, 729405, 
      810001, 810002, 810003, 810004, 810005, 810006, 810007, 810008, 810009, 810010, 810099, 891600, 892401, 892402, 892403, 893200, 899101, 899102, 
      899103, 899199, 990401, 990402, 990403, 1121600, 1721400, 1722200, 1910100, 2011800, 2012600, 2013401, 2013402, 2019301, 2019399, 2051700, 2071100, 
      2092401, 2092402, 2092403, 2093200, 2094100, 2099199, 2311700, 2312500, 2319200, 2320600, 2330301, 2330302, 2330303, 2330304, 2330305, 2330399, 2341900, 
      2342701, 2342702, 2349401, 2349499, 2391501, 2391502, 2391503, 2392300, 2399101, 2399102, 2399199, 2411300, 2412100, 2421100, 2422901, 2422902, 2423701, 
      2423702, 2424501, 2424502, 2431800, 2439300, 2441501, 2441502, 2442300, 2443100, 2449101, 2449102, 2449103, 2449199, 2451200, 2452100, 2511000, 2512800, 
      2513600, 2521700, 2522500, 2531401, 2531402, 2532201, 2532202, 2539001, 2539002, 2541100, 2542000, 2543800, 2550101, 2550102, 2591800, 2592601, 2592602, 
      2593400, 2599301, 2599302, 2599399, 2652300, 2721000, 2722801, 2722802, 2740601, 3102100, 3211601, 3211602, 3211603, 3212400)
  
  
}

 
#  ARQUIVOS V�NCULOS ****************** ----
dlgMessage(
  c('Selecione o Ano')
)
PERIODO <-
  dlgList(c(
    intervalo_datas
  ),
  multiple = TRUE,
  title = ' Use CTRL para v�rios       '
  )$res


# ## Caixa de di�logo de CBOs. Tem problema de subsetting
#  res <- 
#    dlg_message(c("Selecionar CBO?"),
#                "yesno")$res
#  
#  if (res == "yes") {
#    dlgMessage(
#      c('Selecione CBO (2002)')
#    )
#  
#  Lista_CBO <-
#    dlgList(
#      c(#subclasse  denomina��o
#        322205,   #"T�cnico de enfermagem"
#      # 322210,   #"T�cnico de enfermagem de terapia intensiva"
#        322215,   #"T�cnico de enfermagem do trabalho"
#        322220,   #"T�cnico de enfermagem psiqui�trica"
#        322230,   #"Auxiliar de enfermagem"
#        322235,   #"Auxiliar de enfermagem do trabalho"
#        322245,   #"T�cnico de enfermagem da estrat�gia de sa�de da fam�lia"
#        322250,   #"Auxiliar de enfermagem da estrat�gia de sa�de da fam�lia"
#        515110,   #"Atendente de enfermagem"
#        516210,   #"Cuidador de idosos"
#        516220    #"Cuidador em sa�de"
#      ),
#    multiple = TRUE,
#    title = ' Use CTRL para v�rios       '
#    )$res  } else { 
#    Lista_CBO <- c(".")   # TODAS AS CBOs 2002
#    
#  }



REGIOES <- c("_CENTRO_OESTE", "_MG_ES_RJ", "_NORDESTE", "_NORTE", "_SP", "_SUL") # Regi�es

# _____ colunas ----             
lista <- list()
i <- 1
for (r in 1:length(REGIOES)) {
  for (p in 1:length(PERIODO)) {
    lista[[i]] <-
        read.table(
           paste('./data/mcdRAIS/', PERIODO[[p]], "/RAIS_VINC_PUB", REGIOES[[r]], ".txt", sep = "")
          ,header = TRUE
          # ,nrows = 20000  
          ,sep = ";"
          ,dec = ","
          ,quote = ""
          ,stringsAsFactors = FALSE
          ,fileEncoding = "Latin1"
          ,encoding = "Latin1"
          ,colClasses = c(
             "NULL"     #  Bairros SP
            ,"NULL"     #  Bairros Fortaleza
            ,"NULL"     #  Bairros RJ
            ,"NULL"     #  Causa Afastamento 1
            ,"NULL"     #  Causa Afastamento 2
            ,"NULL"     #  Causa Afastamento 3
            ,"NULL"     #  Motivo Desligamento
            ,"NULL"     #  CBO Ocupa��o 2002
            ,"NULL"     #  CNAE 2.0 Classe
            ,"NULL"     #  CNAE 95 Classe
            ,"NULL"     #  Distritos SP
            ,"integer"  #  V�nculo Ativo 31/12
            ,"NULL"     #  Faixa Et�ria
            ,"NULL"     #  Faixa Hora Contrat
            ,"NULL"     #  Faixa Remun Dezem (SM)
            ,"NULL"     #  Faixa Remun M�dia (SM)
            ,"NULL"     #  Faixa Tempo Emprego
            ,"NULL"     #  Escolaridade ap�s 2005
            ,"NULL"     #  Qtd Hora Contr
            ,"NULL"     #  Idade
            ,"NULL"     #  Ind CEI Vinculado
            ,"NULL"     #  Ind Simples
            ,"NULL"     #  M�s Admiss�o
            ,"NULL"     #  M�s Desligamento
            ,"integer"  #  Mun Trab
            ,"NULL"     #  Munic�pio
            ,"NULL"     #  Nacionalidade
            ,"NULL"     #  Natureza Jur�dica
            ,"NULL"     #  Ind Portador Defic
            ,"NULL"     #  Qtd Dias Afastamento
            ,"NULL"     #  Ra�a Cor
            ,"NULL"     #  Regi�es Adm DF
            ,"NULL"     #  Vl Remun Dezembro Nom
            ,"NULL"     #  Vl Remun Dezembro (SM)
            ,"NULL"     #  Vl Remun M�dia Nom
            ,"NULL"     #  Vl Remun M�dia (SM)
            ,"integer"  #  CNAE 2.0 Subclasse
            ,"NULL"     #  Sexo Trabalhador
            ,"NULL"     #  Tamanho Estabelecimento
            ,"NULL"     #  Tempo Emprego
            ,"NULL"     #  Tipo Admiss�oo
            ,"NULL"     #  Tipo Estab
            ,"NULL"     #  Tipo Estab
            ,"NULL"     #  Tipo Defic
            ,"NULL"     #  Tipo V�nculo
            ,"integer"  #  IBGE Subsetor
            ,"NULL"     #  Vl Rem Janeiro SC
            ,"NULL"     #  Vl Rem Fevereiro SC
            ,"NULL"     #  Vl Rem Mar�o SC
            ,"NULL"     #  Vl Rem Abril SC
            ,"NULL"     #  Vl Rem Maio SC
            ,"NULL"     #  Vl Rem Junho SC
            ,"NULL"     #  Vl Rem Julho SC
            ,"NULL"     #  Vl Rem Agosto SC
            ,"NULL"     #  Vl Rem Setembro SC
            ,"NULL"     #  Vl Rem Outubro SC
            ,"NULL"     #  Vl Rem Novembro SC
            ,"NULL"     #  Ano Chegada Brasil
            ,"NULL"     #  Ind Trab Intermitente
            ,"NULL"     #  Ind Trab Parcial
          )
        )
      
      df <- lista[[i]]
      
      # ______________________________________
      #  Delimitando Subclasses de interesse
      df <- 
        df[df$CNAE.2.0.Subclasse %in% subclasses_alvo,]  

      
      df$Ano <- PERIODO[[p]]
      
      lista[[i]] <- df
      
      i <- i+1
      
    }
  }

vinculos <- 
  do.call("rbind", lista)
rm(lista)

alarm()



# _____ Join CNAE Grupo/Subclasse ----
vinculos <-
  left_join(vinculos, 
            CNAE_Subclasses_2_0, 
            by = c("CNAE.2.0.Subclasse" = "subclasse"))


# _____ Join Munic�pios ----

vinculos <-
  left_join(vinculos, 
            geocod[, c("UF_sigla", "GEOCOD_6", "Munic�pio")], 
            by = c("Mun.Trab" = "GEOCOD_6"))



# exportando em RDS/RDA ---- 

# saveRDS(vinculos, 
#         file = paste0("./data/RAIS_Vinculos_IPEA_",format(Sys.time(),"%Y%m%d%H%M"),".Rds"))

  
  
  
  
  
  
  