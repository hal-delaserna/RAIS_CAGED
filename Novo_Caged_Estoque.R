#   rm(list = ls())
options(editor = 'notepad')
library(tidyverse)
library(lubridate)


                        
# CARREGAMENTO DOS MICRODADOS E PREPARAÇÃO DA BASE ----

 # CNAE 2.0 e 2.3 ----
 
  source('D:/Users/humberto.serna/Documents/CSV_Data/RAIS_CAGED/CNAE_2_3.R')
 
cnae <- 
   CNAE_Subclasses_2_0

  
 # _____ Estoque de Trabalhadores ----
 
 arquivos <- c(
   "EstoqueRefCaged2020_01_01.txt",
  # "EstoqueRefCaged2021_01_01.txt",
   "EstoqueRefCaged2020_12_31.txt"
   )
 
 
 lista <- list()
 for (i in 1:length(arquivos)) {
   lista[[i]] <-
     read.table(
       paste('D:/Users/humberto.serna/Documents/CSV_Data/RAIS_CAGED/Novo_Caged/', arquivos[[i]], sep = ""),
       header = TRUE,
       sep = ";",
       dec = ".",
       #quote = "",
       colClasses = c(
         "character",
         "character",
         "numeric"
         ),
       stringsAsFactors = FALSE,
       encoding = "UTF-8"
       )
   
   if (i == 1) {
      lista[[1]][c("data")] <- c('2020.01.01')
      
   } else {
      
      lista[[2]][c("data")] <- c('2021.01.01')
   }
   }
 
 estoque_trabalhadores <- 
   do.call("rbind", lista)
 rm(lista)
 
 


# Descrição subclasses e Grupos
 
 estoque_trabalhadores <- 
    left_join(estoque_trabalhadores, 
              CNAE_Subclasses_2_0, by = c('cnae20subclas' = 'subclasse')
              )

 # Geocod e junção coluna de municípios - UF ----
 geocod <- 
   read.table(file = "D:/Users/humberto.serna/Documents/CSV_Data/GeoCodigos_IBGE.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE, 
              colClasses = "character", encoding = 'iso-8859-1', quote = "") 
 geocod$GEOCOD <- 
   str_extract(geocod$GEOCOD, "......")
 
 geocod$GEOCOD <- 
    str_extract(geocod$GEOCOD, "......")
 
  estoque_trabalhadores <- 
   left_join(estoque_trabalhadores, geocod[, c("UF_sigla", "GEOCOD", "Município")], by = c("codmun" = "GEOCOD"))
 
# exportando em RDS ---- 
 
# saveRDS(estoque_trabalhadores, file = "./CSV_Data/Estoque_Trabalhadores_NovoCaged_Completo.RDATA")
  
  
# delimitando pelas subclasses alvo das seções B e C ----
  
  
  # _____ SUBCLASSES alvo na seção B (Extrativa Mineral - Exceto petróleo & Gás) ----
  subclasses_alvo_SECAO_B <-                    
     c(#subclasse  denominação
        "500301",	#Extração de carvão mineral
        "500302",	#Beneficiamento de carvão mineral
        "710301",	#Extração de minério de ferro
        "710302",	#Pelotização, sinterização e outros beneficiamentos de minério de ferro
        "721901",	#Extração de minério de alumínio
        "721902",	#Beneficiamento de minério de alumínio
        "722701",	#Extração de minério de estanho
        "722702",	#Beneficiamento de minério de estanho
        "723501",	#Extração de minério de manganês
        "723502",	#Beneficiamento de minério de manganês
        "724301",	#Extração de minério de metais preciosos
        "724302",	#Beneficiamento de minério de metais preciosos
        "725100",	#Extração de minerais radioativos
        "729401",	#Extração de minérios de nióbio e titânio
        "729402",	#Extração de minério de tungstênio
        "729403",	#Extração de minério de níquel
        "729404",	#Extração de minérios de cobre, chumbo, zinco e outros minerais metálicos não ferrosos não especificados anteriormente
        "729405",	#Beneficiamento de minérios de cobre, chumbo, zinco e outros minerais metálicos não ferrosos não especificados anteriormente
        "810001",	#Extração de ardósia e beneficiamento associado
        "810002",	#Extração de granito e beneficiamento associado
        "810003",	#Extração de mármore e beneficiamento associado
        "810004",	#Extração de calcário e dolomita e beneficiamento associado
        "810005",	#Extração de gesso e caulim
        "810006",	#Extração de areia, cascalho ou pedregulho e beneficiamento associado
        "810007",	#Extração de argila e beneficiamento associado
        "810008",	#Extração de saibro e beneficiamento associado
        "810009",	#Extração de basalto e beneficiamento associado
        "810010",	#Beneficiamento de gesso e caulim associado à extração
        "810099",	#Extração e britamento de pedras e outros materiais para construção e beneficiamento associado
        "891600",	#Extração de minerais para fabricação de adubos, fertilizantes e outros produtos químicos
        "892401",	#Extração de sal marinho
        "892402",	#Extração de salgema
        "892403",	#Refino e outros tratamentos do sal
        "893200",	#Extração de gemas (pedras preciosas e semipreciosas)
        "899101",	#Extração de grafita
        "899102",	#Extração de quartzo
        "899103",	#Extração de amianto
        "899199",	#Extração de outros minerais não metálicos não especificados anteriormente
        "990401",	#Atividades de apoio à extração de minério de ferro
        "990402",	#Atividades de apoio à extração de minerais metálicos não ferrosos
        "990403"	#Atividades de apoio à extração de minerais não metálicos
     )
  
  
  # _____ Lista classes Alvo na Seção C (Indústria de Transformação Relacionada não associada/consecutiva à mineração) ----
  
  classes_alvo_SECAO_C <- 
     c("20126",	        #          Fabricação de Intermediários para Fertilizantes 
       "20134",		          #          Fabricação de Adubos e Fertilizantes 
       "23206",		          #          Fabricação de Cimento 
       "23303",		          #          Fabricação de Artefatos de Concreto, Cimento, Fibrocimento, Gesso e Materiais Semelhantes 
       "23419",		          #          Fabricação de Produtos Cerâmicos Refratários 
       "23427",		          #          Fabricação de Produtos Cerâmicos NãoRefratários para Uso Estrutural na Construção 
       "23494",		          #          Fabricação de Produtos Cerâmicos NãoRefratários não Especificados Anteriormente 
       "23915",		          #          Aparelhamento e Outros Trabalhos em Pedras 
       "23923",		          #          Fabricação de Cal e Gesso 
       "23991",		          #          Fabricação de Produtos de Minerais NãoMetálicos não Especificados Anteriormente 
       "24113",		          #          Produção de FerroGusa 
       "24121",		          #          Produção de Ferroligas 
       "24211",		          #          Produção de SemiAcabados de Aço 
       "24229",		          #          Produção de Laminados Planos de Aço 
       "24237",		          #          Produção de Laminados Longos de Aço 
       "24245",		          #          Produção de Relaminados, Trefilados e Perfilados de Aço 
       "24318",		          #          Produção de Tubos de Aço com Costura 
       "24393",		          #          Produção de Outros Tubos de Ferro e Aço 
       "24415",		          #          Metalurgia do Alumínio e Suas Ligas 
       "24423",		          #          Metalurgia dos Metais Preciosos 
       "24431",		          #          Metalurgia do Cobre 
       "24491",		          #          Metalurgia dos Metais NãoFerrosos e Suas Ligas não Especificados Anteriormente 
       "24512",		          #          Fundição de Ferro e Aço 
       "25314",		          #          Produção de forjados de aço e de metais nãoferrosos e suas ligas 
       "24521",		          #          Fundição de Metais NãoFerrosos e Suas Ligas 
       "32116")		          #          Lapidação de Gemas e Fabricação de Artefatos de Ourivesaria e Joalheria 
  
  
  # __________ SUBCLASSES alvo na Seção C ----
  
  subclasses_alvo_SECAO_C <- 
     unique(cnae[cnae$classe %in% classes_alvo_SECAO_C, c("subclasse")])
  
  
  
   estoque_trabalhadores <-
     estoque_trabalhadores[estoque_trabalhadores$cnae20subclas %in% c(subclasses_alvo_SECAO_B, 
                                                                      subclasses_alvo_SECAO_C
     ), ]
  
# 
saveRDS(estoque_trabalhadores, 
		file = "./CSV_Data/Novo_Caged_estoque_trabalhadores_2020_2021_secoes_CNAE23_B_C.RDATA")
  
  


# *************  AJUSTE ESTOQUE MUNICÍPIO ************* ----
#   rm(list = ls())
options(editor = 'notepad')
library(tidyverse)
library(lubridate)


# CARREGAMENTO DOS MICRODADOS E PREPARAÇÃO DA BASE ----

# CNAE 2.0 e 2.3 ----

source('D:/Users/humberto.serna/Documents/CSV_Data/RAIS_CAGED/CNAE_2_3.R')

cnae <- 
  CNAE_Subclasses_2_0


geocod <- 
  read.table(file = "./CSV_Data/GeoCodigos_IBGE.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE, 
             colClasses = "character", encoding = 'iso-8859-1', quote = "")
geocod$GEOCOD <- 
  str_extract(geocod$GEOCOD, "......")

geocod$GEOCOD <- 
  as.integer(geocod$GEOCOD)


# _____ ANO 2020 ----

# carregamento
EstoqueRefCaged2020_01_01 <- 
  read.table("./CSV_Data/RAIS_CAGED/Novo_Caged/EstoqueRefCaged2020_01_01.txt",
             header = TRUE, sep = ";")

# agregação subclasse
Estoque_df <- 
  summarise(
    group_by(
      EstoqueRefCaged2020_01_01,cnae20subclas), 
    'estoqueref_SUM(subclas)' = sum(estoqueref, na.rm = T))

EstoqueRefCaged2020_01_01 <- 
  left_join(EstoqueRefCaged2020_01_01, Estoque_df, by = c('cnae20subclas'))

# base mais Atual (mas sem detalhamento por município)
EstoqueRefCaged <- 
  read.table("./CSV_Data/RAIS_CAGED/Novo_Caged/EstoqueRefCaged.csv",
             header = TRUE, sep = ";")

# união
EstoqueRefCaged2020_01_01 <- 
  left_join(EstoqueRefCaged2020_01_01, 
            EstoqueRefCaged[EstoqueRefCaged$data == "2020.01.01",], 
            by = c('cnae20subclas'))

# exclusão das subclasses not Indústria
EstoqueRefCaged2020_01_01 <-
  EstoqueRefCaged2020_01_01[is.na(EstoqueRefCaged2020_01_01$estoqueref.y) == FALSE, c(-5)]

# porporção município-subclasse
EstoqueRefCaged2020_01_01$proporção <- 
  EstoqueRefCaged2020_01_01$estoqueref.x / 
  EstoqueRefCaged2020_01_01$`estoqueref_SUM(subclas)`

# Estoque Ajustado por porporção município-subclasse
EstoqueRefCaged2020_01_01$estoqueref_ajustado <-
  round(
    EstoqueRefCaged2020_01_01$estoqueref.y * 
      EstoqueRefCaged2020_01_01$proporção, digits = 0)


CNAE_Subclasses_2_0$subclasse <- 
  as.integer(CNAE_Subclasses_2_0$subclasse)


EstoqueRefCaged2020_01_01 <- 
  left_join(EstoqueRefCaged2020_01_01, 
            CNAE_Subclasses_2_0, 
            by = c('cnae20subclas' = 'subclasse'))


EstoqueRefCaged2020_01_01 <- 
  left_join(EstoqueRefCaged2020_01_01, geocod, by = c("codmun.x" = "GEOCOD"))
# 
# EstoqueRefCaged2020_01_01 <- 
#   EstoqueRefCaged2020_01_01[,c("data", "cnae20subclas","subclasse.descrição", "estoqueref.x", "estoqueref_ajustado", "estoqueref_SUM(subclas)", 
#                                "estoqueref.y", "proporção", "UF_sigla", "Município")]
# 



# _____ ANO 2021 ----

# carregamento
EstoqueRefCaged2021_01_01 <- 
  read.table("./CSV_Data/RAIS_CAGED/Novo_Caged/EstoqueRefCaged2021_01_01.txt",
             header = TRUE, sep = ";")

# agregação subclasse
Estoque_df <- 
  summarise(
    group_by(
      EstoqueRefCaged2021_01_01,cnae20subclas), 
    'estoqueref_SUM(subclas)' = sum(estoqueref, na.rm = T))

EstoqueRefCaged2021_01_01 <- 
  left_join(EstoqueRefCaged2021_01_01, Estoque_df, by = c('cnae20subclas'))

# base mais Atual (mas sem detalhamento por município)
EstoqueRefCaged <- 
  read.table("./CSV_Data/RAIS_CAGED/Novo_Caged/EstoqueRefCaged.csv",
             header = TRUE, sep = ";")

# união
EstoqueRefCaged2021_01_01 <- 
  left_join(EstoqueRefCaged2021_01_01, 
            EstoqueRefCaged[EstoqueRefCaged$data == "2021.01.01",], 
            by = c('cnae20subclas'))

# exclusão das subclasses not Indústria
EstoqueRefCaged2021_01_01 <-
  EstoqueRefCaged2021_01_01[is.na(EstoqueRefCaged2021_01_01$estoqueref.y) == FALSE, c(-5)]

# porporção município-subclasse
EstoqueRefCaged2021_01_01$proporção <- 
  EstoqueRefCaged2021_01_01$estoqueref.x / 
  EstoqueRefCaged2021_01_01$`estoqueref_SUM(subclas)`

# Estoque Ajustado por porporção município-subclasse
EstoqueRefCaged2021_01_01$estoqueref_ajustado <-
  round(
    EstoqueRefCaged2021_01_01$estoqueref.y * 
      EstoqueRefCaged2021_01_01$proporção, digits = 0)


CNAE_Subclasses_2_0$subclasse <- 
  as.integer(CNAE_Subclasses_2_0$subclasse)


EstoqueRefCaged2021_01_01 <- 
  left_join(EstoqueRefCaged2021_01_01, 
            CNAE_Subclasses_2_0, 
            by = c('cnae20subclas' = 'subclasse'))


EstoqueRefCaged2021_01_01 <- 
  left_join(EstoqueRefCaged2021_01_01, geocod, by = c("codmun.x" = "GEOCOD"))

# EstoqueRefCaged2021_01_01 <- 
#   EstoqueRefCaged2021_01_01[,c("data", "cnae20subclas","subclasse.descrição", "estoqueref.x", "estoqueref_ajustado", "estoqueref_SUM(subclas)", 
#                                "estoqueref.y", "proporção", "UF_sigla", "Município")]


# Unindo ambos anos

EstoqueRefNovoCaged <- 
  rbind(EstoqueRefCaged2020_01_01, EstoqueRefCaged2021_01_01)



# delimitando pelas subclasses alvo das seções B e C ----


# _____ SUBCLASSES alvo na seção B (Extrativa Mineral - Exceto petróleo & Gás) ----
subclasses_alvo_SECAO_B <-                    
  c(#subclasse  denominação
    "500301",	#Extração de carvão mineral
    "500302",	#Beneficiamento de carvão mineral
    "710301",	#Extração de minério de ferro
    "710302",	#Pelotização, sinterização e outros beneficiamentos de minério de ferro
    "721901",	#Extração de minério de alumínio
    "721902",	#Beneficiamento de minério de alumínio
    "722701",	#Extração de minério de estanho
    "722702",	#Beneficiamento de minério de estanho
    "723501",	#Extração de minério de manganês
    "723502",	#Beneficiamento de minério de manganês
    "724301",	#Extração de minério de metais preciosos
    "724302",	#Beneficiamento de minério de metais preciosos
    "725100",	#Extração de minerais radioativos
    "729401",	#Extração de minérios de nióbio e titânio
    "729402",	#Extração de minério de tungstênio
    "729403",	#Extração de minério de níquel
    "729404",	#Extração de minérios de cobre, chumbo, zinco e outros minerais metálicos não ferrosos não especificados anteriormente
    "729405",	#Beneficiamento de minérios de cobre, chumbo, zinco e outros minerais metálicos não ferrosos não especificados anteriormente
    "810001",	#Extração de ardósia e beneficiamento associado
    "810002",	#Extração de granito e beneficiamento associado
    "810003",	#Extração de mármore e beneficiamento associado
    "810004",	#Extração de calcário e dolomita e beneficiamento associado
    "810005",	#Extração de gesso e caulim
    "810006",	#Extração de areia, cascalho ou pedregulho e beneficiamento associado
    "810007",	#Extração de argila e beneficiamento associado
    "810008",	#Extração de saibro e beneficiamento associado
    "810009",	#Extração de basalto e beneficiamento associado
    "810010",	#Beneficiamento de gesso e caulim associado à extração
    "810099",	#Extração e britamento de pedras e outros materiais para construção e beneficiamento associado
    "891600",	#Extração de minerais para fabricação de adubos, fertilizantes e outros produtos químicos
    "892401",	#Extração de sal marinho
    "892402",	#Extração de salgema
    "892403",	#Refino e outros tratamentos do sal
    "893200",	#Extração de gemas (pedras preciosas e semipreciosas)
    "899101",	#Extração de grafita
    "899102",	#Extração de quartzo
    "899103",	#Extração de amianto
    "899199",	#Extração de outros minerais não metálicos não especificados anteriormente
    "990401",	#Atividades de apoio à extração de minério de ferro
    "990402",	#Atividades de apoio à extração de minerais metálicos não ferrosos
    "990403"	#Atividades de apoio à extração de minerais não metálicos
  )


# _____ Lista classes Alvo na Seção C (Indústria de Transformação Relacionada não associada/consecutiva à mineração) ----

classes_alvo_SECAO_C <- 
  c("20126",	        #          Fabricação de Intermediários para Fertilizantes 
    "20134",		          #          Fabricação de Adubos e Fertilizantes 
    "23206",		          #          Fabricação de Cimento 
    "23303",		          #          Fabricação de Artefatos de Concreto, Cimento, Fibrocimento, Gesso e Materiais Semelhantes 
    "23419",		          #          Fabricação de Produtos Cerâmicos Refratários 
    "23427",		          #          Fabricação de Produtos Cerâmicos NãoRefratários para Uso Estrutural na Construção 
    "23494",		          #          Fabricação de Produtos Cerâmicos NãoRefratários não Especificados Anteriormente 
    "23915",		          #          Aparelhamento e Outros Trabalhos em Pedras 
    "23923",		          #          Fabricação de Cal e Gesso 
    "23991",		          #          Fabricação de Produtos de Minerais NãoMetálicos não Especificados Anteriormente 
    "24113",		          #          Produção de FerroGusa 
    "24121",		          #          Produção de Ferroligas 
    "24211",		          #          Produção de SemiAcabados de Aço 
    "24229",		          #          Produção de Laminados Planos de Aço 
    "24237",		          #          Produção de Laminados Longos de Aço 
    "24245",		          #          Produção de Relaminados, Trefilados e Perfilados de Aço 
    "24318",		          #          Produção de Tubos de Aço com Costura 
    "24393",		          #          Produção de Outros Tubos de Ferro e Aço 
    "24415",		          #          Metalurgia do Alumínio e Suas Ligas 
    "24423",		          #          Metalurgia dos Metais Preciosos 
    "24431",		          #          Metalurgia do Cobre 
    "24491",		          #          Metalurgia dos Metais NãoFerrosos e Suas Ligas não Especificados Anteriormente 
    "24512",		          #          Fundição de Ferro e Aço 
    "25314",		          #          Produção de forjados de aço e de metais nãoferrosos e suas ligas 
    "24521",		          #          Fundição de Metais NãoFerrosos e Suas Ligas 
    "32116")		          #          Lapidação de Gemas e Fabricação de Artefatos de Ourivesaria e Joalheria 


# __________ SUBCLASSES alvo na Seção C ----

subclasses_alvo_SECAO_C <- 
  unique(cnae[cnae$classe %in% classes_alvo_SECAO_C, c("subclasse")])



EstoqueRefNovoCaged <-
  EstoqueRefNovoCaged[EstoqueRefNovoCaged$cnae20subclas %in% c(subclasses_alvo_SECAO_B, 
                                                               subclasses_alvo_SECAO_C), ]

# 
saveRDS(EstoqueRefNovoCaged, 
        file = "./CSV_Data/Novo_Caged_NovoCaged_2020_2021_secoes_CNAE23_B_C_Municipio_Ajustado.RDATA")




