#   rm(list = ls())
options(editor = 'notepad')
library(tidyverse)
library(lubridate)


                        
# CARREGAMENTO DOS MICRODADOS E PREPARA??O DA BASE ----

 # CNAE 2.0 e 2.3 ----
 
  source('D:/Users/humberto.serna/Documents/D_Lake/CNAE.R')
 
cnae <- 
   CNAE_Subclasses_2_0

  
 # _____ Estoque de Trabalhadores Novo Caged ----

# __________ oriundos do Mailing List do Min do Pdet
 
 arquivos <- 
  c(
  "EstoqueRefCaged2020_01_01.txt",
  # "EstoqueRefCaged2020_12_31.txt",
  "EstoqueRefCaged2021_01_01.txt",
  "EstoqueRefCaged2022_01_01.txt"
  )
   

 lista <- list()
 for (i in 1:length(arquivos)) {
   lista[[i]] <-
     read.table(
       paste('D:/Users/humberto.serna/Documents/D_Lake/RAIS_CAGED/Novo_Caged/', arquivos[[i]], sep = ""),
       header = TRUE,
       sep = ";",
       dec = ".",
       #quote = "",
       colClasses = c(
         "integer",
         "character",
         "integer"
         ),
       stringsAsFactors = FALSE,
       encoding = "UTF-8"
       )
   
   if (i == 1) {
     lista[[1]][c("data")] <- c('2020.01.01')
     
   } else if (i == 2)  {
     lista[[2]][c("data")] <- c('2021.01.01')
   } else {
     lista[[3]][c("data")] <- c('2022.01.01')
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

 # Geocod e junçãoo coluna de munic?pios - UF ----
 
 source("D:/Users/humberto.serna/Documents/D_Lake/geocod.R") 
 
  estoque_trabalhadores <- 
   left_join(estoque_trabalhadores, geocod[, c("UF_sigla", "GEOCOD_6", "Município")], by = c("codmun" = "GEOCOD_6"))
 
# exportando em RDS Estoque Novo Caged Completo ---- 
 
 saveRDS(estoque_trabalhadores, file = "./D_Lake/Estoque_Trabalhadores_NovoCaged_Completo_202204.RDS",)
  
  
# delimitando pelas subclasses alvo das se??es B e C ----
  
  
  # _____ SUBCLASSES alvo na se??o B (Extrativa Mineral - Exceto petr?leo & G?s) ----
  subclasses_alvo_SECAO_B <-                    
     c(#subclasse  denomina??o
        "500301",	#Extra??o de carv?o mineral
        "500302",	#Beneficiamento de carv?o mineral
        "710301",	#Extra??o de min?rio de ferro
        "710302",	#Pelotiza??o, sinteriza??o e outros beneficiamentos de min?rio de ferro
        "721901",	#Extra??o de min?rio de alum?nio
        "721902",	#Beneficiamento de min?rio de alum?nio
        "722701",	#Extra??o de min?rio de estanho
        "722702",	#Beneficiamento de min?rio de estanho
        "723501",	#Extra??o de min?rio de mangan?s
        "723502",	#Beneficiamento de min?rio de mangan?s
        "724301",	#Extra??o de min?rio de metais preciosos
        "724302",	#Beneficiamento de min?rio de metais preciosos
        "725100",	#Extra??o de minerais radioativos
        "729401",	#Extra??o de min?rios de ni?bio e tit?nio
        "729402",	#Extra??o de min?rio de tungst?nio
        "729403",	#Extra??o de min?rio de n?quel
        "729404",	#Extra??o de min?rios de cobre, chumbo, zinco e outros minerais met?licos n?o ferrosos n?o especificados anteriormente
        "729405",	#Beneficiamento de min?rios de cobre, chumbo, zinco e outros minerais met?licos n?o ferrosos n?o especificados anteriormente
        "810001",	#Extra??o de ard?sia e beneficiamento associado
        "810002",	#Extra??o de granito e beneficiamento associado
        "810003",	#Extra??o de m?rmore e beneficiamento associado
        "810004",	#Extra??o de calc?rio e dolomita e beneficiamento associado
        "810005",	#Extra??o de gesso e caulim
        "810006",	#Extra??o de areia, cascalho ou pedregulho e beneficiamento associado
        "810007",	#Extra??o de argila e beneficiamento associado
        "810008",	#Extra??o de saibro e beneficiamento associado
        "810009",	#Extra??o de basalto e beneficiamento associado
        "810010",	#Beneficiamento de gesso e caulim associado ? extra??o
        "810099",	#Extra??o e britamento de pedras e outros materiais para constru??o e beneficiamento associado
        "891600",	#Extra??o de minerais para fabrica??o de adubos, fertilizantes e outros produtos qu?micos
        "892401",	#Extra??o de sal marinho
        "892402",	#Extra??o de salgema
        "892403",	#Refino e outros tratamentos do sal
        "893200",	#Extra??o de gemas (pedras preciosas e semipreciosas)
        "899101",	#Extra??o de grafita
        "899102",	#Extra??o de quartzo
        "899103",	#Extra??o de amianto
        "899199",	#Extra??o de outros minerais n?o met?licos n?o especificados anteriormente
        "990401",	#Atividades de apoio ? extra??o de min?rio de ferro
        "990402",	#Atividades de apoio ? extra??o de minerais met?licos n?o ferrosos
        "990403"	#Atividades de apoio ? extra??o de minerais n?o met?licos
     )
  
  
  # _____ Lista classes Alvo na Se??o C (Ind?stria de Transforma??o Relacionada n?o associada/consecutiva ? minera??o) ----
  
  classes_alvo_SECAO_C <- 
     c("20126",	        #          Fabrica??o de Intermedi?rios para Fertilizantes 
       "20134",		          #          Fabrica??o de Adubos e Fertilizantes 
       "23206",		          #          Fabrica??o de Cimento 
       "23303",		          #          Fabrica??o de Artefatos de Concreto, Cimento, Fibrocimento, Gesso e Materiais Semelhantes 
       "23419",		          #          Fabrica??o de Produtos Cer?micos Refrat?rios 
       "23427",		          #          Fabrica??o de Produtos Cer?micos N?oRefrat?rios para Uso Estrutural na Constru??o 
       "23494",		          #          Fabrica??o de Produtos Cer?micos N?oRefrat?rios n?o Especificados Anteriormente 
       "23915",		          #          Aparelhamento e Outros Trabalhos em Pedras 
       "23923",		          #          Fabrica??o de Cal e Gesso 
       "23991",		          #          Fabrica??o de Produtos de Minerais N?oMet?licos n?o Especificados Anteriormente 
       "24113",		          #          Produ??o de FerroGusa 
       "24121",		          #          Produ??o de Ferroligas 
       "24211",		          #          Produ??o de SemiAcabados de A?o 
       "24229",		          #          Produ??o de Laminados Planos de A?o 
       "24237",		          #          Produ??o de Laminados Longos de A?o 
       "24245",		          #          Produ??o de Relaminados, Trefilados e Perfilados de A?o 
       "24318",		          #          Produ??o de Tubos de A?o com Costura 
       "24393",		          #          Produ??o de Outros Tubos de Ferro e A?o 
       "24415",		          #          Metalurgia do Alum?nio e Suas Ligas 
       "24423",		          #          Metalurgia dos Metais Preciosos 
       "24431",		          #          Metalurgia do Cobre 
       "24491",		          #          Metalurgia dos Metais N?oFerrosos e Suas Ligas n?o Especificados Anteriormente 
       "24512",		          #          Fundi??o de Ferro e A?o 
       "25314",		          #          Produ??o de forjados de a?o e de metais n?oferrosos e suas ligas 
       "24521",		          #          Fundi??o de Metais N?oFerrosos e Suas Ligas 
       "32116")		          #          Lapida??o de Gemas e Fabrica??o de Artefatos de Ourivesaria e Joalheria 
  
  
  # __________ SUBCLASSES alvo na Se??o C ----
  
  subclasses_alvo_SECAO_C <- 
     unique(cnae[cnae$classe %in% classes_alvo_SECAO_C, c("subclasse")])
  
  
  
   estoque_trabalhadores <-
     estoque_trabalhadores[estoque_trabalhadores$cnae20subclas %in% c(subclasses_alvo_SECAO_B, 
                                                                      subclasses_alvo_SECAO_C
     ), ]
  
      saveRDS(estoque_trabalhadores, 
           file = "./D_Lake/Novo_Caged_estoque_trabalhadores_secoes_CNAE23_B_C_202204.RDS")
   
   
#    Estoques oriundos do PAINEL PDET ----
      
      
   # Alternativa: PAINEL PDET----
   # estoques_painel <- 
   #   read.table('D:/Users/humberto.serna/Documents/D_Lake/Novo_Caged_Painel_PDET_Estoques_202112.csv',
   #   header = TRUE, sep = ";", dec = ",", 
   #   #quote = "",
   #   colClasses = 
   #     c("character","integer","character","character","character",
   #       "integer","character","integer","integer","integer"), 
   #   stringsAsFactors = FALSE,
   #   encoding = "ANSI")
   #    
   # estoque_trabalhadores <-
   #    estoques_painel[estoques_painel$CNAE.2.0.Subclasse %in% 
   #                      c(subclasses_alvo_SECAO_B, 
   #                        subclasses_alvo_SECAO_C), ]
   # 
   # saveRDS(estoque_trabalhadores, 
   #         file = "./D_Lake/Novo_Caged_estoque_PAINEL_PDET_2020_2021_secoes_CNAE23_B_C202112.RDATA")
   # 
   # 

  
  


# *************  AJUSTE ESTOQUE MUNIC?PIO ************* ----
# #   rm(list = ls())
# options(editor = 'notepad')
# library(tidyverse)
# library(lubridate)
# 
# 
# # CARREGAMENTO DOS MICRODADOS E PREPARA??O DA BASE
# 
# # CNAE 2.0 e 2.3
# 
# source('D:/Users/humberto.serna/Documents/D_Lake/RAIS_CAGED/CNAE_2_3.R')
# 
# cnae <- 
#   CNAE_Subclasses_2_0
# 
# 
# geocod <- 
#   read.table(file = "./D_Lake/GeoCodigos_IBGE.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE, 
#              colClasses = "character", encoding = 'iso-8859-1', quote = "")
# geocod$GEOCOD <- 
#   str_extract(geocod$GEOCOD, "......")
# 
# geocod$GEOCOD <- 
#   as.integer(geocod$GEOCOD)
# 
# 
# # _____ ANO 2020
# 
# # carregamento
# EstoqueRefCaged2020_01_01 <- 
#   read.table("./D_Lake/RAIS_CAGED/Novo_Caged/EstoqueRefCaged2020_01_01.txt",
#              header = TRUE, sep = ";")
# 
# # agrega??o subclasse
# Estoque_df <- 
#   summarise(
#     group_by(
#       EstoqueRefCaged2020_01_01,cnae20subclas), 
#     'estoqueref_SUM(subclas)' = sum(estoqueref, na.rm = T))
# 
# EstoqueRefCaged2020_01_01 <- 
#   left_join(EstoqueRefCaged2020_01_01, Estoque_df, by = c('cnae20subclas'))
# 
# # base mais Atual (mas sem detalhamento por munic?pio)
# EstoqueRefCaged <- 
#   #read.table("./D_Lake/RAIS_CAGED/Novo_Caged/EstoqueRefCaged.csv",header = TRUE, sep = ";")
#   read.table("./D_Lake/RAIS_CAGED/Novo_Caged/Novo_Caged_Painel_Estoques.csv",header = TRUE, sep = ";")
# 
# 
# # uni?o
# EstoqueRefCaged2020_01_01 <- 
#   left_join(EstoqueRefCaged2020_01_01, 
#             EstoqueRefCaged[EstoqueRefCaged$data == "2020.01.01",], 
#             by = c('cnae20subclas'))
# 
# # exclus?o das subclasses not Ind?stria
# EstoqueRefCaged2020_01_01 <-
#   EstoqueRefCaged2020_01_01[is.na(EstoqueRefCaged2020_01_01$estoqueref.y) == FALSE, c(-5)]
# 
# # porpor??o munic?pio-subclasse
# EstoqueRefCaged2020_01_01$propor??o <- 
#   EstoqueRefCaged2020_01_01$estoqueref.x / 
#   EstoqueRefCaged2020_01_01$`estoqueref_SUM(subclas)`
# 
# # Estoque Ajustado por porpor??o munic?pio-subclasse
# EstoqueRefCaged2020_01_01$estoqueref_ajustado <-
#   round(
#     EstoqueRefCaged2020_01_01$estoqueref.y * 
#       EstoqueRefCaged2020_01_01$propor??o, digits = 0)
# 
# 
# CNAE_Subclasses_2_0$subclasse <- 
#   as.integer(CNAE_Subclasses_2_0$subclasse)
# 
# 
# EstoqueRefCaged2020_01_01 <- 
#   left_join(EstoqueRefCaged2020_01_01, 
#             CNAE_Subclasses_2_0, 
#             by = c('cnae20subclas' = 'subclasse'))
# 
# 
# EstoqueRefCaged2020_01_01 <- 
#   left_join(EstoqueRefCaged2020_01_01, geocod, by = c("codmun.x" = "GEOCOD"))
# # 
# # EstoqueRefCaged2020_01_01 <- 
# #   EstoqueRefCaged2020_01_01[,c("data", "cnae20subclas","subclasse.descri??o", "estoqueref.x", "estoqueref_ajustado", "estoqueref_SUM(subclas)", 
# #                                "estoqueref.y", "propor??o", "UF_sigla", "Munic?pio")]
# # 
# 
# 
# 
# # _____ ANO 2021
# 
# # carregamento
# EstoqueRefCaged2021_01_01 <- 
#   read.table("./D_Lake/RAIS_CAGED/Novo_Caged/EstoqueRefCaged2021_01_01.txt",
#              header = TRUE, sep = ";")
# 
# # agrega??o subclasse
# Estoque_df <- 
#   summarise(
#     group_by(
#       EstoqueRefCaged2021_01_01,cnae20subclas), 
#     'estoqueref_SUM(subclas)' = sum(estoqueref, na.rm = T))
# 
# EstoqueRefCaged2021_01_01 <- 
#   left_join(EstoqueRefCaged2021_01_01, Estoque_df, by = c('cnae20subclas'))
# 
# # base mais Atual (mas sem detalhamento por munic?pio)
# EstoqueRefCaged <- 
#   read.table("./D_Lake/RAIS_CAGED/Novo_Caged/EstoqueRefCaged.csv",
#              header = TRUE, sep = ";")
# 
# # uni?o
# EstoqueRefCaged2021_01_01 <- 
#   left_join(EstoqueRefCaged2021_01_01, 
#             EstoqueRefCaged[EstoqueRefCaged$data == "2021.01.01",], 
#             by = c('cnae20subclas'))
# 
# # exclus?o das subclasses not Ind?stria
# EstoqueRefCaged2021_01_01 <-
#   EstoqueRefCaged2021_01_01[is.na(EstoqueRefCaged2021_01_01$estoqueref.y) == FALSE, c(-5)]
# 
# # porpor??o munic?pio-subclasse
# EstoqueRefCaged2021_01_01$propor??o <- 
#   EstoqueRefCaged2021_01_01$estoqueref.x / 
#   EstoqueRefCaged2021_01_01$`estoqueref_SUM(subclas)`
# 
# # Estoque Ajustado por porpor??o munic?pio-subclasse
# EstoqueRefCaged2021_01_01$estoqueref_ajustado <-
#   round(
#     EstoqueRefCaged2021_01_01$estoqueref.y * 
#       EstoqueRefCaged2021_01_01$propor??o, digits = 0)
# 
# 
# CNAE_Subclasses_2_0$subclasse <- 
#   as.integer(CNAE_Subclasses_2_0$subclasse)
# 
# 
# EstoqueRefCaged2021_01_01 <- 
#   left_join(EstoqueRefCaged2021_01_01, 
#             CNAE_Subclasses_2_0, 
#             by = c('cnae20subclas' = 'subclasse'))
# 
# 
# EstoqueRefCaged2021_01_01 <- 
#   left_join(EstoqueRefCaged2021_01_01, geocod, by = c("codmun.x" = "GEOCOD"))
# 
# # EstoqueRefCaged2021_01_01 <- 
# #   EstoqueRefCaged2021_01_01[,c("data", "cnae20subclas","subclasse.descri??o", "estoqueref.x", "estoqueref_ajustado", "estoqueref_SUM(subclas)", 
# #                                "estoqueref.y", "propor??o", "UF_sigla", "Munic?pio")]
# 
# 
# # Unindo ambos anos
# 
# EstoqueRefNovoCaged <- 
#   rbind(EstoqueRefCaged2020_01_01, EstoqueRefCaged2021_01_01)
# 
# 
# 
# # delimitando pelas subclasses alvo das se??es B e C
# 
# 
# # _____ SUBCLASSES alvo na se??o B (Extrativa Mineral - Exceto petr?leo & G?s)
# subclasses_alvo_SECAO_B <-                    
#   c(#subclasse  denomina??o
#     "500301",	#Extra??o de carv?o mineral
#     "500302",	#Beneficiamento de carv?o mineral
#     "710301",	#Extra??o de min?rio de ferro
#     "710302",	#Pelotiza??o, sinteriza??o e outros beneficiamentos de min?rio de ferro
#     "721901",	#Extra??o de min?rio de alum?nio
#     "721902",	#Beneficiamento de min?rio de alum?nio
#     "722701",	#Extra??o de min?rio de estanho
#     "722702",	#Beneficiamento de min?rio de estanho
#     "723501",	#Extra??o de min?rio de mangan?s
#     "723502",	#Beneficiamento de min?rio de mangan?s
#     "724301",	#Extra??o de min?rio de metais preciosos
#     "724302",	#Beneficiamento de min?rio de metais preciosos
#     "725100",	#Extra??o de minerais radioativos
#     "729401",	#Extra??o de min?rios de ni?bio e tit?nio
#     "729402",	#Extra??o de min?rio de tungst?nio
#     "729403",	#Extra??o de min?rio de n?quel
#     "729404",	#Extra??o de min?rios de cobre, chumbo, zinco e outros minerais met?licos n?o ferrosos n?o especificados anteriormente
#     "729405",	#Beneficiamento de min?rios de cobre, chumbo, zinco e outros minerais met?licos n?o ferrosos n?o especificados anteriormente
#     "810001",	#Extra??o de ard?sia e beneficiamento associado
#     "810002",	#Extra??o de granito e beneficiamento associado
#     "810003",	#Extra??o de m?rmore e beneficiamento associado
#     "810004",	#Extra??o de calc?rio e dolomita e beneficiamento associado
#     "810005",	#Extra??o de gesso e caulim
#     "810006",	#Extra??o de areia, cascalho ou pedregulho e beneficiamento associado
#     "810007",	#Extra??o de argila e beneficiamento associado
#     "810008",	#Extra??o de saibro e beneficiamento associado
#     "810009",	#Extra??o de basalto e beneficiamento associado
#     "810010",	#Beneficiamento de gesso e caulim associado ? extra??o
#     "810099",	#Extra??o e britamento de pedras e outros materiais para constru??o e beneficiamento associado
#     "891600",	#Extra??o de minerais para fabrica??o de adubos, fertilizantes e outros produtos qu?micos
#     "892401",	#Extra??o de sal marinho
#     "892402",	#Extra??o de salgema
#     "892403",	#Refino e outros tratamentos do sal
#     "893200",	#Extra??o de gemas (pedras preciosas e semipreciosas)
#     "899101",	#Extra??o de grafita
#     "899102",	#Extra??o de quartzo
#     "899103",	#Extra??o de amianto
#     "899199",	#Extra??o de outros minerais n?o met?licos n?o especificados anteriormente
#     "990401",	#Atividades de apoio ? extra??o de min?rio de ferro
#     "990402",	#Atividades de apoio ? extra??o de minerais met?licos n?o ferrosos
#     "990403"	#Atividades de apoio ? extra??o de minerais n?o met?licos
#   )
# 
# 
# # _____ Lista classes Alvo na Se??o C (Ind?stria de Transforma??o Relacionada n?o associada/consecutiva ? minera??o)
# 
# classes_alvo_SECAO_C <- 
#   c("20126",	        #          Fabrica??o de Intermedi?rios para Fertilizantes 
#     "20134",		          #          Fabrica??o de Adubos e Fertilizantes 
#     "23206",		          #          Fabrica??o de Cimento 
#     "23303",		          #          Fabrica??o de Artefatos de Concreto, Cimento, Fibrocimento, Gesso e Materiais Semelhantes 
#     "23419",		          #          Fabrica??o de Produtos Cer?micos Refrat?rios 
#     "23427",		          #          Fabrica??o de Produtos Cer?micos N?oRefrat?rios para Uso Estrutural na Constru??o 
#     "23494",		          #          Fabrica??o de Produtos Cer?micos N?oRefrat?rios n?o Especificados Anteriormente 
#     "23915",		          #          Aparelhamento e Outros Trabalhos em Pedras 
#     "23923",		          #          Fabrica??o de Cal e Gesso 
#     "23991",		          #          Fabrica??o de Produtos de Minerais N?oMet?licos n?o Especificados Anteriormente 
#     "24113",		          #          Produ??o de FerroGusa 
#     "24121",		          #          Produ??o de Ferroligas 
#     "24211",		          #          Produ??o de SemiAcabados de A?o 
#     "24229",		          #          Produ??o de Laminados Planos de A?o 
#     "24237",		          #          Produ??o de Laminados Longos de A?o 
#     "24245",		          #          Produ??o de Relaminados, Trefilados e Perfilados de A?o 
#     "24318",		          #          Produ??o de Tubos de A?o com Costura 
#     "24393",		          #          Produ??o de Outros Tubos de Ferro e A?o 
#     "24415",		          #          Metalurgia do Alum?nio e Suas Ligas 
#     "24423",		          #          Metalurgia dos Metais Preciosos 
#     "24431",		          #          Metalurgia do Cobre 
#     "24491",		          #          Metalurgia dos Metais N?oFerrosos e Suas Ligas n?o Especificados Anteriormente 
#     "24512",		          #          Fundi??o de Ferro e A?o 
#     "25314",		          #          Produ??o de forjados de a?o e de metais n?oferrosos e suas ligas 
#     "24521",		          #          Fundi??o de Metais N?oFerrosos e Suas Ligas 
#     "32116")		          #          Lapida??o de Gemas e Fabrica??o de Artefatos de Ourivesaria e Joalheria 
# 
# 
# # __________ SUBCLASSES alvo na Se??o C
# 
# subclasses_alvo_SECAO_C <- 
#   unique(cnae[cnae$classe %in% classes_alvo_SECAO_C, c("subclasse")])
# 
# 
# 
# EstoqueRefNovoCaged <-
#   EstoqueRefNovoCaged[EstoqueRefNovoCaged$cnae20subclas %in% c(subclasses_alvo_SECAO_B, 
#                                                                subclasses_alvo_SECAO_C), ]
# 
# # 
# saveRDS(EstoqueRefNovoCaged, 
#         file = "./D_Lake/Novo_Caged_NovoCaged_2020_2021_secoes_CNAE23_B_C_Municipio_Ajustado.RDATA")
# 
# 
# 
# 
