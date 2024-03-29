#  rm(list = ls())
options(editor = 'notepad')
library(tidyverse)
library(lubridate)

# Geocod e jun��o coluna de munic�pios ----
geocod <- 
  read.table(file = "./CSV_Data/GeoCodigos_IBGE.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE, 
             colClasses = "character", encoding = 'iso-8859-1', quote = "")
geocod$GEOCOD <- 
  str_extract(geocod$GEOCOD, "......")

# source(file = 'D:/Users/humberto.serna/Documents/CSV_Data/RAIS_CAGED/Novo_Caged_Estoque.R')

movimentacao <- 
  readRDS(file = "./CSV_Data/Novo_Caged_microdados_2020_movimentacao_CNAE23_B_C.RDATA")
  
  # readRDS(file = "./CSV_Data/Novo_Caged_microdados_2020_secoes_CNAE23_B_C.RDATA")

# estabelecimento <- 
#  readRDS(file = "./CSV_Data/Novo_Caged_microdados_ESTABELECIMENTOS_2020_secoes_CNAE23_B_C.RDATA")


# CUIDADO ! ATEN��O! NOS MICRODADOS A SUBCLASSE EST� SEM '0' NA FRENTE. SUBCLASSES da Se��o B com 6 d�gitos. E na C, com 7 ....

#___Grupo	denomina��o_______________________________________________________________
#                                                                                   |
#		05.0	Extra��o de carv�o mineral													                      |
#		07.1	Extra��o de min�rio de ferro  												                    |
#		07.2	Extra��o de minerais met�licos n�o ferrosos									              |
#		08.1	Extra��o de pedra, areia e argila											                    |
#		08.9	Extra��o de outros minerais n�o met�licos									                |
#		09.9	Atividades de apoio � extra��o de minerais, exceto petr�leo e g�s natural	|
#___________________________________________________________________________________|


source('./CSV_Data/RAIS_CAGED/CNAE_2_3.R')
# _____ SUBCLASSES alvo na se��o B (Extrativa Mineral - Exceto petr�leo & G�s) ----
subclasses_alvo_SECAO_B <-                    
  c(#subclasse  denomina��o
    "500301",	#Extra��o de carv�o mineral
    "500302",	#Beneficiamento de carv�o mineral
    "710301",	#Extra��o de min�rio de ferro
    "710302",	#Pelotiza��o, sinteriza��o e outros beneficiamentos de min�rio de ferro
    "721901",	#Extra��o de min�rio de alum�nio
    "721902",	#Beneficiamento de min�rio de alum�nio
    "722701",	#Extra��o de min�rio de estanho
    "722702",	#Beneficiamento de min�rio de estanho
    "723501",	#Extra��o de min�rio de mangan�s
    "723502",	#Beneficiamento de min�rio de mangan�s
    "724301",	#Extra��o de min�rio de metais preciosos
    "724302",	#Beneficiamento de min�rio de metais preciosos
    "725100",	#Extra��o de minerais radioativos
    "729401",	#Extra��o de min�rios de ni�bio e tit�nio
    "729402",	#Extra��o de min�rio de tungst�nio
    "729403",	#Extra��o de min�rio de n�quel
    "729404",	#Extra��o de min�rios de cobre, chumbo, zinco e outros minerais met�licos n�o ferrosos n�o especificados anteriormente
    "729405",	#Beneficiamento de min�rios de cobre, chumbo, zinco e outros minerais met�licos n�o ferrosos n�o especificados anteriormente
    "810001",	#Extra��o de ard�sia e beneficiamento associado
    "810002",	#Extra��o de granito e beneficiamento associado
    "810003",	#Extra��o de m�rmore e beneficiamento associado
    "810004",	#Extra��o de calc�rio e dolomita e beneficiamento associado
    "810005",	#Extra��o de gesso e caulim
    "810006",	#Extra��o de areia, cascalho ou pedregulho e beneficiamento associado
    "810007",	#Extra��o de argila e beneficiamento associado
    "810008",	#Extra��o de saibro e beneficiamento associado
    "810009",	#Extra��o de basalto e beneficiamento associado
    "810010",	#Beneficiamento de gesso e caulim associado � extra��o
    "810099",	#Extra��o e britamento de pedras e outros materiais para constru��o e beneficiamento associado
    "891600",	#Extra��o de minerais para fabrica��o de adubos, fertilizantes e outros produtos qu�micos
    "892401",	#Extra��o de sal marinho
    "892402",	#Extra��o de salgema
    "892403",	#Refino e outros tratamentos do sal
    "893200",	#Extra��o de gemas (pedras preciosas e semipreciosas)
    "899101",	#Extra��o de grafita
    "899102",	#Extra��o de quartzo
    "899103",	#Extra��o de amianto
    "899199",	#Extra��o de outros minerais n�o met�licos n�o especificados anteriormente
    "990401",	#Atividades de apoio � extra��o de min�rio de ferro
    "990402",	#Atividades de apoio � extra��o de minerais met�licos n�o ferrosos
    "990403"	#Atividades de apoio � extra��o de minerais n�o met�licos
  )


# _____ Lista classes Alvo na Se��o C (Ind�stria de Transforma��o Relacionada n�o associada/consecutiva � minera��o) ----

classes_alvo_SECAO_C <- 
  c("20126",	        #          Fabrica��o de Intermedi�rios para Fertilizantes 
    "20134",		          #          Fabrica��o de Adubos e Fertilizantes 
    "23206",		          #          Fabrica��o de Cimento 
    "23303",		          #          Fabrica��o de Artefatos de Concreto, Cimento, Fibrocimento, Gesso e Materiais Semelhantes 
    "23419",		          #          Fabrica��o de Produtos Cer�micos Refrat�rios 
    "23427",		          #          Fabrica��o de Produtos Cer�micos N�oRefrat�rios para Uso Estrutural na Constru��o 
    "23494",		          #          Fabrica��o de Produtos Cer�micos N�oRefrat�rios n�o Especificados Anteriormente 
    "23915",		          #          Aparelhamento e Outros Trabalhos em Pedras 
    "23923",		          #          Fabrica��o de Cal e Gesso 
    "23991",		          #          Fabrica��o de Produtos de Minerais N�oMet�licos n�o Especificados Anteriormente 
    "24113",		          #          Produ��o de FerroGusa 
    "24121",		          #          Produ��o de Ferroligas 
    "24211",		          #          Produ��o de SemiAcabados de A�o 
    "24229",		          #          Produ��o de Laminados Planos de A�o 
    "24237",		          #          Produ��o de Laminados Longos de A�o 
    "24245",		          #          Produ��o de Relaminados, Trefilados e Perfilados de A�o 
    "24318",		          #          Produ��o de Tubos de A�o com Costura 
    "24393",		          #          Produ��o de Outros Tubos de Ferro e A�o 
    "24415",		          #          Metalurgia do Alum�nio e Suas Ligas 
    "24423",		          #          Metalurgia dos Metais Preciosos 
    "24431",		          #          Metalurgia do Cobre 
    "24491",		          #          Metalurgia dos Metais N�oFerrosos e Suas Ligas n�o Especificados Anteriormente 
    "24512",		          #          Fundi��o de Ferro e A�o 
    "25314",		          #          Produ��o de forjados de a�o e de metais n�oferrosos e suas ligas 
    "24521",		          #          Fundi��o de Metais N�oFerrosos e Suas Ligas 
    "32116")		          #          Lapida��o de Gemas e Fabrica��o de Artefatos de Ourivesaria e Joalheria 


# __________ SUBCLASSES alvo na Se��o C ----

subclasses_alvo_SECAO_C <- 
  cnae[cnae$classe %in% classes_alvo_SECAO_C & cnae$subclasse != 0, c("subclasse")]



# _____________________________________________________________----

# EXTRATIVA MINERAL ----


# _____ delimitando df por subclasses alvo na se��o B (Extrativa Mineral - Exceto petr�leo & G�s) ----

  #       df_Extrativa <- estabelecimento[estabelecimento$subclasse %in% subclasses_alvo_SECAO_B, ]
         df_Extrativa <- movimentacao[movimentacao$subclasse %in% subclasses_alvo_SECAO_B, ]
          


# _____ saldo_movimenta��o ----
df_Extrativa_resultado <-
  summarise(
    group_by(df_Extrativa[, c(
                              #      "compet�ncia",
                              #      "regi�o",
                                    "UF_sigla",
                                    "Munic�pio",
                                    "se��o",
                                    "grupo",
                                    "classe",
                                    "subclasse",
                                    "saldomovimenta��o",
                                    "denomina��o" ,
                              #      "cbo2002ocupa��o",
                              #      "categoria",
                              #      "graudeinstru��o",
                              #      "idade",
                              #      "horascontratuais",
                              #      "ra�acor",
                              #      "sexo",
                              #      "tipoempregador",
                              #      "tipoestabelecimento",
                              #      "tipomovimenta��o",
                              #      "tipodedefici�ncia",
                              #      "indtrabintermitente",
                              #      "indtrabparcial",
                                      "sal�rio"  #,   
                              #      "tamestabjan",
                              #      "indicadoraprendiz",
                              #      "fonte"
                                  )],
                #     compet�ncia,
                #     regi�o,
                     uf,
                     munic�pio,
                #     se��o,
                     grupo  ,
                     classe,
                     subclasse     #,
                #     cbo2002ocupa��o,
                #     categoria,
                #     graudeinstru��o,
                #     horascontratuais,
                #     ra�acor,
                #     sexo,
                #     tipoempregador,
                #     tipoestabelecimento,
                #     tipomovimenta��o,
                #     tipodedefici�ncia,
                #     indtrabintermitente,
                #     indtrabparcial,
                #     tamestabjan,
                #     indicadoraprendiz,
                #     fonte
                      ),
  "Saldo_da_Movimenta��o" = sum(saldomovimenta��o, na.rm = TRUE),
  "sal�rio_mediana" = median(sal�rio, na.rm = TRUE),
  "sal�rio_medio" = mean(sal�rio, na.rm = TRUE))


                #: exclua-me
df_Extrativa_uf_grupo <-  # df_Extrativa_resultado      
  left_join(left_join(df_Extrativa_uf_grupo, unique(geocod[, c("Cod_UF", "UF_sigla")]), by = c("uf" = "Cod_UF")),
            cnae[cnae$grupo != 0 & cnae$classe == 0 & cnae$subclasse == 0, c("grupo", "denomina��o")], # grupo
            by = "grupo")

df_Extrativa_municipio_grupo <- # df_Extrativa_resultado 
  left_join(left_join(df_Extrativa_municipio_grupo, geocod[, c("GEOCOD", "Munic�pio", "UF_sigla")], by = c("munic�pio" = "GEOCOD")),
            cnae[cnae$grupo != 0 & cnae$classe == 0 & cnae$subclasse == 0, c("grupo", "denomina��o")], # grupo
            by = "grupo")

df_Extrativa_municipio_subclasse <- # df_Extrativa_resultado
  left_join(left_join(df_Extrativa_municipio_subclasse, geocod[, c("GEOCOD", "Munic�pio", "UF_sigla")], by = c("munic�pio" = "GEOCOD")),
            cnae[cnae$subclasse != 0, c("subclasse", "denomina��o")],  # subclasses cnae
            by = c("subclasse" = "subclasse"))

df_Extrativa_uf_subclasse <- #  df_Extrativa_resultado 
  left_join(left_join(df_Extrativa_uf_subclasse, unique(geocod[, c("Cod_UF", "UF_sigla")]), by = c("uf" = "Cod_UF")),
            cnae[cnae$subclasse != 0, c("subclasse", "denomina��o")],  # subclasses cnae
            by = c("subclasse" = "subclasse"))

df_Extrativa_BR_grupo <-  # df_Extrativa_resultado 
  left_join(df_Extrativa_BR_grupo, 
            cnae[cnae$grupo != 0 & cnae$classe == 0 & cnae$subclasse == 0, c("grupo", "denomina��o")], # grupo
            by = "grupo")

df_Extrativa_BR_classe <-  # df_Extrativa_resultado 
  left_join(df_Extrativa_BR_classe, 
            cnae[cnae$classe != 0 & cnae$subclasse == 0, c("classe", "denomina��o")], # classe
            by = "classe")


rbind(df_Extrativa_BR_classe, df_Extrativa_resultado, BR)



BR <- 
summarise(movimentacao, 
          'Saldo_da_Movimenta��o' = sum(saldomovimenta��o), 
          "sal�rio_mediana" = median(sal�rio, na.rm = TRUE),
          "sal�rio_medio" = mean(sal�rio, na.rm = TRUE))



# IND�STRIA DE TRANSFORMA��O (MINERAL) ----
    




# _____ delimitando df por subclasses alvo na Se��o C ----

#         df_Transformacao <- estabelecimentos[estabelecimentos$subclasse %in% subclasses_alvo, ]
df_Transformacao <- movimentacao[movimentacao$subclasse %in% subclasses_alvo_SECAO_C, ]


# _____ saldo_movimenta��o ----
df_Transformacao_resultado <-
  summarise(
    group_by(df_Transformacao[df_Transformacao$compet�ncia != "202007", c(
      #      "compet�ncia",
      #      "regi�o",
      "uf",
      "munic�pio",
      "se��o",
      "grupo",
      "classe",
      "subclasse",
      "saldomovimenta��o",
      #      "cbo2002ocupa��o",
      #      "categoria",
      #      "graudeinstru��o",
      #      "idade",
      #      "horascontratuais",
      #      "ra�acor",
      #      "sexo",
      #      "tipoempregador",
      #      "tipoestabelecimento",
      #      "tipomovimenta��o",
      #      "tipodedefici�ncia",
      #      "indtrabintermitente",
      #      "indtrabparcial",
      "sal�rio"  #,   # a base 'Estabelecimentos' n�o consta sal�rio
      #      "tamestabjan",
      #      "indicadoraprendiz",
      #      "fonte"
    )],
    #     compet�ncia,
    #     regi�o,
    #     uf,
    #     munic�pio,
    #     se��o,
    grupo  ,
    #     classe#,
    #     subclasse     #,
    #     cbo2002ocupa��o,
    #     categoria,
    #     graudeinstru��o,
    #     horascontratuais,
    #     ra�acor,
    #     sexo,
    #     tipoempregador,
    #     tipoestabelecimento,
    #     tipomovimenta��o,
    #     tipodedefici�ncia,
    #     indtrabintermitente,
    #     indtrabparcial,
    #     tamestabjan,
    #     indicadoraprendiz,
    #     fonte
    ),
    "Saldo_da_Movimenta��o" = sum(saldomovimenta��o),
    "sal�rio_mediana" = median(sal�rio),
    "sal�rio_medio" = mean(sal�rio))



#: exclua-me
df_Transformacao_uf_grupo <-  # df_Transformacao_resultado      
  left_join(left_join(df_Transformacao_uf_grupo, unique(geocod[, c("Cod_UF", "UF_sigla")]), by = c("uf" = "Cod_UF")),
            cnae[cnae$grupo != 0 & cnae$classe == 0 & cnae$subclasse == 0, c("grupo", "denomina��o")], # grupo
            by = "grupo")

df_Transformacao_municipio_grupo <- # df_Transformacao_resultado 
  left_join(left_join(df_Transformacao_municipio_grupo, geocod[, c("GEOCOD", "Munic�pio", "UF_sigla")], by = c("munic�pio" = "GEOCOD")),
            cnae[cnae$grupo != 0 & cnae$classe == 0 & cnae$subclasse == 0, c("grupo", "denomina��o")], # grupo
            by = "grupo")

df_Transformacao_municipio_subclasse <- # df_Transformacao_resultado
  left_join(left_join(df_Transformacao_municipio_subclasse, geocod[, c("GEOCOD", "Munic�pio", "UF_sigla")], by = c("munic�pio" = "GEOCOD")),
            cnae[cnae$subclasse != 0, c("subclasse", "denomina��o")],  # subclasses cnae
            by = c("subclasse" = "subclasse"))

df_Transformacao_uf_subclasse <- #  df_Transformacao_resultado 
  left_join(left_join(df_Transformacao_uf_subclasse, unique(geocod[, c("Cod_UF", "UF_sigla")]), by = c("uf" = "Cod_UF")),
            cnae[cnae$subclasse != 0, c("subclasse", "denomina��o")],  # subclasses cnae
            by = c("subclasse" = "subclasse"))

df_Transformacao_BR_grupo <-  # df_Transformacao_resultado 
  left_join(df_Transformacao_BR_grupo, 
            cnae[cnae$grupo != 0 & cnae$classe == 0 & cnae$subclasse == 0, c("grupo", "denomina��o")], # grupo
            by = "grupo")

df_Transformacao_BR_classe <-  # df_Transformacao_resultado 
  left_join(df_Transformacao_BR_classe, 
            cnae[cnae$classe != 0 & cnae$subclasse == 0, c("classe", "denomina��o")], # classe
            by = "classe")


rbind(df_Transformacao_BR_classe, df_Transformacao_resultado, BR)



BR <- 
  summarise(movimentacao, 
            'Saldo_da_Movimenta��o' = sum(saldomovimenta��o), 
            "sal�rio_mediana" = median(sal�rio, na.rm = TRUE),
            "sal�rio_medio" = mean(sal�rio, na.rm = TRUE))



# Salvar
                        write.table(
  df_BR_classe          ,file = paste("D:/Users/humberto.serna/Desktop/GEMI/RAIS_CAGED/", 
 "df_BR_classe"
                        ,".csv", sep = "") , sep = ";",dec = ",",row.names = FALSE)

                        
# *********************************************************** ----
# *********************************************************** ----
#  AP�NDICE: CARREGAMENTO DOS MICRODADOS E PREPARA��O DA BASE ----

 # CNAE 2.3 ----
 
  source('./CSV_Data/RAIS_CAGED/CNAE_2_3.R')
 

 # _____ SUBCLASSES alvo na se��o B (Extrativa Mineral - Exceto petr�leo & G�s) ----
 subclasses_alvo_SECAO_B <-                    
   c(#subclasse  denomina��o
     "500301",	#Extra��o de carv�o mineral
     "500302",	#Beneficiamento de carv�o mineral
     "710301",	#Extra��o de min�rio de ferro
     "710302",	#Pelotiza��o, sinteriza��o e outros beneficiamentos de min�rio de ferro
     "721901",	#Extra��o de min�rio de alum�nio
     "721902",	#Beneficiamento de min�rio de alum�nio
     "722701",	#Extra��o de min�rio de estanho
     "722702",	#Beneficiamento de min�rio de estanho
     "723501",	#Extra��o de min�rio de mangan�s
     "723502",	#Beneficiamento de min�rio de mangan�s
     "724301",	#Extra��o de min�rio de metais preciosos
     "724302",	#Beneficiamento de min�rio de metais preciosos
     "725100",	#Extra��o de minerais radioativos
     "729401",	#Extra��o de min�rios de ni�bio e tit�nio
     "729402",	#Extra��o de min�rio de tungst�nio
     "729403",	#Extra��o de min�rio de n�quel
     "729404",	#Extra��o de min�rios de cobre, chumbo, zinco e outros minerais met�licos n�o ferrosos n�o especificados anteriormente
     "729405",	#Beneficiamento de min�rios de cobre, chumbo, zinco e outros minerais met�licos n�o ferrosos n�o especificados anteriormente
     "810001",	#Extra��o de ard�sia e beneficiamento associado
     "810002",	#Extra��o de granito e beneficiamento associado
     "810003",	#Extra��o de m�rmore e beneficiamento associado
     "810004",	#Extra��o de calc�rio e dolomita e beneficiamento associado
     "810005",	#Extra��o de gesso e caulim
     "810006",	#Extra��o de areia, cascalho ou pedregulho e beneficiamento associado
     "810007",	#Extra��o de argila e beneficiamento associado
     "810008",	#Extra��o de saibro e beneficiamento associado
     "810009",	#Extra��o de basalto e beneficiamento associado
     "810010",	#Beneficiamento de gesso e caulim associado � extra��o
     "810099",	#Extra��o e britamento de pedras e outros materiais para constru��o e beneficiamento associado
     "891600",	#Extra��o de minerais para fabrica��o de adubos, fertilizantes e outros produtos qu�micos
     "892401",	#Extra��o de sal marinho
     "892402",	#Extra��o de salgema
     "892403",	#Refino e outros tratamentos do sal
     "893200",	#Extra��o de gemas (pedras preciosas e semipreciosas)
     "899101",	#Extra��o de grafita
     "899102",	#Extra��o de quartzo
     "899103",	#Extra��o de amianto
     "899199",	#Extra��o de outros minerais n�o met�licos n�o especificados anteriormente
     "990401",	#Atividades de apoio � extra��o de min�rio de ferro
     "990402",	#Atividades de apoio � extra��o de minerais met�licos n�o ferrosos
     "990403"	#Atividades de apoio � extra��o de minerais n�o met�licos
   )
 
 
 # _____ Lista classes Alvo na Se��o C (Ind�stria de Transforma��o Relacionada n�o associada/consecutiva � minera��o) ----
 
 classes_alvo_SECAO_C <- 
   c("20126",	        #          Fabrica��o de Intermedi�rios para Fertilizantes 
     "20134",		          #          Fabrica��o de Adubos e Fertilizantes 
     "23206",		          #          Fabrica��o de Cimento 
     "23303",		          #          Fabrica��o de Artefatos de Concreto, Cimento, Fibrocimento, Gesso e Materiais Semelhantes 
     "23419",		          #          Fabrica��o de Produtos Cer�micos Refrat�rios 
     "23427",		          #          Fabrica��o de Produtos Cer�micos N�oRefrat�rios para Uso Estrutural na Constru��o 
     "23494",		          #          Fabrica��o de Produtos Cer�micos N�oRefrat�rios n�o Especificados Anteriormente 
     "23915",		          #          Aparelhamento e Outros Trabalhos em Pedras 
     "23923",		          #          Fabrica��o de Cal e Gesso 
     "23991",		          #          Fabrica��o de Produtos de Minerais N�oMet�licos n�o Especificados Anteriormente 
     "24113",		          #          Produ��o de FerroGusa 
     "24121",		          #          Produ��o de Ferroligas 
     "24211",		          #          Produ��o de SemiAcabados de A�o 
     "24229",		          #          Produ��o de Laminados Planos de A�o 
     "24237",		          #          Produ��o de Laminados Longos de A�o 
     "24245",		          #          Produ��o de Relaminados, Trefilados e Perfilados de A�o 
     "24318",		          #          Produ��o de Tubos de A�o com Costura 
     "24393",		          #          Produ��o de Outros Tubos de Ferro e A�o 
     "24415",		          #          Metalurgia do Alum�nio e Suas Ligas 
     "24423",		          #          Metalurgia dos Metais Preciosos 
     "24431",		          #          Metalurgia do Cobre 
     "24491",		          #          Metalurgia dos Metais N�oFerrosos e Suas Ligas n�o Especificados Anteriormente 
     "24512",		          #          Fundi��o de Ferro e A�o 
     "25314",		          #          Produ��o de forjados de a�o e de metais n�oferrosos e suas ligas 
     "24521",		          #          Fundi��o de Metais N�oFerrosos e Suas Ligas 
     "32116")		          #          Lapida��o de Gemas e Fabrica��o de Artefatos de Ourivesaria e Joalheria 
 
 
 # __________ SUBCLASSES alvo na Se��o C ----
 
 subclasses_alvo_SECAO_C <- 
   cnae[cnae$classe %in% classes_alvo_SECAO_C & cnae$subclasse != 0, c("subclasse")]

#  _____ CARREGAMENTO ESTABELECIMENTOS ****************** ----
  arquivos <- c(
    "CAGEDESTAB202001.txt",
    "CAGEDESTAB202002.txt",
    "CAGEDESTAB202003.txt",
    "CAGEDESTAB202004.txt",
    "CAGEDESTAB202005.txt",
    "CAGEDESTAB202006.txt",
    "CAGEDESTAB202007.txt",
    "CAGEDESTAB202008.txt",
    "CAGEDESTAB202009.txt",
    "CAGEDESTAB202010.txt",
    "CAGEDESTAB202011.txt",
    "CAGEDESTAB202012.txt"
    )
  
  lista <- list()
  for (i in 1:length(arquivos)) {
  lista[[i]] <-
    read.table(
      paste('./CSV_Data/RAIS_CAGED/Novo_Caged/', arquivos[[i]], sep = ""),
      header = TRUE,
      sep = ";",
      dec = ".",
      colClasses = c("character", "character", "character", "character", "character", "integer", "integer", "character", "integer", "character", "character", "character"),
      stringsAsFactors = FALSE,
      encoding = "UTF-8"
    )
  }
  
  estabelecimentos <- 
    do.call("rbind", lista)
  rm(lista)
  
  
  # delimitando pelas subclasses alvo das se��es B e C ----
  
  estabelecimentos <- 
    estabelecimentos[estabelecimentos$subclasse %in% c(subclasses_alvo_SECAO_B, subclasses_alvo_SECAO_C),]
  
  # ____ impondo trimestre
  estabelecimentos$trimestre <-
    lubridate::quarter(
      lubridate::ymd(
        paste(str_extract(estabelecimentos$compet�ncia, pattern = "^...."), str_extract(estabelecimentos$compet�ncia, pattern = "..$"), "1", sep = "_")
      ), with_year = TRUE)
  
  # ____ impondo semestre
  estabelecimentos$semestre <-
    lubridate::semester(
      lubridate::ymd(
        paste(str_extract(estabelecimentos$compet�ncia, pattern = "^...."), str_extract(estabelecimentos$compet�ncia, pattern = "..$"), "1", sep = "_")
      ), with_year = TRUE)
  
  # ____ impondo m�s.ANO
  estabelecimentos$compet�ncia <-
    lubridate::ymd(
      paste(str_extract(estabelecimentos$compet�ncia, pattern = "^...."), str_extract(estabelecimentos$compet�ncia, pattern = "..$"), "1", sep = "_")
    )
  
  
  # _____ jun��o grupo - subclasse ----
  estabelecimentos <- 
    left_join(estabelecimentos, cnae[,c("grupo", "classe", "subclasse", "denomina��o")], by = c("subclasse"))   
  
  
  # Geocod e jun��o coluna de munic�pios ----
  geocod <- 
    read.table(file = "./CSV_Data/GeoCodigos_IBGE.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE, 
               colClasses = "character", encoding = 'iso-8859-1', quote = "")
  geocod$GEOCOD <- 
    str_extract(geocod$GEOCOD, "......")
  
  estabelecimentos <- 
    left_join(estabelecimentos, geocod[, c("UF_sigla", "GEOCOD", "Munic�pio")], by = c("munic�pio" = "GEOCOD"))
  
  
  # CBO - OCUPA��ES ----
  cbo <- 
    read.table(file = "./CSV_Data/RAIS_CAGED/CBO_Ocupacoes/CBO2002 - Ocupacao.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE, 
               colClasses = "character", encoding = 'ANSI', quote = "")
    
  estabelecimentos <- 
    left_join(estabelecimentos, cbo, by = c(cbo2002ocupa��o = CO_CBO))   
  
  # exportando em RDS ---- 
  
  saveRDS(estabelecimentos, file = "./CSV_Data/Novo_Caged_microdados_ESTABELECIMENTOS_2020_secoes_CNAE23_B_C.RDATA")
  
 
 # _______________________________________________________________________
 #   O n�mero de estabelecimentos que apresentam declara��es � Rais       | 
 #   difere ano a ano, o que dificulta discriminar se a varia��o do       |
 #   emprego se deve a um real aumento ou redu��o decorrente da situa��o  | 
 #   do mercado de trabalho e/ou a um melhor desempenho na declara��o.    |
 #   Opte por uma amostra longitudinal de empresas que declaram os dados. |
 # _______________________________________________________________________|
 
 
 # _____ CARREGAMENTO MOVIMENTA�AO ****************** ----
 
 arquivos <- c(
    "CAGEDMOV202001.txt",
    "CAGEDMOV202002.txt",
    "CAGEDMOV202003.txt",
    "CAGEDMOV202004.txt",
    "CAGEDMOV202005.txt",
    "CAGEDMOV202006.txt",
    "CAGEDMOV202007.txt",
    "CAGEDMOV202008.txt",
    "CAGEDMOV202009.txt",
    "CAGEDMOV202010.txt",
    "CAGEDMOV202011.txt",
    "CAGEDMOV202012.txt",
    "CAGEDMOV202101.txt",
    "CAGEDMOV202102.txt",
    "CAGEDMOV202103.txt",
    "CAGEDMOV202104.txt",
    "CAGEDMOV202105.txt",
    "CAGEDMOV202106.txt"
   )
 
 
 lista <- list()
 for (i in 1:length(arquivos)) {
   lista[[i]] <-
     read.table(
       paste('./CSV_Data/RAIS_CAGED/Novo_Caged/', arquivos[[i]], sep = ""),
       header = TRUE,
       sep = ";",
       dec = ".",
       quote = "",
       colClasses = c(
         "character",	#	compet�ncia
         "NULL",	      #	regi�o
         "character",	#	uf
         "character",	#	munic�pio
         "character",	#	se��o
         "integer", 	  #	subclasse
         "integer",	  #	saldomovimenta��o
         "character",	#	cbo2002ocupa��o
         "character",	#	categoria
         "character",	#	graudeinstru��o
         "NULL",	      #	idade
         "NULL",	      #	horascontratuais
         "NULL",	#	ra�acor
         "NULL",	#	sexo
         "character",	#	tipoempregador
         "integer",	#	tipoestabelecimento
         "character",	#	tipomovimenta��o
         "NULL",	#	tipodedefici�ncia
         "NULL",	#	indtrabintermitente
         "NULL",	#	indtrabparcial
         "numeric",	#	sal�rio
         "character",	#	tamestabjan
         "NULL",	#	indicadoraprendiz
         "character"	#	fonte
       ),
       stringsAsFactors = FALSE,
       encoding = "UTF-8"
       )
   
   df <- lista[[i]]
   
   df <- 
     df[df$subclasse %in% c(
       subclasses_alvo_SECAO_B, 
       subclasses_alvo_SECAO_C),]
   
   lista[[i]] <- df
   
   
 }
 
 movimentacao <- 
   do.call("rbind", lista)
 rm(lista)
 
 
 # delimitando pelas subclasses alvo das se��es B e C ----
 
 movimentacao <- 
   movimentacao[movimentacao$subclasse %in% c(
                                              subclasses_alvo_SECAO_B, 
                                              subclasses_alvo_SECAO_C),
                                                                        ]
 
 # ____ impondo trimestre
 movimentacao$trimestre <-
   lubridate::quarter(
     lubridate::ymd(
       paste(str_extract(movimentacao$compet�ncia, pattern = "^...."), str_extract(movimentacao$compet�ncia, pattern = "..$"), "1", sep = "_")
     ), with_year = TRUE)
 
 # ____ impondo semestre
 movimentacao$semestre <-
   lubridate::semester(
     lubridate::ymd(
       paste(str_extract(movimentacao$compet�ncia, pattern = "^...."), str_extract(movimentacao$compet�ncia, pattern = "..$"), "1", sep = "_")
       ), with_year = TRUE)
     
 # ____ impondo m�s.ANO
 movimentacao$compet�ncia <-
   lubridate::ymd(
     paste(str_extract(movimentacao$compet�ncia, pattern = "^...."), str_extract(movimentacao$compet�ncia, pattern = "..$"), "1", sep = "_")
   )
 
 
 # _____ jun��o grupo - subclasse ----
 source('./CSV_Data/RAIS_CAGED/CNAE_2_3.R')
 
 movimentacao <- 
   left_join(movimentacao, cnae[,c("grupo", "classe", "subclasse", "denomina��o")], by = c("subclasse"))   
 
 
 # Geocod e jun��o coluna de munic�pios ----
 geocod <- 
   read.table(file = "./CSV_Data/GeoCodigos_IBGE.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE, 
              colClasses = "character", encoding = 'iso-8859-1', quote = "")
 geocod$GEOCOD <- 
   str_extract(geocod$GEOCOD, "......")
 
 movimentacao <- 
   left_join(movimentacao, geocod[, c("UF_sigla", "GEOCOD", "Munic�pio")], by = c("munic�pio" = "GEOCOD"))
 
 
 # CBO - OCUPA��ES ----
 cbo <- 
   read.table(file = "./CSV_Data/RAIS_CAGED/CBO_Ocupacoes/CBO2002 - Ocupacao.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE, 
              colClasses = "character", encoding = 'ANSI', quote = "")
 
 movimentacao <- 
   left_join(movimentacao, cbo, by = c('cbo2002ocupa��o' = 'CO_CBO')) 
 
 
 
 # Tipo de Movimenta��o ----  
 tipodemovimentacao <- 
   read.table(file = "./CSV_Data/RAIS_CAGED/Novo_Caged/tipomovimenta��o.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE, 
              colClasses = "character", encoding = 'UTF-8', quote = "")
 
 movimentacao <- 
   left_join(movimentacao, tipodemovimentacao, by = c("tipomovimenta��o" = 'Codigo'))
 
 colnames(movimentacao) <- 
   c("compet�ncia", "uf", "munic�pio", "se��o", "subclasse", "saldomovimenta��o", 
     "cbo2002ocupa��o", "categoria", "graudeinstru��o", "tipoempregador", 
     "tipoestabelecimento", "tipomovimenta��o", "sal�rio", "tamestabjan", 
     "fonte", "trimestre", "semestre", "grupo", "classe", "subclasse_denomina��o", 
     "UF_sigla", "Munic�pio", "NO_CBO", "subclasse_Descricao")
 
  
 
 # exportando em RDS ---- 
 
 saveRDS(movimentacao, file = "./CSV_Data/Novo_Caged_microdados_2021_movimentacao_secoes_CNAE23_B_C.RDATA")
 
 
 
 # tela livre 

 summarise(
   group_by(
     movimentacao, 
     semestre),
   sum(saldomovimenta��o),
   mean(sal�rio, na.rm = TRUE, trim = 0.01)) %>% kable()
 
 
#  semestre `sum(saldomovimenta��o)`
#     <dbl>                    <int>
#    2020.                 -1275857
#    2020.                  1418547
 

 # CBO - Eng de Minas
 
lista_CBO <-  
  c( 
 "214705",   #Engenheiro de minas Ocupa��o
 "214710",   #Engenheiro de minas (beneficiamento) Ocupa��o
 "214705",   #Engenheiro de minas (carv�o) Sin�nimo
 "214715",   #Engenheiro de minas (lavra a c�u aberto) Ocupa��o
 "214720",   #Engenheiro de minas (lavra subterr�nea) Ocupa��o
 "214725",   #Engenheiro de minas (pesquisa mineral) Ocupa��o
 "214730",   #Engenheiro de minas (planejamento) Ocupa��o
 "214735",   #Engenheiro de minas (processo) Ocupa��o
 "214740"    #Engenheiro de minas (projeto) Ocupa��o	
         )


movimentacao <-
  movimentacao[movimentacao$cbo2002ocupa��o %in% lista_CBO, ]


tipoestabelecimento <- 
  read.table(file = "./CSV_Data/RAIS_CAGED/Novo_CAGED/tipoestabelecimento.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE, 
             encoding = 'UTF-8', quote = "")


movimentacao <- 
left_join(
  movimentacao,
  tipoestabelecimento,
  by = c("tipoestabelecimento" = "CO_tipoestabelecimento")
)

