#        rm(list = ls())
#     options(editor = 'notepad')
library(tidyverse)
library(lubridate)


readRDS(file = "./CSV_Data/Novo_Caged_microdados_2020_secoes_CNAE23_B_C.RDATA")

# CUIDADO ! ATENÇÃO! OS MICRODADOS ESTÃO COM SUBCLASSE SEM '0' NA FRENTE. SUBCLASSES da Seção B com 6 dígitoa, da C com 7 ....

#___Grupo	denominação_______________________________________________________________
#                                                                                   |
#		05.0	Extração de carvão mineral													                      |
#		07.1	Extração de minério de ferro  												                    |
#		07.2	Extração de minerais metálicos não ferrosos									              |
#		08.1	Extração de pedra, areia e argila											                    |
#		08.9	Extração de outros minerais não metálicos									                |
#		09.9	Atividades de apoio à extração de minerais, exceto petróleo e gás natural	|
#___________________________________________________________________________________|



# _____________________________________________________________----

# EXTRATIVA MINERAL ----


# _____ delimitando df por subclasses alvo na seção B (Extrativa Mineral - Exceto petróleo & Gás) ----

#         df <- estabelecimentos[estabelecimentos$subclasse %in% subclasses_alvo, ]
          df_Extrativa <- movimentacao[movimentacao$subclasse %in% subclasses_alvo_SECAO_B, ]
          


# _____ saldo_movimentação ----
df_Extrativa_resultado <-
  summarise(
    group_by(df_Extrativa[, c(
                              #      "competência",
                              #      "região",
                                    "UF_sigla",
                                    "Município",
                                    "seção",
                                    "grupo",
                                    "classe",
                                    "subclasse",
                                    "saldomovimentação",
                                    "denominação" ,
                              #      "cbo2002ocupação",
                              #      "categoria",
                              #      "graudeinstrução",
                              #      "idade",
                              #      "horascontratuais",
                              #      "raçacor",
                              #      "sexo",
                              #      "tipoempregador",
                              #      "tipoestabelecimento",
                              #      "tipomovimentação",
                              #      "tipodedeficiência",
                              #      "indtrabintermitente",
                              #      "indtrabparcial",
                                      "salário"  #,   
                              #      "tamestabjan",
                              #      "indicadoraprendiz",
                              #      "fonte"
                                  )],
                #     competência,
                #     região,
                #     uf,
                #     município,
                #     seção,
                     grupo  ,
                #     classe#,
                #     subclasse     #,
                #     cbo2002ocupação,
                #     categoria,
                #     graudeinstrução,
                #     horascontratuais,
                #     raçacor,
                #     sexo,
                #     tipoempregador,
                #     tipoestabelecimento,
                #     tipomovimentação,
                #     tipodedeficiência,
                #     indtrabintermitente,
                #     indtrabparcial,
                #     tamestabjan,
                #     indicadoraprendiz,
                #     fonte
                      ),
  "Saldo_da_Movimentação" = sum(saldomovimentação),
  "salário_mediana" = median(salário),
  "salário_medio" = mean(salário))


                #: exclua-me
df_Extrativa_uf_grupo <-  # df_Extrativa_resultado      
  left_join(left_join(df_Extrativa_uf_grupo, unique(geocod[, c("Cod_UF", "UF_sigla")]), by = c("uf" = "Cod_UF")),
            cnae[cnae$grupo != 0 & cnae$classe == 0 & cnae$subclasse == 0, c("grupo", "denominação")], # grupo
            by = "grupo")

df_Extrativa_municipio_grupo <- # df_Extrativa_resultado 
  left_join(left_join(df_Extrativa_municipio_grupo, geocod[, c("GEOCOD", "Município", "UF_sigla")], by = c("município" = "GEOCOD")),
            cnae[cnae$grupo != 0 & cnae$classe == 0 & cnae$subclasse == 0, c("grupo", "denominação")], # grupo
            by = "grupo")

df_Extrativa_municipio_subclasse <- # df_Extrativa_resultado
  left_join(left_join(df_Extrativa_municipio_subclasse, geocod[, c("GEOCOD", "Município", "UF_sigla")], by = c("município" = "GEOCOD")),
            cnae[cnae$subclasse != 0, c("subclasse", "denominação")],  # subclasses cnae
            by = c("subclasse" = "subclasse"))

df_Extrativa_uf_subclasse <- #  df_Extrativa_resultado 
  left_join(left_join(df_Extrativa_uf_subclasse, unique(geocod[, c("Cod_UF", "UF_sigla")]), by = c("uf" = "Cod_UF")),
            cnae[cnae$subclasse != 0, c("subclasse", "denominação")],  # subclasses cnae
            by = c("subclasse" = "subclasse"))

df_Extrativa_BR_grupo <-  # df_Extrativa_resultado 
  left_join(df_Extrativa_BR_grupo, 
            cnae[cnae$grupo != 0 & cnae$classe == 0 & cnae$subclasse == 0, c("grupo", "denominação")], # grupo
            by = "grupo")

df_Extrativa_BR_classe <-  # df_Extrativa_resultado 
  left_join(df_Extrativa_BR_classe, 
            cnae[cnae$classe != 0 & cnae$subclasse == 0, c("classe", "denominação")], # classe
            by = "classe")


rbind(df_Extrativa_BR_classe, df_Extrativa_resultado, BR)



BR <- 
summarise(movimentacao, 
          'Saldo_da_Movimentação' = sum(saldomovimentação), 
          "salário_mediana" = median(salário, na.rm = TRUE),
          "salário_medio" = mean(salário, na.rm = TRUE))



# INDÚSTRIA DE TRANSFORMAÇÃO (MINERAL) ----
    




# _____ delimitando df por subclasses alvo na Seção C ----

#         df_Transformacao <- estabelecimentos[estabelecimentos$subclasse %in% subclasses_alvo, ]
df_Transformacao <- movimentacao[movimentacao$subclasse %in% subclasses_alvo_SECAO_B, ]


# _____ saldo_movimentação ----
df_Transformacao_resultado <-
  summarise(
    group_by(df_Transformacao[df_Transformacao$competência != "202007", c(
      #      "competência",
      #      "região",
      "uf",
      "município",
      "seção",
      "grupo",
      "classe",
      "subclasse",
      "saldomovimentação",
      #      "cbo2002ocupação",
      #      "categoria",
      #      "graudeinstrução",
      #      "idade",
      #      "horascontratuais",
      #      "raçacor",
      #      "sexo",
      #      "tipoempregador",
      #      "tipoestabelecimento",
      #      "tipomovimentação",
      #      "tipodedeficiência",
      #      "indtrabintermitente",
      #      "indtrabparcial",
      "salário"  #,   # a base 'Estabelecimentos' não consta salário
      #      "tamestabjan",
      #      "indicadoraprendiz",
      #      "fonte"
    )],
    #     competência,
    #     região,
    #     uf,
    #     município,
    #     seção,
    grupo  ,
    #     classe#,
    #     subclasse     #,
    #     cbo2002ocupação,
    #     categoria,
    #     graudeinstrução,
    #     horascontratuais,
    #     raçacor,
    #     sexo,
    #     tipoempregador,
    #     tipoestabelecimento,
    #     tipomovimentação,
    #     tipodedeficiência,
    #     indtrabintermitente,
    #     indtrabparcial,
    #     tamestabjan,
    #     indicadoraprendiz,
    #     fonte
    ),
    "Saldo_da_Movimentação" = sum(saldomovimentação),
    "salário_mediana" = median(salário),
    "salário_medio" = mean(salário))



#: exclua-me
df_Transformacao_uf_grupo <-  # df_Transformacao_resultado      
  left_join(left_join(df_Transformacao_uf_grupo, unique(geocod[, c("Cod_UF", "UF_sigla")]), by = c("uf" = "Cod_UF")),
            cnae[cnae$grupo != 0 & cnae$classe == 0 & cnae$subclasse == 0, c("grupo", "denominação")], # grupo
            by = "grupo")

df_Transformacao_municipio_grupo <- # df_Transformacao_resultado 
  left_join(left_join(df_Transformacao_municipio_grupo, geocod[, c("GEOCOD", "Município", "UF_sigla")], by = c("município" = "GEOCOD")),
            cnae[cnae$grupo != 0 & cnae$classe == 0 & cnae$subclasse == 0, c("grupo", "denominação")], # grupo
            by = "grupo")

df_Transformacao_municipio_subclasse <- # df_Transformacao_resultado
  left_join(left_join(df_Transformacao_municipio_subclasse, geocod[, c("GEOCOD", "Município", "UF_sigla")], by = c("município" = "GEOCOD")),
            cnae[cnae$subclasse != 0, c("subclasse", "denominação")],  # subclasses cnae
            by = c("subclasse" = "subclasse"))

df_Transformacao_uf_subclasse <- #  df_Transformacao_resultado 
  left_join(left_join(df_Transformacao_uf_subclasse, unique(geocod[, c("Cod_UF", "UF_sigla")]), by = c("uf" = "Cod_UF")),
            cnae[cnae$subclasse != 0, c("subclasse", "denominação")],  # subclasses cnae
            by = c("subclasse" = "subclasse"))

df_Transformacao_BR_grupo <-  # df_Transformacao_resultado 
  left_join(df_Transformacao_BR_grupo, 
            cnae[cnae$grupo != 0 & cnae$classe == 0 & cnae$subclasse == 0, c("grupo", "denominação")], # grupo
            by = "grupo")

df_Transformacao_BR_classe <-  # df_Transformacao_resultado 
  left_join(df_Transformacao_BR_classe, 
            cnae[cnae$classe != 0 & cnae$subclasse == 0, c("classe", "denominação")], # classe
            by = "classe")


rbind(df_Transformacao_BR_classe, df_Transformacao_resultado, BR)



BR <- 
  summarise(movimentacao, 
            'Saldo_da_Movimentação' = sum(saldomovimentação), 
            "salário_mediana" = median(salário, na.rm = TRUE),
            "salário_medio" = mean(salário, na.rm = TRUE))



# Salvar
                        write.table(
  df_BR_classe          ,file = paste("D:/Users/humberto.serna/Desktop/GEMI/RAIS_CAGED/", 
 "df_BR_classe"
                        ,".csv", sep = "") , sep = ";",dec = ",",row.names = FALSE)

                        
# *********************************************************** ----
# *********************************************************** ----
#  APÊNDICE: CARREGAMENTO DOS MICRODADOS E PREPARAÇÃO DA BASE ----

 # CNAE 2.3 ----
 
  source('./CSV_Data/RAIS_CAGED/CNAE_2_3.R')
 
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
   cnae[cnae$classe %in% classes_alvo_SECAO_C & cnae$subclasse != 0, c("subclasse")]
 
 
 # carregamento ----
 # _____ estabelecimentos ----
 # arquivos <- c(
 #   "CAGEDESTAB202001.txt",
 #   "CAGEDESTAB202002.txt",
 #   "CAGEDESTAB202003.txt",
 #   "CAGEDESTAB202004.txt",
 #   "CAGEDESTAB202005.txt",
 #   "CAGEDESTAB202006.txt",
 #   "CAGEDESTAB202007.txt",
 #   "CAGEDESTAB202008.txt",
 #   "CAGEDESTAB202009.txt",
 #   "CAGEDESTAB202010.txt",
 #   "CAGEDESTAB202011.txt",
 #   "CAGEDESTAB202012.txt"
 #   )
 # 
 # lista <- list()
 # for (i in 1:length(arquivos)) {
 # lista[[i]] <-
 #   read.table(
 #     paste('./CSV_Data/RAIS_CAGED/Novo_Caged/', arquivos[[i]], sep = ""),
 #     header = TRUE,
 #     sep = ";",
 #     dec = ".",
 #     colClasses = c("character", "character", "character", "character", "character", "integer", "integer", "integer", "character", "integer", "character", "character", "character"),
 #     stringsAsFactors = FALSE,
 #     encoding = "UTF-8"
 #   )
 # }
 # 
 # estabelecimentos <- 
 #   do.call("rbind", lista)
 # rm(lista)
 
 # _______________________________________________________________________
 #   O número de estabelecimentos que apresentam declarações à Rais       | 
 #   difere ano a ano, o que dificulta discriminar se a variação do       |
 #   emprego se deve a um real aumento ou redução decorrente da situação  | 
 #   do mercado de trabalho e/ou a um melhor desempenho na declaração.    |
 #   Opte por uma amostra longitudinal de empresas que declaram os dados. |
 # _______________________________________________________________________|
 
 
 # _____ movimentação ----
 
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
   "CAGEDMOV202012.txt"
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
         "character",	#	competência
         "NULL",	      #	região
         "character",	#	uf
         "character",	#	município
         "character",	#	seção
         "integer", 	  #	subclasse
         "integer",	  #	saldomovimentação
         "character",	#	cbo2002ocupação
         "character",	#	categoria
         "character",	#	graudeinstrução
         "NULL",	      #	idade
         "NULL",	      #	horascontratuais
         "NULL",	#	raçacor
         "NULL",	#	sexo
         "NULL",	#	tipoempregador
         "NULL",	#	tipoestabelecimento
         "NULL",	#	tipomovimentação
         "NULL",	#	tipodedeficiência
         "NULL",	#	indtrabintermitente
         "NULL",	#	indtrabparcial
         "numeric",	#	salário
         "NULL",	#	tamestabjan
         "NULL",	#	indicadoraprendiz
         "NULL"	#	fonte
       ),
       stringsAsFactors = FALSE,
       encoding = "UTF-8"
       )
 }
 
 movimentacao <- 
   do.call("rbind", lista)
 rm(lista)
 
 
 # delimitando pelas subclasses alvo das seções B e C ----
 
 movimentacao <- 
   movimentacao[movimentacao$subclasse %in% c(subclasses_alvo_SECAO_B, subclasses_alvo_SECAO_C),]
 
 # ____ impondo trimestre
 movimentacao$trimestre <-
   lubridate::quarter(
     lubridate::ymd(
       paste(str_extract(movimentacao$competência, pattern = "^...."), str_extract(movimentacao$competência, pattern = "..$"), "1", sep = "_")
     ), with_year = TRUE)
 
 # ____ impondo semestre
 movimentacao$semestre <-
   lubridate::semester(
     lubridate::ymd(
       paste(str_extract(movimentacao$competência, pattern = "^...."), str_extract(movimentacao$competência, pattern = "..$"), "1", sep = "_")
       ), with_year = TRUE)
     
 # ____ impondo mês.ANO
 movimentacao$competência <-
   lubridate::ymd(
     paste(str_extract(movimentacao$competência, pattern = "^...."), str_extract(movimentacao$competência, pattern = "..$"), "1", sep = "_")
   )
 
 
 # _____ junção grupo - subclasse ----
 movimentacao <- 
   left_join(movimentacao, cnae[,c("grupo", "classe", "subclasse", "denominação")], by = c("subclasse"))   
 
 
 # Geocod e junção coluna de municípios ----
 geocod <- 
   read.table(file = "./CSV_Data/GeoCodigos_IBGE.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE, 
              colClasses = "character", encoding = 'iso-8859-1', quote = "") %>% as_tibble()
 geocod$GEOCOD <- 
   str_extract(geocod$GEOCOD, "......")
 
 movimentacao <- 
   left_join(movimentacao, geocod[, c("UF_sigla", "GEOCOD", "Município")], by = c("município" = "GEOCOD"))
 
 
 # exportando em RDS ---- 
 
 saveRDS(movimentacao, file = "./CSV_Data/Novo_Caged_microdados_2020_secoes_CNAE23_B_C_ascii.RDATA", ascii = TRUE)