#        rm(list = ls())
#     options(editor = 'notepad')
library(tidyverse)

# CUIDADO ! ATENÇÃO! OS MICRODADOS ESTÃO COM SUBCLASSE SEM '0' NA FRENTE. HÁ SUBCLASSES COM 7 E OUTRA COM 6 DÍGITOS....

#___Grupo	Denominação_______________________________________________________________
#                                                                                   |
#		05.0	Extração de carvão mineral													                      |
#		07.1	Extração de minério de ferrosos												                    |
#		07.2	Extração de minerais metálicos não ferrosos									              |
#		08.1	Extração de pedra, areia e argila											                    |
#		08.9	Extração de outros minerais não metálicos									                |
#		09.9	Atividades de apoio à extração de minerais, exceto petróleo e gás natural	|
#___________________________________________________________________________________|

# carregamento ----
# _____ estabelecimentos ----
arquivos <- c(
  "CAGEDESTAB202001.txt",
  "CAGEDESTAB202002.txt",
  "CAGEDESTAB202003.txt",
  "CAGEDESTAB202004.txt",
  "CAGEDESTAB202005.txt",
  "CAGEDESTAB202006.txt" #,
#  "CAGEDESTAB202007.txt"
  )

lista <- list()
for (i in 1:length(arquivos)) {
lista[[i]] <-
  read.table(
    paste('./CSV_Data/RAIS_CAGED/Novo_Caged/', arquivos[[i]], sep = ""),
    header = TRUE,
    sep = ";",
    dec = ".",
    colClasses = c("character", "character", "character", "character", "character", "integer", "integer", "integer", "character", "integer", "character", "character", "character"),
    stringsAsFactors = FALSE,
    encoding = "UTF-8"
  )
}

estabelecimentos <- 
  do.call("rbind", lista)
rm(lista)

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
  "CAGEDMOV202006.txt"  #,
#  "CAGEDMOV202007.txt"
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
      colClasses = c("character","character","character","character","character","integer","numeric","character","character","character","numeric","numeric","character","character","character","character","character","character","character","character","numeric","character","character","character"),
      stringsAsFactors = FALSE,
      encoding = "UTF-8"
    )
}

movimentacao <- 
  do.call("rbind", lista)
rm(lista)



# _____ CNAE ----
CNAE_Subclasses_2_3 <-
 read.table(file = 'D:/Users/humberto.serna/Documents/CSV_Data/CNAE_Subclasses_2_3_Estrutura_Detalhada.csv', 
            header = TRUE, sep = ";", stringsAsFactors = FALSE, 
            colClasses = c('character'), encoding = "ISO-8859")

# __________ formato painel DataFrame ----
cnae <- CNAE_Subclasses_2_3
for (i in 2:nrow(cnae)) {
  if (cnae[i, 1] == "0") {
    cnae[i, 1] <- cnae[i - 1, 1]
  }}

for (i in 2:nrow(cnae)) {
  for (j in 2:length(cnae)) {
    if ((cnae[i, j] == "0") & (cnae[i, j - 1] == cnae[i - 1, j - 1])) {
      cnae[i, j] <- cnae[i - 1, j]
    }}}

# __________ uniformização dos Cods CNAE ----
 # na base Novo Caged as CNAEs são números inteiros

cnae$Grupo <- 
  cnae$Grupo %>% gsub(pattern = "-", replacement = "") %>% gsub(pattern = "\\.", replacement = "") %>% 
  gsub(pattern = "/", replacement = "") %>% as.integer()

cnae$Classe <- 
  cnae$Classe %>% gsub(pattern = "-", replacement = "") %>% gsub(pattern = "\\.", replacement = "") %>% 
  gsub(pattern = "/", replacement = "") %>% as.integer()

cnae$Subclasse <- 
  cnae$Subclasse %>% gsub(pattern = "-", replacement = "") %>% gsub(pattern = "\\.", replacement = "") %>% 
  gsub(pattern = "/", replacement = "") %>% as.integer() 


#      estrutura da CNAE: A  x x.x x-x/x x
#       seção ____________|  | | | | | | |      B e C - INDÚSTRIAS EXTRATIVAS e INDÚSTRIAS DE TRANSFORMAÇÃO
#       divisão _____________|_| | | | | |
#       grupo ___________________| | | | |
#       classe ____________________|_| | |
#       subclasse _____________________|_|

# __________ Função de busca CNAE ----

FUNA_CNAE_busca <-
  function(Seção = ".",
           Divisão = ".",
           Grupo = ".",
           Classe = ".",
           Subclasse = ".") {
    x <- 
    unique(cnae[grepl(cnae$Seção, pattern = Seção) == TRUE &          # consulta Regex Exclusão "[^_]"
                grepl(cnae$Divisão, pattern = Divisão) == TRUE &
                grepl(cnae$Grupo, pattern = Grupo) == TRUE &
                grepl(cnae$Classe, pattern = Classe) == TRUE &
                grepl(cnae$Subclasse, pattern = Subclasse) == TRUE,])
    return(x)}


# Geocod  ----
geocod <- 
  read.table(file = "./CSV_Data/GeoCodigos_IBGE.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE, 
           colClasses = "character", encoding = 'iso-8859-1', quote = "") %>% as_tibble()
geocod$GEOCOD <- 
  str_extract(geocod$GEOCOD, "......")

# _____________________________________________________________----

# Relatórios ----
# _____ lista subclasses_alvo (Extrativa Mineral - Exceto petróleo & Gás) ----
subclasses_alvo <-                    # subclasses da Extrativa Mineral, exceto Petróleo e Gás
    c(#subclasse  Denominação
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

# _____ Lista Classes Alvo (Indústria de Transformação Relacionada não associada/consecutiva à mineração) ----

classes_alvo <- 
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
   

# __________ subclasses associadas às classes alvo ----
  
subclasses_alvo <- 
    cnae[cnae$Classe %in% classes_alvo & cnae$Subclasse != 0, c("Subclasse")]

# _____ delimitando df por subclasses alvo ----

#         df <- estabelecimentos[estabelecimentos$subclasse %in% subclasses_alvo, ]
          df <- movimentacao[movimentacao$subclasse %in% subclasses_alvo, ]

# _____ junção coluna de municípios ----

df <- 
  left_join(df, geocod[, c("UF_sigla", "GEOCOD", "Município")], by = c("município" = "GEOCOD"))

# _____ junção Grupo - Subclasse ----
df <- 
  left_join(df, cnae[,c("Grupo", "Classe", "Subclasse", "Denominação")], by = c("subclasse" = "Subclasse"))   

#  left_join(df, cnae[,c("Grupo", "Subclasse", "Denominação")], by = c("subclasse" = "Subclasse"))   


# _____ saldo_movimentação ----
df_resultado <-
  summarise(
    group_by(df[df$competência != "202007", c(
                              #      "competência",
                              #      "região",
                                    "uf",
                                    "município",
                                    "seção",
                                    "Grupo",
                                    "Classe",
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
                     Grupo  ,
                #     Classe#,
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

# ----

                #: exclua-me
df_uf_grupo <-  # df_resultado      
  left_join(left_join(df_uf_grupo, unique(geocod[, c("Cod_UF", "UF_sigla")]), by = c("uf" = "Cod_UF")),
            cnae[cnae$Grupo != 0 & cnae$Classe == 0 & cnae$Subclasse == 0, c("Grupo", "Denominação")], # Grupo
            by = "Grupo")

df_municipio_grupo <- # df_resultado 
  left_join(left_join(df_municipio_grupo, geocod[, c("GEOCOD", "Município", "UF_sigla")], by = c("município" = "GEOCOD")),
            cnae[cnae$Grupo != 0 & cnae$Classe == 0 & cnae$Subclasse == 0, c("Grupo", "Denominação")], # Grupo
            by = "Grupo")

df_municipio_subclasse <- # df_resultado
  left_join(left_join(df_municipio_subclasse, geocod[, c("GEOCOD", "Município", "UF_sigla")], by = c("município" = "GEOCOD")),
            cnae[cnae$Subclasse != 0, c("Subclasse", "Denominação")],  # subclasses cnae
            by = c("subclasse" = "Subclasse"))

df_uf_subclasse <- #  df_resultado 
  left_join(left_join(df_uf_subclasse, unique(geocod[, c("Cod_UF", "UF_sigla")]), by = c("uf" = "Cod_UF")),
            cnae[cnae$Subclasse != 0, c("Subclasse", "Denominação")],  # subclasses cnae
            by = c("subclasse" = "Subclasse"))

df_BR_Grupo <-  # df_resultado 
  left_join(df_BR_Grupo, 
            cnae[cnae$Grupo != 0 & cnae$Classe == 0 & cnae$Subclasse == 0, c("Grupo", "Denominação")], # Grupo
            by = "Grupo")

df_BR_Classe <-  # df_resultado 
  left_join(df_BR_Classe, 
            cnae[cnae$Classe != 0 & cnae$Subclasse == 0, c("Classe", "Denominação")], # Classe
            by = "Classe")


rbind(df_BR_Classe, df_resultado, BR)



BR <- 
summarise(movimentacao, 
          'Saldo_da_Movimentação' = sum(saldomovimentação), 
          "salário_mediana" = median(salário, na.rm = TRUE),
          "salário_medio" = mean(salário, na.rm = TRUE))



# Salvar
                        write.table(
  df_BR_Classe          ,file = paste("D:/Users/humberto.serna/Desktop/GEMI/RAIS_CAGED/", 
 "df_BR_Classe"
                        ,".csv", sep = "") , sep = ";",dec = ",",row.names = FALSE)











                        
                        
                        
                        
                        
                        
                        

                        
                        
                        
                        
                        
                        
                        
                        
                        
                        
                        