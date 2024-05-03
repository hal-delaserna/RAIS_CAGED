#        rm(list = ls())
#     options(editor = 'notepad')
library(tidyverse)

# source('D:/Users/humberto.serna/Desktop/Anuario_Mineral_Brasileiro/Funcoes_de_Formatacao_Estilo/Funcoes_de_Formatacao_Estilo.R')

#      estrutura da CNAE: A  x x.x x-x/x x
#       seção ____________|  | | | | | | |      B e C - INDÚSTRIAS EXTRATIVAS e INDÚSTRIAS DE TRANSFORMAÇÃO
#       divisão _____________|_| | | | | |
#       grupo ___________________| | | | |
#       classe ____________________|_| | |
#       subclasse _____________________|_|



# Carregamento CNAE 2.0 ----
CNAE_Subclasses_2_0 <-
  read.table(file = './data/CNAE_2_0.csv', 
             header = TRUE, sep = "\t", stringsAsFactors = FALSE, 
             colClasses = c('character'), 
             fileEncoding = "latin1")

colnames(CNAE_Subclasses_2_0) <- 
  c("subclasse", "subclasse.descrição", "classe", 
    "classe.descrição", "grupo", "grupo.descrição", "divisão", 
    "divisão.descrição", "seção", "seção.descrição", "grupamento", 
    "grande.Grupamento")

#_____ Grupos CNAE em CAIXA ALTA
# CNAE_Subclasses_2_0$grupo.descrição <- 
#   CNAE_Subclasses_2_0$grupo.descrição |> 
#   iconv(to = "ASCII//TRANSLIT") |> toupper()


# 
# # Carregamento CNAE 2.3 ----
# CNAE_Subclasses_2_3 <-
#   read.table(file = '../CNAE_Subclasses_2_3_Estrutura_Detalhada.csv', 
#              header = TRUE, sep = ";", stringsAsFactors = FALSE, 
#              colClasses = c('character'), encoding = "latin1")
# 
# 
# CNAE_Subclasses_2_3[CNAE_Subclasses_2_3$grupo != "0",]$denominação <- 
#   CNAE_Subclasses_2_3[CNAE_Subclasses_2_3$grupo != "0",]$denominação |> 
#   iconv(to = "ASCII//TRANSLIT") |> toupper()
# 
# 
# # __________ formato painel DataFrame ----
# cnae <- CNAE_Subclasses_2_3
# for (i in 2:nrow(cnae)) {
#   if (cnae[i, 1] == "0") {
#     cnae[i, 1] <- cnae[i - 1, 1]
#   }}
# 
# for (i in 2:nrow(cnae)) {
#   for (j in 2:length(cnae)) {
#     if ((cnae[i, j] == "0") & (cnae[i, j - 1] == cnae[i - 1, j - 1])) {
#       cnae[i, j] <- cnae[i - 1, j]
#     }}}
# 
# 
# 
# 
# 
# # __________ uniformiza??o dos Cods CNAE ----
# # na base Novo Caged as CNAEs são números inteiros
# 
# cnae$grupo <- 
#   cnae$grupo |> gsub(pattern = "-", replacement = "") |> gsub(pattern = "\\.", replacement = "") |> 
#   gsub(pattern = "/", replacement = "")  |> as.integer() 
# 
# cnae$classe <- 
#   cnae$classe |> gsub(pattern = "-", replacement = "") |> gsub(pattern = "\\.", replacement = "") |> 
#   gsub(pattern = "/", replacement = "")  |> as.integer() 
# 
# cnae$subclasse <- 
#   cnae$subclasse |> gsub(pattern = "-", replacement = "") |> gsub(pattern = "\\.", replacement = "") |> 
#   gsub(pattern = "/", replacement = "")  |> as.integer() 



# __________ Função de busca CNAE ----

FUNA_CNAE_busca <-
  function(seção = ".",
           divisão = ".",
           grupo = ".",
           classe = ".",
           subclasse = ".") {
    x <- 
      unique(cnae[grepl(cnae$seção, pattern = seção) == TRUE &          # consulta Regex Exclusão "[^_]"
                    grepl(cnae$divisão, pattern = divisão) == TRUE &
                    grepl(cnae$grupo, pattern = grupo) == TRUE &
                    grepl(cnae$classe, pattern = classe) == TRUE &
                    grepl(cnae$subclasse, pattern = subclasse) == TRUE,])
    return(x)}



# _____ lista subclasses seção B (Extrativa Mineral - Exceto petr?leo & G?s) ----

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
    "729403",	#Extração de minério de n?quel
    "729404",	#Extração de minérios de cobre, chumbo, zinco e outros minerais met?licos n?o ferrosos n?o especificados anteriormente
    "729405",	#Beneficiamento de minérios de cobre, chumbo, zinco e outros minerais met?licos n?o ferrosos n?o especificados anteriormente
    "810001",	#Extração de ard?sia e beneficiamento associado
    "810002",	#Extração de granito e beneficiamento associado
    "810003",	#Extração de m?rmore e beneficiamento associado
    "810004",	#Extração de calc?rio e dolomita e beneficiamento associado
    "810005",	#Extração de gesso e caulim
    "810006",	#Extração de areia, cascalho ou pedregulho e beneficiamento associado
    "810007",	#Extração de argila e beneficiamento associado
    "810008",	#Extração de saibro e beneficiamento associado
    "810009",	#Extração de basalto e beneficiamento associado
    "810010",	#Beneficiamento de gesso e caulim associado ? extra??o
    "810099",	#Extração e britamento de pedras e outros materiais para construção e beneficiamento associado
    "891600",	#Extração de minerais para fabrica??o de adubos, fertilizantes e outros produtos qu?micos
    "892401",	#Extração de sal marinho
    "892402",	#Extração de salgema
    "892403",	#Refino e outros tratamentos do sal
    "893200",	#Extração de gemas (pedras preciosas e semipreciosas)
    "899101",	#Extração de grafita
    "899102",	#Extração de quartzo
    "899103",	#Extração de amianto
    "899199",	#Extração de outros minerais n?o met?licos n?o especificados anteriormente
    "990401",	#Atividades de apoio ? extra??o de minério de ferro
    "990402",	#Atividades de apoio ? extra??o de minerais met?licos n?o ferrosos
    "990403"	#Atividades de apoio ? extra??o de minerais n?o met?licos
  )


# _____ Lista classes Alvo na Seção C (Ind?stria de Transforma??o Relacionada n?o associada/consecutiva ? minera??o) ----

classes_alvo_SECAO_C <- 
  c(
    "20126",	  #    Fabrica??o de Intermedi?rios para Fertilizantes 
    "20134",    #    Fabrica??o de Adubos e Fertilizantes 
    "23206",    #    Fabrica??o de Cimento 
    "23303",    #    Fabrica??o de Artefatos de Concreto, Cimento, Fibrocimento, Gesso e Materiais Semelhantes 
    "23419",    #    Fabrica??o de Produtos Cer?micos Refrat?rios 
    "23427",    #    Fabrica??o de Produtos Cer?micos N?oRefrat?rios para Uso Estrutural na Constru??o 
    "23494",    #    Fabrica??o de Produtos Cer?micos N?oRefrat?rios n?o Especificados Anteriormente 
    "23915",    #    Aparelhamento e Outros Trabalhos em Pedras 
    "23923",    #    Fabrica??o de Cal e Gesso 
    "23991",    #    Fabrica??o de Produtos de Minerais N?oMet?licos n?o Especificados Anteriormente 
    "24113",    #    Produção de FerroGusa 
    "24121",    #    Produção de Ferroligas 
    "24211",    #    Produção de SemiAcabados de A?o 
    "24229",    #    Produção de Laminados Planos de A?o 
    "24237",    #    Produção de Laminados Longos de A?o 
    "24245",    #    Produção de Relaminados, Trefilados e Perfilados de A?o 
    "24318",    #    Produção de Tubos de A?o com Costura 
    "24393",    #    Produção de Outros Tubos de Ferro e A?o 
    "24415",    #    Metalurgia do Alum?nio e Suas Ligas 
    "24423",    #    Metalurgia dos Metais Preciosos 
    "24431",    #    Metalurgia do Cobre 
    "24491",    #    Metalurgia dos Metais N?oFerrosos e Suas Ligas n?o Especificados Anteriormente 
    "24512",    #    Fundi??o de Ferro e A?o 
    "25314",    #    Produção de forjados de a?o e de metais n?oferrosos e suas ligas 
    "24521",    #    Fundi??o de Metais N?oFerrosos e Suas Ligas 
    "32116")    #    Lapida??o de Gemas e Fabrica??o de Artefatos de Ourivesaria e Joalheria 


# subclasses_alvo_SECAO_C ----
subclasses_alvo_SECAO_C <-    
  c(
    '2012600',		#	Fabricação de Intermediários para Fertilizantes
    '2013400',		#	Fabricação de adubos e fertilizantes
    '2013401',		#	Fabricação de Adubos e Fertilizantes Organominerais
    '2013402',		#	Fabricação de Adubos e Fertilizantes, Exceto Organominerais
    '2320600',		#	Fabricação de Cimento
    '2330301',		#	Fabricação de Estruturas Pré-Moldadas de Concreto Armado, em Série e Sob Encomenda
    '2330302',		#	Fabricação de Artefatos de Cimento para Uso na Construção
    '2330303',		#	Fabricação de Artefatos de Fibrocimento para Uso na Construção
    '2330304',		#	Fabricação de Casas Pré-Moldadas de Concreto
    '2330305',		#	Preparação de Massa de Concreto e Argamassa para Construção
    '2330399',		#	Fabricação de Outros Artefatos e Produtos de Concreto, Cimento, Fibrocimento, Gesso e Materiais Semelhantes
    '2341900',		#	Fabricação de Produtos Cerâmicos Refratários
    '2342701',		#	Fabricação de Azulejos e Pisos
    '2342702',		#	Fabricação de Artefatos de Cerâmica e Barro Cozido para Uso na Construção, Exceto Azulejos e Pisos
    '2349401',		#	Fabricação de Material Sanitário de Cerâmica
    '2349499',		#	Fabricação de Produtos Cerâmicos Não-Refratários não Especificados Anteriormente
    '2391501',		#	Britamento de Pedras, Exceto Associado à Extração
    '2391502',		#	Aparelhamento de Pedras para Construção, Exceto Associado à Extração
    '2391503',		#	Aparelhamento de Placas e Execução de Trabalhos em Mármore, Granito, Ardósia e Outras Pedras
    '2392300',		#	Fabricação de Cal e Gesso
    '2399101',		#	Decoração, Lapidação, Gravação, Vitrificação e Outros Trabalhos em Cerâmica, Louça, Vidro e Cristal
    '2399102',		#	Fabricação de Abrasivos
    '2399199',		#	Fabricação de Outros Produtos de Minerais Não-Metálicos não Especificados Anteriormente
    '2411300',		#	Produção de Ferro-Gusa
    '2412100',		#	Produção de Ferroligas
    '2421100',		#	Produção de Semi-Acabados de Aço
    '2422901',		#	Produção de Laminados Planos de Aço ao Carbono, Revestidos ou Não
    '2422902',		#	Produção de Laminados Planos de Aços Especiais
    '2423701',		#	Produção de Tubos de Aço sem Costura
    '2423702',		#	Produção de Laminados Longos de Aço, Exceto Tubos
    '2424501',		#	Produção de Arames de Aço
    '2424502',		#	Produção de Relaminados, Trefilados e Perfilados de Aço, Exceto Arames
    '2431800',		#	Produção de Tubos de Aço com Costura
    '2439300',		#	Produção de Outros Tubos de Ferro e Aço
    '2441501',		#	Produção de Alumínio e Suas Ligas em Formas Primárias
    '2441502',		#	Produção de Laminados de Alumínio
    '2442300',		#	Metalurgia dos Metais Preciosos
    '2443100',		#	Metalurgia do Cobre
    '2449101',		#	Produção de Zinco em Formas Primárias
    '2449102',		#	Produção de Laminados de Zinco
    '2449103',		#	Produção de Soldas e ânodos para Galvanoplastia
    '2449199',		#	Metalurgia de Outros Metais Não-Ferrosos e Suas Ligas não Especificados Anteriormente
    '2451200',		#	Fundição de Ferro e Aço
    '2452100',		#	Fundição de Metais Não-Ferrosos e Suas Ligas
    '2531401',		#	Produção de Forjados de Aço
    '2531402',		#	Produção de Forjados de Metais Não-Ferrosos e Suas Ligas
    '3211601',		#	Lapidação de Gemas
    '3211602',		#	Fabricação de Artefatos de Joalheria e Ourivesaria
    '3211603' 		#	Cunhagem de Moedas e Medalhas
  )

