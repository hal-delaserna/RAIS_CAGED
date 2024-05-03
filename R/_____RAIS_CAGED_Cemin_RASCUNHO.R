################################################################################
## Baixar e importar os microdados da RAIS no R                               ##
## Guilherme Cemin de Paula                                                   ##
## http://cemin.wikidot.com                                                   ##
## cemin@outlook.com                                                          ##
################################################################################
## Este script far� o download dos microdados da RAIS (v�nculos) do site do   ##
## MTE e os salvar� no formato .Rda (R)                                       ## 
## Ele est� automatizado atrav�s do uso de caixas de di�logo. Se preferir, o  ##
## script manual se encontra na seguinte p�gina                               ##
## http://cemin.wikidot.com/raisrm                                            ##
################################################################################ 


## O script utilizar� os seguintes pacotes
if (!require(data.table)) { 
  install.packages('data.table') 
  library(data.table) 
}
## Fornece a fun��o fread (leitura r�pida de microdados)
if (!require(svDialogs)) { 
  install.packages('svDialogs') 
  library(svDialogs) 
}
## Caixas de di�logo

########################### A PARTIR DAQUI N�O EDITE ###########################

print('Possivelmente a primeira caixa de di�logo abrir� minimizada.')

dlgMessage('Aten��o: Este script far� o download dos microdados da RAIS (v�nculos) do site do MTE (ou utilizar� os microdados previamente baixados) e os salvar� no formato .Rda (R).') 

dlgMessage('O tamanho dos arquivos contendo os microdados varia de menos de 1MB a mais de 700MB, dependendo da UF e ano. Certifique-se de que n�o h� cobran�as adicionais e limite de download na sua conex�o.') 

dlgMessage('Para evitar travamentos, dependendo das configura��es do seu computador, baixe poucos dados por vez.') 

dlgMessage('Primeiro defina a pasta em que os arquivos finais ser�o salvos.') 
diret <- dlgDir(default = getwd(), title='Defina a pasta')$res
if (length(diret)==0) {
  stop('Voc� cancelou a escolha. Rode o script novamente.')
} else {
  setwd(diret)
  
  dlgMessage(c('Possui arquivos previamente baixados do site do MTE? Informe a pasta onde se encontram ou cancele para continuar.',
               'Os arquivos dever�o estar compactados no formato .7z, conforme disponibilizados pelo MTE.')) 
  prevdir <- dlgDir(default = getwd(), title='Defina a pasta dos arquivos j� baixados.')$res
  
  if (length(prevdir)>0 & length(list.files(prevdir,pattern = "([A-Z][A-Z][0-9][0-9][0-9][0-9]).7z"))>0) {
    dlgMessage(c('Com quais arquivos deseja trabalhar?',
                 'Utilize CTRL ou SHIFT para selecionar mais de um arquivo ou cancele para continuar sem utilizar dados previamente baixados.'))
    prevres <- dlgList(c(list.files(prevdir,pattern = "([A-Z][A-Z][0-9][0-9][0-9][0-9]).7z")), multiple = TRUE, title = 'Selecione os anos')$res
  }
  if (length(prevdir)>0 & !length(list.files(prevdir,pattern = "([A-Z][A-Z][0-9][0-9][0-9][0-9]).7z"))){
    dlgMessage('N�o h� arquivos previamente baixados na pasta informada, ou o formato n�o est� correto.')
    prevres <- NULL
  }
  if (!length(prevdir)){
    prevres <- NULL
  }
  if (!length(prevres)) {
    prevres <- NULL
  }
  
  ## Criando arquivos e diret�rio tempor�rios
  tfzip <- tempfile(fileext='.zip') 
  td <- tempdir()
  tf7z <- tempfile(pattern='7za', tmpdir=td, fileext='.exe')
  fart <- tempfile(pattern='fart', tmpdir=td, fileext='.exe');  
  
  dlgMessage(c('Selecione os anos desejados para download.',
               'Utilize CTRL ou SHIFT para selecionar mais de um ano.'))
  resyr <- dlgList(c(1985:2017), multiple = TRUE, title = 'Selecione os anos:')$res
  if (!length(resyr) & !length(prevres)) {
    stop('Voc� cancelou a escolha. Rode o script novamente.')
  } else {
    
    dlgMessage(c('Selecione os estados desejados para download.',
                 'Utilize CTRL ou SHIFT para selecionar mais de um estado.'))
    resuf <- dlgList(c('AC','AL','AM','AP','BA','CE','DF','ES','GO','MA','MG','MS','MT','PA','PB','PE','PI','PR','RJ',
                       'RN','RO','RR','RS','SC','SE','SP','TO'), multiple = TRUE, title = 'Selecione os estados')$res
    if (!length(resuf) & !length(prevres)) {
      stop('Voc� cancelou a escolha. Rode o script novamente.')
    } else {
      
      
      dlgMessage(c('A seguir selecione as vari�veis desejadas.',
                   'Utilize CTRL ou SHIFT para selecionar mais de uma.',
                   'Algumas vari�veis n�o est�o dispon�veis para todos os anos.'))
      vardisp <- c('Bairros SP', 'Bairros Fortaleza (1996-16)', 'Bairros RJ (1996-16)', 'Causa Afastamento 1 (2002-16)', 'Causa Afastamento 2 (2002-16)', 
                   'IBGE Subatividade (1985-1993)', 'IBGE Subsetor (1985-1993)', 'IBGE Subsetor (2015-16)', 'Causa Afastamento 3 (2002-16)', 'Motivo Desligamento', 'CBO Ocupa��o (1985-1993)',
                   'CBO 94 Ocupa��o (1994-02)', 'CBO Ocupa��o 2002 (2003-16)', 'CNAE 2.0 Classe (2004-16)', 'CNAE 95 Classe (1994-16)', 'Distritos SP', 'V�nculo Ativo 31/12', 
                   'Faixa Et�ria', 'Faixa Hora Contrat (1994-16)', 'Faixa Remun Dezem {SM}', 'Faixa Remun M�dia {SM}', 'Faixa Tempo Emprego', 
                   'Grau Instru��o 2005-1985 (1985-05)', 'Escolaridade ap�s 2005 (2006-16)', 'Qtd Hora Contr (1994-16)', 'Idade (1994-16)', 
                   'Ind CEI Vinculado (1999-16)', 'Ind Simples (2001-16)', 'M�s Admiss�o', 'M�s Desligamento', 'Mun Trab (2002-16)', 
                   'Munic�pio', 'Nacionalidade', 'Natureza Jur�dica (1994-16)', 'Ind Portador Defic (2007-16)', 'Qtd Dias Afastamento (2002-16)', 
                   'Ra�a Cor (2006-16)', 'Regi�es Adm DF (1996-16)', 'Vl Remun Dezembro Nom (1999-16)', 'Vl Remun Dezembro {SM}', 
                   'Vl Remun M�dia Nom (1999-16)', 'Vl Remun M�dia {SM}', 'CNAE 2.0 Subclasse (2004-16)', 'Sexo Trabalhador', 
                   'Tamanho Estabelecimento', 'Tempo Emprego', 'Tipo Admiss�o (1994-16)', 'Tipo Estab1', 'Tipo Estab2', 
                   'Tipo Defic (2007-16)', 'Tipo V�nculo', 'Vl Rem Janeiro CC (2015-16)', 'Vl Rem Fevereiro CC (2015-16)', 'Vl Rem Mar�o CC (2015-16)',
                   'Vl Rem Abril CC (2015-16)', 'Vl Rem Maio CC (2015-16)', 'Vl Rem Junho CC (2015-16)', 'Vl Rem Julho CC (2015-16)', 'Vl Rem Agosto CC (2015-16)',
                   'Vl Rem Setembro CC (2015-16)', 'Vl Rem Outubro CC (2015-16)', 'Vl Rem Novembro CC (2015-16)', 'Ano Chegada Brasil (2016)')
      
      res <- dlgList(vardisp, multiple = TRUE, title = 'Selecione as vari�veis:')$res
      
      if (!length(res)) {
        stop('Voc� cancelou a escolha. Rode o script novamente.')
      } else {
        res <- gsub('\\s*\\([^\\)]+\\)','',res)
        res <- gsub('\\{','(',res)
        res <- gsub('\\}',')',res)
        res <- unique(res)
        
        # sele��o de munic�pios
        if (any(res=='Munic�pio') | !length(res)) {
          dlgMessage(c('Deseja salvar os dados de apenas alguns munic�pios?',
                       'Veja os c�digos em http://cemin.wikidot.com/codmun',
                       'e digite a seguir para todos os estados selecionados de uma s� vez.'))
          resmun <- dlgInput(c('Digite cada c�digo separado por v�rgula, como no',
                               'exemplo abaixo, ou cancele para selecionar todos.'),'120040, 120045')$res
        } else {
          resmun=NULL}
        if (length(resmun)) {
          resmun <- unlist(strsplit(noquote(resmun),', '))
          resmun <- unlist(strsplit(noquote(resmun),','))
          resmun <- unlist(strsplit(noquote(resmun),' ,'))
        }
        
        # sele��o de subclasses 2.0 _________________________________________________________________________________
         
        if (any(res=='CNAE 2.0 Subclasse') | !length(res)) {
          dlgMessage(c('Deseja salvar os dados de apenas algumas Subclasses CNAE 2.0?',
                       'digite a seguir, sem o zero na frente (6 ou 7 d�gitos)'))
          ressubCNAE <- dlgInput(c('Digite cada c�digo separado por v�rgula, como no',
                               'exemplo abaixo, ou cancele para selecionar todos.'),'4213800, 151201')$res
        } else {
          ressubCNAE=NULL}
        if (length(ressubCNAE)) {
          ressubCNAE <- unlist(strsplit(noquote(ressubCNAE),', '))
          ressubCNAE <- unlist(strsplit(noquote(ressubCNAE),','))
          ressubCNAE <- unlist(strsplit(noquote(ressubCNAE),' ,'))
        }
        
        # ***********************************************************************************************************
        
        filest <- sprintf('%s%s', expand.grid(resuf,resyr)[,1], expand.grid(resuf,resyr)[,2])
        filest <- setdiff(filest,substr(prevres, 1, 6))
        
        if (length(prevres)>0 & length(filest)>0){
          fileslist <- sort(c(paste0(filest,' (Download)'),paste0(substr(prevres, 1, 6),' (Disco Local)')))
        }
        if (length(prevres)>0 & !length(filest)){
          fileslist <- sort(paste0(substr(prevres, 1, 6),' (Disco Local)'))
        }
        if (!length(prevres) & length(filest)>0){
          fileslist <- sort(paste0(filest,' (Download)'))
        }
        
        filest <- sort(c(filest,substr(prevres, 1, 6)))
        
        dlgMessage(c('A seguir verifique os arquivos que ser�o baixados e/ou utilizados.',
                     'Se houver inconsist�ncias com suas escolhas cancele e comece novamente.'))
        
        filesres <- dlgList(fileslist, multiple = TRUE, title = 'Selecione Ok para continuar.')$res
        
        if (!length(filesres)) {
          stop('Voc� cancelou a escolha. Rode o script novamente.')
        } else {
          
          
          ## Baixando o 7zip para descompactar os microdados. O 7zip traz mais fun��es que o unzip nativo do R
          ## O arquivo � um execut�vel que ser� baixado no diret�rio tempor�rio e deletado no fim do processo
          download.file('http://cemin.wikidot.com/local--files/raisrm/7za.exe', tf7z, mode='wb')
          
          for(i in filest){
            # for(j in UF){
            file <- i
            year <- unique(gsub('[A-Z]','',i))
            uf <- unique(gsub('[0-9]','',i))
            files <- paste0(file,'.7z')
            files.txt <- paste0(file,'.txt')
            if(files %in% prevres){
              prevzip <- paste0(prevdir,'/',files)
            } else {
              # Gerar url do arquivo
              ftp.path <- paste0('ftp://ftp.mtps.gov.br/pdet/microdados/RAIS/',year,'/',files)
              # Tentar baixar cada arquivo 7z e extrair txt
              counter <- 0
              while(counter<10){
                counter <- sum(counter, 1)
                try1 <- try(download.file(ftp.path, destfile=tfzip, mode='wb', method='libcurl'), silent=F)
                if(class(try1)=='try-error'){
                  countdown <- function(from)
                  {
                    while(from!=0)
                    {
                      Sys.sleep(1)
                      from <- from - 1
                      cat(rep("\n",50))
                      cat('Tentativa', counter, 'de 10. Falha na conex�o com o servidor. ')
                      cat('O programa far� nova tentativa em',from,'segundo(s).')
                    }
                  }
                  countdown(60)
                }
                else{break}
              }
            }
            dirtxt <- paste0(td,'\\txt',year,sep='')
            cat('Descompactando os microdados. Aguarde.')
            if(files %in% prevres){
              system(paste0(tf7z,' e ',prevzip,' -o',dirtxt,' -y',sep=''))
            } else {
              system(paste0(tf7z,' e ',tfzip,' -o',dirtxt,' -y',sep=''))
            }
            path.file <- paste0(dirtxt,'\\',files.txt,sep='')
            
            selvar <- res
            
            ## Se uma das vari�veis 'Tipo Estab' for selecionada � preciso baixar o Fart para alterar o nome da vari�vel no txt
            ## O arquivo � um execut�vel que ser� baixado no diret�rio tempor�rio e deletado no fim do processo
            if ('Tipo Estab1' %in% res|'Tipo Estab2' %in% res){
              download.file('http://cemin.wikidot.com/local--files/raisr/fart.exe', fart, mode='wb')
              system(print(paste0(fart,' -c ',path.file,' "Tipo Estab;Tipo Estab" "Tipo Estab1;Tipo Estab2"',sep=''),quote=F))
            }
            # Ler e salvar em Rda
            df <- suppressWarnings(fread(path.file, sep = ';', dec = ',', select=selvar, header = TRUE, encoding = 'Latin-1'))
            
            if(length(resmun) > 0){
              df <- df[(df$`Munic�pio` %in% resmun),]}
            
            # sele��o de subclasses 2.0 _________________________________________________
            
            if(length(ressubCNAE) > 0){                                     # filtro p/ subclasses
              df <- df[(df$`CNAE 2.0 Subclasse` %in% ressubCNAE),]}         # filtro p/ subclasses
            
            # ***************************************************************************
            
            
            dfname <- file
            assign(dfname, df)
            save(list=dfname, file=paste0(getwd(),'/',file,'.Rda'))
            # Liberar ram a cada loop
            gc()
          }
        }
      }
    }
    # }
    ## Deletar pasta tempor�ria e limpar objetos utilizados
    dlgMessage('Fim do processo. Se n�o houve erros os arquivos devem estar na pasta indicada e ser�o carregados no R a seguir.') 
    
    if(okCancelBox('Remover a pasta tempor�ria? \n Se deseja fazer o download de mais dados nessa se��o n�o remova.')){ 
      unlink(td, recursive = T)}
    
    rm(list = ls())
    lapply(list.files(getwd(),pattern = ".Rda"),load,.GlobalEnv)
    
    dlgMessage(c('Em caso de d�vidas ou erros entre em contato:', 'cemin@outlook.com'))  
  }
}

