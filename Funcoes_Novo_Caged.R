#Funcoes Novo Caged

FUNA_var_grupo <-
  function(ano = ano,
           what = "percentual",
           nivel_cnae = nivel_cnae,
           subclasses_alvo = subclasses_alvo) {
    
    date <- paste(ano,"01","01",sep = ".")
    trimestre <- paste(ano,seq(1,4),sep = ".")
    
    
    # calculando variação absoluta
    variacao_absoluta <-     
      left_join(
        pivot_wider(
          summarise(
            group_by(estoque_trabalhadores[
              estoque_trabalhadores$cnae20subclas %in% subclasses_alvo &
                estoque_trabalhadores$data == date, ], 
              {{nivel_cnae}}, 
              data),
            "Estoque_01Jan" = sum(estoqueref, na.rm = T)),
          names_from = data,
          values_from = Estoque_01Jan),
        pivot_wider(names_from = trimestre, values_from = Saldo,
                    summarise(
                      group_by(df_Extrativa[df_Extrativa$trimestre %in% trimestre, 
                                            c(
                                              "UF_sigla",
                                              "município",
                                              # "seção",
                                              "grupo",
                                              "classe",
                                              "subclasse",
                                              "saldomovimentação",
                                              "trimestre"
                                            )], grupo, trimestre),
                      "Saldo" = sum(saldomovimentação, na.rm = TRUE)
                    )))
    
    
    # calculando estoque 
    estoque_CNAE <- variacao_absoluta
    for (j in 3:ncol(variacao_absoluta)) {
      estoque_CNAE[,j] <- 
        estoque_CNAE[,j] + estoque_CNAE[,j-1]
    }
    
    
    # calculando variação percentual
    variacao_percentual <- estoque_CNAE[,c(1,3:6)]
    
    variacao_percentual[,2:5] <- 100 *
      (variacao_absoluta/estoque_CNAE)[,3:6]
    
    
  }