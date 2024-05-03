geocod <-
  read.table(
    file = paste(sep = "",
                 "./data/",
                 "GeoCodigos_IBGE_202201.csv"),
    header = TRUE,
    sep = ";",
    fill = TRUE,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8",
    encoding = "UTF-8",
    quote = "\"",
    colClasses = c(
      'integer',	     # Cod_UF
      'character',	   # UF
      'character',	   # UF_sigla
      'integer',	     # Cod Região Intermed
      'character',	   # Região Intermediária
      'integer',	     # Cod Região Imed
      'character',	   # Região Imediata
      'integer',	     # Cod_Mesorregião.Geográfica
      'character',	   # Mesorregião
      'integer',	     # Cod_Microrregião
      'character',	   # Microrregião
      'integer',	     # GEOCOD
      'character',	   # Município
      'character',     # Região_Administrativa_SP
      'character',     # Cod_Região_Metropolitana
      'character'	     # Região_Metropolitana
    )
  )


geocod$GEOCOD_6 <-
  as.integer(gsub(geocod$GEOCOD, pattern = ".$", replacement = ""))
  
geocod$id_mun_UF <-
  paste(
    geocod$Município |> 
      iconv(from = 'utf-8', to = 'ASCII//TRANSLIT') |> 
      gsub(pattern = "-| {1,}|'|\"", replacement = "") |> 
      toupper(),
    geocod$UF_sigla, sep = "_")
    
  