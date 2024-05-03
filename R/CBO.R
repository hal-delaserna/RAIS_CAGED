

cbo <-
  read.table(
    file = "./data/CBO2002 - Ocupacao.csv",
    sep = ";",
    stringsAsFactors = FALSE,
    header = TRUE,
    colClasses = "character",
    fileEncoding = 'Latin1',
    quote = ""
  )