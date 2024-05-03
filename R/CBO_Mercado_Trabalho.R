a <- movimentacao[movimentacao$cbo2002ocupação %in% c("223505","322230","322205","516210"),]

a <- a[a$município %in% c("355030"),]

a <- 
summarise(group_by(a, competênciamov, NO_CBO), mean(salário)) %>%
  spread(key = competênciamov, value = `mean(salário)`)



med <- movimentacao[movimentacao$cbo2002ocupação %in% c("223505","322230","322205","516210"),]

med <- med[med$município %in% c("355030"),]

med <- 
summarise(group_by(med, competênciamov, NO_CBO), median(salário)) %>%
  spread(key = competênciamov, value = `median(salário)`)



plot(as.numeric(med[1,]), type = "l")
plot(as.numeric(med[2,]), type = "l")
plot(as.numeric(med[3,]), type = "l")
plot(as.numeric(med[4,]), type = "l")




a <- 
med[med$cbo2002ocupação == "516210" & 
      med$denominação %in% c(
        "Clínicas e residências geriátricas",
        "Instituições de longa permanência para idosos",
        "Atividades de fornecimento de infraestrutura de apoio e assistência a paciente no domicílio",
        "Condomínios residenciais para idosos"
      ),]


  summarise(group_by(a,competênciamov), median(salário))
  
  summarise(group_by(a,competênciamov), median(salário))
  
  a$salario_hora <- 
    round(a$salário / 
            (a$horascontratuais)*100, 0)