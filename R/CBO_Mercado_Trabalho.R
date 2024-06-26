a <- movimentacao[movimentacao$cbo2002ocupa��o %in% c("223505","322230","322205","516210"),]

a <- a[a$munic�pio %in% c("355030"),]

a <- 
summarise(group_by(a, compet�nciamov, NO_CBO), mean(sal�rio)) %>%
  spread(key = compet�nciamov, value = `mean(sal�rio)`)



med <- movimentacao[movimentacao$cbo2002ocupa��o %in% c("223505","322230","322205","516210"),]

med <- med[med$munic�pio %in% c("355030"),]

med <- 
summarise(group_by(med, compet�nciamov, NO_CBO), median(sal�rio)) %>%
  spread(key = compet�nciamov, value = `median(sal�rio)`)



plot(as.numeric(med[1,]), type = "l")
plot(as.numeric(med[2,]), type = "l")
plot(as.numeric(med[3,]), type = "l")
plot(as.numeric(med[4,]), type = "l")




a <- 
med[med$cbo2002ocupa��o == "516210" & 
      med$denomina��o %in% c(
        "Cl�nicas e resid�ncias geri�tricas",
        "Institui��es de longa perman�ncia para idosos",
        "Atividades de fornecimento de infraestrutura de apoio e assist�ncia a paciente no domic�lio",
        "Condom�nios residenciais para idosos"
      ),]


  summarise(group_by(a,compet�nciamov), median(sal�rio))
  
  summarise(group_by(a,compet�nciamov), median(sal�rio))
  
  a$salario_hora <- 
    round(a$sal�rio / 
            (a$horascontratuais)*100, 0)