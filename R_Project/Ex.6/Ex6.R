dist_benford <- function(x) {
  log10(1+1/x)
}

#1
prob4 <- dist_benford(4)
prob9 <- dist_benford(9)
prob_total <- prob4 + prob9

#2
contador_num = 0
contador_potencia = 0
for(potencia2 in 3:31){ #mete-se 31 para fazer o 30
  primeiro_numero <- as.integer(substr(2^potencia2, 1, 1))
  if(primeiro_numero == 4 || primeiro_numero == 9){
    contador_num <- contador_num + 1
  }
  contador_potencia <- contador_potencia + 1
}
fracao_potencias = contador_num / contador_potencia

#3
desvio_absoluto12 <- abs(prob_total - fracao_potencias)

#4
desvio_absoluto12 <- round(desvio_absoluto12, 4)
desvio_absoluto12