set.seed(1832)

p <- 0.25
n <- 17

# Função de distribuição acumulada da distribuição geométrica
F_X <- function(x, p) {
  if (x < 0) {
    return(0)
  } else {
    return(1-p*(1-p)^x)
  }
}

# Gerar amostra da distribuição geométrica usando o método de transformação inversa
amostra <- numeric(n)
for (i in 1:n) {
  u <- runif(1)  # Simular valor proveniente de uma distribuição uniforme
  x <- 0
  while (F_X(x-1, p) < u) {
    x <- x + 1
  }
  amostra[i] <- x
}

# Calcular média e desvio padrão amostrais
media_amostral <- mean(amostra)
desvio_padrao_amostral <- sd(amostra)

# Calcular a proporção de valores simulados que atendem às condições
proporcao <- sum(amostra > media_amostral + desvio_padrao_amostral) / n

# Apresentar o resultado com 4 casas decimais
proporcao <- round(proporcao, 4)
proporcao