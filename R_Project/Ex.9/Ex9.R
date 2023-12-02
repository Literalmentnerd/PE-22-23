set.seed(1083)

n <- c(30, 50, 100, 200, 300, 500, 1000)
k <- 1500
p <- 0.5
confidence_level <- 0.96

amostras <- rbinom(n,k,p)

----------------------------------------------------------
set.seed(1461)

p <- 0.6
gamma <- 0.96
n_values <- c(30, 50, 100, 200, 300, 500, 1000)
k <- 2000
mean_diff <- numeric(length(n_values))

for (i in 1:length(n_values)) {
  n <- n_values[i]
  diff_sum <- 0
  
  for (j in 1:k) {
    samples <- rbinom(n, 1, p)
    x_bar <- mean(samples)
    
    z <- qnorm((1 + gamma) / 2)
    
    p_hat1 <- (-x_bar + sqrt(x_bar^2 - (z^2) * x_bar * (1 - x_bar) / n)) / (x_bar^2 + 1)
    p_hat2 <- x_bar
    
    diff <- 2 * abs(p_hat2 - p_hat1)
    diff_sum <- diff_sum + diff
  }
  
  mean_diff[i] <- diff_sum / k
}

plot(n_values, mean_diff, type = "b", xlab = "Tamanho da Amostra (n)", ylab = "Diferença Média",
     main = "Variação das Diferenças Médias em Função do Tamanho da Amostra") 
---------------------------------------------------------------------
set.seed(1083)  # Fixar a semente em 1083

n_values <- c(30, 50, 100, 200, 300, 500, 1000)  # Valores de n
k <- 1500  # Número de amostras
p <- 0.5  # Parâmetro da distribuição de Bernoulli
gamma <- 0.96  # Nível de confiança aproximado

mean_diff <- numeric(length(n_values))  # Vetor para armazenar as médias das diferenças

# Loop sobre os valores de n
for (i in seq_along(n_values)) {
  n <- n_values[i]
  diff_lengths <- numeric(k)  # Vetor para armazenar as diferenças de comprimento
  
  # Loop para gerar as amostras e calcular as diferenças de comprimento
  for (j in 1:k) {
    # Gerar amostras de tamanho n da distribuição de Bernoulli com parâmetro p
    samples <- rbinom(n, 1, p)
    
    # Método 1
    x_bar <- mean(samples)
    z <- qnorm(1 + gamma/2)
    p1 <- (-x_bar + sqrt(x_bar^2 + 4 * z^2 * p * (1 - p) / n)) / (2 * (x_bar + z^2 / (2 * n)))
    
    # Método 2
    p2 <- x_bar + z^2 / (2 * n)
    
    # Calcular a diferença de comprimento
    diff_lengths[j] <- abs(p2 - p1)
  }
  
  # Calcular a média das diferenças de comprimento
  mean_diff[i] <- mean(diff_lengths)
}

# Construir o gráfico
plot(n_values, mean_diff, type = "b", xlab = "Tamanho da Amostra (n)", ylab = "Diferença Média",
     main = "Diferença Média entre os Métodos 2 e 1", pch = 19)
-----------------------------------------------------------------------------------------
library(ggplot2)
set.seed(1461)

n_values <- c(30, 50, 100, 200, 300, 500, 1000)
k <- 2000
p <- 0.6
nivel_de_confianca <- 0.96

# Metodo 1
method1 <- function(x_bar, n, z) {
  discriminant <- x_bar^2 - 2 * p * x_bar + p^2 - z^2 * p * (1 - p) / n
  intervalo <- 2 * sqrt(max(0, discriminant))
  return(intervalo)
}

# Metodo 2
method2 <- function(x_bar, n) {
  intervalo <- 2 * qnorm((1 + nivel_de_confianca) / 2) * sqrt(x_bar * (1 - x_bar) / n)
  return(intervalo)
}

diferenca_media <- numeric(length(n_values))

for (i in seq_along(n_values)) {
  n <- n_values[i]
  diferenca <- numeric(k)
  
  for (j in 1:k) {
    sample <- rbinom(n, size = 1, prob = p)
    x_bar <- mean(sample)
    z <- qnorm((1 + nivel_de_confianca) / 2)
    
    intervalo_method1 <- method1(x_bar, n, z)
    intervalo_method2 <- method2(x_bar, n)
    
    diferenca[j] <- intervalo_method2 - intervalo_method1
  }
  
  valid_differences <- diferenca[is.finite(diferenca)]
  diferenca_media[i] <- mean(valid_differences)
}

data <- data.frame(n = n_values, diferenca_media = diferenca_media)

ggplot(data, aes(x = n, y = diferenca_media)) +
  geom_point() +
  geom_line() +
  labs(x = "Tamanho da amostra (n)", y = "Média das diferenças",
       title = "Média das diferenças em função da dimensão da amostra") +
  ylim(min(diferenca_media), max(diferenca_media))