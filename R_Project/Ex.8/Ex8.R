library(ggplot2)

set.seed(1680)
n <- 182
amostra_c <- sort(rcauchy(n, location = -4, scale = 1.8))
quantis_c <- quantile(amostra_c, seq(1/(n+1), n/(n+1), length.out = n))

amostra_n <- sort(rnorm(n, mean = -1.4, sd = sqrt(3.8)))
quantis_n <- quantile(amostra_n, seq(1/(n+1), n/(n+1), length.out = n))

df <- data.frame(
  valores_cauchy = amostra_c,
  quantis_cauchy = quantis_c,
  quantis_normal = quantis_n)

ggplot(df, aes(x = valores_cauchy)) +
  geom_point(aes(y = quantis_cauchy), color = "green") +
  geom_point(aes(y = quantis_normal), color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "orange") +
  labs(x = "Valores Ordenados", y = "Quantis de Probabilidade", title = "Distribuição de Cauchy vs. Distribuição Normal")

------------------------------------------------------------------------------

set.seed(2009)

cauchy_sample <- rcauchy(107, 1, 1.8)
quantiles_cauchy <- quantile(cauchy_sample, seq(1/108, 107/108, by = 1/108))

normal_sample <- rnorm(107, -2.8, sqrt(1.4))
quantiles_normal <- quantile(normal_sample, seq(1/108, 107/108, by = 1/108))

plot(sort(cauchy_sample), quantiles_cauchy, type = "l", col = "blue", xlab = "Valores Ordenados", ylab = "Quantis de Probabilidade", main = "Distribuição de Cauchy vs. Distribuição Normal")
lines(sort(cauchy_sample), quantiles_normal, type = "l", col = "red")

abline(0, 1, lty = 2, col = "green")

------------------------------------------------------------------------------

# Definindo a semente
set.seed(123)

# Parâmetros da distribuição de Cauchy
e <- 0  # Parâmetro de localização
s <- 1  # Parâmetro de escala

# Tamanho da amostra
n <- 100

# Gerando uma amostra da distribuição de Cauchy
sample_cauchy <- rcauchy(n, e, s)

# Ordenando os valores gerados
sample_cauchy_sorted <- sort(sample_cauchy)

# Calculando os quantis de probabilidade para a distribuição de Cauchy
probs_cauchy <- (1:n - 0.5) / n

# Calculando os quantis de probabilidade para a distribuição Normal
mean_normal <- e  # Média da distribuição normal
var_normal <- s^2  # Variância da distribuição normal
probs_normal <- qnorm(probs_cauchy, mean = mean_normal, sd = sqrt(var_normal))

# Preparando o gráfico
plot(sample_cauchy_sorted, probs_cauchy, xlab = "Valores gerados", ylab = "Quantil de probabilidade", main = "Gráfico Q-Q", xlim = c(min(sample_cauchy_sorted), max(sample_cauchy_sorted)), ylim = c(min(probs_cauchy, probs_normal), max(probs_cauchy, probs_normal)), type = "n")

# Adicionando os pontos para a distribuição de Cauchy
points(sample_cauchy_sorted, probs_cauchy, col = "blue", pch = 19)

# Adicionando os pontos para a distribuição Normal
points(sample_cauchy_sorted, probs_normal, col = "red", pch = 19)

# Adicionando a reta bissectriz dos quadrantes ímpares
abline(a = 0, b = 1, col = "black", lty = 2)

# Adicionando legenda
legend("bottomright", legend = c("Cauchy", "Normal", "Reta bissectriz"), col = c("blue", "red", "black"), pch = 19, lty = c(1, 1, 2))