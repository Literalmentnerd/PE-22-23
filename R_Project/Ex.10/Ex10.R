set.seed(615)

m <- 100  # número de amostras
n <- 31   # tamanho da amostra

mu_true <- 72.8
sigma <- 4  # desvio padrão (raiz quadrada da variância)

alpha <- 0.06  # nível de significância
reject_count <- 0  # contador de rejeições

for (i in 1:m) {
  # Gerar amostra aleatória
  sample <- rnorm(n, mean = mu_true, sd = sigma)
  
  # Teste de hipóteses
  t_stat <- (mean(sample) - 34.4) / (sigma / sqrt(n))  # estatística de teste
  
  # Rejeitar H0 se o valor absoluto da estatística de teste for maior que o valor crítico
  if (abs(t_stat) > qnorm(1 - alpha/2)) {
    reject_count <- reject_count + 1
  }
}

# Estimativa da probabilidade de não rejeição de H0
p_non_reject <- 1 - reject_count/m

# Exibir o resultado com 3 casas decimais
round(p_non_reject, 3)

set.seed(225)
m = 100
n = 43
mu = 33.2
sigma = 2

# Gerar m amostras de tamanho n
samples = replicate(m, rnorm(n, mu, sigma))

# Calcular a média de cada amostra
sample_means = colMeans(samples)

# Calcular o valor z para cada amostra
z_values = (sample_means - 34.4) / (sigma / sqrt(n))

# Calcular os valores críticos para α = 0.06
alpha = 0.06
z_critical = qnorm(c(alpha / 2, 1 - alpha / 2))

# Verificar se os valores z estão dentro dos valores críticos
not_rejected = z_values >= z_critical[1] & z_values <= z_critical[2]

# Calcular a proporção de testes que não rejeitaram H0
p_not_rejected = sum(not_rejected) / m

# Imprimir o resultado com 3 casas decimais
print(round(p_not_rejected, 3))

---------------------------------------------------------

null_value <- 34.4  # Valor esperado sob a hipótese nula
mu <- 33.2  # Valor esperado sob a hipótese alternativa
sigma <- 2  # Desvio padrão conhecido

set.seed(225)

m <- 100  # Número de amostras
n <- 43  # Tamanho das amostras
samples <- matrix(rnorm(m * n, mean = mu, sd = sigma), ncol = n)

test_results <- numeric(m)

for (i in 1:m) {
  sample <- samples[i,]
  test_results[i] <- t.test(sample, mu = null_value)$p.value
}

rejection_prob <- mean(test_results >= 0.06)
print(round(rejection_prob, 3))