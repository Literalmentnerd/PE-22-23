k <- 2593
lambda <- 2

set.seed(2313)

#fexp <- function(x, lambda){ 
# fx <- ifelse(x<0, 0, (lambda)*exp(-x*lambda))
#return(fx)
#}

amostra <- rexp(k, rate = lambda)

# Calcular instante de ocorrência do último acontecimento
s <- cumsum(amostra)  # Soma cumulativa das observações
T <- ceiling(s[k])  # Menor número inteiro maior ou igual ao instante de ocorrência do último acontecimento

# Criar vetor com os limites dos subintervalos
limites <- seq(0, T, by = 1)

# Contabilizar o número de acontecimentos em cada subintervalo
num_acontecimentos <- tabulate(findInterval(s, limites))

# Calcular o valor esperado teórico do número de acontecimentos em um subintervalo
valor_esperado <- lambda

# Calcular a média do número de acontecimentos por subintervalo
media <- mean(num_acontecimentos)

# Calcular o desvio absoluto entre a média e o valor esperado
desvio_absoluto <- abs(media - valor_esperado)

# Arredondar o desvio absoluto para 4 casas decimais
desvio_absoluto <- round(desvio_absoluto, 4)

# Apresentar o desvio absoluto
desvio_absoluto