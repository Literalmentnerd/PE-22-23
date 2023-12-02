set.seed(1793)

n <- 14
m <- 2705

amostras <- matrix(rnorm(m*n, mean = 0, sd = 1), nrow = m, ncol = n)

soma_quadrados <- apply(amostras, 1, function(x) sum(x^2))

quantil_amostra <- quantile(soma_quadrados, probs = 0.75, type = 2)

quantil_teorico <- qchisq(0.75, df = n)

diferença <- abs(quantil_amostra - quantil_teorico)

diferença <- round(diferença, 4)
diferença