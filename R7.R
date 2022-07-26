#Dados iniciais
nAmostras <- 7680
expSeed <- 212
dim <- 49
nBern <- 35
pSuccess <- 0.95

#Gerar Amostra
set.seed(seed = expSeed)
amostra <- replicate(nAmostras, rbinom(n = dim, size = nBern, prob = pSuccess))
mMedia <- apply(amostra,2,mean)

#Resultado
valEspMed <- mean(mMedia)
valEspX <- nBern*pSuccess
res <- abs(valEspMed-valEspX)
format(round(res, 6), nsmall = 6)
