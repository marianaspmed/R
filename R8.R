#Dados iniciais
expSeed <- 60
nAmostras <- 1200
dim <- 829
lamb <- 1.12
lvlConf <- 0.94

set.seed(seed = expSeed)
amostra <- replicate(nAmostras, rexp(n = dim, rate = lamb))
media <- apply(amostra,2,mean)

alpha <- 1 - lvlConf
quantAlpha <- qnorm(1-alpha/2)

limAmp <- array (dim = nAmostras)
for (i in 1:length(media)){
  limInf <- ((1-(quantAlpha/sqrt(dim)))/media[i])
  limSup <- ((1+(quantAlpha/sqrt(dim)))/media[i])
  limAmp[i] <- limSup - limInf
}
res <- mean(limAmp)
format(round(res, 6), nsmall = 6)
