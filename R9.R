library(tidyverse)
nvlConf <- 0.999
numAmostras <- 1200
expSeed <- 816
lambda <- 3.43
dims <- seq(100, 5000, by = 100)
alpha <- 1 - nvlConf
quantAlpha <- qnorm(1-alpha/2)
j <- 0
graf <- data.frame(matrix(nrow = length(dims),ncol = 2))
colnames(graf) <- c('dim','MA')
for (dim in dims){
  set.seed(seed = expSeed)
  amostra <- replicate(numAmostras, rexp(n = dim, rate = lambda))
  limAmp <- array (dim = numAmostras)
  mediaN <- apply(amostra,2,mean)
  for (i in 1:length(mediaN)){
    limSup <- ((1+(quantAlpha/sqrt(dim)))/mediaN[i])
    limInf <- ((1-(quantAlpha/sqrt(dim)))/mediaN[i])
    limAmp[i] <- limSup - limInf
  }
  j <- j + 1
  graf[j,2] <- mean(limAmp)
  graf[j,1] <- dim
}
ggplot(graf,aes(x = dim, y = MA))+ geom_point(colour = "orange", size = 4)+
  labs(title = 'Media da amplitude dos 1200 intervalos de confianca com a dimensao da amostra no eixo xxx',
       x = 'Dimensao da amostra n',
       y = 'Media da amplitude do intervalo de confianca dos m intervalos de confianca')

