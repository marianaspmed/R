library(tidyverse)
#Dados iniciais
expSeed <- 975 ; nAmostras <- 950 ; dimArray <- seq(100, 2500, by = 100)
lambOrig <- 0.85  ; lambCont <- 0.37  ; lvlConf <- 0.96 ; epsilon <- 0.4

alpha <- 1 - lvlConf
quantAlpha <- qnorm(1-alpha/2)
dfGraf <- data.frame(matrix(nrow = 2*length(dimArray),ncol = 3))
colnames(dfGraf) <- c('dim','type','medAmp')

j <- 0
for (dim in dimArray){
  j <- j + 1
  set.seed(seed = expSeed)
  amostra <- replicate(nAmostras, rexp(n = dim, rate = lambOrig))
  media <- apply(amostra,2,mean)
  limAmp <- array(dim = nAmostras)
  for (i in 1:nAmostras){
    limInf <- ((1-(quantAlpha/sqrt(dim)))/media[i])
    limSup <- ((1+(quantAlpha/sqrt(dim)))/media[i])
    limAmp[i] <- limSup - limInf
  }
  dfGraf[j,1] <- dim ; dfGraf[j,2] <- 'MAn' ; dfGraf[j,3] <- mean(limAmp)
}

for (dim in dimArray){
  j <- j + 1
  set.seed(seed = expSeed)
  amostra <- replicate(nAmostras, rexp(n = dim, rate = lambOrig))
  outliers <- replicate(nAmostras, rexp(n = dim*epsilon, rate = lambCont))
  amostra[1:nrow(outliers),1:nAmostras] <- outliers
  media <- apply(amostra,2,mean)
  limAmp <- array(dim = nAmostras)
  for (i in 1:nAmostras){
    limInf <- ((1-(quantAlpha/sqrt(dim)))/media[i])
    limSup <- ((1+(quantAlpha/sqrt(dim)))/media[i])
    limAmp[i] <- limSup - limInf
  }
  dfGraf[j,1] <- dim ; dfGraf[j,2] <- 'MACn' ; dfGraf[j,3] <- mean(limAmp)
}

ggplot(dfGraf,aes(x = dim, y = medAmp, color = type))+
  geom_point(size = 5, alpha = 0.7)+
  scale_color_manual(values=c('#ad1519','#008724'))+
  theme_minimal()+
  labs(title = 'Media da amplitude do intervalo de confianca vs dimensao da amostra',
       caption = 'Semente = 50 | m = 1000 | lambda = 4.57 | lambdaC = 0.19 | epsilon = 0.2 | 1 - alfa = 0.98',
       x = 'Dimensao das m amostras',
       y = 'Media da amplitude do intervalo de confianca das m amostras')
