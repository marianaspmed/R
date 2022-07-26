library(tidyverse)
library(ggpubr)

vals <- c(4,29,76)
expSeed <- 1528
numAmostras <- 950
minInt <- 11
maxInt <- 15
i <- 0
nMedia <- data.frame(matrix(0,numAmostras,length(vals)))
DesvPad <- matrix(0,1,length(vals))
var <- (((maxInt-minInt)^2)/12)
esp <- ((maxInt+minInt)/2)

for (v in vals) {
  set.seed(seed = expSeed)
  amostra <- replicate(numAmostras, runif(n = v, min = minInt, max = maxInt))
  i <- i + 1
  DesvPad[i] <- sqrt(var/v)
  nMedia[,i] <- apply(amostra,2,mean)
  grafico <- ggplot(nMedia, aes(x = nMedia[,i])) + scale_y_continuous(labels = scales::percent) +
    geom_histogram(aes(y = after_stat(count / sum(count))), color="lightblue", bins = 14) +
    stat_function(fun = dnorm, args = list(mean = esp,sd = DesvPad[i])) +
    labs(x = 'Valores obtidos',y = 'Frequencia Relativa')
  newV <- paste0("grafico", i)
  assign(newV,grafico)
}
histGraf <- ggarrange(grafico1,grafico2,grafico3,ncol = 3,labels = c('n = 4','n = 29','n = 76'), vjust = 1.5, hjust = -0.2)
annotate_figure(histGraf, top = text_grob("Histograma de frequencia relativa associado aos valores obtidos da distribuicao da media de X",
                color = "black", size = 12))

