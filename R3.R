library(readxl)
library(ggplot2)
library(reshape2)

data <- read_excel("data/QualidadeARO3.xlsx", sheet = 1,col_types = "numeric")
cols<-c(grep("Entrecampos",names(data)),grep("Mem-Martins",names(data)))
melted_data <- melt(data.frame(data[cols]),id.vars=0)

g<-ggplot(data=melted_data,aes(x=value,fill=variable))+geom_histogram(binwidth = 7)
g<-g+labs(title = "Niveis de ozono em Entrecampos e Mem-Martins - 2020", x="Valores de niveis de ozono",y="Numero de observacoes")
g
