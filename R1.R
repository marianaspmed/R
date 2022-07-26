library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)

residuos<-read_excel("./data/ResiduosperCapita.xlsx",sheet = 1,skip=11,n_max = 31,col_types = c("text","numeric","numeric"), col_names = c('pais','2004','2018'))

linhas<-c(grep("HR",residuos$pais),grep("IE",residuos$pais),grep("FR",residuos$pais))
melted_data<-melt(data.frame(residuos[linhas,]),id.vars="pais")

grafico<-ggplot(data=melted_data,aes(fill=variable, x=pais, y=value))
grafico<-grafico+geom_bar(stat="identity",position = "dodge",width = 0.8)
grafico<-grafico+labs(title = "Residuos Per Capita em Paises da UE",x="Pais",y="Residuos per capita",fill="Anos")

grafico

