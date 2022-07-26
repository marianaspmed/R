library(readxl)
library(ggplot2)
library(tidyr)

data <- read_excel("Utentes.xlsx")

ggplot(data, aes(x=IMC, y=Colesterol)) +
  geom_point(size=2, shape=23) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
