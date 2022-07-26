library(readxl)
library(ggplot2)
library(tidyr)

emv <- read_excel("./data/EsperancaVida.xlsx")

colunas <- c("Anos", "DK - Dinamarca", "FR - França", "SE - Suécia")
emv <- emv[-c(1:3), -c(2:35)]
emv <- Filter(function(x)!all(is.na(x)), emv)
emv[1, 36:69] <- "Mulheres"
emv[1, 2:35] <- "Homens"
emv[2,1] <- "Anos"
emv <- emv[, emv[2,] %in% colunas]
emv <- emv[c(1, 2, 45:62),]
colnames(emv) <- emv[2,]
emv <- emv[-c(2),]
mulher_emv <- emv[,c(1, 5:7)]
emv <- emv[, -c(5:7)]
emv <- pivot_longer(emv, values_to = "EMV", names_to = "pais", c("DK - Dinamarca", "FR - França", "SE - Suécia"))[-c(1:3),]
mulher_emv <- pivot_longer(mulher_emv, c("DK - Dinamarca", "FR - França", "SE - Suécia"),
                            values_to = "EMV", names_to = "pais")[-c(1:3),]
mulher_emv$Genero <- "Mulher"
emv$Genero <- "Homem"
emv <- rbind(emv,mulher_emv)
emv$EMV <- as.double(emv$EMV)
round(emv$EMV, digits=1)
ggplot(emv, aes(x=Anos, y=EMV, shape = Genero)) +
  geom_line(lwd=1.1,aes(group=interaction(pais,Genero),colour=pais)) + geom_point(size = 5, alpha = 0.7)+
  geom_smooth(se = F)+ theme_minimal()+ scale_color_manual(values=c("#ad1519", "#001489",'#008724'))
