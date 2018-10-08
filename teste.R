setwd("/Users/fsmoura/Documents/R-files/machine-learning-e-data-science-com-r/")
dados = read.csv(file = "autuacoes-ocup.csv", header = TRUE, sep = ";")
#View(dados)
library(ggplot2)
par(mfrow=c(1,1))#divide a area de plotagem em uma coluna
scatter.smooth(x=dados$Ocupacao, y=dados$Autuacao, mail="Ocupacao ~ Autuacao")
dados$Autuacao = as.numeric(dados$Autuacao)
dados$Autuacao <- as.numeric(dados$Autuacao)
dados$Ocupacao.1 <- as.double(dados$Ocupacao.1)

qplot(Autuacao, Ocupacao.1, data = dados, main = "Relação Fiscalização vs Ocupação") +
  stat_smooth(method="lm", col="red")

par(mfrow=c(1,2))
qplot(log(Autuacao), Ocupacao.1, data = dados, main = "Relação Fiscalização vs Ocupação") +
  stat_smooth(method="lm", col="red")
mod1 = lm(formula = Ocupacao.1 ~ Autuacao, data = dados)
summary(mod1)

dados2 = read.csv(file = "monitores-ocup.csv", header = TRUE, sep = ";")
#View(dados)
qplot(Orientadores, Ocupa, data = dados2, main = "Relação Fiscalização vs Ocupação") +
  stat_smooth(method="lm", col="red")
mod2 = lm(formula = Ocupa ~ Orientadores, data = dados2)
summary(mod2)
summary(mod2)$r.squared
summary(mod2)$adj.r.squared
summary(lm(Ocupa~Orientadores, dados2))$r.squared
summary(lm(Ocupa~Orientadores, dados2))$adj.r.squared
cor(dados2$Orientadores, dados2$Ocupa)

