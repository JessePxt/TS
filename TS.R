#Limpa todo o workspace
rm(list=ls())

#Pacotes
install.packages("quantmod")
install.packages("fBasics")
install.packages("car")
library(quantmod)
library(fBasics)
library(car)

#Dados da Hapvida
getSymbols("HAPV3.SA",from="2021-01-01",to="2022-12-31") #período específico

#Serie do indice bovespa
ibv.ts <- ts(data=HAPV3.SA$HAPV3.SA.Adjusted, frequency = 1) 
plot(ibv.ts, col="blue", lwd=2, ylab="ibv", xlab="")

#Retorno, diferentes formulas
Ribv.ts <- diff(ibv.ts)/lag(ibv.ts, k=-1)     #retorno liquido simples
ribv.ts <- diff(log(ibv.ts))     #log-retorno
plot(Ribv.ts, col="blue", lwd=2, ylab="Ribv", xlab="")
plot(ribv.ts, col="blue", lwd=2, ylab="ribv", xlab="")

#Plot do retorno simples
hist(ribv.ts, freq=FALSE, nclass=24, ylab="", xlab="", ylim=c(0,25), main="") #histograma
lines(density(ribv.ts, adjust=5), col="blue") #estimação da densidade
qqnorm(ribv.ts, pch=19) #Plot QQ
qqline(ribv.ts, col = "blue")
qqPlot(ribv.ts, envelope=0.95) #Plot alternativo com intervalo de confiança

#Estatísticas descritivas, assimetria e curtose
basicStats(ribv.ts) #estatísticas descritivas
mean(ribv.ts) #média
var(ribv.ts) #variância
stdev(ribv.ts) #desvio-padrão
t.test(ribv.ts)  # teste para média dos retornos = 0
normalTest(ribv.ts,method='jb') # teste de Normalidade de Jarque-Bera

#Log retorno
HAPV3.SA.rtn <- diff(log(HAPV3.SA$HAPV3.SA.Adjusted)) # log retorno
chartSeries(HAPV3.SA.rtn,theme="white")
