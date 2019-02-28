rm(list=ls())
Sys.setenv(TZ='UTC')

#install.packages("xts")
#install.packages("estudy2")
#install.packages("tidyquant")
#install.packages("PerformanceAnalytics")
#install.packages("DescTools")
#install.packages('RDCOMClient', repos = 'http://www.omegahat.net/R/')
library(readxl)
library(xts)
library(estudy2)
library(tidyquant)
library(PerformanceAnalytics)
library(DescTools)
library(RDCOMClient)
getwd()
dir()

#loading data ----

mydata <- read_xlsx(file.choose(),
                    sheet = "precios",
                    skip=2,
                    na='NA',
                    col_types = c('date',rep("numeric",10)))


summary(mydata)
class(mydata)
names(mydata)
mydata$ECO

hist(mydata$BOG)

#converting to xts format -----
prices <- as.xts(mydata[,-1],mydata[[1]])
prices['2018-01-05/2018-01-10',"ECO"]

prices <- to.monthly( prices, OHLC=FALSE)
rates <- get_rates_from_prices(prices,
                      multi_day = T,
                      compounding = "continuous")


#Analisis de los activos ------
table.Stats(rates)
#para sacar reportes automáticos
#XLView(table.Stats(rates),row.names = T)

#chart.Histogram(rates$BOG)
#se quita myshares para mayor número de acciones
#myshares <- c("BOG","SUR","CHO")
#rates <- rates[,myshares]

#calcula la media por cada columna es la opción 2
#Calculo e retorno promedio
myreturns <- apply(rates,2,mean)
mysd <- apply(rates,2,sd)
mycov <- cov(rates)
mycor <- cor(rates)
plot(mysd,myreturns)

w1 <- matrix(c(0.1,0.2,0.1,0.1,0,0,0,0.2,0.2,0.1),ncol = 1)

rp <- t(w1)%*%myreturns

s2p <- t(w1)%*%mycov%*%w1
sp <- sqrt(s2p)


meanVariancePortfolio <- function(weight,returns,variances){
  meanPortfolio <- t(weight)%*%returns
  varPortfolio <- t(weight)%*%variances%*%weight
  return(c(meanPortfolio,sqrt(varPortfolio)))
  
}

#meanVariancePortfolio(w1,myreturns,mycov)
#Se coloca 1:10 debido a capacidad del PC
cant <- 1:10
#Depende del número de activos se agrega el número de cant
p2 <- expand.grid(cant,cant,cant,cant,cant)
p3<- expand.grid(var6=cant,var7=cant,var8=cant,var9=cant,var10=cant)
p2New <- cbind(p2,p3)
#p2 <- cbind(p2,expand.grid(cant,cant,cant,cant,cant))
grandTotal <- apply(p2New,1,sum)
w2 <- p2New/grandTotal

p2New.returns <- apply(w2,1,function(x)meanVariancePortfolio(x,myreturns,mycov)[1])
p2New.sd <- apply(w2,1,function(x) meanVariancePortfolio(x,myreturns,mycov)[2])

plot(p2New.sd,p2New.returns)

p2News <- cbind(w2,p2New.returns,p2New.sd)
which(p2News$p2New.sd==min(p2News$p2New.sd))

ggplot(p2News,aes(x=p2New.sd, y=p2New.returns))+geom_point()
