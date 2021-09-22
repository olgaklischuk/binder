library("forecast")
library("ggplot2")
library("gridExtra")
library("gtable")
setwd(Dir.)
#Ukraine----
{load("H:/Стаття 7/article(2018)0")
AR.interest.ua<-auto.arima(channeljd1$credit)
AR.interest.ua<-auto.arima(channeljd1$interest)
AR.credit.ua<-auto.arima(channeljd1$credit)
AR.reer.ua<-auto.arima(channeljd1$reer,ic="aic") #- no unit roots
AR.broad.money.ua<-auto.arima(channeljd1$broad.money)
AR.GDP.ua<-auto.arima(channeljd1$GDP)
AR.CPI.ua<-auto.arima(channeljd1$CPI)
AR.stock.index.ua<-auto.arima(channeljd1$stock.index)}
title<-c("Available unit roots in interest time series","Available unit roots in crediting size time series",
         "Available unit roots in reer time series","Available unit roots in broad money time series",
         "Available unit roots in GDP time series","Available unit roots in CPI time series",
         "Available unit roots in stock.index time series")
subtitle<-c("Ukraine")

#par(mfrow=c(4,4),oma=c(.1,.1,.1,.1))#,mfg=c(RU.Ukraine[1:7],1:14),RU.Ukraine[2],RU.Ukraine[3],RU.Ukraine[4],RU.Ukraine[5],
#RU.Ukraine[6],RU.Ukraine[7],RU.Ukraine[8],RU.Ukraine[9],RU.Ukraine[10])
#plot#####
{RU.interest.ua<-ggplot2::autoplot(AR.interest.ua,type = "ar",main = title[1],sub=subtitle[1])
  RU.credit.ua<-ggplot2::autoplot(AR.credit.ua,type="ar",main = title[2],sub=subtitle[1])
  RU.broad.money.ua<-ggplot2::autoplot(AR.broad.money.ua,type = "ar",main = title[4],sub=subtitle[1])
  RU.GDP.ua<-ggplot2::autoplot(AR.GDP.ua,type = "ar",main = title[5],sub=subtitle[1])
  RU.CPI.ua<-ggplot2::autoplot(AR.CPI.ua,type = "ar",main = title[6],sub=subtitle[1])
  RU.stock.index.ua<-ggplot2::autoplot(AR.stock.index.ua,type = "both",main = title[7],sub=subtitle[1])}

{par(mfrow=c(4,4),oma=c(.1,.1,.1,.1))
  library(ggplot2)
  library(gtable)
  library(gridExtra)
  setwd(Dir.2.10)
  autoplot(AR.interest.ua,type = "ar",main = title[1],sub=subtitle[1])
  autoplot(AR.credit.ua,type="ar",main = title[2],sub=subtitle[1])
  autoplot(AR.broad.money.ua,type = "ar",main = title[4],sub=subtitle[1])
  autoplot(AR.GDP.ua,type = "ar",main = title[5],sub=subtitle[1])
  autoplot(AR.CPI.ua,type = "ar",main = title[6],sub=subtitle[1])
  autoplot(AR.stock.index.ua,type = "both",main = title[7],sub=subtitle[1])}
RU.Ukraine<-c(RU.interest.ua,RU.credit.ua,RU.broad.money.ua,RU.GDP.ua,RU.CPI.ua,RU.stock.index.ua)

plot.ukraine<-grid.arrange(RU.interest.ua,RU.credit.ua,RU.broad.money.ua,RU.GDP.ua,RU.CPI.ua,RU.stock.index.ua,nrow=4)

setwd(Dir.)

ggsave("UR.Ukraine.png",plot=plot.ukraine)
