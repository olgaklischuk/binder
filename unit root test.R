load("J:/І розділ/Монетарний механізм/data/3.06.RData")

library(urca)
transmission<-transmission[1:135,]
#"http://cowles.econ.yale.edu/"
{
  REER.df.1<-ur.df((transmission$REER),type="trend",lags=12,selectlags="AIC")
  cpi.df.1<-ur.df(transmission$cpi.a,type="trend",lags=12,selectlags="AIC")
  ppi.df.1<-ur.df(transmission$ppi.a,type="trend",lags=12,selectlags="AIC")
  ccpi.df.1<-ur.df(transmission$ccpi,type="trend",lags=12,selectlags="AIC")
  asset.prices.df.1<-ur.df(transmission$asset.price,type="trend",lags=12,selectlags="AIC")
  food.prices.df.1<-ur.df(transmission$food.prices,type="trend",lags=12,selectlags="AIC")
  administered.prices.df.1<-ur.df(transmission$administered.prices,type="trend",lags=12,selectlags="AIC")
  deflator.df.1<-ur.df(transmission$deflator[1:133],type="trend",lags=12,selectlags="AIC")
  imported.inflation.df.1<-ur.df(transmission$imported.inflation[1:133],type="trend",lags=12,selectlags="AIC")
  Real.GDP.df.1<-ur.df(transmission$Real.GDP[1:133],type="trend",lags=12,selectlags="AIC")

  DISCOUNT.RATE.df.1<-ur.df(transmission$Discount.rate,type="trend",lags=12,selectlags="AIC")
  real.rate.df.1<-ur.df(transmission$real.rate,type="trend",lags=12,selectlags="AIC")
  interest.gap.df.1<-ur.df(transmission$Interest.gap,type="trend",lags=12,selectlags="AIC")
  arr.df.1<-ur.df(transmission$Average.refinancing.rate,type="trend",lags=12,selectlags="AIC")
  tr.df.1<-ur.df(transmission$tender.rate,type="trend",lags=12,selectlags="AIC")
  or.df.1<-ur.df(transmission$overnight.rate,type="trend",lags=12,selectlags="AIC")
  repo.df.1<-ur.df(transmission$repo.rate[2:130],type="trend",lags=12,selectlags="AIC")
  wir.df.1<-ur.df(transmission$Weighed.interbank.rate,type="trend",lags=12,selectlags="AIC")
  bond.yields.df.1<-ur.df(transmission$bond.yields.h,type="trend",lags=12,selectlags="AIC")

  m3.df.1<-ur.df(transmission$`d(ln(M3_a))`[2:134],type="trend",lags=12,selectlags="AIC")
  rr.df.1<-ur.df(transmission$rr,type="trend",lags=12,selectlags="AIC")

  di.df.1<-ur.df(transmission$`Direct.investment,.net`[3:135],type="trend",lags=12,selectlags="AIC")
  #pi.df<-ur.df(transmission$`Portfolio.investment,.net`,type="trend",selectlags="AIC")
  nx.df.1<-ur.df(transmission$NX[1:133],type="trend",lags=12,selectlags="AIC")
  wages.df.1<-ur.df(transmission$Wages[3:135],type="trend",lags=12,selectlags="AIC")
  shares.df.1<-ur.df(transmission$shares[2:135],type="trend",lags=12,selectlags="AIC")
  bonds.df.1<-ur.df(transmission$`Bonds.issued.by.non-deposit.corporations`,type="trend",lags=12,selectlags="AIC")
}
#####after linearixation and first lags differences#####
{
  REER.df<-ur.df(channeljd1$REER,type="trend",lags = 12,selectlags="AIC")
  cpi.df<-ur.df(channeljd2$cpi.a,type="trend",lags = 12,selectlags="AIC")
  ppi.df<-ur.df(channeljd2$ppi.a,type="trend",lags = 12,selectlags="AIC")
  ccpi.df<-ur.df(channeljd2$ccpi,type="trend",lags = 12,selectlags="AIC")
  asset.prices.df<-ur.df(channeljd1$asset.price,type="trend",lags = 12,selectlags="AIC")
  food.prices.df<-ur.df(channeljd6$food.prices,type="trend",lags = 12,selectlags="AIC")
  administered.prices.df<-ur.df(channeljd6$administered.prices,type="trend",lags = 12,selectlags="AIC")
  deflator.df<-ur.df(channeljd8$deflator,type="trend",lags = 12,selectlags="AIC")
  imported.inflation.df<-ur.df(channeljd6$imported.inflation,type="trend",lags = 12,selectlags="AIC")
  Real.GDP.df<-ur.df(channeljd2$Real.GDP,type="trend",lags = 12,selectlags="AIC")

  DISCOUNT.RATE.df<-ur.df(channeljd1$Discount.rate,type="trend",lags = 12,selectlags="AIC")
  real.rate.df<-ur.df(channeljd1$real.rate,type="trend",lags = 12,selectlags="AIC")
  interest.gap.df<-ur.df(channeljd1$Interest.gap,type="trend",lags = 12,selectlags="AIC")
  arr.df<-ur.df(channeljd3$Average.refinancing.rate,type="trend",lags = 12,selectlags="AIC")
  tr.df<-ur.df(channeljd3$tender.rate,type="trend",lags = 12,selectlags="AIC")
  or.df<-ur.df(channeljd3$overnight.rate,type="trend",lags = 12,selectlags="AIC")
  repo.df<-ur.df(channeljd3$repo.rate,type="trend",lags = 12,selectlags="AIC")
  wir.df<-ur.df(channeljd3$Weighed.interbank.rate,type="trend",lags = 12,selectlags="AIC")
  bond.yields.df<-ur.df(channeljd3$bond.yields.h,type="trend",lags = 12,selectlags="AIC")

  m3.df<-ur.df(channeljd3$`d(ln(M3_a))`,type="trend",lags = 12,selectlags="AIC")
  rr.df<-ur.df(channeljd3$rr,type="trend",lags = 12,selectlags="AIC")

  di.df<-ur.df(channeljd8$`Direct.investment,.net`[2:135],type="trend",lags = 12,selectlags="AIC")
  #pi.df<-ur.df(transmission$Portfolio.investment,.net,type="trend",selectlags="AIC")
  nx.df<-ur.df(channeljd6$NX,type="trend",lags = 12,selectlags="AIC")
  wages.df<-ur.df(channeljd9$Wages,type="trend",lags = 12,selectlags="AIC")
  shares.df<-ur.df(channeljd9$shares,type="trend",lags = 12,selectlags="AIC")
  bonds.df<-ur.df(channeljd8$`Bonds.issued.by.non-deposit.corporations`,type="trend",lags = 12,selectlags="AIC")
}
options(digits=4)
{n<-26;Table.Unit.root.test.before<-matrix(c(array(REER.df.1@teststat),
                                             array(cpi.df.1@teststat),
                                             array(ppi.df.1@teststat),
                                             array(ccpi.df.1@teststat),
                                             array(asset.prices.df.1@teststat),
                                             array(food.prices.df.1@teststat),
                                             array(administered.prices.df.1@teststat),
                                             array(deflator.df.1@teststat),
                                             array(imported.inflation.df.1@teststat),
                                             array(Real.GDP.df.1@teststat),
                                             array(DISCOUNT.RATE.df.1@teststat),
                                             array(real.rate.df.1@teststat),
                                             array(interest.gap.df.1@teststat),
                                             array(arr.df.1@teststat),
                                             array(tr.df.1@teststat),
                                             array(or.df.1@teststat),
                                             array(repo.df.1@teststat),
                                             array(wir.df.1@teststat),
                                             array(bond.yields.df.1@teststat),
                                             array(m3.df.1@teststat),
                                             array(rr.df.1@teststat),
                                             array(di.df.1@teststat),
                                             array(nx.df.1@teststat),
                                             array(wages.df.1@teststat),
                                             array(shares.df.1@teststat),
                                             array(bonds.df.1@teststat)),n,3)
  n<-26;Table.Critical.values<-matrix(c(array(REER.df.1@cval),
                                        array(cpi.df.1@cval),
                                        array(ppi.df.1@cval),
                                        array(ccpi.df.1@cval),
                                        array(asset.prices.df.1@cval),
                                        array(food.prices.df.1@cval),
                                        array(administered.prices.df.1@cval),
                                        array(deflator.df.1@cval),
                                        array(imported.inflation.df.1@cval),
                                        array(Real.GDP.df.1@cval),
                                        array(DISCOUNT.RATE.df.1@cval),
                                        array(real.rate.df.1@cval),
                                        array(interest.gap.df.1@cval),
                                        array(arr.df.1@cval),
                                        array(tr.df.1@cval),
                                        array(or.df.1@cval),
                                        array(repo.df.1@cval),
                                        array(wir.df.1@cval),
                                        array(bond.yields.df.1@cval),
                                        array(m3.df.1@cval),
                                        array(rr.df.1@cval),
                                        array(di.df.1@cval),
                                        array(nx.df.1@cval),
                                        array(wages.df.1@cval),
                                        array(shares.df.1@cval),
                                        array(bonds.df.1@cval)),n,3)

  Table.Unit.root.test<-matrix(c(array(REER.df@teststat),
                                 array(cpi.df@teststat),
                                 array(ppi.df@teststat),
                                 array(ccpi.df@teststat),
                                 array(asset.prices.df@teststat),
                                 array(food.prices.df@teststat),
                                 array(administered.prices.df@teststat),
                                 array(deflator.df@teststat),
                                 array(imported.inflation.df@teststat),
                                 array(Real.GDP.df@teststat),
                                 array(DISCOUNT.RATE.df@teststat),
                                 array(real.rate.df@teststat),
                                 array(interest.gap.df@teststat),
                                 array(arr.df@teststat),
                                 array(tr.df@teststat),
                                 array(or.df@teststat),
                                 array(repo.df@teststat),
                                 array(wir.df@teststat),
                                 array(bond.yields.df@teststat),
                                 array(m3.df@teststat),
                                 array(rr.df@teststat),
                                 array(di.df@teststat),
                                 array(nx.df@teststat),
                                 array(wages.df@teststat),
                                 array(shares.df@teststat),
                                 array(bonds.df@teststat)),n,3)}
colnames(Table.Unit.root.test.before)<-c("none","with constant","with trend")
colnames(Table.Unit.root.test.before)<-c("без тренду та сталої","зі сталою","з детерміністичним трендом")

rows.df1<-c("REER","CPI","PPI","CCPI","Asset prices","Food prices","Administered prices","deflator","imported inflation","Real GDP","Discount rate","Real rate","interest.gap","average rate of refinancing","tender rate","overnight rate","repo rate","weighed interbank rate","bond yields","money supply","reserves","DI",
            #"PI",
            "NX","Wages","Shares","Bonds");rows.df<-c("РЕОК","ІСЦ","ІСВ","Базова інфляція","Ціни на активи","Ціни на продовольство","Адміністративно-регульовані ціни","Дефлятор","Імпортована інфляція","Реальний ВВП","Облікова ставка","Реальна процентна ставка","Розрив процентних ставок","Середньозважена ставка рефінансування","Ставка за тендерним рефінансуванням","Ставка овернайт","Ставка РЕПО","Середньозважена ставка за міжбанківськими кредитами","Дохідність за облігаціями резидентів","Грошова пропозиція","Офіційні резерви","ППІ",
                                                      #"ПІІ",
                                                      "Чистий експорт","Номінальна зарплата","Акції у портфелях","Облігації у портфелях");row.names(Table.Unit.root.test.before)<-rows.df
Table.Unit.root.test.before<-data.frame(Table.Unit.root.test.before)

colnames(Table.Unit.root.test)<-c("none","with constant","with trend");colnames(Table.Unit.root.test)<-c("без тренду та сталої","зі сталою","з детерміністичним трендом")
rows.df<-c("REER in logs","CPI in logs","PPI in logs","CCPI in logs","Asset prices in logs","Food prices in logs","Administered prices in logs","deflator in logs","imported inflation in logs","Real GDP in logs","Discount rate in logs","Real rate in logs","interest.gap in logs","average rate of refinancing in logs","tender rate in logs","overnight rate in logs","repo rate in logs","weighed interbank rate in logs","bond yields in logs","money supply in logs","reserves in logs","DI in logs",
           #"PI in logs",
           "NX in logs","Wages in logs","Shares in logs","Bonds in logs");rows.df<-c("РЕОК у логарифмах та рівнях","ІСЦ у логарифмах та рівнях","ІСВ у логарифмах та рівнях","Базова інфляція у логарифмах та рівнях","Ціни на активи у логарифмах та рівнях","Ціни на продовольство у логарифмах та рівнях","Адміністративно-регульовані ціни у логарифмах та рівнях","Дефлятор у логарифмах та рівнях","Імпортована інфляція у логарифмах та рівнях","Реальний ВВП у логарифмах та рівнях","Облікова ставка у логарифмах та рівнях","Реальна процентна ставка у логарифмах та рівнях","Розрив процентних ставок у логарифмах та рівнях","Середньозважена ставка рефінансування у логарифмах та рівнях","Ставка за тендерним рефінансуванням у логарифмах та рівнях","Ставка овернайт у логарифмах та рівнях","Ставка РЕПО у логарифмах та рівнях","Середньозважена ставка за міжбанківськими кредитами у логарифмах та рівнях","Дохідність за облігаціями резидентів у логарифмах та рівнях","Грошова пропозиція у логарифмах та рівнях","Офіційні резерви у логарифмах та рівнях","ППІ у логарифмах та рівнях",
                                                                                     #"ПІІ у логарифмах та рівнях",
                                                                                     "Чистий експорт у логарифмах та рівнях","Номінальна зарплата у логарифмах та рівнях","Акції у портфелях у логарифмах та рівнях","Облігації у портфелях у логарифмах та рівнях");
row.names(Table.Unit.root.test)<-rows.df
Table.Unit.root.test<-data.frame(Table.Unit.root.test)

colnames(Table.Critical.values)<-c("Критичне значення МакКінона без тренду та сталої","зі сталою","з детерміністичним трендом")
colnames(Table.Critical.values)<-c("MacKinnon cvalues","with constant","with trend")
rownames(Table.Critical.values)<-rows.df


Table.Unit.root.test.before<-data.frame(cbind(Table.Unit.root.test.before,Table.Critical.values))
Table.Unit.root.test<-data.frame(cbind(Table.Unit.root.test,Table.Critical.values))
# setwd("J:/І розділ/Монетарний механізм/unit root test")
WriteXLS::WriteXLS(Table.Unit.root.test.before,ExcelFileName = "Table.Unit.root.test.before.xlsx",row.names = TRUE,FreezeRow = 1,FreezeCol = 1)

WriteXLS::WriteXLS(Table.Unit.root.test,ExcelFileName = "Table.Unit.root.test.xlsx",row.names = TRUE,FreezeRow = 1,FreezeCol = 1)
######
options(add.smooth=	TRUE,check.bounds=	FALSE,continue=	"+ ",digits=	7,echo=	TRUE,encoding=	"native.enc",error=	NULL,expressions=	5000,keep.source=	interactive(),keep.source.pkgs=	FALSE,max.print=	99999,
        OutDec=	".",prompt=	"> ",scipen=	0,show.error.messages=	TRUE,timeout=	60,verbose=	FALSE,warn=	0,warning.length=	1000,
        width=	80)
options(OutDec=",")
######
htmlTable::htmlTable(htmlTable::txtRound(rbind(Table.Unit.root.test.before,Table.Unit.root.test),1,dec = ","),
                     header =  paste(c("none","with constant","with trend","none","with constant","with trend")),
                     rnames = paste(c(rows.df1,rep(rows.df,1))),
                     rgroup = c("Without levels",
                                "In first difference and logs"),
                     n.rgroup = c(n,n),
                     cgroup = c("Test statistics", "Critical value of MacKinnon statistics&dagger;"),
                     n.cgroup = c(3,3),
                     caption="Table 3.1 Test on availability of one unit roots for macro variables in Ukraine",
                     tfoot="&dagger; on 95% levels of confidence",
                     col.rgroup = c("none"),
                     col.columns = c(rep("#EFEFF0", 3),
                                     rep("none", 3)),
                     align="rrr|rrr")
htmlTable::htmlTable(Table.Unit.root.test.before,
                     col.rgroup = c("none", "#FFFFCC"),
                     col.columns = c(rep("#EFEFF0", 3),
                                     rep("none", ncol(Table.Unit.root.test.before) - 3)),
                     align="rrr|rrr",
                     cgroup = c("Тестова статистика", "Критичне значення за асимптотачним розподілом МакКінона&dagger"),
                     n.cgroup = c(3,3),
                     # I use the &nbsp; - the no breaking space as I don't want to have a
                     # row break in the row group. This adds a little space in the table
                     # when used together with the cspan.rgroup=1.
                     rgroup = c("Без рівнів"
                     ),
                     n.rgroup = rep(1,(n-1)),
                     cspan.rgroup = 1)

######
# save.image("J:/І розділ/Монетарний механізм/data/3.06.RData")

rm(list = ls())
{REER.ar<-auto.arima(diffinv(exp(transmission$REER)))
cpi.ar<-auto.arima(transmission$cpi.a)
ppi.ar<-auto.arima(transmission$ppi.a)
ccpi.ar<-auto.arima(transmission$ccpi)
asset.prices.ar<-auto.arima(transmission$asset.price)
food.prices.ar<-auto.arima(transmission$food.prices)
administered.prices.ar<-auto.arima(transmission$administered.prices)
deflator.ar<-auto.arima(transmission$deflator[])
imported.inflation.ar<-auto.arima(transmission$imported.inflation[])
Real.GDP.ar<-auto.arima(transmission$Real.GDP[])

Discount.rate.ar<-auto.arima(transmission$Discount.rate)
real.rate.ar<-auto.arima(transmission$real.rate)
interest.gap.ar<-auto.arima(transmission$Interest.gap)
arr.ar<-auto.arima(transmission$Average.refinancing.rate)
tr.ar<-auto.arima(transmission$tender.rate)
or.ar<-auto.arima(transmission$overnight.rate)
repo.ar<-auto.arima(transmission$repo.rate[])
wir.ar<-auto.arima(transmission$Weighed.interbank.rate)
bond.yields.ar<-auto.arima(transmission$bond.yields.h)

m3.ar<-auto.arima(transmission$`d(ln(M3_a))`[])
rr.ar<-auto.arima(transmission$rr)

di.ar<-auto.arima(transmission$`Direct.investment,.net`[])
#pi.df<-auto.arima(transmission$`Portfolio.investment,.net`,type="trend",selectlags="AIC")
nx.ar<-auto.arima(transmission$NX[])
wages.ar<-auto.arima(transmission$Wages[])
shares.ar<-auto.arima(transmission$shares[])
bonds.ar<-auto.arima(transmission$`Bonds.issued.by.non-deposit.corporations`)
}
title<-c("Available unit roots")
subtitle<-c("REER","cpi","ppi","ccpi","asset.prices","food.prices","administered.prices","deflator","imported.inflation","Real.GDP","Discount.rate","real.rate","interest.gap","arr","tr","or","repo","wir","bond.yields","m3","rr","di","nx","wages","shares","bonds")
RU.REER.ar.ua<-ggplot2::autoplot(REER.ar,type = "ar",subtitle=subtitle[1])+title(main=title,outer=TRUE)
for(d in 2:26){
assign(pst("RU.",subtitle[d],".ar.ua"),ggplot2::autoplot(getp(subtitle[d],".ar"),type = "both",subtitle=subtitle[d]))}
  for(d in 2:26)show(getp("RU.",subtitle[d],".ar.ua"))
