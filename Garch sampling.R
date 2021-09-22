.libPaths("C:/Users/1/Documents/R/win-library/3.3")
save.image("H:/GARCH code/data/2.RData")
load("H:/GARCH code/data/2.RData")
#===================================================

libarary(foreach)
#foreach::foreach(i = seq(10,50,length.out=5),.combine=combine)foreach::%dopar%{
 #number of iterations####
 i<-37
  {
    ## Chunk 1: Model Definition ####

    {Cogparam11 <- list(a1 = 1.06e-02,  b1 =  1.60e-02, a0 = 0.01,
                        lambda = 1, alpha = sqrt(2), beta = 0, mu = 0, y01 = -103.2049)#mean(na.omit(allobs))
    Carma11<-yuima::setCarma(p = 3, q = 2, ar.par =  "beta", ma.par = "alpha",
                             loc.par = "c0", lin.par = "alpha", Carma.var = "v", Latent.var = "y",
                             XinExpr = TRUE, Cogarch = TRUE)
    Cogmodel11 <- yuima::setCogarch(p = 1, q = 1,
                                    measure = list(df = "rvgamma(z, lambda, alpha, beta, mu)"),
                                    measure.type = "code", XinExpr = TRUE,Latent.var = "y");Cogmodel11@drift;Cogmodel11@diffusion}

  #save s4 ####
  #problem return list
  #rawlist<-list.files(envir=.GlobalEnv)
  #outputlist<-list()
  #for(i in 1:length(rawlist)){
   # Raws<-xcmsRaw(rawlist[i],profstep=.04,includeMSn=TRUE)
    #outputlist[[i]]<-Raws
  #}

## Chunk 2: Market Data ####
names<-c("NBU rate","interbank rate","bureau de exchange buy","bureau de exchange sell")
if(!Sys.info()[4]=="MacBook-Air-Ulia.local"){Data<-readxl::read_excel("J:/І розділ/Монетарний механізм трансмісії/Monetary transmission mechanism in Ukraine.xlsx",sheet = "Sheet1..")}else{
Data<-readxl::read_excel("~/OneDrive/Thesis Ph.D (2)/Statistical appendicies/монетарний трансмісійний механізм 2.1.xlsx",sheet = "Exch rate differential")};Data[,1]<-as.Date(seq.Date(from=as.Date("2016-12-01"),to=as.Date("2020-07-01"),by="months"))

View(Data[,c("date","Official.exch.rate","interbank.exc.rate.average" , "bureaux.de.exch.rate.cash.segment.buy" ,
             "bureaux.de.exch.rate.cash.segment.sell")]);Data<-Data[1:(135+29),]

allobs<-Data$Official.exch.rate
allobs.2<-Data$interbank.exc.rate.average
allobs.3<-Data$bureaux.de.exch.rate.cash.segment.buy
allobs.4<-Data$bureaux.de.exch.rate.cash.segment.sell

n1<-(which(!is.na(allobs),arr.ind=TRUE)[1])
n2<-length(na.omit(allobs))
Monthout <- allobs[abs(n2-i):n2]#!!!
Monthin <- allobs[1:(n2-i)]#!!!
logmonthrate<-log2(allobs[n1:n2])
#-2----
{ n1<-(which(!is.na(allobs.2),arr.ind=TRUE)[1])
n2<-length(na.omit(allobs.2))
Monthout.2 <- allobs.2[abs(n2-i):n2]#!!!
Monthin.2 <- allobs.2[(n1):(n2-i)]#!!!
logmonthrate.2<-log2(allobs.2[n1:n2])}
#-3----
{ n1<-(which(!is.na(allobs.3),arr.ind=TRUE)[1])
n2<-length(na.omit(allobs.3))
Monthout.3 <- allobs.3[abs(n2-i):n2]#!!!
Monthin.3 <- allobs.3[(n1):(n2-i)]#!!!
logmonthrate.3<-log2(allobs.3[n1:n2])}
#-4----
{ n1<-(which(!is.na(allobs.4),arr.ind=TRUE)[1])
n2<-length(na.omit(allobs.4))
Monthout.4 <- allobs.4[abs(n2-i):n2]#!!!
Monthin.4 <- allobs.4[(n1):(n2-i)]#!!!
logmonthrate.4<-log2(allobs.4[n1:n2])}

#data####
{incr.dataI <- diff(Monthin) - mean(diff(Monthin))#!!!
  dataI <- yuima::setData(as.matrix(cumsum(c(0, incr.dataI))), delta = 1 / 1);dataI@original.data##!!!
  incr.dataO <- diff(Monthout) - mean(diff(Monthout))#!!!
  dataO <- yuima::setData(as.matrix(cumsum(c(0, incr.dataO))), delta = 1 / 1);dataO@original.data}##!!!
{incr.dataI.2 <- diff(Monthin.2) - mean(diff(Monthin.2))#!!!
  dataI.2 <- yuima::setData(as.matrix(cumsum(c(0, incr.dataI.2))), delta = 1 / 1);dataI.2@original.data##!!!
  incr.dataO.2 <- diff(Monthout.2) - mean(diff(Monthout.2))#!!!
  dataO.2 <- yuima::setData(as.matrix(cumsum(c(0, incr.dataO.2))), delta = 1 / 1);dataO.2@original.data}##!!!
{incr.dataI.3 <- diff(Monthin.3) - mean(diff(Monthin.3))#!!!
  dataI.3 <- yuima::setData(as.matrix(cumsum(c(0, incr.dataI.3))), delta = 1 / 1);dataI.3@original.data##!!!
  incr.dataO.3 <- diff(Monthout.3) - mean(diff(Monthout.3))#!!!
  dataO.3 <- yuima::setData(as.matrix(cumsum(c(0, incr.dataO.3))), delta = 1 / 1);dataO.3@original.data}##!!!
{incr.dataI.4 <- diff(Monthin.4) - mean(diff(Monthin.4))#!!!
  dataI.4 <- yuima::setData(as.matrix(cumsum(c(0, incr.dataI.4))), delta = 1 / 1);dataI.4@original.data##!!!
  incr.dataO.4 <- diff(Monthout.4) - mean(diff(Monthout.4))#!!!
  dataO.4 <- yuima::setData(as.matrix(cumsum(c(0, incr.dataO.4))), delta = 1 / 1);dataO.4@original.data}##!!!

}

## Chunk 3: Estimation COGARCH using In-Sample and Out-of-sample Data ####
library(yuima)
CogYuima11 <- yuima::setYuima(data = dataI, model = Cogmodel11);print(CogYuima11@model); warning(paste0("THE INITIAL VALUE X =",CogYuima11@model@xinit))
  resCog11 <- yuima::gmm(yuima = CogYuima11, start = Cogparam11,
                         Est.Incr = "IncrPar")
  resCog11.result1<-resCog11@coef;resCog11.cov<-resCog11@vcov;Increments.1<-data.frame(resCog11@Incr.Lev)
  TestStat.1 <- yuima::Diagnostic.Cogarch(resCog11);Mean.var.process<-TestStat.1$meanVarianceProc;mean.sv<-TestStat.1$meanStateVariable

  CogYuima21 <- yuima::setYuima(data = dataI.2, model = Cogmodel11);print(CogYuima21@model); warning("THE INITIAL VALUE X =","\n0\n")
  resCog21 <- yuima::gmm(yuima = CogYuima21, start = Cogparam11,
                         Est.Incr = "IncrPar")
  resCog21.result21<-resCog21@coef;resCog21.cov<-resCog21@vcov;Increments.2<-data.frame(resCog21@Incr.Lev)
  TestStat.2 <- yuima::Diagnostic.Cogarch(resCog21);Mean.var.process.2<-TestStat.2$meanVarianceProc;mean.sv.2<-TestStat.2$meanStateVariable

  CogYuima31 <- yuima::setYuima(data = dataI.3, model = Cogmodel11)
  resCog31 <- yuima::gmm(yuima = CogYuima31, start = Cogparam11,
                         Est.Incr = "IncrPar")
  resCog31.result1<-resCog31@coef;resCog31.cov<-resCog31@vcov;Increments.3<-data.frame(resCog31@Incr.Lev)
  TestStat.3 <- yuima::Diagnostic.Cogarch(resCog31);Mean.var.process.3<-TestStat.3$meanVarianceProc;mean.sv.3<-TestStat.3$meanStateVariable

  CogYuima41 <- yuima::setYuima(data = dataI.4, model = Cogmodel11)
  resCog41 <- yuima::gmm(yuima = CogYuima41, start = Cogparam11,
                         Est.Incr = "IncrPar")
  resCog41.result1<-resCog41@coef;resCog41.cov<-resCog41@vcov;Increments.4<-data.frame(resCog41@Incr.Lev)
  TestStat.4 <- yuima::Diagnostic.Cogarch(resCog41);Mean.var.process<-TestStat.4$meanVarianceProc;mean.sv<-TestStat.4$meanStateVariable

##Chunk.3.2####
library(yuima)
CogYuima12 <- yuima::setYuima(data = dataO, model = Cogmodel11)
  resCog12 <- yuima::gmm(yuima = CogYuima12, start = Cogparam11,
                         Est.Incr = "IncrPar")
  resCog12.result1<-resCog12@coef;resCog12.cov<-resCog12@vcov;Increments.1.2<-data.frame(resCog12@Incr.Lev)
  TestStat.1.2 <- yuima::Diagnostic.Cogarch(resCog12);Mean.var.process.1.2<-TestStat.1.2$meanVarianceProc;mean.sv.1.2<-TestStat.1.2$meanStateVariable

  CogYuima22 <- yuima::setYuima(data = dataO.2, model = Cogmodel11)
  resCog22 <- yuima::gmm(yuima = CogYuima22, start = Cogparam11,
                         Est.Incr = "IncrPar")
  resCog22.result1<-resCog22@coef;resCog22.cov<-resCog22@vcov;Increments.2.2<-data.frame(resCog22@Incr.Lev)
  TestStat.2.2 <- yuima::Diagnostic.Cogarch(resCog22);Mean.var.process.2.2<-TestStat.2.2$meanVarianceProc;mean.sv.2.2<-TestStat.2.2$meanStateVariable

  CogYuima32 <- yuima::setYuima(data = dataO.3, model = Cogmodel11)
  resCog32 <- yuima::gmm(yuima = CogYuima32, start = Cogparam11,
                         Est.Incr = "IncrPar")
  resCog32.result1<-resCog32@coef;resCog32.cov<-resCog32@vcov;Increments.3.2<-data.frame(resCog32@Incr.Lev)
  TestStat.3.2 <- yuima::Diagnostic.Cogarch(resCog32);Mean.var.process.3.2<-TestStat.3.2$meanVarianceProc;mean.sv.3.2<-TestStat.3.2$meanStateVariable

  CogYuima42 <- yuima::setYuima(data = dataO.4, model = Cogmodel11)
  resCog42 <- yuima::gmm(yuima = CogYuima42, start = Cogparam11,
                         Est.Incr = "IncrPar")
  resCog42.result1<-resCog42@coef;resCog42.cov<-resCog42@vcov;Increments.4.2<-data.frame(resCog42@Incr.Lev)
  TestStat.4.2 <- yuima::Diagnostic.Cogarch(resCog42);Mean.var.process.4.2<-TestStat.4.2$meanVarianceProc;mean.sv.4.2<-TestStat.4.2$meanStateVariable

## Chunk 4: Estimated Increments Fig. 16 ####
#-LEVY----
#load("H:/GARCH code/data/4.RData")
#foreach(i=1:5)%do%{
Incr.1<-data.frame(resCog11@Incr.Lev);View(Incr.1)
Incr.2<-data.frame(resCog21@Incr.Lev);View(Incr.2)
Incr.3<-data.frame(resCog31@Incr.Lev);View(Incr.3)
Incr.4<-data.frame(resCog41@Incr.Lev);View(Incr.4)
#out-of-sample####
Incr.1.o<-data.frame(resCog12@Incr.Lev);View(Incr.1.o)
Incr.2.o<-data.frame(resCog22@Incr.Lev);View(Incr.2.o)
Incr.3.o<-data.frame(resCog32@Incr.Lev);View(Incr.3.o)
Incr.4.o<-data.frame(resCog42@Incr.Lev);View(Incr.4.o)

## Chunk 6: Reconstruction of COGARCH trajectory  with estimated increments Fig. 17 ####

simCog <- yuima::simulate(resCog11,hurst = TRUE)
simCog2 <- yuima::simulate(resCog21,hurst = TRUE)
simCog3 <- yuima::simulate(resCog31,hurst = TRUE)
simCog4 <- yuima::simulate(resCog41,hurst = TRUE)

simCog.2 <- yuima::simulate(resCog12,hurst = TRUE)
simCog2.2 <- yuima::simulate(resCog22,hurst = TRUE)
simCog3.2 <- yuima::simulate(resCog32,hurst = TRUE)
simCog4.2 <- yuima::simulate(resCog42,hurst = TRUE)
#Varience#########
variance1.in.sample<-data.frame(simCog@data@original.data[,1]);View(variance1.in.sample)
variance2.in.sample<-data.frame(simCog2@data@original.data[,1]);View(variance2.in.sample)
variance3.in.sample<-data.frame(simCog3@data@original.data[,1]);View(variance3.in.sample)
variance4.in.sample<-data.frame(simCog4@data@original.data[,1]);View(variance4.in.sample)
#out-of-sample#####
variance1.out.of.sample<-data.frame(simCog.2@data@original.data[,1]);View(variance1.out.of.sample)
variance2.out.of.sample<-data.frame(simCog2.2@data@original.data[,1]);View(variance2.out.of.sample)
variance3.out.of.sample<-data.frame(simCog3.2@data@original.data[,1]);View(variance3.out.of.sample)
variance4.out.of.sample<-data.frame(simCog4.2@data@original.data[,1]);View(variance4.out.of.sample)

#i<-i+1
#}

#figure####
#for(i in seq(1,5))
  #env<-outputlist[i]
#assign("dataI", dataI, envir = .GlobalEnv);assign("dataI.2", dataI.2, envir = .GlobalEnv);assign("dataI.3", dataI.3, envir = .GlobalEnv);assign("dataI.4", dataI.4, envir = .GlobalEnv)
#assign("dataO", dataO, envir = .GlobalEnv);assign("dataO.2", dataO.2, envir = .GlobalEnv);assign("dataO.3", dataO.3, envir = .GlobalEnv);assign("dataO.4", dataO.4, envir = .GlobalEnv)

  par(mfrow = c(4, 2), mar = c(4.5, 4.5, 2.5, 2))#"Часові ряди валютного курсу в межах вибірки" #"Out-Of-Sample Data"
{yuima::plot(dataI, main = "Часові ряди курсу НБУ в межах вибірки",type="l",lty="solid",col="black",lwd=2)
  yuima::plot(dataO, main = "Часові ряди курсу НБУ поза вибіркою","b",col="black",lwd=2)+abline(v=1,col="grey",lty=3)
  abline(h=dataO@original.data[2],col="red",lty=3)
  yuima::plot(dataI.2, main = "Часові ряди міжбанківського валютного курсу в межах вибірки",type="l",lty="solid",col="black",lwd=2)
  yuima::plot(dataO.2, main = "Часові ряди міжбанківського курсу поза вибіркою","b",col="black",lwd=2)+abline(v=2,col="grey",lty=3)
  abline(h=dataO.2@original.data[3], col="red",lty=3)
  yuima::plot(dataI.3, main = "Часові ряди готівкового курсу купівлі в межах вибірки",type="l",lty="solid",col="black",lwd=2)
  yuima::plot(dataO.3, main = "Часові ряди валютного курсу поза вибіркою","b",col="black",lwd=2)+abline(v=1,col="grey",lty=3)
  abline(h=dataO.3@original.data[2],col="red",lty=3)
  yuima::plot(dataI.4, main = "Часові ряди готівкового курсу продажу в межах вибірки",type="l",lty="solid",col="black",lwd=2)
  yuima::plot(dataO.4, main = "Часові ряди валютного курсу поза вибіркою","b",col="black",lwd=2)+abline(v=1,col="grey",lty=3)
  abline(h=dataO.4@original.data[2],col="red",lty=3)
  grDevices::savePlot(filename = "Fig.1.1.Вибіркова та позавибіркова сукупність (37 спостережень).png",type="png")

  }

# Code for generation Figure 1####
setwd("H:/GARCH code/data/sampling 37")
par(mfrow = c(4, 1), mar = c(4.5, 4.5, 3, 2))
plot(resCog11@Incr.Lev, ylab="in decimals", main = "Increments of Levy process for official exchange rate demeaned trend")
#line(resCog11.2@Incr.Lev)+line(resCog11.3@Incr.Lev)+line(resCog11.4@Incr.Lev)+line(resCog11.5@Incr.Lev)
plot(resCog21@Incr.Lev, ylab="in decimals", main = "Increments of Levy process for interbank exchange rate demeaned trend")
#line(resCog21.2@Incr.Lev)+line(resCog21.3@Incr.Lev)+line(resCog21.4@Incr.Lev)+line(resCog21.5@Incr.Lev)
plot(resCog31@Incr.Lev, ylab="in decimals", main = "Increments of Levy process for cash market bid exchange rate demeaned trend")
#line(resCog31.2@Incr.Lev)+line(resCog31.3@Incr.Lev)+line(resCog31.4@Incr.Lev)+line(resCog31.5@Incr.Lev)
plot(resCog41@Incr.Lev, ylab="in decimals", main = "Increments of Levy process for cash market ask exchange rate demeaned trend")
#line(resCog41.2@Incr.Lev)+line(resCog41.3@Incr.Lev)+line(resCog41.4@Incr.Lev)+line(resCog41.5@Incr.Lev)
#legend(x="time",y="exchange rate varience Levy increments")
grDevices::savePlot(filename = "Fig.1.1.98,72 та 66 спостережень.png",type="png")

library(yuima)
par(mfrow = c(2, 1), mar = c(4.5, 4.5, 3, 2), mai=rep(1,4),cex=1)
title(main="Out of the sample data",outer=TRUE)
+yuima::plot(resCog12@Incr.Lev, xlab="time",ylab="in decimals", main = "Increments of Levy process for official exchange rate demeaned trend")
#yuima::line(resCog12.2@Incr.Lev)+yuima::line(resCog12.3@Incr.Lev)+yuima::line(resCog12.4@Incr.Lev)+yuima::line(resCog12.5@Incr.Lev)
+yuima::plot(resCog22@Incr.Lev, xlab="time",ylab="in decimals", main = "Increments of Levy process for interbank exchange rate demeaned trend",lwd=2)
#yuima::line(resCog22.2@Incr.Lev)+yuima::line(resCog22.3@Incr.Lev)+yuima::line(resCog22.4@Incr.Lev)+yuima::line(resCog22.5@Incr.Lev)
+yuima::plot(resCog32@Incr.Lev, xlab="time",ylab="in decimals", main = "Increments of Levy process for cash market bid exchange rate demeaned trend")
#yuima::line(resCog32.2@Incr.Lev)+yuima::line(resCog32.3@Incr.Lev)+yuima::line(resCog32.4@Incr.Lev)+yuima::line(resCog32.5@Incr.Lev)
+yuima::plot(resCog42@Incr.Lev, xlab="time",ylab="in decimals", main = "Increments of Levy process for cash market ask exchange rate demeaned trend")
#yuima::line(resCog42.2@Incr.Lev)+yuima::line(resCog42.3@Incr.Lev)+yuima::line(resCog42.4@Incr.Lev)+yuima::line(resCog42.5@Incr.Lev)
grDevices::savePlot(filename = "Fig.2.1. 37 спостережень.png",type="png")

par(mfrow = c(3, 4), mar = c(1.5, 2.5, 3, 1),mai=rep(1,4),cex=2)
yuima::plot(simCog, main = "Reconstructed COGARCH(1,1) of NBU rate sample path",outer=TRUE)
#yuima::line(simCog.2)+yuima::line(simCog.3)+yuima::line(simCog.4)+yuima::line(simCog.5)
grDevices::savePlot(filename = "Fig.3.1.Reconstructed COGARCH(1,1) of NBU rate sample in 37 obs. path.png",type="png")
yuima::plot(simCog2,main = "Reconstructed COGARCH(1,1) of interbank rate sample path")
#yuima::line(simCog2.2)+yuima::line(simCog2.3)+yuima::line(simCog2.4)+yuima::line(simCog2.5)
grDevices::savePlot(filename = "Fig.3.2. Reconstructed COGARCH(1,1) of interbank rate sample in 37 obs. path.png",type="png")
yuima::plot(simCog3,main = "Reconstructed COGARCH(1,1) of buying cash rate sample path")
#yuima::line(simCog3.2)+yuima::line(simCog3.3)+yuima::line(simCog3.4)+yuima::line(simCog3.5)
grDevices::savePlot(filename = "Fig.3.3.Reconstructed COGARCH(1,1) of buying cash rate sample in 37 obs. path.png",type="png")
yuima::plot(simCog4,main = "Reconstructed COGARCH(1,1) of selling cash rate sample path")
#yuima::line(simCog4.2)+yuima::line(simCog4.3)+yuima::line(simCog4.4)+yuima::line(simCog4.5)
grDevices::savePlot(filename = "Fig.3.4.Reconstructed COGARCH(1,1) of selling cash rate sample in 37 obs. path.png",type="png")

savePlot(filename = "Fig.4",type=".png")

par(mfrow = c(1, 4), mar = c(1.5, 4.5, 3, 1))
yuima::plot(simCog.2, main = "Reconstructed COGARCH(1,1) of NBU rate out of the sample path",outer=TRUE)
#yuima::line(simCog.2.2)+yuima::line(simCog.2.3)+yuima::line(simCog.2.4)+yuima::line(simCog.2.5)
grDevices::savePlot(filename = "Fig.4.1.Reconstructed COGARCH(1,1) of NBU rate out of the sample path.png",type="png")
yuima::plot(simCog2.2,main = "Reconstructed COGARCH(1,1) of interbank rate out of the sample in 37 obs. path")
#yuima::line(simCog2.2.2)+yuima::line(simCog.2.2.3)+yuima::line(simCog.2.2.4)+yuima::line(simCog.2.2.5)
grDevices::savePlot(filename = "Fig.4.2.Reconstructed COGARCH(1,1) of interbank rate out of the sample in 37 obs. path.png",type="png")
yuima::plot(simCog3.2,main = "Reconstructed COGARCH(1,1) of buying cash rate out of the sample path")
#yuima::line(simCog3.2.2)+yuima::line(simCog.3.2.3)+yuima::line(simCog.3.2.4)+yuima::line(simCog.3.2.5)
grDevices::savePlot(filename = "Fig.4.3.Reconstructed COGARCH(1,1) of buying cash rate out of the sample in 37 obs. path.png",type="png")
yuima::plot(simCog4.2,main = "Reconstructed COGARCH(1,1) of selling cash rate out of the sample path")
#yuima::line(simCog4.2.2)+yuima::line(simCog.4.2.3)+yuima::line(simCog.4.2.4)+yuima::line(simCog.4.2.5)
grDevices::savePlot(filename = "Fig.4.4.Reconstructed COGARCH(1,1) of selling cash rate out of the sample in 37 obs. path.png",type="png")

save.image("H:/GARCH code/data/3.7.RData")
rm(list=ls())
##Combine####
Increments.all.simulations<-foreach(s=10:50,by=10,.combine=cbind)%do%{
  Increments.all.simulations.NBU<-cbind(Incr.1.1,...,Incr.1.s)
}
Increments.all.simulations<-foreach(s=10:50,by=10,.combine=cbind)%do%{
  Increments.all.simulations.Interbank<-cbind(Incr.2.1,...,Incr.2.s)
}
Increments.all.simulations<-foreach(s=10:50,by=10,.combine=cbind)%do%{
  Increments.all.simulations.bde.buy<-cbind(Incr.3.1,...,Incr.3.s)
}
Increments.all.simulations<-foreach(s=10:50,by=10,.combine=cbind)%do%{
  Increments.all.simulations.bde.sell<-cbind(Incr.4.1,...,Incr.4.s)
}

Increments.all.simulations.NBU.out.of<-foreach(s=10:50,by=10,.combine=cbind)%do%{
  Increments.all.simulations.NBU.out.of<-cbind(Incr.1.o.1,...,Incr.1.o.s)
}
Increments.all.simulations.Interbank.out.of<-foreach(s=10:50,by=10,.combine=cbind)%do%{
  Increments.all.simulations.Interbank.out.of<-cbind(Incr.2.o.1,...,Incr.2.o.s)
}
Increments.all.simulations.bde.buy.out.of<-foreach(s=10:50,by=10,.combine=cbind)%do%{
  Increments.all.simulations.bde.buy.out.of<-cbind(Incr.3.o.1,...,Incr.3.o.s)
}
Increments.all.simulations.bde.sell.out.of<-foreach(s=10:50,by=10,.combine=cbind)%do%{
  Increments.all.simulations.bde.sell.out.of<-cbind(Incr.4.o.1,...,Incr.4.o.s)
}
#saving####
save.image("H:/GARCH code/data/2.RData")#10
rm(list=ls())
#saving####
save.image("H:/GARCH code/data/3.RData")#20
rm(list=ls())
#saving####
save.image("H:/GARCH code/data/4.RData")#30
rm(list=ls())
#saving####
save.image("H:/GARCH code/data/5.RData")#40
rm(list=ls())
#saving####
save.image("H:/GARCH code/data/6.RData")#45
rm(list=ls())
#saving####
save.image("H:/GARCH code/data/7.RData")#50
rm(list=ls())
#Different samples####
save.image("H:/GARCH code/data/1.1.RData")#5
rm(list=ls())
#saving####
load("H:/GARCH code/data/2.RData")#10
save.image("H:/GARCH code/data/2.RData")
rm(list=ls())
#saving####
load("H:/GARCH code/data/3.RData")
save.image("H:/GARCH code/data/3.RData")#20
rm(list=ls())
#saving####
load("H:/GARCH code/data/4.RData")
save.image("H:/GARCH code/data/4.RData")#30
rm(list=ls())
#saving####
load("~/OneDrive/GARCH code/data/3.7.RData")
save.image("~/OneDrive/GARCH code/data/3.7.RData")#37
rm(list=ls())
#saving####
load("H:/GARCH code/data/5.RData")
save.image("H:/GARCH code/data/5.RData")#40
rm(list=ls())
#saving####
load("H:/GARCH code/data/6.RData")
save.image("H:/GARCH code/data/6.RData")#45
rm(list=ls())
#saving####
load("H:/GARCH code/data/7.RData")
save.image("H:/GARCH code/data/7.RData")#50
rm(list=ls())

#Report####

load("H:/GARCH code/data/train.RData")
save.image("H:/GARCH code/data/train.RData")

rm(list=ls())
load("H:/GARCH code/data/train.0.5.RData")
load("H:/GARCH code/data/train.1.RData")
load("H:/GARCH code/data/train.2.RData")
load("H:/GARCH code/data/train.3.RData")
load("H:/GARCH code/data/train.3.7.RData")
load("H:/GARCH code/data/train.4.RData")
load("H:/GARCH code/data/train.4.5.RData")
load("H:/GARCH code/data/train.5.RData")
save.image("H:/GARCH code/data/train.all.RData")
load("H:/GARCH code/data/train.all.RData")
#coef summaries#####
rm(list=ls())

load("H:/GARCH code/data/coef.5.RData")
load("H:/GARCH code/data/coef.10.RData")
load("H:/GARCH code/data/coef.20.RData")
load("H:/GARCH code/data/coef.30.RData")
load("H:/GARCH code/data/coef.37.RData")
load("H:/GARCH code/data/coef.40.RData")
load("H:/GARCH code/data/coef.45.RData")
load("H:/GARCH code/data/coef.50.RData")
save.image("H:/GARCH code/data/coef.all.RData")
load("H:/GARCH code/data/coef.all.RData")

#fig.1####
setwd("H:/GARCH code/data")
{par(mfrow=c(4,2),mar=c(2.5, 4.5, 3.5, 1),oma=c(1,1,2,1))
  plot(seq(1,length(Incr.1.5[,1])),Incr.1.5$resCog11.Incr.Lev,type="l",xlab = "time",main="sample 130 observations",ylab="L")
   plot(seq(1,length(Incr.1.10[,1])),Incr.1.10$resCog11.Incr.Lev,type="l",xlab = "time",main="sample 125 observations",ylab="L")
   plot(seq(1,length(Incr.1.20[,1])),Incr.1.20$resCog11.Incr.Lev,type="l",xlab = "time",main="sample 115 observations",ylab="L")
   plot(seq(1,length(Incr.1.30[,1])),Incr.1.30$resCog11.Incr.Lev,type="l",xlab = "time",main="sample 105 observations",ylab="L")
   plot(seq(1,length(Incr.1.37[,1])),Incr.1.37$resCog11.Incr.Lev,type="l",xlab = "time",main="sample 98 observations",ylab="L")
   plot(seq(1,length(Incr.1.40[,1])),Incr.1.40$resCog11.Incr.Lev,type="l",xlab = "time",main="sample 95 observations",ylab="L")
   plot(seq(1,length(Incr.1.45[,1])),Incr.1.45$resCog11.Incr.Lev,type="l",xlab = "time",main="sample 85 observations",ylab="L")
   plot(seq(1,length(Incr.1.50[,1])),Incr.1.50$resCog11.Incr.Lev,type="l",xlab = "time",main="sample 75 observations",ylab="L")
   title(main="Волатильність дисперсії для курсу, встановленого НБУ",outer=TRUE)
grDevices::savePlot("Волатильність дисперсії для курсу, встановленого НБУ.png",type="png")
   }

  {par(mfrow=c(4,2),mar=c(2.5, 4.5, 1.5, 1),oma=c(1,1,2.25,1))
    plot(seq(1,length(Incr.2.5[,1])),Incr.2.5$resCog21.Incr.Lev,type="l",xlab = "time",main="sample 104 observations",ylab="L")
   plot(seq(1,length(Incr.2.10[,1])),Incr.2.10$resCog21.Incr.Lev,type="l",xlab = "time",main="sample 99 observations",ylab="L")
   plot(seq(1,length(Incr.2.20[,1])),Incr.2.20$resCog21.Incr.Lev,type="l",xlab = "time",main="sample 89 observations",ylab="L")
   plot(seq(1,length(Incr.2.30[,1])),Incr.2.30$resCog21.Incr.Lev,type="l",xlab = "time",main="sample 79 observations",ylab="L")
   plot(seq(1,length(Incr.2.37[,1])),Incr.2.37$resCog21.Incr.Lev,type="l",xlab = "time",main="sample 72 observations",ylab="L")
   plot(seq(1,length(Incr.2.40[,1])),Incr.2.40$resCog21.Incr.Lev,type="l",xlab = "time",main="sample 69 observations",ylab="L")
   plot(seq(1,length(Incr.2.45[,1])),Incr.2.45$resCog21.Incr.Lev,type="l",xlab = "time",main="sample 59 observations",ylab="L")
   plot(seq(1,length(Incr.2.50[,1])),Incr.2.50$resCog21.Incr.Lev,type="l",xlab = "time",main="sample 49 observations",ylab="L")
   title(main="Волатильність дисперсії для міжбанківського курсу",outer =TRUE)
   grDevices::savePlot("Волатильність дисперсії для міжбанківського курсу.png",type="png")
  }

  {par(mfrow=c(4,2),mar=c(2.5, 4.5, 3.5, 1),oma=c(1,1,2,1))
    plot(seq(1,length(Incr.3.5[,1])),Incr.3.5$resCog31.Incr.Lev,type="l",xlab = "time",main="sample 98 observations",ylab="L")
   plot(seq(1,length(Incr.3.10[,1])),Incr.3.10$resCog31.Incr.Lev,type="l",xlab = "time",main="sample 93 observations",ylab="L")
   plot(seq(1,length(Incr.3.20[,1])),Incr.3.20$resCog31.Incr.Lev,type="l",xlab = "time",main="sample 83 observations",ylab="L")
   plot(seq(1,length(Incr.3.30[,1])),Incr.3.30$resCog31.Incr.Lev,type="l",xlab = "time",main="sample 73 observations",ylab="L")
   plot(seq(1,length(Incr.3.37[,1])),Incr.3.37$resCog31.Incr.Lev,type="l",xlab = "time",main="sample 66 observations",ylab="L")
   plot(seq(1,length(Incr.3.40[,1])),Incr.3.40$resCog31.Incr.Lev,type="l",xlab = "time",main="sample 63 observations",ylab="L")
   plot(seq(1,length(Incr.3.45[,1])),Incr.3.45$resCog31.Incr.Lev,type="l",xlab = "time",main="sample 53 observations",ylab="L")
   plot(seq(1,length(Incr.3.50[,1])),Incr.3.50$resCog31.Incr.Lev,type="l",xlab = "time",main="sample 43 observations",ylab="L")
   title(main="Волатильність дисперсії для курсу купівлі на готівковому ринку",outer=TRUE)
   grDevices::savePlot("Волатильність дисперсії для курсу купівлі на готівковому ринку.png",type="png")

   }

{  par(mfrow=c(4,2),mar=c(2.5, 4.5, 3.5, 1),oma=c(1,1,2,1))
   plot(seq(1,length(Incr.4.5[,1])),Incr.4.5$resCog41.Incr.Lev,type="l",xlab = "time",main="sample 98 observations",ylab="L")
   plot(seq(1,length(Incr.4.10[,1])),Incr.4.10$resCog41.Incr.Lev,type="l",xlab = "time",main="sample 93 observations",ylab="L")
   plot(seq(1,length(Incr.4.20[,1])),Incr.4.20$resCog41.Incr.Lev,type="l",xlab = "time",main="sample 83 observations",ylab="L")
   plot(seq(1,length(Incr.4.30[,1])),Incr.4.30$resCog41.Incr.Lev,type="l",xlab = "time",main="sample 73 observations",ylab="L")
   plot(seq(1,length(Incr.4.37[,1])),Incr.4.37$resCog41.Incr.Lev,type="l",xlab = "time",main="sample 66 observations",ylab="L")
   plot(seq(1,length(Incr.4.40[,1])),Incr.4.40$resCog41.Incr.Lev,type="l",xlab = "time",main="sample 63 observations",ylab="L")
   plot(seq(1,length(Incr.4.45[,1])),Incr.4.45$resCog41.Incr.Lev,type="l",xlab = "time",main="sample 53 observations",ylab="L")
   plot(seq(1,length(Incr.4.50[,1])),Incr.4.50$resCog41.Incr.Lev,type="l",xlab = "time",main="sample 43 observations",ylab="L")
   title(main="Волатильність дисперсії для курсу продажу на готівковому ринку",outer=TRUE)
   grDevices::savePlot("Волатильність дисперсії для курсу продажу на готівковому ринку.png",type="png")
   }
#fig.2####
{par(mfrow=c(4,2),mar=c(2.5, 4.5, 3.5, 1),oma=c(1,1,2,1))
  plot(seq(1,length(variance1.in.sample.5[,1])),variance1.in.sample.5[,1],type="l",xlab = "time",main="sample 130 observations",ylab="sigma")
  plot(seq(1,length(variance1.in.sample.10[,1])),variance1.in.sample.10[,1],type="l",xlab = "time",main="sample 125 observations",ylab="sigma")
  plot(seq(1,length(variance1.in.sample.20[,1])),variance1.in.sample.20[,1],type="l",xlab = "time",main="sample 115 observations",ylab="sigma")
  plot(seq(1,length(variance1.in.sample.30[,1])),variance1.in.sample.30[,1],type="l",xlab = "time",main="sample 105 observations",ylab="sigma")
  plot(seq(1,length(variance1.in.sample.37[,1])),variance1.in.sample.37[,1],type="l",xlab = "time",main="sample 98 observations",ylab="sigma")
  plot(seq(1,length(variance1.in.sample.40[,1])),variance1.in.sample.40[,1],type="l",xlab = "time",main="sample 95 observations",ylab="sigma")
  plot(seq(1,length(variance1.in.sample.45[,1])),variance1.in.sample.45[,1],type="l",xlab = "time",main="sample 85 observations",ylab="sigma")
  plot(seq(1,length(variance1.in.sample.50[,1])),variance1.in.sample.50[,1],type="l",xlab = "time",main="sample 75 observations",ylab="sigma")
  title(main="Дисперсія для курсу, встановленого НБУ",outer=TRUE)
  grDevices::savePlot("Дисперсія для курсу, встановленого НБУ.png",type="png")
  }

{ par(mfrow=c(4,2),mar=c(2.5, 4.5, 3.5, 1),oma=c(1,1,2.5,1))
  plot(seq(1,length(variance2.in.sample.5[,1])),variance2.in.sample.5[,1],type="l",xlab = "time",main="sample 104 observations",ylab="sigma")
  plot(seq(1,length(variance2.in.sample.10[,1])),variance2.in.sample.10[,1],type="l",xlab = "time",main="sample 99 observations",ylab="sigma")
  plot(seq(1,length(variance2.in.sample.20[,1])),variance2.in.sample.20[,1],type="l",xlab = "time",main="sample 89 observations",ylab="sigma")
  plot(seq(1,length(variance2.in.sample.30[,1])),variance2.in.sample.30[,1],type="l",xlab = "time",main="sample 79 observations",ylab="sigma")
  plot(seq(1,length(variance2.in.sample.37[,1])),variance2.in.sample.37[,1],type="l",xlab = "time",main="sample 72 observations",ylab="sigma")
  plot(seq(1,length(variance2.in.sample.40[,1])),variance2.in.sample.40[,1],type="l",xlab = "time",main="sample 69 observations",ylab="sigma")
  plot(seq(1,length(variance2.in.sample.45[,1])),variance2.in.sample.45[,1],type="l",xlab = "time",main="sample 59 observations",ylab="sigma")
  plot(seq(1,length(variance2.in.sample.50[,1])),variance2.in.sample.50[,1],type="l",xlab = "time",main="sample 49 observations",ylab="sigma")
  title(main="Дисперсія для міжбанківського курсу",outer=TRUE)
  grDevices::savePlot("Дисперсія для міжбанківського курсу.png",type="png")
  }

{par(mfrow=c(4,2),mar=c(2.5, 4.5, 3.5, 1),oma=c(1,1,2,1))
  plot(seq(1,length(variance3.in.sample.5[,1])),variance3.in.sample.5[,1],type="l",xlab = "time",main="sample 98 observations",ylab="sigma")
  plot(seq(1,length(variance3.in.sample.10[,1])),variance3.in.sample.10[,1],type="l",xlab = "time",main="sample 93 observations",ylab="sigma")
  plot(seq(1,length(variance3.in.sample.20[,1])),variance3.in.sample.20[,1],type="l",xlab = "time",main="sample 83 observations",ylab="sigma")
  plot(seq(1,length(variance3.in.sample.30[,1])),variance3.in.sample.30[,1],type="l",xlab = "time",main="sample 73 observations",ylab="sigma")
  plot(seq(1,length(variance3.in.sample.37[,1])),variance3.in.sample.37[,1],type="l",xlab = "time",main="sample 66 observations",ylab="sigma")
  plot(seq(1,length(variance3.in.sample.40[,1])),variance3.in.sample.40[,1],type="l",xlab = "time",main="sample 63 observations",ylab="sigma")
  plot(seq(1,length(variance3.in.sample.45[,1])),variance3.in.sample.45[,1],type="l",xlab = "time",main="sample 53 observations",ylab="sigma")
  plot(seq(1,length(variance3.in.sample.50[,1])),variance3.in.sample.50[,1],type="l",xlab = "time",main="sample 43 observations",ylab="sigma")
  title(main="Дисперсія для курсу купівлі на готівковому ринку",outer=TRUE)
  grDevices::savePlot("Дисперсія для курсу купівлі на готівковому ринку.png",type="png")
}

{ par(mfrow=c(4,2),mar=c(2.5, 4.5, 3.5, 1),oma=c(1,1,2,1))
  plot(seq(1,length(variance4.in.sample.5[,1])),variance4.in.sample.5[,1],type="l",xlab = "time",main="sample 98 observations",ylab="sigma")
  plot(seq(1,length(variance4.in.sample.10[,1])),variance4.in.sample.10[,1],type="l",xlab = "time",main="sample 93 observations",ylab="sigma")
  plot(seq(1,length(variance4.in.sample.20[,1])),variance4.in.sample.20[,1],type="l",xlab = "time",main="sample 83 observations",ylab="sigma")
  plot(seq(1,length(variance4.in.sample.30[,1])),variance4.in.sample.30[,1],type="l",xlab = "time",main="sample 73 observations",ylab="sigma")
  plot(seq(1,length(variance4.in.sample.37[,1])),variance4.in.sample.37[,1],type="l",xlab = "time",main="sample 66 observations",ylab="sigma")
  plot(seq(1,length(variance4.in.sample.40[,1])),variance4.in.sample.40[,1],type="l",xlab = "time",main="sample 63 observations",ylab="sigma")
  plot(seq(1,length(variance4.in.sample.45[,1])),variance4.in.sample.45[,1],type="l",xlab = "time",main="sample 53 observations",ylab="sigma")
  plot(seq(1,length(variance4.in.sample.50[,1])),variance4.in.sample.50[,1],type="l",xlab = "time",main="sample 43 observations",ylab="sigma")
  title(main="Дисперсія для курсу продажу на готівковому ринку",outer=TRUE)
  grDevices::savePlot("Дисперсія для курсу продажу на готівковому ринку.png",type="png")}

stationarity.5_50<-cbind(stationarity.test.10,stationarity.test.20,stationarity.test.30,stationarity.test.37,stationarity.test.40,stationarity.test.45,stationarity.test.50)
is.data.frame(stationarity.10_50)
setwd("H:/GARCH code/data")
WriteXLS::WriteXLS(stationarity.5_50,"stationarity.5_50.xlsx",row.names = TRUE)
WriteXLS::WriteXLS(stationarity.test.45,"stationarity.45.xlsx",row.names = TRUE)


#fig.3####
{par(mfrow=c(4,2),mar=c(2.5, 4.5, 3.5, 1),oma=c(1,1,2,1))
  plot(seq(1,length(Incr.1.o.10[,1])),Incr.1.10$resCog12.Incr.Lev,type="l",xlab = "time",main="sample 10 observations",ylab="L")
  plot(seq(1,length(Incr.1.o.20[,1])),Incr.1.o.20$resCog12.Incr.Lev,type="l",xlab = "time",main="sample 20 observations",ylab="L")
  plot(seq(1,length(Incr.1.o.30[,1])),Incr.1.o.30$resCog12.Incr.Lev,type="l",xlab = "time",main="sample 30 observations",ylab="L")
  plot(seq(1,length(Incr.1.o.37[,1])),Incr.1.o.37$resCog12.Incr.Lev,type="l",xlab = "time",main="sample 37 observations",ylab="L")
  plot(seq(1,length(Incr.1.o.40[,1])),Incr.1.o.40$resCog12.Incr.Lev,type="l",xlab = "time",main="sample 40 observations",ylab="L")
  plot(seq(1,length(Incr.1.o.45[,1])),Incr.1.o.45$resCog12.Incr.Lev,type="l",xlab = "time",main="sample 45 observations",ylab="L")
  plot(seq(1,length(Incr.1.o.50[,1])),Incr.1.o.50$resCog12.Incr.Lev,type="l",xlab = "time",main="sample 50 observations",ylab="L")
  title(main="Волатильність дисперсії для курсу, встановленого НБУ",outer=TRUE)
  grDevices::savePlot("Волатильність дисперсії для курсу, встановленого НБУ для позавибіркової сукупності.png",type="png")
}

{par(mfrow=c(4,2),mar=c(2.5, 4.5, 1.5, 1),oma=c(1,1,2.25,1))
  plot(seq(1,length(Incr.2.o.10[,1])),Incr.2.o.10$resCog22.Incr.Lev,type="l",xlab = "time",main="sample 10 observations",ylab="L")
  plot(seq(1,length(Incr.2.o.20[,1])),Incr.2.o.20$resCog22.Incr.Lev,type="l",xlab = "time",main="sample 20 observations",ylab="L")
  plot(seq(1,length(Incr.2.o.30[,1])),Incr.2.o.30$resCog22.Incr.Lev,type="l",xlab = "time",main="sample 30 observations",ylab="L")
  plot(seq(1,length(Incr.2.o.37[,1])),Incr.2.o.37$resCog22.Incr.Lev,type="l",xlab = "time",main="sample 37 observations",ylab="L")
  plot(seq(1,length(Incr.2.o.40[,1])),Incr.2.o.40$resCog22.Incr.Lev,type="l",xlab = "time",main="sample 40 observations",ylab="L")
  plot(seq(1,length(Incr.2.o.45[,1])),Incr.2.o.45$resCog22.Incr.Lev,type="l",xlab = "time",main="sample 45 observations",ylab="L")
  plot(seq(1,length(Incr.2.o.50[,1])),Incr.2.o.50$resCog22.Incr.Lev,type="l",xlab = "time",main="sample 50 observations",ylab="L")
  title(main="Волатильність дисперсії для міжбанківського курсу",outer =TRUE)
  grDevices::savePlot("Волатильність дисперсії для міжбанківського курсу для позавибіркової сукупності.png",type="png")
}

{par(mfrow=c(4,2),mar=c(2.5, 4.5, 3.5, 1),oma=c(1,1,2,1))
  plot(seq(1,length(Incr.3.o.10[,1])),Incr.3.o.10$resCog32.Incr.Lev,type="l",xlab = "time",main="sample 10 observations",ylab="L")
  plot(seq(1,length(Incr.3.o.20[,1])),Incr.3.o.20$resCog32.Incr.Lev,type="l",xlab = "time",main="sample 20 observations",ylab="L")
  plot(seq(1,length(Incr.3.o.30[,1])),Incr.3.o.30$resCog32.Incr.Lev,type="l",xlab = "time",main="sample 30 observations",ylab="L")
  plot(seq(1,length(Incr.3.o.37[,1])),Incr.3.o.37$resCog32.Incr.Lev,type="l",xlab = "time",main="sample 37 observations",ylab="L")
  plot(seq(1,length(Incr.3.o.40[,1])),Incr.3.o.40$resCog32.Incr.Lev,type="l",xlab = "time",main="sample 40 observations",ylab="L")
  plot(seq(1,length(Incr.3.o.45[,1])),Incr.3.o.45$resCog32.Incr.Lev,type="l",xlab = "time",main="sample 45 observations",ylab="L")
  plot(seq(1,length(Incr.3.o.50[,1])),Incr.3.o.50$resCog32.Incr.Lev,type="l",xlab = "time",main="sample 50 observations",ylab="L")
  title(main="Волатильність дисперсії для курсу купівлі на готівковому ринку",outer=TRUE)
  grDevices::savePlot("Волатильність дисперсії для курсу купівлі на готівковому ринку для позавибіркової сукупності.png",type="png")
}

{  par(mfrow=c(4,2),mar=c(2.5, 4.5, 3.5, 1),oma=c(1,1,2,1))
  plot(seq(1,length(Incr.4.o.10[,1])),Incr.4.o.10$resCog42.Incr.Lev,type="l",xlab = "time",main="sample 10 observations",ylab="L")
  plot(seq(1,length(Incr.4.o.20[,1])),Incr.4.o.20$resCog42.Incr.Lev,type="l",xlab = "time",main="sample 20 observations",ylab="L")
  plot(seq(1,length(Incr.4.o.30[,1])),Incr.4.o.30$resCog42.Incr.Lev,type="l",xlab = "time",main="sample 30 observations",ylab="L")
  plot(seq(1,length(Incr.4.o.37[,1])),Incr.4.o.37$resCog42.Incr.Lev,type="l",xlab = "time",main="sample 37 observations",ylab="L")
  plot(seq(1,length(Incr.4.o.40[,1])),Incr.4.o.40$resCog42.Incr.Lev,type="l",xlab = "time",main="sample 40 observations",ylab="L")
  plot(seq(1,length(Incr.4.o.45[,1])),Incr.4.o.45$resCog42.Incr.Lev,type="l",xlab = "time",main="sample 45 observations",ylab="L")
  plot(seq(1,length(Incr.4.o.50[,1])),Incr.4.o.50$resCog42.Incr.Lev,type="l",xlab = "time",main="sample 50 observations",ylab="L")
  title(main="Волатильність дисперсії для курсу продажу на готівковому ринку",outer=TRUE)
  grDevices::savePlot("Волатильність дисперсії для курсу продажу на готівковому ринку для позавибіркової сукупності.png",type="png")
}
#fig.4####
{par(mfrow=c(4,2),mar=c(2.5, 4.5, 3.5, 1),oma=c(1,1,2,1))
  plot(seq(1,length(variance1.out.of.sample.10[,1])),variance1.out.of.sample.10[,1],type="l",xlab = "time",main="sample 10 observations",ylab="sigma")
  plot(seq(1,length(variance1.out.of.sample.20[,1])),variance1.out.of.sample.20[,1],type="l",xlab = "time",main="sample 20 observations",ylab="sigma")
  plot(seq(1,length(variance1.out.of.sample.30[,1])),variance1.out.of.sample.30[,1],type="l",xlab = "time",main="sample 30 observations",ylab="sigma")
  plot(seq(1,length(variance1.out.of.sample.37[,1])),variance1.out.of.sample.37[,1],type="l",xlab = "time",main="sample 37 observations",ylab="sigma")
  plot(seq(1,length(variance1.out.of.sample.40[,1])),variance1.out.of.sample.40[,1],type="l",xlab = "time",main="sample 40 observations",ylab="sigma")
  plot(seq(1,length(variance1.out.of.sample.45[,1])),variance1.out.of.sample.45[,1],type="l",xlab = "time",main="sample 45 observations",ylab="sigma")
  plot(seq(1,length(variance1.out.of.sample.50[,1])),variance1.out.of.sample.50[,1],type="l",xlab = "time",main="sample 50 observations",ylab="sigma")
  title(main="Дисперсія для курсу, встановленого НБУ у позавибірковій сукупності",outer=TRUE)
  grDevices::savePlot("Дисперсія для курсу, встановленого НБУ.png",type="png")
}

{ par(mfrow=c(4,2),mar=c(2.5, 4.5, 3.5, 1),oma=c(1,1,2.5,1))
  plot(seq(1,length(variance2.out.of.sample.10[,1])),variance2.out.of.sample.10[,1],type="l",xlab = "time",main="sample 10 observations",ylab="sigma")
  plot(seq(1,length(variance2.out.of.sample.20[,1])),variance2.out.of.sample.20[,1],type="l",xlab = "time",main="sample 20 observations",ylab="sigma")
  plot(seq(1,length(variance2.out.of.sample.30[,1])),variance2.out.of.sample.30[,1],type="l",xlab = "time",main="sample 30 observations",ylab="sigma")
  plot(seq(1,length(variance2.out.of.sample.37[,1])),variance2.out.of.sample.37[,1],type="l",xlab = "time",main="sample 37 observations",ylab="sigma")
  plot(seq(1,length(variance2.out.of.sample.40[,1])),variance2.out.of.sample.40[,1],type="l",xlab = "time",main="sample 40 observations",ylab="sigma")
  plot(seq(1,length(variance2.out.of.sample.45[,1])),variance2.out.of.sample.45[,1],type="l",xlab = "time",main="sample 45 observations",ylab="sigma")
  plot(seq(1,length(variance2.out.of.sample.50[,1])),variance2.out.of.sample.50[,1],type="l",xlab = "time",main="sample 50 observations",ylab="sigma")
  title(main="Дисперсія для міжбанківського курсу",outer=TRUE)
  grDevices::savePlot("Дисперсія для міжбанківського курсу у позавибірковій сукупності.png",type="png")
}

{par(mfrow=c(4,2),mar=c(2.5, 4.5, 3.5, 1),oma=c(1,1,2,1))
  plot(seq(1,length(variance3.out.of.sample.10[,1])),variance3.out.of.sample.10[,1],type="l",xlab = "time",main="sample 10 observations",ylab="sigma")
  plot(seq(1,length(variance3.out.of.sample.20[,1])),variance3.out.of.sample.20[,1],type="l",xlab = "time",main="sample 20 observations",ylab="sigma")
  plot(seq(1,length(variance3.out.of.sample.30[,1])),variance3.out.of.sample.30[,1],type="l",xlab = "time",main="sample 30 observations",ylab="sigma")
  plot(seq(1,length(variance3.out.of.sample.37[,1])),variance3.out.of.sample.37[,1],type="l",xlab = "time",main="sample 37 observations",ylab="sigma")
  plot(seq(1,length(variance3.out.of.sample.40[,1])),variance3.out.of.sample.40[,1],type="l",xlab = "time",main="sample 40 observations",ylab="sigma")
  plot(seq(1,length(variance3.out.of.sample.45[,1])),variance3.out.of.sample.45[,1],type="l",xlab = "time",main="sample 45 observations",ylab="sigma")
  plot(seq(1,length(variance3.out.of.sample.50[,1])),variance3.out.of.sample.50[,1],type="l",xlab = "time",main="sample 50 observations",ylab="sigma")
  title(main="Дисперсія для курсу купівлі на готівковому ринку",outer=TRUE)
  grDevices::savePlot("Дисперсія для курсу купівлі на готівковому ринку у позавибірковій сукупності.png",type="png")
}

{ par(mfrow=c(4,2),mar=c(2.5, 4.5, 3.5, 1),oma=c(1,1,2,1))
  plot(seq(1,length(variance4.out.of.sample.10[,1])),variance4.out.of.sample.10[,1],type="l",xlab = "time",main="sample 10 observations",ylab="sigma")
  plot(seq(1,length(variance4.out.of.sample.20[,1])),variance4.out.of.sample.20[,1],type="l",xlab = "time",main="sample 20 observations",ylab="sigma")
  plot(seq(1,length(variance4.out.of.sample.30[,1])),variance4.out.of.sample.30[,1],type="l",xlab = "time",main="sample 30 observations",ylab="sigma")
  plot(seq(1,length(variance4.out.of.sample.37[,1])),variance4.out.of.sample.37[,1],type="l",xlab = "time",main="sample 37 observations",ylab="sigma")
  plot(seq(1,length(variance4.out.of.sample.40[,1])),variance4.out.of.sample.40[,1],type="l",xlab = "time",main="sample 40 observations",ylab="sigma")
  plot(seq(1,length(variance4.out.of.sample.45[,1])),variance4.out.of.sample.45[,1],type="l",xlab = "time",main="sample 45 observations",ylab="sigma")
  plot(seq(1,length(variance4.out.of.sample.50[,1])),variance4.out.of.sample.50[,1],type="l",xlab = "time",main="sample 50 observations",ylab="sigma")
  title(main="Дисперсія для курсу продажу на готівковому ринку",outer=TRUE)
  grDevices::savePlot("Дисперсія для курсу продажу на готівковому ринку у позавибірковій сукупності.png",type="png")}

stationarity.5_50<-cbind(stationarity.test.5,stationarity.test.10,stationarity.test.20,stationarity.test.30,stationarity.test.37,stationarity.test.40,stationarity.test.45,stationarity.test.50)
is.data.frame(stationarity.5_50)
setwd("H:/GARCH code/data")
WriteXLS::WriteXLS(stationarity.5_50,"stationarity.5_50.xlsx",row.names = TRUE)
WriteXLS::WriteXLS(stationarity.test.45,"stationarity.45.xlsx",row.names = TRUE)
###end####
rm(list=ls())
q(save="no",1,FALSE)#or else
q(save="yes")
