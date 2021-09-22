folder.auto<-c("C:/Users/Asus/AppData/Roaming/Microsoft/Word/") #clean
list.to.clean<-(dir("C:/Users/Asus/AppData/Roaming/Microsoft/Word"))

if(comp=="mac"){Sys.setlocale("LC_ALL",locale="uk_UA.UTF-8")} else Sys.setlocale("LC_ALL",locale="Ukrainian")
FormatDecimal <- function(x, k) {
  return(formatC(format(round(as.numeric(x), k), nsmall=k),scientific=FALSE,big.mark=" "))
}
#Directory################
DATA<-c("20.05","21.05","24.05","3.06","NA","25.08","23.12","7.02")
i<-6
#"Esc+Ctrl+C"
#load#####
# load(paste("H:/svar.code/data/1/",DATA[i],".RData",sep=""))
{if(Sys.info()[4]=="MacBook-Air-Ulia.local"){comp=c("Mac","Asus")[1]}else{comp=c("Mac","Asus")[2]}
disk=c("OneDrive","H","I")[1]}
if(comp=="Mac"){if(disk=="OneDrive"){Dir.svar<- paste("~/OneDrive/Computer/I/svar.code/data/1/",DATA[i],".RData",sep="")}else{Dir.svar<- paste("~/Volumes/USB/svar.code/data/1/",DATA[i],".RData",sep="")}}else{
  if(disk=="OneDrive"){Dir.svar<- paste("C:/Users/Asus/OneDrive/Computer/I/svar.code/data/1/",DATA[i],".RData",sep="")}else{if(disk=="H"){Dir.svar<- paste("H:/svar.code/data/1/",DATA[i],".RData",sep="")}else{Dir.svar<- paste("I:/svar.code/data/1/",DATA[i],".RData",sep="")}}}
##Disk######
if(Sys.info()[4]=="MacBook-Air-Ulia.local"){comp="Mac"}else{comp="Asus"}
if(comp=="Mac"){disk="~/OneDrive/"}else{disk="C:/Users/Asus/OneDrive/"}
if(comp=="Asus"){
  OneDrive<-"C:/Users/Asus/OneDrive/Computer/"}else{OneDrive<-"~/OneDrive/Computer/"}
if(comp=="Mac"){Dir.svar<-paste(disk,"Computer/I/svar.code/data/var.and.svar.RData",sep="")}else{Dir.svar<-"C:/Users/Asus/OneDrive/Computer/I/svar.code/data/2var.and.svar.RData"}
##load ws####
load(Dir.svar)
# load("/Volumes/USB/svar.code/data/1/23.12.RData");welcome()#9084850@olha 1887
##save ws####
save.image(Dir.svar)
rm(list=ls())
#libraries#####
.libPaths()
{library("foreach")
  library("urca")
  library("vars")
  library("VARsignR")
  library("HI")
  library("mvnfast")
  library("readr")
  library("readxl")
  library("ggplot2")
  library("RColorBrewer")
}
packages<-data.frame(list.files("C:/Users/Asus/Documents/R/win-library/4.0.2"))

##setwd()####
disk.1=c("OneDrive","H","I")[1]
if(comp=="Asus"){if(disk.1=="I"){setwd("C:/Users/Asus/OneDrive/Computer/I/svar.code/result5")}else{setwd("C:/Users/Asus/OneDrive/Computer/H/svar.code/result")}}else{if(disk.1=="H"){setwd("~/Volumes/USB/svar.code/result")}else{
  setwd("~/OneDrive/Computer/I/svar.code/result 6")}}

setwd(paste(strsplit(getwd(),"/result")[[1]][1],c("","fin.spillover")[1],sep="/"))
if(strsplit(getwd(),split="/")[[1]][8]=="result"){directory.1<-c("/result")}else{directory.1<-c("/model")};i=c("",2,3,4)[4];setwd(paste(strsplit(getwd(),directory.1)[[1]][1],paste("result",i,sep=""),sep="/"))

if(comp=="Mac")setwd("~/OneDrive/Computer/H/functions/project/econpckg/")else{setwd("C:/Users/Asus/OneDrive/Computer/H/functions/project/econpckg")}
####OPEN SRYPT####################
if(comp=="Mac"){disk="~/OneDrive/"}else{disk="C:/Users/Asus/OneDrive/"};slot<-"I"
path=c("Computer/I/svar.code/","Computer/I/functions/")[2]
file.edit(paste(disk,path,"yahoo.finance.R",sep=""))
f<-c("NBU",1:11)
for(f in f){file.edit(paste(disk,"Computer/I/svar.code/svarchannel",f,".R",sep=""))}
file.edit("H:/functions/project/econpckg/graphical.par.R")
{functions<-c("add.row","Welcome!","identify","identify.svec","trend.interpolation","seasonally adjusted","rugarch.plots","plot.irf.2.1","ArchTest","Berkowitz test","causality.R","svarchannel","svecchannel","svartest","qmleL_yuima",".gofGarch",".ARCHLMTEST","remove","mode","linearised.ts","plot.irf.2","plot.irf","JSON","graphical.par","Euler.discretization","GMM","division")
  which(functions=="division",arr.ind=TRUE)->f;disk<-ifelse(comp=="Mac","~/OneDrive/Computer/I",ifelse(disk=="C:/Users/Asus/OneDrive/",ifelse(slot=="H","C:/Users/Asus/OneDrive/Computer/H","C:/Users/Asus/OneDrive/Computer/I"),ifelse(slot=="H","H:/","I:/")))
  file.edit(paste(disk,"/functions/project/econpckg/R/",functions[f],".R",sep=""))
}
if(strsplit(getwd(),split="/")[[1]][8]=="model"){setwd(paste(strsplit(getwd(),"/model")[[1]][1],sep="/"))}
file.edit(paste(getwd(),"/analysis.R",sep=""))
for(i in c("",".short",".only.model")[2]){file.edit(paste(getwd(),"/varchannel",i,".R",sep=""))}
file.edit(paste(getwd(),c("/restrictions.NEW.R","/additional.models.R")[1],sep=""))
for(i in c("","NBU",1:11)[]){file.edit(paste(getwd(),"/svarchannel",i,".R",sep=""))}
for(i in c("",1:2)[]){file.edit(paste(getwd(),"/Garch sampling",i,".R",sep=""))}
for(i in ""){file.edit(paste(getwd(),"/rugarch sampling",i,".R",sep=""))}

packages<-list.files(paste(strsplit(getwd(),split="/svar.code")[[1]][1],"/functions/project/econpckg/R/",sep=""))
file.edit(paste(strsplit(getwd(),split="/svar.code")[[1]][1],"/functions/project/econpckg/R/","trend.interpolation.R",sep=""))

#macrofin.stab patterns####
table<-readxl::read_excel("F:/І розділ/Монетарний механізм/монетарний трансмісійний механізм 2.xlsx",sheet="Лист2")
cov1<-cov(table[,names(table)[c(3:20,23:34,41:48,51:230)]])
table2<-readxl::read_excel("F:/І розділ/statistics!/статистика/Стат_09.17/forex.data 1.xlsx",sheet="sample augmented")
x.model<-stats::model.matrix(table2[,1]$`Official exchange rate`~0+table2[,2][[1]]+table2[,3][[1]]+table2[,4][[1]]+table2[,5][[1]]+table2[,6][[1]]+table2[,7][[1]]+table2[,8][[1]]+table2[,9][[1]]+table2[,10][[1]]+table2[,11][[1]]+table2[,12][[1]]+table2[,13][[1]]+table2[,14][[1]]+table2[,15][[1]],table2[36:126,])
colnames(x.model)<-c(names(table2)[2:15])
model.fin.stab<-lm(table2[[1]][36:126]~0+x.model[,1:14])
model.fin.stab$coefficients<-data.frame(model.fin.stab$coefficients)
colnames(model.fin.stab$coefficients)<-c("Intercept","0",names(table2)[2:15])
WriteXLS::WriteXLS(model.fin.stab$coefficients,"finstab.reaction on volatility.xlsx")
#financial stability########
hedging<-read_excel("H:/data/Monetary transmission mechanism in Ukraine.xlsx",
                   sheet = "Sheet8")[2:47,184]
colnames(hedging)<-"hedge.to.trade.loss"
hedging<-data.frame(as.numeric(hedging$hedge.to.trade.loss))
x=(hedging$hedge.to.trade.loss);n=length(hedging$hedge.to.trade.loss);m=3;hedging.interp=trend.interpolation(x,n,m);colnames(hedging.interp)<-"hedge.to.trade.loss"
WriteXLS::WriteXLS(hedging.interp,ExcelFileName = "hedging.interp.xlsx")
#I########################
###gap#######
gap.filter<-function(x,freq,drift=TRUE){if(drift==TRUE){
  Filtering<-mFilter::hpfilter(ts(x),freq=freq,type="frequency",drift=TRUE)}else{Filtering<-mFilter::hpfilter(ts(x),freq=freq,type="frequency",drift=FALSE)}
  trend.1<-Filtering$trend

  cycle.1<-Filtering$cycle
  real.var.gap<-(1-((cycle.1)/x))*100
  result=list(trend=trend.1,cycle=cycle.1,gap=real.var.gap)
  return(result)
}
#####interests and money#######
N=163
discount.rate<-readxl::read_excel(paste(disk,"/OneDrive/Thesis Ph.D (2)/Statistical appendicies/Monetary transmission mechanism in Ukraine compare - Mac.xlsx",sep=""),sheet="Sheet1..")[1:N,]
discount.rate<-gap.filter(discount.rate$Discount.rate,12)
Discount.rate.trend<-discount.rate$trend
Discount.rate.gap<-discount.rate$gap;discount.rate.sa<-data.frame(Discount.rate.trend,Discount.rate.gap)
real.rate<-gap.filter(real.rate$real.rate,12)
real.rate.trend<-real.rate$trend
real.rate.gap<-real.rate$gap;real.rate.sa<-data.frame(real.rate.trend,real.rate.gap)
library(readxl)
M3<-read_excel("~/OneDrive/Thesis Ph.D (2)/Statistical appendicies/Monetary transmission mechanism in Ukraine compare.xlsx",sheet="Sheet1..")[1:163,"Money.stock"]
tail(M3);M3<-gap.filter(M3$Money.stock,12)
M3.trend<-M3$trend
M3.gap<-M3$gap;WriteXLS::WriteXLS(M3.sa,"M3.sa.May.2020.xlsx",FreezeRow = 1,FreezeCol = 1)<-data.frame(as.Date(seq.Date(from=as.Date("2006-12-1"),to=as.Date("2020-06-01"),by="months")),M3.trend,M3.trend-M3$cycle,M3.gap)
#####sa#####
Real.GDP.S.A=seasonal.adjusting(na.omit(dataset6$real.gdp[2:66]),year = 2002,n = 4)
x1=matrix(Real.GDP.S.A,ncol=1);n=length(Real.GDP.S.A);m=3;Real.GDP.S.A.interp=trend.interpolation(x1,n,m)
#gdp from 2002
Real.GDP<-Real.GDP.S.A.interp[61:195,]
transmission2$Real.GDP<-Real.GDP
consumption.s.a=seasonal.adjusting(na.omit(dataset6$Consumption[2:66]),year = 2002,n = 4)
savings.s.a=seasonal.adjusting(na.omit(dataset6$savings[2:66]),year = 2002,n = 4)
nx.s.a=seasonal.adjusting(na.omit(dataset6$NX[2:66]),year = 2002,n = 4)
x1=matrix(consumption.s.a,ncol=1);n=length(consumption.s.a);m=3;consumption.s.a.interp=trend.interpolation(x1,n,m)
x1=matrix(savings.s.a,ncol=1);n=length(savings.s.a);m=3;savings.s.a.interp=trend.interpolation(x1,n,m)
x1=matrix(nx.s.a,ncol=1);n=length(nx.s.a);m=3;nx.s.a.interp=trend.interpolation(x1,n,m)
Real.GDP.gap<-mFilter::hpfilter(Real.GDP,freq = 12,type="frequency",drift=TRUE)$cycle/mFilter::hpfilter(Real.GDP,freq = 12,type="frequency",drift=TRUE)$x
Real.GDP.gap.t<-cbind(time[61:193],Real.GDP.gap);colnames(Real.GDP.gap.t)[1]<-"time"
WriteXLS::WriteXLS(Real.GDP.gap.t,"Real.GDP.gap.t.xlsx",FreezeRow=1,FreezeCol=1)
if(comp=="mac"){OneDrive<-"/Users/ulia/OneDrive/Computer"}else{"/C:/Users/Asus/OneDrive/Computer"}
setwd(paste(OneDrive,"/H/svar.code/result/New estimation",sep=""))
#II########################
#######data#####
View(data.frame(names(dataset1)))
View(dataset1[1:135,c(1,which("REER"==names(dataset1),arr.ind=TRUE))])
View(dataset1[which(as.character("2008-10-01")==dataset1$date,arr.ind=TRUE),c(1,which("REER"==names(dataset1),arr.ind=TRUE))])
filter<-(transmission2[1:135,c(1,which("Monetary.policy.rule"==names(dataset1),arr.ind=TRUE))])
View(filter)
k2=sample.model2$K;k;ynames.2
getp("table.var",i)[1,5:(5+sample.model9$K-1)]
exp(svarchannel2.irf.tab)
##wd#############
# if(comp=="Mac)setwd("~/Volumes/USB/svar.code/result")else{setwd("C:/Users/Asus/OneDrive/Computer/svar.code/result")}
###Estimated model###########
svarchannel0$svar.irf.ABSVAR
svarchannel1$svar.irf.ABSVAR
svarchannel2$svar.irf.ABSVAR
svarchannel3$svar.irf.ABSVAR
svarchannel3$svar.irf.ABSVAR
svarchannel4$svar.irf.ABSVAR
svarchannel5$svar.irf.ABSVAR
svarchannel6$svar.irf.ABSVAR
svarchannel7$svar.irf.ABSVAR
svarchannel8$svar.irf.ABSVAR
svarchannel9$svar.irf.ABSVAR
svarchannel10$svar.irf.ABSVAR
svarchannel11$svar.irf.ABSVAR
svarchannel12$svar.irf.ABSVAR

####query#########"switch to the either sample.model.2 and svarchannel.2. or sample.model and svarchannel####

auto=TRUE
set.color<-c(heat.colors(1,.5),hcl.colors(10,"greens",rev=TRUE)[10],gray(.47,.9),heat.colors(1,.5))[3]
eta=c("NBU",1:12,14:16)[2];for(j in 2:getp("sample.model",eta)$K){
  if(result6==TRUE){if(eta=="NBU"){eta<-0}else{eta<-as.numeric(eta)}}
k2<-getp("svarchannel",eta)$ABSVAR$var$K;im1=ifelse(auto==TRUE,get(paste("ynames.",eta,sep=""))[1],c("Real.GDP","REER")[2]);im1=which(getp("ynames.",eta)==im1,arr.ind=TRUE)#
if(eta%in%0:12){teta=eta+1}else{teta=eta}
if(eta%in%0:12){model=c("NBU",1:12)[eta+1]}else{model=eta}
if(getp("svarchannel",eta)$ABSVAR$var$K%%2==0){v=-getp("svarchannel",eta)$ABSVAR$var$K+2}else{v=-getp("svarchannel",eta)$ABSVAR$var$K}
plot.irf.2(eta,name.eta=getp("ynames.",eta),model=model,v=v,w=2,mu=1:3,plot.kind="with area",se=TRUE,svar=TRUE)
plot.irf.2(eta,name.eta=getp("ynames.",eta),model=model,main="інструментів регуляції волатильності\n",v=0,w=1,mu=1:3,plot.kind="with area",se=TRUE,svar=TRUE)
cat(paste("\n",getp("ynames.",eta)[j],":"))[1]
cat(" Month-by-month difference",array(diff(exp(getp("svarchannel",c("",".2.")[1],eta)$svar.irf.AB[,1:3+(j-1)*3])[,2],lag = 1))) #
cat("\n\nQ-by-Q difference",diff(exp(getp("svarchannel",eta)$svar.irf.AB[,1:3+(j-1)*3])[,2],lag = 4)[c(1,4,7,8)])
cat("\n\nh.y-by-h.y difference",diff(exp(getp("svarchannel",eta)$svar.irf.AB[,1:3+(j-1)*3])[,2],lag = 6)[c(1,6)])
# cat("\n\n8 month difference",diff(getp("svarchannel",eta,".irf.tab")[,1:3+(j-1)*3][1:10,2],lag = 8)[1])
# cat("\n\n9 month difference",diff(getp("svarchannel",eta,".irf.tab")[,1:3+(j-1)*3][1:10,2],lag = 9)[1])
# cat("\n\nmean difference",mean(diff(exp(getp("svarchannel",eta)$svar.irf.AB[,1:3+(j-1)*3])[,2],lag = 1)))
cat("\n\nMonth-by-month change",div(getp("svarchannel",eta,".irf.tab")[,1:3+(j-1)*3][1:10,2],lag = 2)[c(1:8)])
cat("\n\nQ-by-Q change",div(getp("svarchannel",eta,".irf.tab")[,1:3+(j-1)*3][1:10,2],lag = 4)[c(1:6)])
cat("\n\n8 month change",div(getp("svarchannel",eta,".irf.tab")[,1:3+(j-1)*3][1:10,2],lag = 8)[1:2])
cat("\n\n9 month change",div(getp("svarchannel",eta,".irf.tab")[,1:3+(j-1)*3][1:10,2],lag = 9)[1])
cat("\n\nmean change:",mean(diff(getp("svarchannel",eta,".irf.tab")[,1:3+(j-1)*3][1:10,2],lag = 1)[2:9] ))
# cat("\n\nsvec half year change ir:",div((getp("svec.channel",eta)$svec$svec.irf.AB[,1:3+(j-1)*3])[,2],6)[c(1,6)])
cat("\n\nSVAR forecast fan chart for", getp("ynames.",eta)[j]," :\n");transmission2[163-which(as.Date(transmission2$date)==as.Date("2008-11-01"),arr.ind=TRUE),getp("ynames.",eta)[j]][[1]]*exp(data.frame(cbind(predict(getp("svarchannel",eta)$ABSVAR$var,ci=.95)$fcst[[j]])))
par(mfrow=c(1,1),mar=c(rep(.1,2),1,1),mai=c(rep(.2,2),.2,.4),oma=c(.12,rep(.1,3)))
cat("\n\nSVAR forecast :");show(data.frame(res=getp("svarchannel",eta)$ABsvar.fit[,j]))
cat("\n\nSVAR forecast change path :");print(data.frame(res=na.omit(div(getp("svarchannel",eta)$ABsvar.fit[,j],lag=2))))
####progress###########################
total=5
pb=tcltk::tkProgressBar(title="rate of progress",min=0,max=total,width=300)
for(i in 1:total){Sys.sleep(.1);tcltk::setTkProgressBar(pb,i,label=paste(round(i/total*100,0),"% done"))}
for(i in 1:total){print(i);Sys.sleep(.1)}
 }
for(eta in 16){
for(im1 in 1:k2){
  # grDevices::png(paste("H:/svar.code/result/Fig.", getp("ynames.",eta)[im1], "fanchart with target levels.png"),width = 660,height=428,units="px",pointsize="16")
  try(.fanchart(predict(getp("svarchannel",eta)$ABSVAR$var,n.ahead=36),
                indiff = FALSE,
                base.year=ifelse(eta==2,NULL,c(transmission2[[getp("ynames.",eta)[im1]]][135-12])),
                plot.type="single",main=paste("Віяловий графік прогнозу", getp("NAMES.",eta,".U.3")[im1], "в Україні")[],
                nd.low = 7, nd.upp=7,
                nv1=im1, cis=c(seq(.125,.875,length=8)),n.grid=NULL,names=getp("ynames.",eta)[im1] ,colors=2))
  legend(.062,.42,c(pst(c(pst(seq(90,10,by=-10),"%")), " рівень ймовірності"), "інтервали таргету"), fill = c(gray(sqrt(seq(.1,.9,length=9))),"white"), border= c(rep("black",9),"white"), pch=c(rep(15,9),NA), bty=c(rep("o",9),"n"), bg = "white",lty=c(rep(NA,9),2),col=c(rep(NA,9),"red"),cex=.5)
  # dev.off()
}}
#model table#####
ggdata<-NULL
for(j1 in 1:k2){
  ggdata[[j1]] <- transmission2[153-which(as.Date(transmission2$date)==as.Date("2008-11-01"),arr.ind=TRUE),getp("ynames.",eta)[im1]][[1]]*exp(predict(getp("svarchannel",eta)$ABSVAR$var, n.ahead = 36, ci = seq(.1,.9,by=.1)[j1])$fcst[[getp("ynames.",eta)[im1]]])
}
im2="cpi.a"
ggdata<-data.frame(cbind(ggdata[[1]],ggdata[[2]],ggdata[[3]],ggdata[[4]],ggdata[[5]],ggdata[[6]],ggdata[[7]],ggdata[[8]],ggdata[[9]]))
ggplot2::ggplot(data=ggdata,aes(y=ggdata[,(1+4*1)],x=seq(1,36)),color="#999999",type="line")+geom_ribbon(aes(ymin=ggdata[,seq(2,34,by=4)[1]],ymax = ggdata[,seq(3,35,by=4)[1]],fill=scale_alpha("#E69F00",range=seq(.1,.9,by=.1)[1])))
#+legend(,c("прогноз",pst(c(pst(seq(90,10,by=-10),"%")), " рівень ймовірності")),lty = c(1,rep(NA,9)), col=c("#999999",rep(NA,9)),fill = c(NA,("#E69F00",alpha=seq(.1,.9,by=.1))),border= c("white",rep("black",9)))

#plotting forecast of REER by models 5, 6, 8#####
ggplot2::ggplot(data=REER.forecast,aes(x=Date,y=model_5),ylab="differece from central tendency",colour="#999999")+ggplot2::labs(ylab="differece from central tendency, in decimals")+ggplot2::geom_line(aes(y=model_6), colour="#56D4E9")+ggplot2::geom_line(aes(y=model_8),colour="#E69FO")+ggplot2::theme(legend.position="bottom")
#for targeting interval#####
svarchannel1.fan.chart<-cbind(exp(transmission$trade.balance)[(length(na.omit(transmission$trade.balance))-36+1):length(na.omit(transmission$trade.balance))],exp(transmission$NX)[(length(na.omit(transmission$NX))-36+1):length(na.omit(transmission$NX))],dataset1$REER[23]*exp(cbind(predict(getp("svarchannel",eta)$ABSVAR$var,n.ahead=36,ci=seq(.85,.95,by=.05)[1])$fcst[["REER"]],predict(getp("svarchannel",eta)$ABSVAR$var,n.ahead=36,ci=seq(.85,.95,by=.05)[2])$fcst[["REER"]],predict(getp("svarchannel",eta)$ABSVAR$var,n.ahead=36,ci=seq(.85,.95,by=.05)[3])$fcst[["REER"]])),exp(cbind(predict(getp("svarchannel",eta)$ABSVAR$var,n.ahead=36,ci=seq(.85,.95,by=.05)[1])$fcst[["REER"]],predict(getp("svarchannel",eta)$ABSVAR$var,n.ahead=36,ci=seq(.85,.95,by=.05)[2])$fcst[["REER"]],predict(getp("svarchannel",eta)$ABSVAR$var,n.ahead=36,ci=seq(.85,.95,by=.05)[3])$fcst[["REER"]])));colnames(svarchannel.2.1.fan.chart)<-c("trade balance","NX",colnames(svarchannel.2.1.fan.chart)[3:14],pst(colnames(svarchannel.2.1.fan.chart)[3:14]," in difference"))
1.1899030*exp(data.frame(cbind(predict(getp("svarchannel",eta)$ABSVAR$var,ci=.87)$fcst[[j]])))
1.1899030*exp(data.frame(cbind(predict(getp("svarchannel",eta)$ABSVAR$var,ci=.90)$fcst[[j]])))

##forecast for models 0, 1, 5, 6, 8, 9, 16####
eta=6;eta=i;j=1;eta.1=2;if(eta.1==2){base.year=NULL}else{base.year=c(transmission2[[getp("ynames.",i)[j]]][135-12])}
cat("\n\nSVAR forecast change path for REER in model", eta, " : ",(div(getp("svarchannel.2.",eta)$ABsvar.fit[,j],lag=2)))
print(paste("SVAR forecast predictive interval in model", eta, " : "));target.interval.7<-cbind(.fanchart(predict(getp("varchannel.2.",eta),n.ahead=36),indiff = TRUE,base.year=base.year,plot.type="single",main=paste("Віяловий графік прогнозу", getp("NAMES.",i,".U.3"), "в Україні"),
                                                                              nv1=j,n.grid=NULL,cis=c(.18,.82),names=0,nd.low=0,nd.upp = NULL,colors=1)$fcst[[1]][[1]],exp(transmission$trade.balance)[(length(na.omit(transmission$trade.balance))-36+1):length(na.omit(transmission$trade.balance))],exp(transmission$NX)[(length(na.omit(transmission$NX))-36+1):length(na.omit(transmission$NX))])
colnames(target.interval.7)<-c("fcst",  "lower", "upper", "CI","trade.balance.sheet","NX");target.interval.7
target.interval.7.1<-cbind(exp(predict(getp("varchannel.2.",eta), ci = .87,n.ahead=36)$fcst[[j]])*base.year,exp(transmission$trade.balance)[(length(na.omit(transmission$trade.balance))-36+1):length(na.omit(transmission$trade.balance))],exp(transmission$NX)[(length(na.omit(transmission$NX))-36+1):length(na.omit(transmission$NX))])
colnames(target.interval.7.1)<-c(paste("fcst of trade\n balance sheet"),  "lower", "upper", "CI","trade.balance.sheet","NX");target.interval.7.1
##lag=24##svarchannel 2#####
for(j in 2:getp(c("sample.model","sample.model.2")[1],eta)$K){
cat("\n",getp("ynames.",eta)[j],"Monthly change",div(getp("svarchannel",eta)$svar.irf.AB24[,1:3+(j-1)*3][1:10,2],lag = 2)[]) #-1.2544766,-1.603399
cat("\n\nQuarterly change",div(getp("svarchannel",eta)$svar.irf.AB24[,1:3+(j-1)*3][1:10,2],lag = 4)[c(1,4,6)]) #-1.2544766,-1.603399
cat("\n\n9 month change",div(getp("svarchannel",eta)$svar.irf.AB24[,1:3+(j-1)*3][1:10,2],lag = 9)[1])
cat("\n\nMean change:",mean(diff(getp("svarchannel",eta)$svar.irf.AB24[,1:3+(j-1)*3][1:10,3],lag = 1)[2:9] ))}
instruments.irf<-cbind(exp(svarchannel.2.16$svar.irf.AB.exch.pos.loss.risk.m[,7]),
exp(svarchannel16$svar.irf.AB.pi.e[,7]),
exp(svarchannel16$svar.irf.AB.Discount.rate[,7]),
exp(svarchannel16$svar.irf.AB.Direct.investment..net[,7]),
exp(svarchannel16$svar.irf.AB.NX[,7]),
exp(svarchannel16$svar.irf.AB.Monetary.policy.shock[,7]),
exp(svarchannel16$svar.irf.AB.net.intervention[,7]));colnames(instruments.irf)<-c("exch.pos.loss.risk.m","pi.e","Discount.rate","Direct.investment..net","NX","Monetary.policy.shock","net.intervention")
instruments.fevd<-svarchannel16.fevd.tab[,seq(7,(k2-1)*k2+7,length.out=k2)]
###plot 3.10#####
plot.irf.3(eta=16,name.eta="Discount.rate",model = 16,main="інструментів регуляції волатильності\n",v=0,w=1,mu=1:3,plot.kind="with area",se=TRUE,svar=TRUE)

eta=15;if(eta==0){eta.1<-"NBU"} else eta.1<-eta
j=2;k2=getp(c("sample.model","sample.model.2")[1],eta.1)$K
print(getp("ynames.",eta)[j]);show(matrix(c(format(seq(as.Date("2015-04-01"),by="months",length.out=35),"%Y-%m-%d"),as.array(tail(exp(getp("svarchannel.2.",eta)$ABsvar.fit[,j]),35))),ncol=2));show(tail(exp(getp("svarchannel.2.",eta)$ABsvar.fit[,j]),12)[9]+exp(getp("svarchannel.2.",eta)$svar.irf.AB[,1:3+(j-1)*3])[12,2])
par(mai=c(1,1,.7,0.2),font.lab=3,cex.axis=.8,cex.lab=.8);plot(ts(tail(exp(getp("svarchannel.2.",eta)$ABsvar.fit[,j]),35),start=c(2015,4),frequency=12),type="l",ylab=paste(getp("NAMES.",eta,".U")[j+1]));polygon(c(ts(tail(exp(getp("svarchannel.2.",eta)$ABsvar.fit[,j]),35),start=c(2015,4),frequency=12),rev(rep(0,35))),ylab=paste(getp("NAMES.",eta,".U")[j+1]),col=heat.colors(.5,.1))+grid(col=gray(.5,.6))

#diff in channel 1 and 5#####
eta=5;if(eta==0){eta.1<-"NBU"} else eta.1<-eta
j=1;k2=getp(c("sample.model","sample.model.2")[1],eta.1)$K
print(getp("ynames.",eta)[j]);show(diff.5.to.1<-matrix(c(ts(tail(exp(getp("svarchannel.2.",eta)$ABsvar.fit[,j]),35)-tail(exp(getp("svarchannel.2.",1)$ABsvar.fit[,8]),35))),ncol=1))
#diff in channel 1,6####
eta=6;if(eta==0){eta.1<-"NBU"} else eta.1<-eta
j=1;k2=getp(c("sample.model","sample.model.2")[1],eta.1)$K
print(getp("ynames.",eta)[j]);show(diff.6.to.1<-matrix(c(ts(tail(exp(getp("svarchannel.2.",eta)$ABsvar.fit[,j]),35)-tail(exp(getp("svarchannel.2.",1)$ABsvar.fit[,8]),35))),ncol=1))
#diff in channel 1,8####
eta=8;if(eta==0){eta.1<-"NBU"} else eta.1<-eta
j=6;k2=getp(c("sample.model","sample.model.2")[1],eta.1)$K
print(getp("ynames.",eta)[j]);show(diff.8.to.1<-matrix(c(ts(tail(exp(getp("svarchannel.2.",eta)$ABsvar.fit[,j]),35)-tail(exp(getp("svarchannel.2.",1)$ABsvar.fit[,8]),35))),ncol=1))
#forecast###############
varchannel.4.forecast<-cbind(predict(getp("varchannel",4),n.ahead=36)$fcst);varchannel.4.forecast<-data.frame(varchannel.4.forecast$cpi.a)
WriteXLS::WriteXLS(varchannel.4.forecast,"varchannel.4.forecast of cpi.xlsx")
varchannel.2.forecast<-predict(getp("varchannel",2),n.ahead=36)$fcst;varchannel.2.forecast<-data.frame(varchannel.2.forecast$cpi.a)
varchannel.6.forecast<-predict(getp("varchannel",6),n.ahead=36)$fcst;varchannel.6.forecast<-data.frame(varchannel.6.forecast$ccpi)
varchannel.7.forecast<-predict(getp("varchannel",7),n.ahead=36)$fcst;varchannel.7.forecast<-data.frame(varchannel.7.forecast$cpi.a)
varchannel.9.forecast<-predict(getp("varchannel",9),n.ahead=36)$fcst;varchannel.9.forecast<-data.frame(varchannel.9.forecast$pi.e)

###REER.FORECAST#####
REER.forecast<-data.frame(cbind(diff.5.to.1,diff.6.to.1,diff.8.to.1))
colnames(REER.forecast)<-c("model 5", "model 6", "model 8")
View(REER.forecast); par(mfrow=c(1,1),mai=c(1,.9,0.1,.4),font.lab=3,cex.lab=.8,cex.axis=.6);plot(REER.forecast[,1],type="l",ylab="in p.p")+grid()+lines(REER.forecast[,2],col="blue")+lines(REER.forecast[,3],col="tomato1")+legend("bottomright",legend=colnames(REER.forecast),col = c("black","blue","tomato1"),lty=1,cex =.6)+legend("topright",c("model 5 - impact of pi.e","model 6 - impact of NX","model 8 - impact of a financial capital spillover"))+mtext(side = 1,outer = TRUE,text="SVAR")
#fevd for model 1 and 6######
for(j in 1:sample.model.21$K){eta=1
  assign(paste("svar.fevd.model.1.",ynames.1[j],sep=""),cbind(getp("svarchannel.2.",eta,".fevd.tab")[,seq(j,(k2-1)*k2+j,length.out=k2)],getp("svarchannel.2.",eta,".fevd.tab")[,1:k2+(j-1)*(k2)]))
  assign(paste("svec.fevd.model.1.",ynames.1[j],sep=""),cbind(getp("svec.channel",eta,".fevd.tab")[,1:k2+(j-1)*(k2)],getp("svec.channel",eta,".fevd.tab")[,seq(j,(k2-1)*k2+j,length.out=k2)]))}
  if(!j==2){
    assign("svar.fevd.model.1",cbind(getp("svar.fevd.model.1.",ynames.1[1]),getp("svar.fevd.model.1.",ynames.1[j])))
    assign("svec.fevd.model.1",cbind(getp("svec.fevd.model.1.",ynames.1[1]),getp("svec.fevd.model.1.",ynames.1[j])))}

for(j in 1:sample.model.26$K){
assign(paste("svar.fevd.model.6.",ynames.6[j],sep=""),cbind(getp("svarchannel.2.",eta,".fevd.tab")[,seq(j,(k2-1)*k2+j,length.out=k2)],getp("svarchannel.2.",eta,".fevd.tab")[,1:k2+(j-1)*(k2)]))
assign(paste("svec.fevd.model.6.",ynames.6[j],sep=""),cbind(getp("svec.channel",eta,".fevd.tab")[,1:k2+(j-1)*(k2)],getp("svec.channel",eta,".fevd.tab")[,seq(j,(k2-1)*k2+j,length.out=k2)]))}
if(!j==2){
  for(j in 3:sample.model.26$K){
assign("svar.fevd.model.6",cbind(getp("svar.fevd.model.6.",ynames.6[1]),getp("svar.fevd.model.6.",ynames.6[j])))
assign("svec.fevd.model.6",cbind(getp("svec.fevd.model.6.",ynames.6[1]),getp("svec.fevd.model.6.",ynames.6[j])))}}
{show(getp("svarchannel.2.",eta,".fevd.tab")[,seq(j,(k2-1)*k2+j,length.out=k2)])#influence each factor to j
show(getp("svarchannel.2.",eta,".fevd.tab")[,1:k2+(j-1)*(k2)])#influence of j on each factor
show(getp("svec.channel",eta,".fevd.tab")[,1:k2+(j-1)*(k2)])#influence of j on each factor
show(getp("svec.channel",eta,".fevd.tab")[,seq(j,(k2-1)*k2+j,length.out=k2)])}#influence each factor to j

WriteXLS::WriteXLS(data.frame(svar.fevd.model.1),"svar.fevd.model.1.xlsx")
WriteXLS::WriteXLS(data.frame(svec.fevd.model.1),"svec.fevd.model.1.xlsx")
WriteXLS::WriteXLS(data.frame(svar.fevd.model.6),"svar.fevd.model.6.xlsx")
WriteXLS::WriteXLS(data.frame(svec.fevd.model.6),"svec.fevd.model.6.xlsx")

##REER irf#########
# REER.exch.loss.risk<-irf(svec.channel5$svec$ABSvec,impulse="exch.pos.loss.risk.m",n.ahead=11);REER.exch.pos.loss.risk$irf$exch.pos.loss.risk.m[,"REER"]
REER.exch.norm<-irf(svarchannel5$ABSVAR,impulse="exch.pos.loss.risk.m",n.ahead=11);REER.exch.norm$irf$exch.pos.loss.risk.m[,"REER"]
exch.norm.REER<-irf(svarchannel15$ABSVAR,impulse="exch.pos.loss.risk.m",n.ahead=11);exch.norm.REER$irf$exch.pos.loss.risk.m[,"REER"]
REER.disc<-svarchannel1$svar.irf.ABSVAR$irf$Discount.rate[,"REER"]
REER.pi.e<-irf(svarchannel6$ABSVAR,impulse="pi.e",n.ahead=11);REER.pi.e$irf$pi.e[,"REER"]
REER.im.inf<-irf(svarchannel6$ABSVAR,impulse="imported.inflation",n.ahead=11);REER.im.inf$irf$imported.inflation[,"REER"]
 REER.administered.inf<-irf(svarchannel6$ABSVAR,impulse="administered.prices",n.ahead=11);REER.administered.inf$irf$administered.prices[,"REER"]
 REER.ppi<-irf(svarchannel6$ABSVAR,impulse="ppi.a",n.ahead=11);REER.ppi$irf$ppi.a[,"REER"]
REER.ccpi<-irf(svarchannel6$ABSVAR,impulse="ccpi",n.ahead=11);REER.ccpi$irf$ccpi[,"REER"]
REER.rate<-irf(svarchannel6$ABSVAR,impulse="rule.rate",n.ahead=11);REER.rate$irf$rule.rate[,"REER"]
REER.NX<-irf(svarchannel6$ABSVAR,impulse="NX",n.ahead=11);REER.NX$irf$NX[,"REER"]
REER.DI<-irf(svec.channel8$svec$ABSvec,impulse="Direct.investment..net",n.ahead=11);REER.DI$irf$Direct.investment..net[,"REER"]
REER.Real.GDP.Monetary.policy<-irf(svec.channel7$svec$ABSvec,impulse="Monetary.policy.shock",n.ahead=11);REER.Real.GDP.Monetary.policy$irf$Monetary.policy.shock[,"REER"]
REER.fine.tuning.operations<-irf(svarchannel15.extension$ABSVAR,impulse="svarchannel3.ABsvar.fit....bond.yields.h..",n.ahead=11);REER.fine.tuning.operations$irf$svarchannel3.ABsvar.fit....bond.yields.h..[,"REER"]
REER.RESPONCE<-cbind(exch.norm.REER$irf$exch.pos.loss.risk.m[,"REER"],REER.exch.norm$irf$exch.pos.loss.risk.m[,"REER"],REER.disc,REER.pi.e$irf$pi.e[,"REER"],REER.im.inf$irf$imported.inflation[,"REER"],REER.administered.inf$irf$administered.prices[,"REER"],REER.ppi$irf$ppi.a[,"REER"],REER.ccpi$irf$ccpi[,"REER"],REER.rate$irf$rule.rate[,"REER"],REER.NX$irf$NX[,"REER"],REER.DI$irf$Direct.investment..net[,"REER"],REER.Real.GDP.Monetary.policy$irf$Monetary.policy.shock[,"Real.GDP"],REER.fine.tuning.operations$irf$svarchannel3.ABsvar.fit....bond.yields.h..[,2])
colnames(REER.RESPONCE)<-c("Open foreign.exchange.position","Open foreign.exchange.position.2","Discount.rate","pi.e","imported inflation","administered prices","ppi","ccpi","rule rate","Net export","Direct investment net","Monetary shock","fine.tuning.operations")
##coefficients#########
coefficients(getp("svarchannel.2.",5)$ABSVAR$var$varresult$REER)[pst(getp("ynames.","5"),".l1")][c(2-1,8-1)]
coefficients(getp("svarchannel.2.",15)$ABSVAR$var$varresult$REER)[pst(getp("ynames.","15"),".l1")][c(2-1,3-1)]
coefficients(getp("svarchannel.2.",1)$ABSVAR$var$varresult$REER)[pst(getp("ynames.","1"),".l1")][c(1,8)]
coefficients(getp("svarchannel.2.",6)$ABSVAR$var$varresult$REER)[pst(getp("ynames.","6"),".l1")][c(1:2,4:8)]
coefficients(getp("svarchannel.2.",8)$ABSVAR$var$varresult$REER)[pst(getp("ynames.","8"),".l1")][c(3)]
coefficients(getp("svarchannel.2.",7)$ABSVAR$var$varresult$Real.GDP)[pst(getp("ynames.","7"),".l1")][c(8)]*coefficients(svarchannel.2.11$ABSVAR$var$varresult$svarchannel.2.1.ABsvar.fit.REER)[1]*coefficients(svarchannel.2.2$ABSVAR$var$varresult$npl)[3]
for(u in 2)show((getp("svec.channel",8)$svec$ABsvec.coef)[seq(c(3,6)[u],c(3,6)[u]+4*length(getp("ynames.","8")),length(getp("ynames.","8"))),c(3,6)[1]])
for(u in 1:2)show((getp("svec.channel",7)$svec$ABsvec.coef)[seq(c(1:2,6)[u],c(1:2,6)[u]+4*length(getp("ynames.","7")),length(getp("ynames.","7"))),c(1:2,6)[3]])
coefficients(svarchannel.2.11$ABSVAR$var$varresult$svarchannel.2.1.ABsvar.fit.REER)[1]*coefficients(svarchannel.2.2$ABSVAR$var$varresult$npl)[3]*for(u in 1)show((getp("svec.channel",7)$svec$ABsvec.coef)[seq(c(1:2,6)[u],c(1:2,6)[u]+4*length(getp("ynames.","7")),length(getp("ynames.","7")))[],c(6)])

#HOW REER INFLUENCE ON CAPITAL SPILLOVER####
coefficients(getp("svarchannel.2.",5)$ABSVAR$var$varresult$exch.pos.loss.risk.m)[pst(getp("ynames.","5"),".l1")][c(2-1,8-1)]
coefficients(getp("svarchannel.2.",15)$ABSVAR$var$varresult$exch.pos.loss.risk.m)[pst(getp("ynames.","15"),".l1")][c(2-1,3-1)]
coefficients(getp("svarchannel.2.",1)$ABSVAR$var$varresult$Discount.rate)[pst(getp("ynames.","1"),".l1")][c(1,8)]
coefficients(getp("svarchannel.2.",6)$ABSVAR$var$varresult$pi.e)[pst(getp("ynames.","6"),".l1")][c(1:2,4:8)]
coefficients(getp("svarchannel.2.",8)$ABSVAR$var$varresult$Direct.investment..net)[pst(getp("ynames.","8"),".l1")][c(6)]
coefficients(getp("svarchannel.2.",7)$ABSVAR$var$varresult$Monetary.policy.shock)[pst(getp("ynames.","7"),".l1")][c(1)]*coefficients(svarchannel.2.11$ABSVAR$var$varresult$npl)[1]*coefficients(svarchannel.2.2$ABSVAR$var$varresult$Real.GDP)[1]
for(u in 1)show((getp("svec.channel",8)$svec$ABsvec.coef)[seq(c(3,6)[u],c(3,6)[u]+4*length(getp("ynames.","8")),length(getp("ynames.","8"))),c(3,6)])
for(u in 3)show((getp("svec.channel",7)$svec$ABsvec.coef)[seq(c(1:2,6)[u],c(1:2,6)[u]+4*length(getp("ynames.","7")),length(getp("ynames.","7"))),c(1:2)])
coefficients(svarchannel.2.11$ABSVAR$var$varresult$npl)[2]*coefficients(svarchannel.2.2$ABSVAR$var$varresult$Real.GDP)[1]*for(u in 3)show((getp("svec.channel",7)$svec$ABsvec.coef)[seq(c(1:2,6)[u],c(1:2,6)[u]+4*length(getp("ynames.","7")),length(getp("ynames.","7")))[],c(1)])
####fevd model 16###########################
j=1;k2<-svarchannel.2.16$ABSVAR$var$K
svarchannel.2.16.fevd.tab[,seq(j,k2^2,k2)]
svarchannel.2.16.fevd.tab[,1:k2+(j-1)*k2]

#query by impulses#####
s=4
j=3;shock=1
k3=getp("svarchannel.2.",eta)$ABSVAR$var$K
name=getp("ynames.",s)[2];{l=which(getp("ynames.",s)==name,arr.ind=TRUE)+c(0:(k3-1))[which(getp("ynames.",s)==name,arr.ind=TRUE)];show(getp("ynames.",s)[which(getp("ynames.",s)==name,arr.ind=TRUE)])}
if(svar==TRUE){print(getp("svarchannel",s,".fevd.tab")[,seq(j,(k3)*k3,k3)][shock])}else{show(getp("svec.channel",s,".fevd.tab")[,seq(j,(k3)*k3,k3)][shock])}

#Discoubt rate shock or first impulse in SVEC
if(svar==TRUE){print(getp("svarchannel",s,".fevd.tab")[,seq(1,(k3)*k3,k3)])}else{print(getp("svec.channel",s,".fevd.tab")[,seq(1,(k3)*k3,k3)])}

#plot######
{teta=eta+1;name.eta=c("cpi.a",ynames.6[1])[2];z=which(getp("ynames.",eta)==name.eta,arr.ind=TRUE)-1;w=2;se=TRUE;mu=c(1:3)[2];K=getp("sample.model",model)$K-2
mar.=c(.25,1.5,5,.3);mai.=1*c(.22,.72,
                                    .2666667,.06);oma.=.2*c(.21/0.2,.9,4.2/0.2,.8);cex.m=.7;font.m.=4
model=c("NBU",1:10)[teta];if(model=="NBU"){if(is.integer(getp("sample.model",model)$K%%2)){par(mfrow=c((round((getp("sample.model",model)$K-K)/2)+0),2),
                                                                                    mar=mar.,mai=mai.,oma=oma.,cex=cex.m,font.main=4)}else{
                                                                                     par(mfrow=c(round((getp("sample.model",model)$K-1)/2),2),
                                                                                         mar=mar.,oma=oma.,mai=mai.,cex=cex.m,font.main=font.m.)};for(j in 2:getp("sample.model",model)$K){plot(exp(getp(c("svarchannel","svarchannel.2.")[1],0)$svar.irf.AB[,1:3+(j-1)*3])[,2],type="l",ylab=getp("NAMES.",0,".U")[j+1],ylim=range(exp(getp(c("svarchannel","svarchannel.2.")[1],model)$svar.irf.AB[,1:3+(j-1)*3])[mu]),xlab="місяці")+title(main=paste("Реакція змінних каналу", 0+1,"на шок",getp("NAMES.",0,".U")[1]),outer=TRUE)+grid(col=gray(.3,.8))
                                                                                           if(se==TRUE){
                                                                                             lines(exp(getp(c("svarchannel","svarchannel.2.")[1],model)$svar.irf.AB[,1:3+(j-1)*3])[,1],lty=2,col=gray(.47,.9))
                                                                                             lines(exp(getp(c("svarchannel","svarchannel.2.")[1],model)$svar.irf.AB[,1:3+(j-1)*3])[,3],lty=2,col=gray(.57,.9))
                                                                                           }}}else{
                                         par(mfrow=c(round((getp("sample.model",model)$K-K)/2),2/w),
                                             mar=mar.,oma=oma.,mai=mai.,cex=cex.m,font.main=font.m.);for(j in c(2:getp("sample.model",model)$K)[z]){plot((getp(c("svarchannel","svarchannel.2.")[1],model)$svar.irf.AB[,1:3+(j-1)*3])[,2],type="l",ylab=getp("NAMES.",model,".U")[j+1],ylim=range(exp(getp(c("svarchannel","svarchannel.2.")[1],model)$svar.irf.AB[,1:3+(j-1)*3][mu])),xlab="місяці")+title(main=paste("Реакція змінних каналу", readr::parse_number(model)+1,"на шок",getp("NAMES.",0,".U")[1]),outer=TRUE)+grid(col=gray(.3,.8))
                                                if(se==TRUE){
                                                lines((getp(c("svarchannel","svarchannel.2.")[1],model)$svar.irf.AB[,1:3+(j-1)*3])[,1],lty=2,col=gray(.47,.9))
                                                lines((getp(c("svarchannel","svarchannel.2.")[1],model)$svar.irf.AB[,1:3+(j-1)*3])[,3],lty=2,col=gray(.57,.9))}
                                             }}
}
setwd(paste(getwd(),"/svarchannel",0.2,"/irf.plot",sep=""));grDevices::savePlot(paste("plot",c(0:9)[teta],"32.AB model.png",sep="."),type="png")
#ylim=(range(exp(getp(c("svarchannel","svarchannel.2.")[1],model)$svar.irf.AB[,1:3+(j-1)*3])*1.1)[1:2]),xlim=c(1,12)
{eta;name.eta="cpi.a";z=which(getp("ynames.",eta)==name.eta,arr.ind=TRUE);v=-getp(c("sample.model","sample.model.2")[1],model)$K+2;w=2;
  svar=TRUE
teta=eta+1;model=c("NBU",1:10)[teta];if(model=="NBU"){ylim=(range(exp(getp(c("svarchannel","svarchannel.2.")[1],0)$svar.irf.AB[,]))*2.1)[1:2]}else{
ylim=(range(exp(getp(c("svarchannel",c("svarchannel","svarchannel.test.sample.","svarchannel.2.")[1],"svarchannel.2.")[1],model)$svar.irf.AB[,]))*2.1);xlim=c(1,12)}
if(model=="NBU"){if(is.integer(getp(c("sample.model","sample.model.2")[1],model)$K%%2)){par(mfrow=c((round(getp(c("sample.model","sample.model.2")[1],model)$K+v)/2),2/w),
                                                                       mar=mar.,mai=mai.,oma=oma.,cex=cex.m,font.main=font.m.)}else{
                                                                         par(mfrow=c((round(getp(c("sample.model","sample.model.2")[1],model)$K+v)/2+0),2/w),
                                                                             mar=mar.,mai=mai.,oma=oma.,cex=cex.m,font.main=font.m.)};for(j in 2:getp(c("sample.model","sample.model.2")[1],model)$K){plot(exp(getp(c("svarchannel","svarchannel.2.")[1],0)$svar.irf.AB[,1:3+(j-1)*3])[,2],ylim=range(exp(getp(c("svarchannel","svarchannel.2.")[1],0)$svar.irf.AB[,1:3+(j-1)*3])),type="l",ylab=NAMES.0.U[j+1],xlab="місяці")+title(main=paste("Реакція змінних каналу", 1,"на шок",getp("NAMES.",0,".U")[1]),outer=TRUE)+grid(col=gray(.3,.8))
                                                                               polygon(c(1:12,rev(1:12)),c(exp(getp(c("svarchannel","svarchannel.2.")[1],0)$svar.irf.AB[,1:3+(j-1)*3])[,1],rev(exp(getp(c("svarchannel","svarchannel.2.")[1],0)$svar.irf.AB[,1:3+(j-1)*3])[,2])),lty=2,col=heat.colors(1,.5),border="red",ylim=ylim)
                                                                               polygon(c(1:12,rev(1:12)),c(exp(getp(c("svarchannel","svarchannel.2.")[1],0)$svar.irf.AB[,1:3+(j-1)*3])[,2],rev(exp(getp(c("svarchannel","svarchannel.2.")[1],0)$svar.irf.AB[,1:3+(j-1)*3])[,3])),lty=2,col=heat.colors(3,.5),border="red",ylim=ylim)}}else{
if(is.integer(getp(c("sample.model","sample.model.2")[1],model)$K%%2)){par(mfrow=c((round(getp(c("sample.model","sample.model.2")[1],model)$K+v)/2),2/w),
                                                              mar=mar.,mai=mai.,oma=oma.,cex=cex.m,font.main=font.m.)}else{
                                                                par(mfrow=c((round(getp(c("sample.model","sample.model.2")[1],model)$K+v)/2),2/w),
                                                                    mar=mar.,mai=mai.,oma=oma.,cex=cex.m,font.main=font.m.)};for(j in c(1:getp(c("sample.model","sample.model.2")[1],model)$K)[z]){plot(exp(getp("svec.channel",model)$svec$svec.irf.AB[,1:3+(j-1)*3])[,2],type="l",lwd=2,col="black",ylim=range(exp(getp("svec.channel",model)$svec$svec.irf.AB[,1:3+(j-1)*3])),ylab=getp("NAMES.",(teta-1),".U")[j+1],xlab="місяці")+title(main=paste("Реакція змінних каналу", readr::parse_number(model)+1,"на шок",getp("NAMES.",(teta-1),".U")[1]),outer=TRUE)+grid(col=gray(.3,.8))
                                                                      polygon(c(1:12,rev(1:12)),c(exp(getp("svec.channel",model)$svec$svec.irf.AB[,1:3+(j-1)*3])[,1],rev(exp(getp("svec.channel",model)$svec$svec.irf.AB[,1:3+(j-1)*3])[,2])),lty=2,col=heat.colors(1,.5),border=NULL,ylim=ylim)
                                                                      polygon(c(1:12,rev(1:12)),c(exp(getp("svec.channel",model)$svec$svec.irf.AB[,1:3+(j-1)*3])[,2],rev(exp(getp("svec.channel",model)$svec$svec.irf.AB[,1:3+(j-1)*3])[,3])),lty=2,col=heat.colors(3,.5),border=NULL,ylim=ylim)}}
# setwd(paste("H:/svar.code/result","/svarchannel",0.2,"/irf.plot",sep=""));grDevices::savePlot(paste("plot",c(0:9)[teta],"32.AB model.png",sep="."),type="png")####
}else{
  # "svec.channel.test.sample."#####
  if(is.integer(getp(c("sample.model","sample.model.2")[1],model)$K%%2==0)){par(mfrow=c((round(getp(c("sample.model",c("sample.model","sample.model.2")[1])[1],model)$K+v)/2),2/w),
                                                        mar=mar.,mai=mai.,oma=oma.,cex=cex.m,font.main=font.m.)}else{
                                                          par(mfrow=c((round(getp(c("sample.model","sample.model.2")[1],model)$K+v)/2+0),2/w),
                                                              mar=mar.,mai=mai.,oma=oma.,cex=cex.m,font.main=font.m.)};for(j in 2:getp(c("sample.model","sample.model.2")[1],model)$K){plot(exp(getp(c("svarchannel","svarchannel.test.sample.","svarchannel.2.")[1],1)$svar.irf.AB[,1:3+(j-1)*3])[,2],ylim=range(exp(getp(c("svarchannel","svarchannel.test.sample.","svarchannel.2.")[1],1)$svar.irf.AB[,1:3+(j-1)*3])),type="l",ylab=NAMES.0.U[j+1],xlab="місяці")+title(main=paste("Реакція змінних каналу", 1,"на шок",getp("NAMES.",0,".U")[1]),outer=TRUE)+grid(col=gray(.3,.8))
                                                                polygon(c(1:12,rev(1:12)),c(exp(getp(c("svarchannel","svarchannel.test.sample.","svarchannel.2.")[1],1)$svar.irf.AB[,1:3+(j-1)*3])[,1],rev(exp(getp(c("svarchannel","svarchannel.test.sample.","svarchannel.2.")[1],1)$svar.irf.AB[,1:3+(j-1)*3])[,2])),lty=2,col=heat.colors(1,.5),border="red",ylim=ylim)
                                                                polygon(c(1:12,rev(1:12)),c(exp(getp(c("svarchannel","svarchannel.test.sample.","svarchannel.2.")[1],1)$svar.irf.AB[,1:3+(j-1)*3])[,2],rev(exp(getp(c("svarchannel","svarchannel.test.sample.","svarchannel.2.")[1],1)$svar.irf.AB[,1:3+(j-1)*3])[,3])),lty=2,col=heat.colors(3,.5),border="red",ylim=ylim)}}else{
                                                                  if(is.integer(getp(c("sample.model","sample.model.2")[1],model)$K%%2==1)){par(mfrow=c((round(getp(c("sample.model","sample.model.2")[1],model)$K+v)/2),2/w),
                                                                                                                        mar=mar.,mai=mai.,oma=oma.,cex=cex.m,font.main=font.m.)}else{
                                                                                                                          par(mfrow=c((round(getp(c("sample.model","sample.model.2")[1],model)$K+v)/2),2/w),
                                                                                                                              mar=mar.,mai=mai.,oma=oma.,cex=cex.m,font.main=font.m.)};for(j in c(1:getp(c("sample.model","sample.model.2")[1],model)$K)[z]){plot(exp(getp("svec.channel.test.sample.",model)$svec$svec.irf.AB[,1:3+(j-1)*3])[,2],type="l",lwd=2,col="black",ylim=range(exp(getp("svec.channel.test.sample.",model)$svec$svec.irf.AB[,1:3+(j-1)*3])),ylab=getp("NAMES.",(teta-1),".U")[j+1],xlab="місяці")+title(main=paste("Реакція змінних каналу", readr::parse_number(model)+1,"на шок",getp("NAMES.",(teta-1),".U")[1]),outer=TRUE)+grid(col=gray(.3,.8))
                                                                                                                                polygon(c(1:12,rev(1:12)),c(exp(getp("svec.channel.test.sample.",model)$svec$svec.irf.AB[,1:3+(j-1)*3])[,1],rev(exp(getp("svec.channel.test.sample.",model)$svec$svec.irf.AB[,1:3+(j-1)*3])[,2])),lty=2,col=heat.colors(1,.5),border=NULL,ylim=ylim)
                                                                                                                                polygon(c(1:12,rev(1:12)),c(exp(getp("svec.channel.test.sample.",model)$svec$svec.irf.AB[,1:3+(j-1)*3])[,2],rev(exp(getp("svec.channel.test.sample.",model)$svec$svec.irf.AB[,1:3+(j-1)*3])[,3])),lty=2,col=heat.colors(3,.5),border=NULL,ylim=ylim)}}

foreach(j=2:k2,.combine="list")%do%({plot(tail(t(exp(svarchannel1$ABsvar.fit[1,])),12),type="l",ylab="");lines(tail(t(exp(svarchannel1$ABsvar.fit[j,])),12),lty=j)+grid()+title(ylab="значення")+legend("topright",c(NAMES.1.U)[1:j],lty=c(1,2:j))})
par(mfrow=c(3,1),mar=c(.05,rep(0.88,2),.76),mai=rep(0.25,4),cex=.5);layout(matrix(c(1,1,2),4-1,1,byrow=TRUE))
m=2;k2=getp("sample.model",m)$K
if(k2==9){
plot(tail(t(exp(getp(getp("svarchannel",m))$ABsvar.fit[1,])),12),type="l",ylab="",ylim=c(min(tail(t(exp(getp(getp("svarchannel",m))$ABsvar.fit)),12)),max(tail(t(exp(getp(getp("svarchannel",m))$ABsvar.fit)),12))),lty=1);lines(tail(t(exp(getp(getp("svarchannel",m))$ABsvar.fit[2,])),12),lty=2)+grid()+title(ylab="значення");lines(tail(t(exp(getp(getp("svarchannel",m))$ABsvar.fit[3,])),12),lty=3)+grid()+title(ylab="значення");lines(tail(t(exp(getp(getp("svarchannel",m))$ABsvar.fit[4,])),12),lty=4)+grid()+title(ylab="значення");lines(tail(t(exp(getp(getp("svarchannel",m))$ABsvar.fit[5,])),12),lty=5)+grid()+title(ylab="значення");lines(tail(t(exp(getp(getp("svarchannel",m))$ABsvar.fit[6,])),12),lty=6);lines(tail(t(exp(getp(getp("svarchannel",m))$ABsvar.fit[7,])),12),lty=7);lines(tail(t(exp(getp(getp("svarchannel",m))$ABsvar.fit[8,])),12),lty=8,pch=1);lines(tail(t(exp(getp(getp("svarchannel",m))$ABsvar.fit[k2,])),12),lty=k2,pch=2)+grid()+title(ylab="значення")+plot.new()+legend("topright",c(NAMES.1.U.2)[2:k2],lty=c(1,2:k2),pch=c(rep(NA,7),1,2))
}else{if(k2==8){plot(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[2,])),12),type="l",ylab="",ylim=c(min(tail(t(exp(getp("svarchannel",m)$ABsvar.fit)),12)),max(tail(t(exp(getp("svarchannel",m)$ABsvar.fit)),12))),lty=1);lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[2,])),12),lty=2)+grid()+title(ylab="значення");lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[3,])),12),lty=3)+grid()+title(ylab="значення");lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[4,])),12),lty=4)+grid()+title(ylab="значення");lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[5,])),12),lty=5)+grid()+title(ylab="значення");lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[6,])),12),lty=6);lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[7,])),12),lty=7);lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[k2,])),12),lty=k2,pch=2)+grid()+title(ylab="значення")+plot.new()+legend("topright",c(getp("NAMES.",m,".U.2"))[2:k2],lty=c(1,2:k2),pch=c(rep(0,7),1))}else{
      if(k2==7){plot(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[1,])),12),type="l",ylab="",ylim=c(min(tail(t(exp(getp("svarchannel",m)$ABsvar.fit)[,2:8]),12)),max(tail(t(exp(getp("svarchannel",m)$ABsvar.fit))[,2:8],12))),lty=1);lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[2,])),12),lty=2)+grid()+title(ylab="значення");lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[3,])),12),lty=3)+grid()+title(ylab="значення");lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[4,])),12),lty=4)+grid()+title(ylab="значення");lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[5,])),12),lty=5)+grid()+title(ylab="значення");lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[6,])),12),lty=6);lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[k2,])),12),lty=k2,pch=2)+grid()+title(ylab="значення")+plot.new()+legend("topright",c(getp("NAMES.",m,".U.2"))[2:k2],lty=c(1,2:k2))}else{
        plot(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[1,])),12),type="l",ylab="",ylim=c(min(tail(t(exp(getp("svarchannel",m)$ABsvar.fit)),12)),max(tail(t(exp(getp("svarchannel",m)$ABsvar.fit)),12))),lty=1);lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[2,])),12),lty=2)+grid()+title(ylab="значення");lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[3,])),12),lty=3)+grid()+title(ylab="значення");lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[4,])),12),lty=4)+grid()+title(ylab="значення");lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[5,])),12),lty=5)+grid()+title(ylab="значення");lines(tail(t(exp(getp("svarchannel",m)$ABsvar.fit[k2,])),12),lty=k2,pch=2)+grid()+title(ylab="значення")+plot.new()+legend("topright",c(getp("NAMES.",m,".U.2"))[2:k2],lty=c(1,2:k2))
      }}}
#table#######
k2<-sample.model1$K
table.1<-data.frame(foreach::foreach(i=c(2:6,8),.combine=rbind)%do%(svarchannel1.fevd.tab[,seq(i,i+(k2-1)*k2,k2)[-7]]))
table.1<-tibble::add_column(table.1,c(rep(ynames.1[2],2),rep(ynames.1[3],2),rep(ynames.1[4],2),rep(ynames.1[5],2),rep(ynames.1[6],2),#rep(ynames.1[7],2),
                                      rep(ynames.1[8],2)),.before = 1)#,rep(ynames.1[9],2))
colnames(table.1)<-c("responce",ynames.1[c(1:6,8)])
k2<-sample.modelNBU$K;table.0<-data.frame(foreach::foreach(i=c(2:8),.combine=rbind)%do%(svarchannel0.fevd.tab[,seq(i,i+(k2-1)*k2,k2)[c(1:(k2))]]))
table.0<-tibble::add_column(table.0,c(rep(ynames.0[2],2),rep(ynames.0[3],2),rep(ynames.0[4],2),rep(ynames.0[5],2),rep(ynames.0[6],2),rep(ynames.0[7],2),
                                      rep(ynames.0[8],2)),.before = 1)
colnames(table.0)<-c("responce",ynames.0[c(1:9)])
k2<-sample.model2$K;table.2<-data.frame(foreach::foreach(i=c(2:k2),.combine=rbind)%do%(svarchannel2.fevd.tab[,seq(i,i+(k2-1)*k2,(k2))]))
k2<-sample.model3$K;table.3<-data.frame(foreach::foreach(i=c(2:k2),.combine=rbind)%do%(svarchannel3.fevd.tab[,seq(i,i+(k2-1)*k2,(k2))]))
k2<-sample.model4$K;table.4<-data.frame(foreach::foreach(i=c(2:k2),.combine=rbind)%do%(svarchannel4.fevd.tab[,seq(i,i+(k2-1)*k2,(k2))]))
k2<-sample.model5$K;table.5<-data.frame(foreach::foreach(i=c(2:k2),.combine=rbind)%do%(svarchannel5.fevd.tab[,seq(i,i+(k2-1)*k2,(k2))]))
k2<-sample.model6$K;table.6<-data.frame(foreach::foreach(i=c(2:k2),.combine=rbind)%do%(svarchannel6.fevd.tab[,seq(i,i+(k2-1)*k2,(k2))]))
k2<-sample.model7$K;table.7<-data.frame(foreach::foreach(i=c(2:k2),.combine=rbind)%do%(svarchannel7.fevd.tab[,seq(i,i+(k2-1)*k2,(k2))]))
k2<-sample.model8$K;table.8<-data.frame(foreach::foreach(i=c(2:k2),.combine=rbind)%do%(svarchannel8.fevd.tab[,seq(i,i+(k2-1)*k2,(k2))]))
k2<-sample.model9$K;table.9<-data.frame(foreach::foreach(i=c(2:k2),.combine=rbind)%do%(svarchannel9.fevd.tab[,seq(i,i+(k2-1)*k2,(k2))]))
k2<-sample.model10$K;table.10<-data.frame(foreach::foreach(i=c(2:k2),.combine=rbind)%do%(svarchannel.2.10.fevd.tab[,seq(i,i+(k2-1)*k2,(k2))]))

table.2<-tibble::add_column(table.2,c(rep(ynames.2[2],2),rep(ynames.2[3],2),rep(ynames.2[4],2),rep(ynames.2[5],2),rep(ynames.2[6],2),rep(ynames.2[7],2),rep(ynames.2[8],2)),.before = 1)
table.3<-tibble::add_column(table.3,c(rep(ynames.3[2],2),rep(ynames.3[3],2),rep(ynames.3[4],2),rep(ynames.3[5],2),rep(ynames.3[6],2),rep(ynames.3[7],2),rep(ynames.3[8],2)),.before = 1)
table.4<-tibble::add_column(table.4,c(rep(ynames.4[2],2),rep(ynames.4[3],2),rep(ynames.4[4],2),rep(ynames.4[5],2),rep(ynames.4[6],2),rep(ynames.4[7],2)),.before = 1)
table.5<-tibble::add_column(table.5,c(rep(ynames.5[2],2),rep(ynames.5[3],2),rep(ynames.5[4],2),rep(ynames.5[5],2),rep(ynames.5[6],2),rep(ynames.5[7],2),rep(ynames.5[8],2)),.before = 1)
table.6<-tibble::add_column(table.6,c(rep(ynames.6[2],2),rep(ynames.6[3],2),rep(ynames.6[4],2),rep(ynames.6[5],2),rep(ynames.6[6],2),rep(ynames.6[7],2),rep(ynames.6[8],2),rep(ynames.6[9],2)),.before = 1)
table.7<-tibble::add_column(table.7,c(rep(ynames.7[2],2),rep(ynames.7[3],2),rep(ynames.7[4],2),rep(ynames.7[5],2),rep(ynames.7[6],2),rep(ynames.7[7],2),rep(ynames.7[8],2)),.before = 1)
table.8<-tibble::add_column(table.8,c(rep(ynames.8[2],2),rep(ynames.8[3],2),rep(ynames.8[4],2),rep(ynames.8[5],2),rep(ynames.8[6],2)),.before = 1)
table.9<-tibble::add_column(table.9,c(rep(ynames.9[2],2),rep(ynames.9[9],2),rep(ynames.9[4],2),rep(ynames.9[5],2),rep(ynames.9[6],2),rep(ynames.9[7],2),rep(ynames.9[8],2),rep(ynames.9[9],2)),.before = 1)
table.10<-tibble::add_column(table.10,c(rep(ynames.10[2],2),rep(ynames.10[3],2),rep(ynames.10[4],2),rep(ynames.10[5],2),rep(ynames.10[6],2)),.before = 1)

colnames(table.2)<-c("name",ynames.2[c(1:8)])
colnames(table.3)<-c("name",ynames.3[c(1:8)])
colnames(table.4)<-c("name",ynames.4[c(1:7)])
colnames(table.5)<-c("name",ynames.5[c(1:8)])
colnames(table.6)<-c("name",ynames.6[c(1:9)])
colnames(table.7)<-c("name",ynames.7[c(1:8)])
colnames(table.8)<-c("name",ynames.8[c(1:6)])
colnames(table.9)<-c("name",ynames.9[c(1:9)])
colnames(table.9)<-c("name",ynames.9[c(1:9)])
colnames(table.10)<-c("name",ynames.10[c(1:6)])

for(eta1 in 10)
WriteXLS::WriteXLS(paste0("table.",eta1,collapse=""),paste("Table.2. FEVD for", eta1,"channels.xlsx"),FreezeCol = 1,FreezeRow = 1)
sample.model0<-sample.modelNBU
#table.2####
k2<-sample.model3$K;table.3<-data.frame(foreach::foreach(i=c(2:k2),.combine=rbind)%do%(svarchannel.2.3.fevd.tab[,seq(i,(k2)*k2,(k2))]))

##plot.1######
no.r<-par(mai=c(rep(.33,2),0.35,.33),oma=c(1,1,1,1));par(no.r,mfrow=c(6,1));layout(matrix(c(rep(1,5),2),nrow=6,ncol=1,byrow=TRUE))
plot.1<-plot(vec.channel.2.1.fitted[,8],type="l",col="tomato",lwd=1.4)+title(main="Прогноз реального ефективного валютного курсу\nз використанням моделі монетарного трансмісійного механізму - канал 1",cex=.4)+lines(t(svarchannel.2.1$ABsvar.fit)[(132-127):132,8],col="steelblue4",lwd=1.4)+plot.new()+legend("bottom",c("vec forecast","svar forecast"),lwd=1,col=c("red","blue"))
plot.2<-plot(vec.channel.2.8.fitted[,6],type="l",col="tomato",lwd=1.4)+title(main="Прогноз реального ефективного валютного\nкурсу з використанням моделі монетарного трансмісійного механізму - канал 8",cex=.4)+lines(t(svarchannel.2.8$ABsvar.fit)[(131-126):131,6],col="steelblue4",lwd=1.4)+plot.new()+legend("bottom",c("vec forecast","svar forecast"),lwd=1,col=c("red","blue"))
screen.split(c(2,1),2)
###plot.1.new############
no.r<-par(mai=c(.00,.33,0.35,.33),oma=c(.999,1,1,1));par(no.r,mfrow=c(6,1));layout(matrix(c(rep(1,5),2),nrow=6,ncol=1,byrow=TRUE))
plot.1<-plot(vec.channel.1.fitted[,8],type="l",col="black",lwd=1)+title(main="Прогноз реального ефективного валютного курсу\nз використанням моделі монетарного трансмісійного механізму - канал 1",ylab=c("у логарифмі"),cex=.4)+grid()+lines(svarchannel1.fitted[4:133,8],col="gray40",lty=2,lwd=1.4)+plot.new()+legend("bottom",c("vec forecast","svar forecast"),lwd=1,col=c("black","gray40"),lty=c(1,2))
plot.2<-plot(vec.channel.8.fitted[,6],type="l",col="black",lwd=1,ylab=c("у логарифмі"))+title(main="Прогноз реального ефективного валютного\nкурсу з використанням моделі монетарного трансмісійного механізму - канал 8",cex=.4)+grid()+lines( svarchannel8.fitted[5:134,6],col="gray40",lty=2,lwd=1.4)+plot.new()+legend("bottom",c("vec forecast","svar forecast"),lwd=1,col=c("black","gray40"),lty=c(1,2))
screen.split(c(2,1),2)
no.r<-par(mai=c(.00,.33,0.35,.33),oma=c(.999,1,1,1));par(no.r,mfrow=c(6,1));layout(matrix(c(rep(1,5),2),nrow=6,ncol=1,byrow=TRUE))
plot.1<-plot(vec.channel.2.1.fitted[,8],type="l",col="black",lwd=1)+title(main="Прогноз реального ефективного валютного курсу\nз використанням моделі монетарного трансмісійного механізму - канал 1",ylab=c("у логарифмі"),cex=.4)+grid()+lines(t(svarchannel.2.1$ABsvar.fit)[(132-127):132,8],col="gray40",lty=2,lwd=1.4)+plot.new()+legend("bottom",c("vec forecast","svar forecast"),lwd=1,col=c("black","gray40"),lty=c(1,2))
plot.2<-plot(vec.channel.2.8.fitted[,6],type="l",col="black",lwd=1,ylab=c("у логарифмі"))+title(main="Прогноз реального ефективного валютного\nкурсу з використанням моделі монетарного трансмісійного механізму - канал 8",cex=.4)+grid()+lines(t(svarchannel.2.8$ABsvar.fit)[(131-126):131,6],col="gray40",lty=2,lwd=1.4)+plot.new()+legend("bottom",c("vec forecast","svar forecast"),lwd=1,col=c("black","gray40"),lty=c(1,2))
screen.split(c(2,1),2)


#colnames####
{t=0;s=0;k=getp("sample.model",t)$K
if (k==9){
column<-foreach(i = 1:9)%do%(paste(c(getp("ynames.",t))[i],"shock"))
colnames(table.0)<-c(column[[1]],column[[2]],column[[3]],column[[4]],column[[5]],column[[6]],column[[7]],column[[8]],column[[9]])
}else{if(k==8){
  column<-foreach(i = 1:8)%do%(paste(c(getp("ynames.",t))[i],"shock"))
  colnames(table.0)<-c(column[[1]],column[[2]],column[[3]],column[[4]],column[[5]],column[[6]],column[[7]],column[[8]])
}else{if(k==7){
  column<-foreach(i = 1:7)%do%(paste(c(getp("ynames.",t))[i],"shock"))
  colnames(table.0)<-c(column[[1]],column[[2]],column[[3]],column[[4]],column[[5]],column[[6]],column[[7]])
}else{
  column<-foreach(i = 1:6)%do%(paste(c(getp("ynames.",t))[i],"shock"))
  colnames(table.0)<-c(column[[1]],column[[2]],column[[3]],column[[4]],column[[5]],column[[6]])
}}}
WriteXLS::WriteXLS(table.0,paste("Table.2. FEVD for", s,"channels.xlsx"),FreezeCol = 1,FreezeRow = 1)}
#var######
 i="NBU"
getp("table.var",i)[1:2,]
Causality.NBU
Causality.1

 #svar#####
obs<-varchannel1$obs
r <- -(K * obs/2) * log(2 * pi) + obs/2 * log(det(A)^2) -  obs/2 * log(det(B)^2) - obs/2 * sum(diag(t(A) %*%  solve(t(B)) %*% solve(B) %*% A %*% Sigma))

identify.0$LRTEST
identify.1$LRTEST
identify.2$LRTEST
identify.3$LRTEST
identify.4$LRTEST
identify.5$LRTEST
identify.6$LRTEST
identify.7$LRTEST
identify.8$LRTEST
identify.9$LRTEST
identify.10$LRTEST

#svar1#fevd#irf#########
library(foreach)
for(i in 0){
  assign(pst("svarchannel",i,".irf.tab"),foreach(j=1:(3*getp("svarchannel",i)$ABSVAR$var$K),.combine=cbind)%do%(div(getp("svarchannel",i)$svar.irf.AB[,j])))}
#+
for(i in c(0:12,14:16,"15.extension")[7]){
assign(pst("svarchannel",i,".irf.tab"),foreach(j=1:(3*getp("svarchannel",i)$ABSVAR$var$K),.combine=cbind)%do%(div(getp("svarchannel",i)$svar.irf.AB[2:12,j])))}
for(i in c(0:12,14:16,"15.extension")[7]){getp("svarchannel",i,".irf.tab")->scirftab;scirftab<-data.frame(scirftab);colnames(scirftab)<-colnames(getp("svarchannel",i)$svar.irf.AB);assign(pst("svarchannel",i,".irf.tab"),scirftab)}
for(i in c(0:12,14:16,"15.extension")[7]){i=i;View(getp("svarchannel",i,".irf.tab"))}
for(i in c(0:12,14:16,"15.extension")[7]){
  for(K in 1:getp("svarchannel",i)$ABSVAR$var$K){
  assign(pst("svarchannel",i,".fevd.tab"),foreach(j=1:(K*getp("svarchannel",i)$ABSVAR$var$K),.combine=cbind)%do%(rbind(max(getp("svarchannel",i)$svar.fevd.AB[,j]),
                                                               which(getp("svarchannel",i)$svar.fevd.AB[,j]==max(getp("svarchannel",i)$svar.fevd.AB[,j]),arr.ind=TRUE))))}}
for(i in c(0:12,14:16,"15.extension")[7]){getp("svarchannel",i,".fevd.tab")->scfevdtab;scfevdtab<-data.frame(scfevdtab);colnames(scfevdtab)<-colnames(getp("svarchannel",i)$svar.fevd.AB);assign(pst("svarchannel",i,".fevd.tab"),scfevdtab)}
for(i in c(0:12,14:16,"15.extension")[7]){show(tail(getp("svarchannel",i,".fevd.tab")))}
#svarchannel.2.i or svarchannel
for(i in c(1:12,14:16, "15.extension")[6]){
  assign(pst("svarchannel.",i,".fitted.tab"),foreach(v = 1:length(getp("ynames.",readr::parse_number(i))),.combine=cbind)%do%(div(getp("svarchannel",i)$ABsvar.fit[,v])))}
for(i in c(1:12,14:16, "15.extension")[6]){getp("svarchannel.",i,".fitted.tab")->scfittab;scfittab<-data.frame(scfittab);colnames(scfittab)<-names(getp("svarchannel",i)$ABSVAR$var$varresult);assign(pst("svarchannel.",i,".fitted.tab"),value=scfittab);print(tail(scfittab))}
for(i in c(1:12,14:16, "15.extension")[6]){
  i;show(tail(getp("svarchannel.",i,".fitted.tab")))}

#--Forecast-----------------------------------------------------------------------------------------------------------
#svar2####
u=6
for(i in c(10:12,14:16)[u]){
    assign(pst("svarchannel.2.",i,".fitted.tab"),foreach(v = 1:length(getp("ynames.",i)),.combine=cbind)%do%(div(getp("svarchannel.2.",i)$ABsvar.fit[,v],2)[1:(length(getp("svarchannel.2.",i)$ABsvar.fit[,v])-2)]))}
for(i in c(10:12,14:16)[u]){getp("svarchannel.2.",i,".fitted.tab")->scfittab;colnames(scfittab)<-names(getp("svarchannel.2.",i)$ABSVAR$var$varresult);assign(pst("svarchannel.2.",i,".fitted.tab"),value=scfittab);print(tail(scfittab))}
for(i in c(10:12,14:16)[u]){
  i;show(tail(getp("svarchannel.2.",i,".fitted.tab")))}

###done####
for(i in ".2.0"){
  assign(pst("svarchannel",i,".irf.tab"),foreach(j=1:(3*getp("svarchannel",i)$ABSVAR$var$K),.combine=cbind)%do%(div(getp("svarchannel",i)$svar.irf.AB[,j])))}
for(i in c("2.0",paste(seq(2.1,2.9,by=.1)))){getp("svarchannel.",i,".irf.tab")->scirftab;colnames(scirftab)<-colnames(getp("svarchannel.",i)$svar.irf.AB);assign(pst("svarchannel.",i,".irf.tab"),scirftab)}
for(i in c("2.0",paste(seq(2.1,2.9,by=.1)))){i=i;View(getp("svarchannel.",i,".irf.tab"))}
for(i in c("2.0",paste(seq(2.1,2.9,by=.1)))){
  for(K in 1:getp("svarchannel.",i)$ABSVAR$var$K){
    assign(pst("svarchannel.",i,".fevd.tab"),foreach(j=1:(K*getp("svarchannel.",i)$ABSVAR$var$K),.combine=cbind)%do%(rbind(max(getp("svarchannel.",i)$svar.fevd.AB[,j]),which(getp("svarchannel.",i)$svar.fevd.AB[,j]==max(getp("svarchannel.",i)$svar.fevd.AB[,j]),arr.ind=TRUE))))}}
for(i in c("2.0",paste(seq(2.1,2.9,by=.1)))){getp("svarchannel.",i,".fevd.tab")->scfevdtab;colnames(scfevdtab)<-colnames(getp("svarchannel.",i)$svar.fevd.AB);assign(pst("svarchannel.",i,".fevd.tab"),scfevdtab)}
u=16
c(".2.0",paste(to.each(seq(2.0,2.9,by=.1),"."),sep=""),".2.10",".2.11",".2.12",".2.14",".2.15",".2.16")[u]
for(i in c(".2.0",paste(to.each(seq(2.0,2.9,by=.1),"."),sep=""),".2.10",".2.11",".2.12",".2.14",".2.15",".2.16")[u]){
  for(K in 1:getp("svarchannel",i)$ABSVAR$var$K){
    assign(pst("svarchannel",i,".fevd.tab"),foreach(j=1:(K*getp("svarchannel",i)$ABSVAR$var$K),.combine=cbind)%do%(rbind(max(getp("svarchannel",i)$svar.fevd.AB[,j]),which(getp("svarchannel",i)$svar.fevd.AB[,j]==max(getp("svarchannel",i)$svar.fevd.AB[,j]),arr.ind=TRUE))))}}
for(i in c(".2.0",paste(to.each(seq(2.0,2.9,by=.1),"."),sep=""),".2.10",".2.11",".2.12",".2.14",".2.15",".2.16")[u]){getp("svarchannel",i,".fevd.tab")->scfevdtab;colnames(scfevdtab)<-colnames(getp("svarchannel",i)$svar.fevd.AB);assign(pst("svarchannel",i,".fevd.tab"),scfevdtab)}

u=6
for(i in c("2.10","2.11","2.12","2.14","2.15","2.16")[u]){
  assign(pst("svarchannel.",i,".irf.tab"),foreach(j=1:(3*getp("svarchannel.",i)$ABSVAR$var$K),.combine=cbind)%do%((div(exp(getp("svarchannel.",i)$svar.irf.AB[,j]),lag=2))[2:11-1]))}
for(i in c("2.10","2.11","2.12","2.14","2.15","2.16")[u]){getp("svarchannel.",i,".irf.tab")->scirftab;colnames(scirftab)<-colnames(getp("svarchannel.",i)$svar.irf.AB);assign(pst("svarchannel.",i,".irf.tab"),scirftab)}
for(i in c("2.10","2.11","2.12","2.14","2.15","2.16")[u]){i=i;View(getp("svarchannel.",i,".irf.tab"))}
for(i in c("2.10","2.11","2.12","2.14","2.15","2.16")[u]){
  for(K in 1:getp("svarchannel.",i)$ABSVAR$var$K){
    assign(pst("svarchannel.",i,".fevd.tab"),foreach(j=1:(K*getp("svarchannel.",i)$ABSVAR$var$K),.combine=cbind)%do%(rbind(max(getp("svarchannel.",i)$svar.fevd.AB[,j]),which(getp("svarchannel.",i)$svar.fevd.AB[,j]==max(getp("svarchannel.",i)$svar.fevd.AB[,j]),arr.ind=TRUE))))}}
for(i in c("2.10","2.11","2.12","2.14","2.15","2.16")[u]){getp("svarchannel.",i,".fevd.tab")->scfevdtab;colnames(scfevdtab)<-colnames(getp("svarchannel.",i)$svar.fevd.AB);assign(pst("svarchannel.",i,".fevd.tab"),scfevdtab)}

for(i in c(".2.10")){
  for(K in 1:getp("svarchannel",i)$ABSVAR$var$K){
    assign(pst("svarchannel",i,".fevd.tab"),foreach(j=1:(K*getp("svarchannel",i)$ABSVAR$var$K),.combine=cbind)%do%(rbind(max(getp("svarchannel",i)$svar.fevd.AB[,j],2),which(getp("svarchannel",i)$svar.fevd.AB[,j]==max(getp("svarchannel",i)$svar.fevd.AB[,j]),arr.ind=TRUE))))}}
for(i in c(".2.10")){getp("svarchannel",i,".fevd.tab")->scfevdtab;colnames(scfevdtab)<-colnames(getp("svarchannel",i)$svar.fevd.AB);assign(pst("svarchannel",i,".fevd.tab"),scfevdtab)}

K<-svarchannel0$ABSVAR$var$K

###EFFECTS########################
if(exists("svarchannel.2.0")){
for (i in c(0:12,14:16) ){
show(getp("svarchannel.2.",i)$ABsvar.ef[,1:(getp("svarchannel.2.",i)$ABSVAR$var$K+1)])}
}else{
  for (i in c(0:12,14:16) ){
    show(getp("svarchannel",i)$ABsvar.ef[,1:getp("svarchannel",i)$ABSVAR$var$K+1])}
}
#svar.coef analysis########
svarchannel0$ABsvar.coef
svarchannel1$ABsvar.coef
svarchannel2$ABsvar.coef
svarchannel3$ABsvar.coef
svarchannel4$ABsvar.coef
svarchannel5$ABsvar.coef
svarchannel6$ABsvar.coef
svarchannel7$ABsvar.coef
svarchannel8$ABsvar.coef
svarchannel9$ABsvar.coef
svarchannel10$ABsvar.coef
svarchannel11$ABsvar.coef
svarchannel12$ABsvar.coef
svarchannel14$ABsvar.coef
svarchannel15$ABsvar.coef
svarchannel16$ABsvar.coef

# svar.2.2 analysis
svarchannel.2.0$ABsvar.coef
svarchannel.2.1$ABsvar.coef
svarchannel.2.2$ABsvar.coef
svarchannel.2.3$ABsvar.coef
svarchannel.2.4$ABsvar.coef
svarchannel.2.5$ABsvar.coef
svarchannel.2.6$ABsvar.coef
svarchannel.2.7$ABsvar.coef
svarchannel.2.8$ABsvar.coef
svarchannel.2.9$ABsvar.coef
svarchannel.2.10$ABsvar.coef
svarchannel.2.11$ABsvar.coef
svarchannel.2.12$ABsvar.coef
svarchannel.2.14$ABsvar.coef
svarchannel.2.15$ABsvar.coef
svarchannel.2.16$ABsvar.coef


row.names(svarchannel3$ABsvar.coef)<-colnames(svarchannel3$ABsvar.coef)[1:sample.model3$K]
row.names(svarchannel4$ABsvar.coef)<-colnames(svarchannel4$ABsvar.coef)[1:sample.model4$K]
row.names(svarchannel5$ABsvar.coef)<-colnames(svarchannel5$ABsvar.coef)[1:sample.model5$K]
row.names(svarchannel6$ABsvar.coef)<-colnames(svarchannel6$ABsvar.coef)[1:sample.model6$K]
row.names(svarchannel7$ABsvar.coef)<-colnames(svarchannel7$ABsvar.coef)[1:sample.model7$K]
row.names(svarchannel8$ABsvar.coef)<-colnames(svarchannel8$ABsvar.coef)[1:sample.model8$K]
row.names(svarchannel9$ABsvar.coef)<-colnames(svarchannel9$ABsvar.coef)[1:sample.model9$K]
row.names(svarchannel10$ABsvar.coef)<-colnames(svarchannel10$ABsvar.coef)[1:sample.model10$K]
row.names(svarchannel11$ABsvar.coef)<-colnames(svarchannel11$ABsvar.coef)[1:sample.model11$K]
row.names(svarchannel12$ABsvar.coef)<-colnames(svarchannel12$ABsvar.coef)[1:sample.model12$K]
row.names(svarchannel14$ABsvar.coef)<-colnames(svarchannel14$ABsvar.coef)[1:sample.model14$K]
row.names(svarchannel15$ABsvar.coef)<-colnames(svarchannel15$ABsvar.coef)[1:sample.model15$K]
row.names(svarchannel16$ABsvar.coef)<-colnames(svarchannel16$ABsvar.coef)[1:sample.model16$K]


svarchannel.2.10.fevd.tab[,1:9];svarchannel.2.10.fevd.tab[,2*1:9];for(i in 3:K){show(svarchannel.2.10.fevd.tab[,i*1:9])}

row.names(svarchannel.2.0$ABsvar.coef)<-colnames(svarchannel.2.0$ABsvar.coef)[1:sample.modelNBU$K]
row.names(svarchannel.2.1$ABsvar.coef)<-colnames(svarchannel.2.1$ABsvar.coef)[1:sample.model1$K]
row.names(svarchannel.2.2$ABsvar.coef)<-colnames(svarchannel.2.2$ABsvar.coef)[1:sample.model2$K]
row.names(svarchannel.2.3$ABsvar.coef)<-colnames(svarchannel.2.3$ABsvar.coef)[1:sample.model3$K]
row.names(svarchannel.2.4$ABsvar.coef)<-colnames(svarchannel.2.4$ABsvar.coef)[1:sample.model4$K]
row.names(svarchannel.2.5$ABsvar.coef)<-colnames(svarchannel.2.5$ABsvar.coef)[1:sample.model5$K]
row.names(svarchannel.2.6$ABsvar.coef)<-colnames(svarchannel.2.6$ABsvar.coef)[1:sample.model6$K]
row.names(svarchannel.2.7$ABsvar.coef)<-colnames(svarchannel.2.7$ABsvar.coef)[1:sample.model7$K]
row.names(svarchannel.2.8$ABsvar.coef)<-colnames(svarchannel.2.8$ABsvar.coef)[1:sample.model8$K]
row.names(svarchannel.2.9$ABsvar.coef)<-colnames(svarchannel.2.9$ABsvar.coef)[1:sample.model9$K]
row.names(svarchannel.2.10$ABsvar.coef)<-colnames(svarchannel.2.10$ABsvar.coef)[1:sample.model10$K]
row.names(svarchannel.2.11$ABsvar.coef)<-colnames(svarchannel.2.11$ABsvar.coef)[1:sample.model11$K]
row.names(svarchannel.2.12$ABsvar.coef)<-colnames(svarchannel.2.12$ABsvar.coef)[1:sample.model12$K]
row.names(svarchannel.2.14$ABsvar.coef)<-colnames(svarchannel.2.14$ABsvar.coef)[1:sample.model14$K]
row.names(svarchannel.2.1$ABsvar.coef)<-colnames(svarchannel.2.12$ABsvar.coef)[1:sample.model12$K]
row.names(svarchannel.2.16$ABsvar.coef)<-colnames(svarchannel.2.14$ABsvar.coef)[1:sample.model14$K]


#vec#####
for(i in c(0:12,14:16)[16]){
show(getp("vec.channel.2.",i))
print(c("johansen test.2.",i));show(getp("johansen.test.2.",i))
print(c("cannonical correlation.2."),i);show(getp("canonical.corr.vec.2.",i))
show(getp("vec.channel.2.",i,".tab"))
show(getp("vec.channel.2.",i,".var"))
show(getp("vec.channel.2.",i,".coef.A"))
show(getp("vec.channel.2.",i,".beta")[seq(7+2,7*5,7),])
show(getp("vec.channel.2.",i,".coef")[seq(7+2,7*5,7),])}
for(i in c(0,1:4,6:12,14)){
  show(getp("vec.channel.2.",i,".irf"));print(paste(i,"loop."))
  resume<-try(show(getp("vec.channel.2.",i,".irf")))
  if(inherits(resume,"try-error")){
    next
  }

}
for(i in c(0,1,3,9:12,14)){
  show(getp("vec.channel.2.",i,".fevd"));print(paste(i,"loop.", "Channel",c("NBU",1:9)[i+1]))
  resume<-try(show(getp("vec.channel.2.",i,".fevd")))
  if(inherits(resume,"try-error")){
    next
  }

}

#svec####
svec.channel0$svec$ABsvec.coef
svec.channel1$svec$ABsvec.coef
svec.channel2$svec$ABsvec.coef
svec.channel3$svec$ABsvec.coef
svec.channel4$svec$ABsvec.coef
svec.channel5$svec$ABsvec.coef
svec.channel6$svec$ABsvec.coef
svec.channel7$svec$ABsvec.coef
svec.channel8$svec$ABsvec.coef
svec.channel9$svec$ABsvec.coef
svec.channel10$svec$ABsvec.coef#!
svec.channel11$svec$ABsvec.coef#!
svec.channel12$svec$ABsvec.coef#!
svec.channel14$svec$ABsvec.coef#!
for(i in c(1:6,8)){
n.lag=1;print(ynames.16[i]);print((identify.svec.16$VEC.rls)[c(3,1:8+(n.lag-1)*8+3),i])}

identify.svec.0<-identify.svec(svec.channel0$svec$ABSvec$var,svec.channel0$svec$ABSvec$LRorig,svec.channel0$svec$ABSvec$SRorig)
identify.svec.1<-identify.svec(svec.channel1$svec$ABSvec$var,svec.channel1$svec$ABSvec$LRorig,svec.channel1$svec$ABSvec$SRorig)
identify.svec.2<-identify.svec(svec.channel2$svec$ABSvec$var,svec.channel2$svec$ABSvec$LRorig,svec.channel2$svec$ABSvec$SRorig)
identify.svec.3<-identify.svec(svec.channel3$svec$ABSvec$var,svec.channel3$svec$ABSvec$LRorig,svec.channel3$svec$ABSvec$SRorig)
identify.svec.4<-identify.svec(svec.channel4$svec$ABSvec$var,svec.channel4$svec$ABSvec$LRorig,svec.channel4$svec$ABSvec$SRorig)
identify.svec.5<-identify.svec(svec.channel5$svec$ABSvec$var,svec.channel5$svec$ABSvec$LRorig,svec.channel5$svec$ABSvec$SRorig)
identify.svec.6<-identify.svec(svec.channel6$svec$ABSvec$var,svec.channel6$svec$ABSvec$LRorig,svec.channel6$svec$ABSvec$SRorig)
identify.svec.7<-identify.svec(svec.channel7$svec$ABSvec$var,svec.channel7$svec$ABSvec$LRorig,svec.channel7$svec$ABSvec$SRorig)
identify.svec.8<-identify.svec(svec.channel8$svec$ABSvec$var,svec.channel8$svec$ABSvec$LRorig,svec.channel8$svec$ABSvec$SRorig)
identify.svec.9<-identify.svec(svec.channel9$svec$ABSvec$var,svec.channel9$svec$ABSvec$LRorig,svec.channel9$svec$ABSvec$SRorig)
identify.svec.10<-identify.svec(svec.channel10$svec$ABSvec$var,svec.channel10$svec$ABSvec$LRorig,svec.channel10$svec$ABSvec$SRorig)
identify.svec.11<-identify.svec(svec.channel11$svec$ABSvec$var,svec.channel11$svec$ABSvec$LRorig,svec.channel11$svec$ABSvec$SRorig,r=1)
identify.svec.12<-identify.svec(svec.channel12$svec$ABSvec$var,svec.channel12$svec$ABSvec$LRorig,svec.channel12$svec$ABSvec$SRorig,r=1)
identify.svec.14<-identify.svec(svec.channel14$svec$ABSvec$var,svec.channel14$svec$ABSvec$LRorig,svec.channel14$svec$ABSvec$SRorig,r=1)
identify.svec.15<-identify.svec(svec.channel15$svec$ABSvec$var,svec.channel15$svec$ABSvec$LRorig,svec.channel15$svec$ABSvec$SRorig,r=1)
identify.svec.16<-identify.svec(svec.channel16$svec$ABSvec$var,svec.channel16$svec$ABSvec$LRorig,svec.channel16$svec$ABSvec$SRorig)

identify.svec.20<-identify.svec(svec.channel0$svec$ABSvec$var,svec.channel0$svec$ABSvec$LRorig,svec.channel0$svec$ABSvec$SRorig)
identify.svec.21<-identify.svec(svec.channel1$svec$ABSvec$var,svec.channel1$svec$ABSvec$LRorig,svec.channel1$svec$ABSvec$SRorig)
identify.svec.22<-identify.svec(svec.channel2$svec$ABSvec$var,svec.channel2$svec$ABSvec$LRorig,svec.channel2$svec$ABSvec$SRorig)
identify.svec.23<-identify.svec(svec.channel3$svec$ABSvec$var,svec.channel3$svec$ABSvec$LRorig,svec.channel3$svec$ABSvec$SRorig)
identify.svec.24<-identify.svec(svec.channel4$svec$ABSvec$var,svec.channel4$svec$ABSvec$LRorig,svec.channel4$svec$ABSvec$SRorig)
identify.svec.25<-identify.svec(svec.channel5$svec$ABSvec$var,svec.channel5$svec$ABSvec$LRorig,svec.channel5$svec$ABSvec$SRorig)
identify.svec.26<-identify.svec(svec.channel6$svec$ABSvec$var,svec.channel6$svec$ABSvec$LRorig,svec.channel6$svec$ABSvec$SRorig)
identify.svec.27<-identify.svec(svec.channel7$svec$ABSvec$var,svec.channel7$svec$ABSvec$LRorig,svec.channel7$svec$ABSvec$SRorig)
identify.svec.28<-identify.svec(svec.channel8$svec$ABSvec$var,svec.channel8$svec$ABSvec$LRorig,svec.channel8$svec$ABSvec$SRorig)
identify.svec.29<-identify.svec(svec.channel9$svec$ABSvec$var,svec.channel9$svec$ABSvec$LRorig,svec.channel9$svec$ABSvec$SRorig)

#svecfit###########
c(10:12,14);0:10
for(i in c(10:12,14:16)[5:6]){
  assign(pst("svecchannel",i,".fitted.tab"),foreach(j=1:ncol(getp("svec.channel",i)$svec$ABSvec$LRorig),.combine=cbind)%do%(div(getp("svec.channel",i)$svec$ABsvec.fit[,j],1)[2:length(getp("svec.channel",i)$svec$ABsvec.fit[,j])-1]))}
for(i in c(10:12,14:16)[5:6]){getp("svecchannel",i,".fitted.tab")->scfittab;colnames(scfittab)<-colnames(getp("svec.channel",i)$svec$ABsvec.var$y);assign(pst("svec.channel",i,".fitted.tab"),value=scfittab);print(tail(scfittab))}
 for(i in c(10:12,14:16)[5:6]){
  i=0;show(tail(getp("svec.channel",i,".fitted.tab")))}
for(i in c(10:12,14:16)[5:6]){
  show(tail(exp(getp("svec.channel",i,".fitted.tab"))))}

#svec.fevd######
for(i in c(10:12,14:16)[6]){K=ncol(getp("svec.channel",i)$svec$ABSvec$LRorig)
  for(K in 1:ncol(getp("svec.channel",i)$svec$ABSvec$LRorig)){
    assign(pst("svec.channel",i,".fevd.tab"),foreach(j=1:(K^2),.combine=cbind)%do%(rbind(max(getp("svec.channel",i)$svec$svec.fevd.AB[,j]),which(getp("svec.channel",i)$svec$svec.fevd.AB[,j]==max(getp("svec.channel",i)$svec$svec.fevd.AB[,j]),arr.ind=TRUE))))}}
for(i in c(10:12,14:16)[6]){K=ncol(getp("svec.channel",i)$svec$ABSvec$LRorig);getp("svec.channel",i,".fevd.tab")->scfevdtab;colnames(scfevdtab)<-colnames(getp("svec.channel",i)$svec$svec.fevd.AB)[1:K^2];assign(pst("svec.channel",i,".fevd.tab"),scfevdtab)}
#svec.irf#######
c(0:4,6:10)
for(i in c(0:4,6:12,14:16)[14:15]){
  assign(pst("svec.channel",i,".irf.tab"),foreach(j=1:(3*ncol(getp("svec.channel",i)$svec$ABSvec$var@x)),.combine=cbind)%do%(div(exp(getp("svec.channel",i)$svec$svec.irf.AB[,j]),2)[1:10]))}
for(i in c(0:4,6:12,14:16)[14:15]){getp("svec.channel",i,".irf.tab")->scirftab;colnames(scirftab)<-colnames(getp("svec.channel",i)$svec$svec.irf.AB);assign(pst("svec.channel",i,".irf.tab"),scirftab)}
#plot#####
library(ggplot2)
map<-aes("x"<-format(seq(as.Date("2007-06-01"),by="months", length.out=130)),"теоретичні значення"<-svec.channel2$svec$ABsvec.fit[,8])
ggplot2::qplot(aes("x","теоретичні значення"))+ggplot2::geom_line()+ggplot2::theme_bw()
for (i in 1:ncol(svec.channel2$svec$ABsvec.res)){
grid::grid.layout(matrix(1:i,sample.model2$K, 2),widths = unit(c(3/2, 1, 1/2, 1),
                  c("lines", "null", "null", "cm")),
                  heights = c(1/2, 1, 2/2, 3))
                  #c("cm", "null", "null", "lines")

ggplot2::qplot(seq(as.Date("2007-06-01"),by="months", length.out=130),svec.channel3$svec$ABsvec.fit[,7])+#geom_title(xlab="час",ylab="прогноз")+
  ggplot2::geom_line()+ggplot2::theme_bw()}
#IMF########
lybrary(dplyr)
AERA<-read_excel("J:/І розділ/2.3/Report AREAER 2000-2016/AREAER-DataQueryReport_12.15 (version 1).xlsb.xlsx")[3:74241,]
colnames(AERA)<-c("Year",	"Country",	"Index",	"Category",	"Status",	"Description")
save.image("H:/svar.code/data/aerimf.RData")
AERA1<-foreach(i = 1:10,.combine=rbind)%do%(data.frame(filter(AERA,AERA$Index==c(paste("III.C.",seq(1:10),".",sep=""))[i])))
currency.regimes<-names(sapply(split(AERA,AERA$Category),function(x)length(unique(x))))
massive1<-filter(x=data.frame(AERA$Category),AERA$Category=="Free floating")


