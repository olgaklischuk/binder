library(vars)
library(readr)
#load####
if(Sys.info()[4]=="MacBook-Air-Ulia.local"){comp="Mac"}else{comp="Asus"}
if(comp=="Mac"){disk="~/OneDrive/"}else{disk="C:/Users/Asus/OneDrive/"}
if(comp=="asus"){
  OneDrive<-"C:/Users/Asus/OneDrive/Computer/"}else{OneDrive<-"~/OneDrive/Computer/"}
if(comp=="Mac"){Dir.svar<-paste(disk,"Computer/I/svar.code/data/2var.and.svar.RData",sep="")}else{Dir.svar<-"C:/Users/Asus/OneDrive/Computer/I/svar.code/data/2var.and.svar.RData"}
load(Dir.svar)
channeljd8<-
  channeljd9<-
for(l in 1:2){if(l==1){T=87}else{T=157}
for(i in c("jd8","jd9")){n<-nrow(na.omit(get(paste("channel",i,sep=""))[1:T,]));if(n<10){warning(paste("channel",i,": ","Stop! Number of observations less then needed to satisfying degrees of freedom.",sep=""))}else{
  try(assign(paste("Var.lag.number",l,i,sep="."),VARselect(na.omit(get(paste("channel",i,sep=""))[1:n,]),lag.max = 1,type="trend")))
}
time.start<-Sys.time();for(i in c("jd8","jd9")) { n<-nrow(na.omit(get(paste("channel",i,sep=""))[1:T,]))
try(assign(x = paste("varchannel",l,j,sep="."),value=VAR(na.omit(get(paste("channel",i,sep=""))[1:T,]),p = get(paste("Var.lag.number",l,i,sep="."))$selection[1],type = "trend",season=NULL, exogen = NULL,  ic = c("AIC", "HQ", "SC", "FPE"))))}
}}
time.end<-Sys.time()-time.start;time.end