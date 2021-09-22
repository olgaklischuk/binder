# varchannel<-function(varchannel,Amat,Bmat,direct=TRUE,estmod="direct",write.xlsx=TRUE,directory=Sys.getenv("R_USER"),n=1,im=1,names.plot,names.plot.2){
library(vars)
for(i in c(16)){
i=c(1:12,14:16,"15.extension")[i];directory=pst(OneDrive,"I");if(i==0){estmod="direct"; varchannel=get(paste("varchannelNBU",sep=""));Amat=as.matrix(get(paste("amat",i,sep=".")));Bmat=as.matrix(get(paste("bmat",i,sep=".")));directory=paste(OneDrive,"I",sep="");direct=TRUE;write.xlsx=TRUE;names.plot<-(get(paste("NAMES",i,"U",sep=".")));im<-1}else{
  if(i==1){estmod="direct"; varchannel=get(paste("varchannel",i,sep=""));Amat=as.matrix(get(paste("amat",sep="")));Bmat=as.matrix(get(paste("bmat",sep="")));directory=pst(OneDrive,"I");direct=TRUE;write.xlsx=TRUE;names.plot<-(get(paste("NAMES",i,"U",sep=".")));im<-1}else {
    if(i=="15.extension"){i_15<-readr::parse_number(i);estmod="direct"; varchannel=get(paste("varchannel",i,sep=""));Amat=as.matrix(get(paste("amat",i_15,sep=".")));Bmat=as.matrix(get(paste("bmat",i_15,sep=".")));directory=pst(OneDrive,"I");direct=TRUE;write.xlsx=TRUE;names.plot<-get(paste("NAMES",i_15,"U",sep="."));im<-1}else{estmod="direct"; varchannel=get(paste("varchannel",i,sep=""));Amat=as.matrix(get(paste("amat",i,sep=".")));Bmat=as.matrix(get(paste("bmat",i,sep=".")));directory=pst(OneDrive,"I");direct=TRUE;write.xlsx=TRUE;names.plot<-get(paste("NAMES",i,"U",sep="."));im<-1}}}
# i=16;directory=pst(OneDrive,"I");if(i==0){estmod="direct"; varchannel=get(paste("varchannelNBU",sep=""));Amat=as.matrix(get(paste("amat",i,sep=".")));Bmat=as.matrix(get(paste("bmat",i,sep=".")));directory=paste(OneDrive,"I",sep="");direct=TRUE;write.xlsx=TRUE;names.plot<-(get(paste("NAMES",i,"U",sep=".")));im<-1}else{
# if(i==1){estmod="direct"; varchannel=get(paste("varchannel",i,sep=""));Amat=as.matrix(get(paste("amat",sep="")));Bmat=as.matrix(get(paste("bmat",sep="")));directory=pst(OneDrive,"I");direct=TRUE;write.xlsx=TRUE;names.plot<-(get(paste("NAMES",i,"U",sep=".")));im<-1}else {estmod="direct"; varchannel=get(paste("varchannel",i,sep=""));Amat=as.matrix(get(paste("amat",i,sep=".")));Bmat=as.matrix(get(paste("bmat",i,sep=".")));directory=pst(OneDrive,"I");direct=TRUE;write.xlsx=TRUE;names.plot<-get(paste("NAMES",i,"U",sep="."));im<-1}}
n<-i
N1<-varchannel$obs
if(is.null(names.plot)){if(is.null(names.plot.2)){
  names.plot<-get(paste("NAMES",n,"U",sep="."))
  names.plot.2<-get(paste("NAMES",n,"U.2",sep="."))}else{names.plot<-get(paste("NAMES",n,"U",sep="."));names.plot.2<-names.plot.2}
}else{if(is.null(names.plot.2)){names.plot<-names.plot;names.plot.2<-get(paste("NAMES",n,"U",sep="."))}else{names.plot<-names.plot;names.plot.2<-names.plot.2}}
if(is.null(im)){im=1} else {im=im}

directory->dir
id<-try(identify(varchannel,Amat,Bmat))
if(id$logLc<0)id$logLc<--id$logLc
START<-Sys.time()

var.coef<-foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(varchannel$varresult[[i]]$coefficients)
var.res<-t(foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(varchannel$varresult[[i]]$residuals))
var.ef<-t(foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(varchannel$varresult[[i]]$effects))
var.fit<-t(foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(varchannel$varresult[[i]]$fitted.values))
rownames(var.coef)<-names(varchannel$varresult)

colnames(var.res)<-names(varchannel$varresult)
rownames(var.res)<-c(1:N1)
colnames(var.ef)<-names(varchannel$varresult)
rownames(var.ef)<-paste(1:N1,"obs")
colnames(var.fit)<-names(varchannel$varresult)
#for(i in names(ASvar$var$varrresult))
var.irf.VAR<-try(irf(varchannel,impulse=names(varchannel$varresult)[im],n.ahead=11))

var.irf<-format(foreach(i=paste(names(varchannel$varresult)),.combine=cbind)%do%(cbind(var.irf.VAR$Lower[[1]][,i],var.irf.VAR$irf[[1]][,i],var.irf.VAR$Upper[[1]][,i])),digits=3)
colnames(var.irf)<-foreach(i =1:length(names(varchannel$varresult)),.combine=cbind )%do%(c(paste(names(varchannel$varresult)[i],"Lower se",sep=" "),paste(names(varchannel$varresult)[i],"IRF",sep=" "),paste(names(varchannel$varresult)[i],"Upper se",sep=" ")))
var.irf<-data.frame(var.irf)
##plotting IRF#########
if(!dir.exists(paste(dir,"/svar.code/result2/",sep="")))dir.create(paste(dir,"/svar.code/result2/",sep=""))
if(n==1){j<-n} else j<-paste(n,".1",sep=""); if(!dir.exists(paste(dir,"/svar.code/result2/varchannel",j,"/plot.irf",sep=""))){dir.create(paste(dir,"/svar.code/result2/varchannel",j,sep=""));dir.create(paste(dir,"/svar.code/result2/varchannel",j,"/plot.irf",sep=""))};if(!getwd()==paste(dir,"/svar.code/result2/varchannel",j,"/plot.irf",sep=""))setwd(paste(dir,"/svar.code/result2/varchannel",j,"/plot.irf",sep=""))
par(cex.lab=.97,lheight=1.1,mai=10*c(.1,.1,.1,.1),oma=c(.1,.1,.1,.1))
title<-paste("Реакція макрозмінних","на шок",names.plot[im], "за VAR моделлю",n+1);png(paste("plot",j,"7.A model.png",sep="."),type="cairo-png")
png(paste("Fig",j,"1.IRF of VAR model.png",sep="."),type="cairo-png")
plot1<-plot(try(irf(varchannel,impulse=names(varchannel$varresult)[im],n.ahead=11)),main=title,ylab=names.plot[1:varchannel$K+1])+title();dev.off();grDevices::png(paste("Fig",j,"8.A model.png",sep="."),type="cairo-png")
dev.off()
var.fevd<-fevd(varchannel,n.ahead=12)
##plotting FEVD#########c("bottomright", "bottom", "bottomleft","left", "topleft", "top", "topright", "right", "center"))########
if(n==1){j<-n} else j<-paste(n,".1",sep="")
if(!dir.exists(paste(dir,"/svar.code/result2/varchannel",j,"/plot.fevd",sep=""))){dir.create(paste(dir,"/svar.code/result2/varchannel",j,sep=""));dir.create(paste(dir,"/svar.code/result2/varchannel",j,"/plot.fevd",sep=""))};if(!getwd()==paste(dir,"/svar.code/result2/varchannel",j,"/plot.fevd",sep=""))setwd(paste(dir,"/svar.code/result2/varchannel",j,"/plot.fevd",sep=""))
par(mar=c(2,0,2.5,8),cex.main=0.8)
# layout(matrix(c(rep(1,8),2,2),5,2,byrow=TRUE))
names.<-names(varchannel$varresult)
names.2<-names.plot.2
title=paste("Факторне розкладання ваги шоків для моделі",n+1)
grDevices::png(paste("Fig",j,"12.FEVD of var model.png",sep="."),type="cairo-png")
plot4<-plot.varfevd(fevd(varchannel,impulse=names(varchannel$varresult)[im],n.ahead=11),main=title,names.=names.plot.2,ylab=NULL,legend = FALSE);dev.off()
# +legend("center",legend=c(names(ASvar$var$varresult)),pch=15,col = grey.colors(8,.3,end = .9,gamma = 2.2));grDevices::savePlot("plot.4-6.Legend",type="png")
var.fevd<-format(100*(foreach(i=paste(names(varchannel$varresult)),.combine=cbind)%do%(var.fevd[[i]])),digits=2)

K<-varchannel$K

if(length(names(varchannel$varresult))==5){
  name1<-foreach(i=c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5)),.combine=cbind )%do%(paste(names(varchannel$varresult)[i]))
  name2<-rep(foreach(j=1:length(names(varchannel$varresult)),.combine=cbind)%do%(paste(" to",names(varchannel$varresult)[j],sep=" ")),5)}
if(!length(names(varchannel$varresult))>8){
  name1<-foreach(i=c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8),rep(7,8),rep(8,8)),.combine=cbind )%do%(paste(names(varchannel$varresult)[i]))
  name2<-rep(foreach(j=1:length(names(varchannel$varresult)),.combine=cbind)%do%(paste(" to",names(varchannel$varresult)[j],sep=" ")),8)
} else{name1<-foreach(i=c(rep(1,9),rep(2,9),rep(3,9),rep(4,9),rep(5,9),rep(6,9),rep(7,9),rep(8,9),rep(9,9)),.combine=cbind )%do%(paste(names(varchannel$varresult)[i]))
name2<-rep(foreach(j=1:length(names(varchannel$varresult)),.combine=cbind)%do%(paste(" to",names(varchannel$varresult)[j],sep=" ")),9)}

colnames(var.fevd)<-foreach(k=1:K^2)%do%(paste(name1[k],name2[k],sep=""))


# class(var.irf)<-"varirf";
# class(var.fevd)<-"varfevd";
call<-match.call()

result.1=list(VAR=varchannel,
              var.irf=data.frame(var.irf),svar.fevd.ASvar=var.fevd,
              call=call,var.coef=data.frame(var.coef),
              var.res=data.frame(var.res),
              var.ef=data.frame(var.ef),var.fit=data.frame(var.fit),
              var.irf=var.irf,
              var.fevd=data.frame(var.fevd))

if(!dir.exists(paste(dir,"/svar.code/result2/",sep="")))dir.create(paste(dir,"/svar.code/result2/",sep=""))
if(write.xlsx==TRUE){if(n==1){j<-n} else j<-paste(n,".1",sep=""); if(!dir.exists(paste(dir,"/svar.code/result2/varchannel",j,sep="")))dir.create(paste(dir,"/svar.code/result2/varchannel",j,sep=""));if(!getwd()==paste(dir,"/svar.code/result2/varchannel",j,sep=""))setwd(paste(dir,"/svar.code/result2/varchannel",j,sep=""))
WriteXLS::WriteXLS(result.1$var.coef,paste(j,"var.coef.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
WriteXLS::WriteXLS(result.1$var.irf,paste(j,"var.irf.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
WriteXLS::WriteXLS(result.1$var.res,paste(j,"var.res.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
WriteXLS::WriteXLS(result.1$var.ef,paste(j,"var.ef.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
WriteXLS::WriteXLS(result.1$var.fit,paste(j,"var.fit.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
WriteXLS::WriteXLS(result.1$var.fevd,paste(j,"var.fevd.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
}
END<-Sys.time();time.execution<-END-START;time.execution
result.1=list(VAR=varchannel,
              var.irf=var.irf,svar.fevd.ASvar=var.fevd,
              call=call,var.coef=data.frame(var.coef),
              var.res=data.frame(var.res),
              var.ef=data.frame(var.ef),var.fit=data.frame(var.fit),
              var.irf=var.irf,
              var.fevd=var.fevd)

class(result.1) <- "var.report"
# return(result.1)
# }
assign(x=paste("varchannel.report",n,sep = ""),result.1)
}

if(n==16){save.image(Dir.svar)}
ls(pattern="varchannel.report")
