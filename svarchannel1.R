# svarchannel<-function(varchannel,Amat,Bmat,direct=TRUE,estmod="direct",write.xlsx=TRUE,directory=Sys.getenv("R_USER"),n=1,im=1,names.plot,names.plot.2){
  library(vars)
  i=1;directory=pst(OneDrive,"I");if(i==0){i->n;estmod="direct"; varchannel=get(paste("varchannelNBU",sep=""));Amat=as.matrix(get(paste("amat",i,sep=".")));Bmat=as.matrix(get(paste("bmat",i,sep=".")));directory=paste(OneDrive,"I",sep="");direct=TRUE;write.xlsx=TRUE;names.plot<-(get(paste("NAMES",i,"U",sep=".")));im<-1}else{
  if(i==1){i->n;estmod="direct"; varchannel=get(paste("varchannel",i,sep=""));Amat=as.matrix(get(paste("amat",sep="")));Bmat=as.matrix(get(paste("bmat",sep="")));directory=pst(OneDrive,"I");direct=TRUE;write.xlsx=TRUE;names.plot<-(get(paste("NAMES",i,"U",sep=".")));im<-1}else {i->n;estmod="direct"; varchannel=get(paste("varchannel",i,sep=""));Amat=as.matrix(get(paste("amat",i,sep=".")));Bmat=as.matrix(get(paste("bmat",i,sep=".")));directory=pst(OneDrive,"I");direct=TRUE;write.xlsx=TRUE;names.plot<-get(paste("NAMES",i,"U",sep="."));im<-1}}
  N1<-varchannel$obs
  estmod="direct"
  estmod<-ifelse(direct==TRUE,"direct","scoring")
  names.plot.2<-NULL
  if(is.null(names.plot)){if(is.null(names.plot.2)){
    names.plot<-get(paste("NAMES",n,"U",sep="."))
    names.plot.2<-get(paste("NAMES",n,"U.2",sep="."))}else{names.plot<-get(paste("NAMES",n,"U",sep="."));names.plot.2<-names.plot.2}
  }else{if(is.null(names.plot.2)){names.plot<-names.plot;names.plot.2<-get(paste("NAMES",n,"U.2",sep="."))}else{names.plot<-names.plot;names.plot.2<-names.plot.2}}
  if(is.null(im)){im=1}else {im=im}

  directory->dir
  id<-try(identify(varchannel,Amat,Bmat))
  if(id$logLc<0)id$logLc<--id$logLc
  START<-Sys.time()
  ASvar<-SVAR(varchannel, estmethod = estmod,#"direct" "scoring" ),
              Amat = Amat, Bmat = NULL,
              start = rep(.01,id$params.A), max.iter = 1000, conv.crit = 1e-07, maxls = 2,
              lrtest = TRUE)
  BSvar<-SVAR(varchannel, estmethod = estmod,#"direct" "scoring" ),
              Amat = NULL, Bmat = Bmat ,
              start = rep(.01,id$params.B), max.iter = 1000, conv.crit = 1e-07, maxls = 2,
              lrtest = TRUE)

  ABSvar<-try(SVAR(varchannel, estmethod = estmod,#"direct" "scoring" ),
                   Amat = Amat, Bmat = Bmat,
                   start = NULL, max.iter = 1000, conv.crit = 1e-07, maxls = 2,
                   lrtest = TRUE))
  suppressWarnings((warning("In SVAR(varchannel, estmethod = estmod, Amat = Amat, Bmat = Bmat,  :
                            The AB-model is just identified. No test possible.")))
  Asvar.A<-data.frame(rbind(ASvar$A,ASvar$Ase))
  Bsvar.B<-data.frame(rbind(BSvar$B,BSvar$Bse))
  ABsvar.AB<-data.frame(rbind(ABSvar$A,ABSvar$Ase,ABSvar$B,ABSvar$Bse))

  Asvar.coef<-format(foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(ASvar$var$varresult[[i]]$coefficients),digits=6)
  Bsvar.coef<-format(foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(BSvar$var$varresult[[i]]$coefficients),digits=6)
  ABsvar.coef<-format(foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(ABSvar$var$varresult[[i]]$coefficients),digits=6)
  Asvar.res<-foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(ASvar$var$varresult[[i]]$residuals)
  Bsvar.res<-foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(BSvar$var$varresult[[i]]$residuals)
  ABsvar.res<-foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(ABSvar$var$varresult[[i]]$residuals)
  Asvar.ef<-t(foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(ASvar$var$varresult[[i]]$effects))
  Bsvar.ef<-t(foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(BSvar$var$varresult[[i]]$effects))
  ABsvar.ef<-t(foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(ABSvar$var$varresult[[i]]$effects))
  Asvar.fit<-t(foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(ASvar$var$varresult[[i]]$fitted.values))
  Bsvar.fit<-t(foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(BSvar$var$varresult[[i]]$fitted.values))
  ABsvar.fit<-t(foreach(i=1:length(names(varchannel$varresult)),.combine=rbind)%do%(ABSvar$var$varresult[[i]]$fitted.values))
  rownames(Asvar.coef)<-names(varchannel$varresult)
  rownames(Bsvar.coef)<-names(varchannel$varresult)
  rownames(ABsvar.coef)<-names(varchannel$varresult)

  rownames(Asvar.res)<-names(varchannel$varresult)
  rownames(Bsvar.res)<-names(varchannel$varresult)
  rownames(ABsvar.res)<-names(varchannel$varresult)
  colnames(Asvar.res)<-c(1:N1)
  colnames(Bsvar.res)<-c(1:N1)
  colnames(ABsvar.res)<-c(1:N1)
  colnames(Asvar.ef)<-names(varchannel$varresult)
  colnames(Bsvar.ef)<-names(varchannel$varresult)
  colnames(ABsvar.ef)<-names(varchannel$varresult)
  rownames(Asvar.ef)<-paste(1:N1,"obs")
  rownames(Bsvar.ef)<-paste(1:N1,"obs")
  rownames(ABsvar.ef)<-paste(1:N1,"obs")
  colnames(Asvar.fit)<-names(varchannel$varresult)
  colnames(Bsvar.fit)<-names(varchannel$varresult)
  colnames(ABsvar.fit)<-names(varchannel$varresult)
  #for(i in names(ASvar$var$varrresult))
  svar.irf.ASVAR<-try(irf(ASvar,impulse=names(ASvar$var$varresult)[im],n.ahead=11))
  svar.irf.BSVAR<-try(irf(BSvar,impulse=names(BSvar$var$varresult)[im],n.ahead=11))
  svar.irf.ABSVAR<-try(irf(ABSvar,impulse=names(ABSvar$var$varresult)[im],n.ahead=11))

  svar.irf.A<-foreach(i=paste(names(ASvar$var$varresult)),.combine=cbind)%do%(cbind(svar.irf.ASVAR$Lower[[1]][,i],svar.irf.ASVAR$irf[[1]][,i],svar.irf.ASVAR$Upper[[1]][,i]))
  svar.irf.B<-foreach(i=paste(names(BSvar$var$varresult)),.combine=cbind)%do%(cbind(svar.irf.BSVAR$Lower[[1]][,i],svar.irf.BSVAR$irf[[1]][,i],svar.irf.BSVAR$Upper[[1]][,i]))
  svar.irf.AB<-foreach(i=paste(names(ABSvar$var$varresult)),.combine=cbind)%do%(cbind(svar.irf.ABSVAR$Lower[[1]][,i],svar.irf.ABSVAR$irf[[1]][,i],svar.irf.ABSVAR$Upper[[1]][,i]))
  colnames(svar.irf.A)<-foreach(i =1:length(names(ASvar$var$varresult)),.combine=cbind )%do%(c(paste(names(ASvar$var$varresult)[i],"Lower se",sep=" "),paste(names(ASvar$var$varresult)[i],"IRF",sep=" "),paste(names(ASvar$var$varresult)[i],"Upper se",sep=" ")))
  colnames(svar.irf.B)<-foreach(i =1:length(names(BSvar$var$varresult)),.combine=cbind )%do%(c(paste(names(BSvar$var$varresult)[i],"Lower se",sep=" "),paste(names(BSvar$var$varresult)[i],"IRF",sep=" "),paste(names(BSvar$var$varresult)[i],"Upper se",sep=" ")))
  colnames(svar.irf.AB)<-foreach(i =1:length(names(ABSvar$var$varresult)),.combine=cbind )%do%(c(paste(names(ABSvar$var$varresult)[i],"Lower se",sep=" "),paste(names(ABSvar$var$varresult)[i],"IRF",sep=" "),paste(names(ABSvar$var$varresult)[i],"Upper se",sep=" ")))
  svar.irf.A<-data.frame(svar.irf.A);svar.irf.B<-data.frame(svar.irf.B);svar.irf.AB<-data.frame(svar.irf.AB)
  ##plotting IRF#########
  if(!dir.exists(paste(dir,"/svar.code/result4/",sep="")))dir.create(paste(dir,"/svar.code/result4/",sep=""))
  if(n==1){j<-n} else j<-paste(n,".1",sep=""); if(!dir.exists(paste(dir,"/svar.code/result4/svarchannel",j,"/plot.irf",sep=""))){dir.create(paste(dir,"/svar.code/result4/svarchannel",j,sep=""));dir.create(paste(dir,"/svar.code/result4/svarchannel",j,"/plot.irf",sep=""))};if(!getwd()==paste(dir,"/svar.code/result4/svarchannel",j,"/plot.irf",sep=""))setwd(paste(dir,"/svar.code/result4/svarchannel",j,"/plot.irf",sep=""))
  par(mfrow=c(ifelse(K%/%2==0,K/2,round((K/2),0)+1),2),cex.lab=.97,lheight=1.1)
  title<-paste("Реакція макрозмінних","на шок",names.plot[im], "за SVAR моделлю",n+1);png(paste("plot",j,"7.A model.png",sep="."),type="cairo-png")
  png(paste("Fig",j,"7.A model.png",sep="."),type="cairo-png")
  plot1<-plot(try(irf(ASvar,impulse=names(ASvar$var$varresult)[im],n.ahead=11)),main=title,names.=names.plot,ylab=names.plot[1:varchannel$K+1])+title();dev.off();grDevices::png(paste("Fig",j,"8.A model.png",sep="."),type="cairo-png")
  plot2<-plot(try(irf(BSvar,impulse=names(BSvar$var$varresult)[im],n.ahead=11)),main=title,names.=names.plot,ylab=names.plot[1:varchannel$K+1])+title();dev.off()
  suppressWarnings(expr = last.warning())
  grDevices::png(paste("Fig",j,"9.AB model.png",sep="."),type="cairo-png")
  plot3<-plot(try(irf(ABSvar,impulse=names(ABSvar$var$varresult)[im],n.ahead=11)),main=title,names.=names.plot,ylab=names.plot[1:varchannel$K+1])+title();dev.off()
  # suppressWarnings("plot.varirf")
  svar.fevd.ASvar<-fevd(ASvar,n.ahead=12)
  svar.fevd.BSvar<-fevd(BSvar,n.ahead=12)
  svar.fevd.ABSvar<-fevd(ABSvar,n.ahead=12)
  ##plotting FEVD#########c("bottomright", "bottom", "bottomleft","left", "topleft", "top", "topright", "right", "center"))########
  if(n==1){j<-n} else j<-paste(n,".1",sep="")
  if(!dir.exists(paste(dir,"/svar.code/result4/svarchannel",j,"/plot.fevd",sep=""))){dir.create(paste(dir,"/svar.code/result4/svarchannel",j,sep=""));dir.create(paste(dir,"/svar.code/result4/svarchannel",j,"/plot.fevd",sep=""))};if(!getwd()==paste(dir,"/svar.code/result4/svarchannel",j,"/plot.fevd",sep=""))setwd(paste(dir,"/svar.code/result4/svarchannel",j,"/plot.fevd",sep=""))
  par(mfrow=c(ifelse(K%/%2==0,K/2,round((K/2),0)+1),2),mar=c(2,0,2.5,8),cex.main=0.8)
  # layout(matrix(c(rep(1,8),2,2),5,2,byrow=TRUE))
  names.<-names(ASvar$var$varresult)
  names.2<-names.plot.2
  title=paste("Факторне розкладання ваги шоків для моделі",n+1)
  grDevices::png(paste("Fig",j,"12.A model.png",sep="."),type="cairo-png")
  plot4<-plot.varfevd(fevd(ASvar,impulse=names(ASvar$var$varresult)[im],n.ahead=11),main=title,names.=names.plot.2,ylab=NULL,legend = FALSE);dev.off();grDevices::png(paste("Fig",j,"22.A model.png",sep="."),type="cairo-png")
  plot5<-plot.varfevd(fevd(BSvar,impulse=names(BSvar$var$varresult)[im],n.ahead=11),main=title,names.=names.plot.2,ylab=NULL,legend = FALSE);dev.off();grDevices::png(paste("Fig",j,"32.A model.png",sep="."),type="cairo-png")
  plot6<-plot.varfevd(fevd(ABSvar,impulse=names(ABSvar$var$varresult)[im],n.ahead=11),main=title,names.=names.plot.2,ylab=NULL,legend = FALSE);dev.off()
  # +legend("center",legend=c(names(ASvar$var$varresult)),pch=15,col = grey.colors(8,.3,end = .9,gamma = 2.2));grDevices::savePlot("plot.4-6.Legend",type="png")
  svar.fevd.A<-100*(foreach(i=paste(names(ASvar$var$varresult)),.combine=cbind)%do%(svar.fevd.ASvar[[i]]))
  svar.fevd.B<-100*(foreach(i=paste(names(BSvar$var$varresult)),.combine=cbind)%do%(svar.fevd.BSvar[[i]]))
  svar.fevd.AB<-100*(foreach(i=paste(names(ABSvar$var$varresult)),.combine=cbind)%do%(svar.fevd.ABSvar[[i]]))

  K<-varchannel$K

  if(length(names(ASvar$var$varresult))==5){
    name1<-foreach(i=c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5)),.combine=cbind )%do%(paste(names(ASvar$var$varresult)[i]))
    name2<-rep(foreach(j=1:length(names(ASvar$var$varresult)),.combine=cbind)%do%(paste(" to",names(ASvar$var$varresult)[j],sep=" ")),5)}
  if(!length(names(ASvar$var$varresult))>8){
    name1<-foreach(i=c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8),rep(7,8),rep(8,8)),.combine=cbind )%do%(paste(names(ASvar$var$varresult)[i]))
    name2<-rep(foreach(j=1:length(names(ASvar$var$varresult)),.combine=cbind)%do%(paste(" to",names(ASvar$var$varresult)[j],sep=" ")),8)
  } else{name1<-foreach(i=c(rep(1,9),rep(2,9),rep(3,9),rep(4,9),rep(5,9),rep(6,9),rep(7,9),rep(8,9),rep(9,9)),.combine=cbind )%do%(paste(names(ASvar$var$varresult)[i]))
  name2<-rep(foreach(j=1:length(names(ASvar$var$varresult)),.combine=cbind)%do%(paste(" to",names(ASvar$var$varresult)[j],sep=" ")),9)}

  colnames(svar.fevd.A)<-foreach(k=1:K^2)%do%(paste(name1[k],name2[k],sep=""))
  colnames(svar.fevd.B)<-foreach(k=1:K^2)%do%(paste(name1[k],name2[k],sep=""))
  colnames(svar.fevd.AB)<-foreach(k=1:K^2)%do%(paste(name1[k],name2[k],sep=""))


  class(ASvar)<-"svarest";class(BSvar)<-"svarest";class(ABSvar)<-"svarest";
  class(svar.irf.ASVAR)<-"svarirf";class(svar.irf.BSVAR)<-"svarirf";class(svar.irf.ABSVAR)<-"svarirf"
  class(svar.fevd.ASvar)<-"svarfevd";class(svar.fevd.BSvar)<-"svarfevd";class(svar.fevd.ABSvar)<-"svarfevd"
  call<-match.call()

  result.1=list(ASVAR=ASvar,BSVAR=BSvar,ABSVAR=ABSvar,
                svar.irf.ASVAR=svar.irf.ASVAR,svar.irf.BSVAR=svar.irf.BSVAR,svar.irf.ABSVAR=svar.irf.ABSVAR,svar.fevd.ASvar=svar.fevd.ASvar,svar.fevd.BSvar=svar.fevd.BSvar,svar.fevd.ABSvar=svar.fevd.ABSvar,
                estmod=estmod,call=call,Asvar.A=as.data.frame(Asvar.A),Bsvar.B=as.data.frame(Bsvar.B),ABsvar.AB=as.data.frame(ABsvar.AB),Asvar.coef=data.frame(Asvar.coef),Bsvar.coef=data.frame(Bsvar.coef),ABsvar.coef=data.frame(ABsvar.coef),
                Asvar.res=data.frame(Asvar.res),Bsvar.res=data.frame(Bsvar.res),ABsvar.res=data.frame(ABsvar.res),
                Asvar.ef=data.frame(Asvar.ef),Bsvar.ef=data.frame(Bsvar.ef),ABsvar.ef=data.frame(ABsvar.ef),Asvar.fit=data.frame(Asvar.fit),Bsvar.fit=data.frame(Bsvar.fit),ABsvar.fit=data.frame(ABsvar.fit),
                svar.irf.A=svar.irf.A,svar.irf.B=svar.irf.B,svar.irf.AB=svar.irf.AB,
                svar.fevd.A=as.data.frame(svar.fevd.A),svar.fevd.B=as.data.frame(svar.fevd.B),svar.fevd.AB=as.data.frame(svar.fevd.AB))

  ##Write in Excel####
  if(!dir.exists(paste(dir,"/svar.code/result5/",sep="")))dir.create(paste(dir,"/svar.code/result5/",sep=""))
  if(write.xlsx==TRUE){if(n==1){j<-n} else j<-paste(n,".1",sep=""); if(!dir.exists(paste(dir,"/svar.code/result5/svarchannel",j,sep="")))dir.create(paste(dir,"/svar.code/result5/svarchannel",j,sep=""));if(!getwd()==paste(dir,"/svar.code/result5/svarchannel",j,sep=""))setwd(paste(dir,"/svar.code/result5/svarchannel",j,sep=""))
  WriteXLS::WriteXLS(result.1$Asvar.A,paste(j,"Asvar.A.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
  WriteXLS::WriteXLS(result.1$Bsvar.B,paste(j,"Bsvar.B.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
  WriteXLS::WriteXLS(result.1$ABsvar.AB,paste(j,"ABsvar.AB.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
  WriteXLS::WriteXLS(result.1$ABsvar.coef,paste(j,"ABsvar.coef.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
  WriteXLS::WriteXLS(result.1$svar.irf.A,paste(j,"svar.irf.A.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
  WriteXLS::WriteXLS(result.1$svar.irf.B,paste(j,"svar.irf.B.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
  WriteXLS::WriteXLS(result.1$svar.irf.AB,paste(j,"svar.irf.AB.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
  WriteXLS::WriteXLS(result.1$ABsvar.res,paste(j,"ABsvar.res.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
  WriteXLS::WriteXLS(result.1$ABsvar.ef,paste(j,"ABsvar.ef.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
  WriteXLS::WriteXLS(result.1$ABsvar.fit,paste(j,"ABsvar.fit.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
  WriteXLS::WriteXLS(result.1$svar.fevd.A,paste(j,"svar.fevd.A.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
  WriteXLS::WriteXLS(result.1$svar.fevd.B,paste(j,"svar.fevd.B.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
  WriteXLS::WriteXLS(result.1$svar.fevd.AB,paste(j,"svar.fevd.AB.xlsx",sep="."),row.names=TRUE,FreezeRow=1,FreezeCol=1)
  }
  END<-Sys.time();time.execution<-END-START;time.execution
  result.1=list(ASVAR=ASvar,BSVAR=BSvar,ABSVAR=ABSvar,
                svar.irf.ASVAR=svar.irf.ASVAR,svar.irf.BSVAR=svar.irf.BSVAR,svar.irf.ABSVAR=svar.irf.ABSVAR,svar.fevd.ASvar=svar.fevd.ASvar,svar.fevd.BSvar=svar.fevd.BSvar,svar.fevd.ABSvar=svar.fevd.ABSvar,
                estmod=estmod,call=call,Asvar.A=as.data.frame(Asvar.A),Bsvar.B=as.data.frame(Bsvar.B),ABsvar.AB=as.data.frame(ABsvar.AB),Asvar.coef=data.frame(Asvar.coef),Bsvar.coef=data.frame(Bsvar.coef),ABsvar.coef=data.frame(ABsvar.coef),
                Asvar.res=data.frame(Asvar.res),Bsvar.res=data.frame(Bsvar.res),ABsvar.res=data.frame(ABsvar.res),
                Asvar.ef=data.frame(Asvar.ef),Bsvar.ef=data.frame(Bsvar.ef),ABsvar.ef=data.frame(ABsvar.ef),Asvar.fit=data.frame(Asvar.fit),Bsvar.fit=data.frame(Bsvar.fit),ABsvar.fit=data.frame(ABsvar.fit),
                svar.irf.A=svar.irf.A,svar.irf.B=svar.irf.B,svar.irf.AB=svar.irf.AB,
                svar.fevd.A=as.data.frame(svar.fevd.A),svar.fevd.B=as.data.frame(svar.fevd.B),svar.fevd.AB=as.data.frame(svar.fevd.AB),#plot1=plot1,plot2=plot2,plot3=plot3,plot4=plot4,plot5=plot5,plot6=plot6,
                "time execution"=time.execution)

  class(result.1) <- "svar.report"
  # return(result.1)
  # }
  assign(x=paste("svarchannel",n,sep = ""),result.1)
  svarchannel1<-result.1
