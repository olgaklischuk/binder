# svecchannel<-function(vecchannel,LR,SR,r=2,scoring=TRUE,dir,write.xlsx=TRUE,n=1,im=1,names.plot=NULL,names.plot.2=NULL){
# for(j in 1:9)

  dir="H:/svar.code/result"
  n = c(0:12,14:16,"15.extension")[17]
  r=2
  im=NULL
  names.plot=NULL;names.plot.2=NULL
  # vecchannel=get(paste("vec.channel",n,sep=""));if(n==1){j<-n;LR=get(paste("lr",n,sep="."));SR=get(paste("sr",n,sep="."));write.xlsx=TRUE}else {j<-n;LR=get(paste("lr",j,sep="."));SR=get(paste("sr",j,sep="."));write.xlsx=TRUE}
  vecchannel=get(paste("vec.channel.2.",n,sep=""));if(n==1){j<-n;LR=get(paste("lr",n,sep="."));SR=get(paste("sr",n,sep="."));write.xlsx=TRUE}else {j<-n;LR=get(paste("lr",j,sep="."));SR=get(paste("sr",j,sep="."));write.xlsx=TRUE}

  if(is.null(names.plot)){if(is.null(names.plot.2)){
  names.plot<-get(paste("NAMES",j,"U",sep="."))
  names.plot.2<-get(paste("NAMES",j,"U.2",sep="."))}else{names.plot.2=names.plot.2}
  }else{names.plot=names.plot}
  if(is.null(im)){im=1}else {im=im}
  #estmod="scoring";
  #estmod<-ifelse(direct==TRUE,"direct","scoring")
  library(foreach)
  library(urca)
  library(vars)
  START<-Sys.time()
  ######
  if(ncol(vecchannel@x)==2) {r=1}else{r=2}
  identify.svec<-function(vecchannel,LR,SR,r=r,svectype="AB-type")
  {
     # vecchannel=vec.channel1;
    r=2;LR=lr.1[,];SR=sr.1[,]
    K <- vecchannel@P
    K2 <- K^2
    P <- vecchannel@lag - 1
    IK <- diag(K)
    IK2 <- diag(K2)
    Kkk <- diag(K2)[, c(sapply(1:K, function(i) seq(i, K2, K)))]
    r <- as.integer(r)
    messeage<-if(!({1 <= r} && {r < ncol(vecchannel@x)})){
      stop(paste("\nThe cointegration rank 'r' must be in the interval [1:", ncol(vecchannel@x) - 1, "].\n", sep = ""))}else {"r is appropriate."}
    params.A <- sum(is.na(LR));print(params.A)
    params.B <- sum(is.na(SR));print(params.B)
    params <- params.A + params.B;print(params)
    svectype="AB-type"
    if(!svectype== "AB-type"& !svectype=="B-type"){
      l<-params.A} else{
        if(svectype=="B-type"){
          l<-params.B} else {l<-params}}

    #as in Bernhard Pfaff package vars####
    vecr<-cajorls(z = vecchannel, r = r)
    Coef.vecr <- coef(vecr$rlm);Coef.vecr
    alpha <- t(Coef.vecr[1:r, ])
    # Null<-function(M)
    # {
    #   #M<-t(R)
    #   tmp <- qr(M)
    #   set <- if(tmp$rank == 0L) seq_len(ncol(M)) else  -seq_len(tmp$rank)
    #   if(set==K^2) print("Provide free parameters.") else print("Rank is ",set,".",sep="")
    #   qr.Q(tmp, complete = TRUE)[, set, drop = FALSE]
    # }
    ifelse(r == 1, alpha.orth <- Null(t(alpha)), alpha.orth <- Null(alpha))
    beta <- vecr$beta[1:K, ]
    beta.orth <- Null(beta)
    Gamma.array <- array(c(t(tail(coef(vecr$rlm), K*P))), c(K, K, P))
    Gamma <- apply(Gamma.array, c(1, 2), sum)
    Xi <- beta.orth %*% solve(t(alpha.orth) %*% (IK - Gamma) %*% beta.orth) %*% t(alpha.orth)

    R0 <- diag(K^2)
    select <- c(apply(SR, c(1, 2), function(x) ifelse(identical(x, 0.0), TRUE, FALSE)))
    R.B <- R0[select, ]
    select <- c(apply(LR, c(1, 2), function(x) ifelse(identical(x, 0.0), TRUE, FALSE)))
    R.L <- R0[select, ]
    if(identical(nrow(R.L), as.integer(0))){
      R.L <- matrix(0, nrow = K2, ncol = K2)
      nResL <- 0
    }
    R.L <- R.L %*% kronecker(IK, Xi)
    nResL <- qr(R.L)$rank;print(nResL)
    ##
    ## Setting up the R matrix (implicit form)#####
    ##
    if(identical(nrow(R.B), as.integer(0))){
      R <- R.L
      nResB <- 0
    } else {
      R <- rbind(R.L, R.B)
      nResB <- qr(R.B)$rank
    }
    print(nResB)

    ##
    ## Obtaining the S matrix and s vector (explicit form)####
    ##

    Sb <- Null(t(R))  #K^2
    S <- rbind(matrix(0, nrow = K^2, ncol = (K^2-qr(t(R))$rank)), Sb=Sb)#2K^2xK^2-rank
    l<- ncol(S)
    gamma <- start <-  rnorm(l)
    s <- c(c(diag(K)), rep(0, K^2)) #K+K^2
    ## Test of unidentification#####
    ##
    test.underidentification<-
      if((nResB + nResL) < (K * (K - 1) / 2)){
        stop("The model is not identified. Use less free parameters.")
      }else {print("The model is just identified.")}
    min.restrictions=K * (K - 1) / 2
    #comfort.arg#####
    comfort.arg<-
      if(ncol(S)==length(gamma)){print("Comfortable argument")}else print("Non-comfortable.")
    vecab <- S %*% gamma + s
    A <- matrix(vecab[1:K2], nrow = K, ncol = K)
    B <- matrix(vecab[(K2 + 1):(2 * K2)], nrow = K, ncol = K)
    v1 <- (IK2 + Kkk) %*% kronecker(t(solve(A) %*% B), solve(B))
    v2 <- -1 * (IK2 + Kkk) %*% kronecker(IK, solve(B))
    v <- cbind(v1, v2)
    idmat <- v %*% S
    ms <- t(v) %*% v
    auto <- eigen(ms)$values
    rni <- 0
    #Test overidentification#####
    for (i in 1:l) {
      if (auto[i] < 1e-11)
        rni <- rni + 1
    }
    if (identical(rni, 0)) {
      if (identical(l, as.integer(K * (K + 1)/2))) {
        ident <- paste("The", svectype, "is just identified.")
      } else {
        ident <- paste("The", svectype, "is over identified.")
      }
    } else {
      ident <- paste("The", svartype, "is unidentified. The non-identification rank is", rni, ".")
      stop(ident)
    }
    result=list(ident=ident,messeage=messeage,min.restrictions=min.restrictions,gamma=gamma,l=l,test.underidentification=test.underidentification,comfort.arg=comfort.arg,lr=params.A,sr=params.B,lsr=params,nResL=nResL,nResB=nResB,VEC.rls=Coef.vecr)
    class(result)<-"identify.svec.report"
    return(result)

  }

  ######
  id<-identify.svec(vecchannel,LR,SR,r = 1)#eigen(ms) - must be finite with complete x
  #
  # ASvec<-SVEC(vecchannel,r=2,# "scoring" ),
  #             LR = LR, SR = NULL,
  #             start = rnorm(21), max.iter = 1000, conv.crit = 1e-07, maxls = 2,
  #             lrtest = FALSE)
  # l<-id$sr
  # BSvec<-SVEC(vecchannel,r=2, # "scoring" ),
  #             LR = NULL, SR = SR ,
  #             start = rnorm(19), max.iter = 1000, conv.crit = 1e-07, maxls = 2,
  #             lrtest = FALSE)
  l<-id$l
  #AB svec#############################
  if(ncol(vecchannel@x)==2) {r=1}else{r=2}
  ABSvec<-try(SVEC(vecchannel,r=r, # "scoring" ),
               LR = LR, SR = SR,
               start = NULL, max.iter = 1000, conv.crit = 1e-07, maxls = 2,
               lrtest = FALSE))
  ABsvec.AB<-data.frame(rbind(ABSvec$LR,ABSvec$LRse,ABSvec$SR,ABSvec$SRse))
  ABsvec.var<-vec2var(ABSvec$var)
  ABsvec.coef<-data.frame(foreach(i=1:ABsvec.var$p,.combine=rbind)%do%(ABsvec.var$A[[i]]))
  ABsvec.det<-data.frame(ABsvec.var$deterministic)
  ABsvec.res<-data.frame(ABsvec.var$resid)
  ABsvec.fit<-data.frame(fitted(ABsvec.var))
  j<-n;if(n==1){j<-n} else j<-paste(n,"",sep=""); if(!dir.exists(paste(dir,"/svec.channel",j,sep=""))){dir.create(paste(dir,"/svec.channel",j,sep=""))}

  # svec.fevd.ASvec<-fevd(ASvec,n.ahead=12)
  # svec.fevd.BSvec<-fevd(BSvec,n.ahead=12)
  svec.fevd.ABSvec<-fevd(vec2var(ABSvec$var),n.ahead=12)
  # svec.fevd.A<-foreach(i=paste(colnames(ASvec$var@x)),.combine=cbind)%do%(svar.fevd.ASvar[[i]])
  # svec.fevd.B<-foreach(i=paste(colnames(BSvec$var@x)),.combine=cbind)%do%(svar.fevd.BSvar[[i]])
  svec.fevd.AB<-data.frame(foreach(i=1:length(colnames(ABSvec$var@x)),.combine=cbind)%do%(svec.fevd.ABSvec[[i]]))
  #plot.fevd####
  if(!dir.exists(paste(dir,"/svec.channel",j,"/plot.fevd",sep=""))){dir.create(paste(dir,"/svec.channel",j,"/plot.fevd",sep=""))}
  if(!getwd()==paste(dir,"/svec.channel",j,"/plot.fevd",sep=""))setwd(paste(dir,"/svarchannel",0.2,"/fevd.plot",sep=""))

  par(mar=c(2,0,2.5,8),cex.main=0.8)

  title<- paste("Факторне розкладання ваги шоків для моделі",n+1)
  # plot(fevd(ASvec,impulse=colnames(ASvec$var@x)[1],n.ahead=11),legend = FALSE);grDevices::savePlot("plot.1.A model",type="png")
  # plot(fevd(BSvec,impulse=colnames(BSvec$var@x)[1],n.ahead=11),legend = FALSE);grDevices::savePlot("plot.2.B model",type="png")

  plot.varfevd(fevd(ABSvec,impulse=colnames(ABSvec$var@x),n.ahead=11),main=title,names.=names.plot,legend = FALSE,ylab="питома вага",plot.type="multiple");grDevices::savePlot("plot.2.AB SVEC model",type="png")
  # +plot.new()+legend("bottomleft",names(Asvec$y));grDevices::savePlot("plot.1-3.LEGEND",type="png")
  K<-ncol(vecchannel@x)
  if(K==6){
    name1<-foreach(i=c(rep(1,6),rep(2,6),rep(3,6),rep(4,6),rep(5,6),rep(6,6)),.combine=cbind )%do%(paste(colnames(ABSvec$var@x)[i]))
    name2<-rep(foreach(j=1:length(colnames(ABSvec$var@x)),.combine=cbind)%do%(paste(" to",colnames(ABSvec$var@x)[j],sep=" ")),6)
  }
  if(K==7){
    name1<-foreach(i=c(rep(1,7),rep(2,7),rep(3,7),rep(4,7),rep(5,7),rep(6,7),rep(7,7)),.combine=cbind )%do%(paste(colnames(ABSvec$var@x)[i]))
    name2<-rep(foreach(j=1:length(colnames(ABSvec$var@x)),.combine=cbind)%do%(paste(" to",colnames(ABSvec$var@x)[j],sep=" ")),7)
  }

  if(K==9)  {
    name1<-foreach(i=c(rep(1,9),rep(2,9),rep(3,9),rep(4,9),rep(5,9),rep(6,9),rep(7,9),rep(8,9),rep(9,9)),.combine=cbind )%do%(paste(colnames(ABSvec$var@x)[i]))
    name2<-rep(foreach(j=1:length(colnames(ABSvec$var@x)),.combine=cbind)%do%(paste(" to",colnames(ABSvec$var@x)[j],sep=" ")),9)
  }else{name1<-foreach(i=c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),rep(6,8),rep(7,8),rep(8,8)),.combine=cbind )%do%(paste(colnames(ABSvec$var@x)[i]))
        name2<-rep(foreach(j=1:length(colnames(ABSvec$var@x)),.combine=cbind)%do%(paste(" to",colnames(ABSvec$var@x)[j],sep=" ")),8)}

  # colnames(svec.fevd.A)<-foreach(k=1:K^2)%do%(paste(name1[k],name2[k],sep=""))
  # colnames(svec.fevd.B)<-foreach(k=1:K^2)%do%(paste(name1[k],name2[k],sep=""))
  colnames(svec.fevd.AB)<-foreach(k=1:K^2)%do%(paste(name1[k],name2[k],sep=""))
  #-----------------------------------------------------------------------------##
  # #for(i in names(ASvar$var$varrresult))
  # # svec.irf.ASvec<-irf(ASvec,impulse=(colnames(Asvec.var$y))[1],n.ahead=11)
  # # svec.irf.BSvec<-irf(BSvec,impulse=(colnames(Bsvec.var$y))[1],n.ahead=11)
   svec.irf.ABSvec<-irf(ABSvec,impulse=colnames(ABsvec.var$y)[1],n.ahead=11)
  #
  # # svec.irf.A<-foreach(i=paste(colnames(Asvec.var$y)),.combine=cbind)%do%(cbind(svec.irf.ASvec$Lower[[1]][,i],svec.irf.ASvec$irf[[1]][,i],svec.irf.ASvec$Upper[[1]][,i]))
  # # svec.irf.B<-foreach(i=paste(colnames(Bsvec.var$y)),.combine=cbind)%do%(cbind(svec.irf.BSvec$Lower[[1]][,i],svec.irf.BSvec$irf[[1]][,i],svec.irf.BSvec$Upper[[1]][,i]))
   svec.irf.AB<-foreach(i=paste(colnames(ABsvec.var$y)),.combine=cbind)%do%(cbind(svec.irf.ABSvec$Lower[[1]][,i],svec.irf.ABSvec$irf[[1]][,i],svec.irf.ABSvec$Upper[[1]][,i]))
  # # colnames(svec.irf.A)<-foreach(i =1:length(colnames(Asvec.var$y)),.combine=cbind )%do%(c(paste(names(ASvar$y)[i],"Lower se",sep=" "),paste(names(ASvar$var$varresult)[i],"IRF",sep=" "),paste(names(ASvar$var$varresult)[i],"Upper se",sep=" ")))
  # # colnames(svec.irf.B)<-foreach(i =1:length(colnames(Bsvec.var$y)),.combine=cbind )%do%(c(paste(names(BSvar$y)[i],"Lower se",sep=" "),paste(names(BSvar$y)[i],"IRF",sep=" "),paste(names(BSvar$y)[i],"Upper se",sep=" ")))
   colnames(svec.irf.AB)<-foreach(i =1:length(colnames(ABsvec.var$y)),.combine=cbind )%do%(c(paste(colnames(ABsvec.var$y)[i],"Lower se",sep=" "),paste(colnames(ABsvec.var$y)[i],"IRF",sep=" "),paste(colnames(ABsvec.var$y)[i],"Upper se",sep=" ")))
  # # svec.irf.A<-data.frame(svec.irf.A);svec.irf.B<-data.frame(svec.irf.B);
   svec.irf.AB<-data.frame(svec.irf.AB)
   colnames(svec.irf.AB)<-foreach(i =1:length(colnames(ABsvec.var$y)),.combine=cbind )%do%(c(paste(colnames(ABsvec.var$y)[i],"Lower se",sep=" "),paste(colnames(ABsvec.var$y)[i],"IRF",sep=" "),paste(colnames(ABsvec.var$y)[i],"Upper se",sep=" ")))
  # #plot.irf######
   if(!dir.exists(paste(dir,"/svec.channel",j,"/plot.irf",sep=""))){dir.create(paste(dir,"/svec.channel",j,"/plot.irf",sep=""))}
  #
   if(!getwd()==paste(dir,"/svec.channel",j,"/plot.irf",sep=""))setwd(paste(dir,"/svarchannel",0.2,"/irf.plot",sep=""))
  # # plot(irf(ASvec,impulse=colnames(ASvec$var@x)[1],n.ahead=11,ci=.1))
  # # plot(irf(BSvec,impulse=colnames(BSvec$var@x)[1],n.ahead=11,ci=.1))
   par(cex.lab=.87,lheight=1.1)
   title<-paste("Реакція макрозмінних","на шок",names.plot[im], "за SVEC моделлю",n+1)

    plot(irf(ABSvec,impulse=colnames(ABSvec$var@x)[1],n.ahead=11,ci=.99),main=title, ylab=names.plot[-im]);try(grDevices::savePlot("plot.1.AB SVEC model",type="png"))
   # mar.multi = 2*c(0.1, .7, 1.2, .4),oma.multi = 2.5*c(.5, .4, .5, .4));grDevices::savePlot("plot.1.AB SVEC model",type="png")


{ class(ABSvec)<-"svecest"
  class(svec.irf.ABSvec)<-"svecirf"
  class(svec.fevd.ABSvec)<-"svecfevd"
  call<-match.call()
}
  result=list(
              #ASvec=ASvec,BSvec=BSvec,
              ABSvec=ABSvec,ABsvec.var=ABsvec.var,
              svec.irf.ABSvec=svec.irf.ABSvec,
              svec.fevd.ABSvec=svec.fevd.ABSvec,
              estmod=estmod,call=call,ABsvec=data.frame(ABsvec.AB),ABsvec.coef=data.frame(ABsvec.coef),
              ABsvec.det=data.frame(ABsvec.det),
              ABsvec.res=data.frame(ABsvec.res),
              ABsvec.fit=data.frame(ABsvec.fit),
              svec.irf.AB=svec.irf.AB,
              svec.fevd.AB=data.frame(svec.fevd.AB))
  if(write.xlsx==TRUE){n;if(!getwd()==paste(dir,"/svec.channel",n,sep=""))setwd(paste(dir,"/svec.channel",n,sep=""))
    # WriteXLS::WriteXLS(result$Asvec,"Asvec.A.xlsx",row.names=TRUE,FreezeRow=1,FreezeCol=1)
    # WriteXLS::WriteXLS(result$Bsvec,"Bsvec.B.xlsx",row.names=TRUE,FreezeRow=1,FreezeCol=1)
    WriteXLS::WriteXLS(result$ABsvec,"ABsvec.AB.xlsx",row.names=TRUE,FreezeRow=1,FreezeCol=1)
    WriteXLS::WriteXLS(result$ABsvec.coef,"ABsvec.coef.xlsx",row.names=TRUE,FreezeRow=1,FreezeCol=1)
    # WriteXLS::WriteXLS(result$svec.irf.A,"svec.irf.A.xlsx",row.names=TRUE,FreezeRow=1,FreezeCol=1)
    # WriteXLS::WriteXLS(result$svec.irf.B,"svec.irf.B.xlsx",row.names=TRUE,FreezeRow=1,FreezeCol=1)
    WriteXLS::WriteXLS(result$svec.irf.AB,"svec.irf.AB.xlsx",row.names=TRUE,FreezeRow=1,FreezeCol=1)
    WriteXLS::WriteXLS(result$ABsvec.res,"ABsvec.res.xlsx",row.names=TRUE,FreezeRow=1,FreezeCol=1)
    WriteXLS::WriteXLS(result$ABsvec.det,"ABsvec.det.xlsx",row.names=TRUE,FreezeRow=1,FreezeCol=1)
    WriteXLS::WriteXLS(result$ABsvec.fit,"ABsvec.fit.xlsx",row.names=TRUE,FreezeRow=1,FreezeCol=1)
    # WriteXLS::WriteXLS(result$svec.fevd.A,"svec.fevd.A.xlsx",row.names=TRUE,FreezeRow=1,FreezeCol=1)
    # WriteXLS::WriteXLS(result$svec.fevd.B,"svec.fevd.B.xlsx",row.names=TRUE,FreezeRow=1,FreezeCol=1)
    WriteXLS::WriteXLS(result$svec.fevd.AB,"svec.fevd.AB.xlsx",row.names=TRUE,FreezeRow=1,FreezeCol=1)
  }
  result=list(ABSvec=ABSvec,ABsvec.var=ABsvec.var,id=id,
              svec.irf.ABSvec=svec.irf.ABSvec,
              svec.fevd.ABSvec=svec.fevd.ABSvec,
              estmod=estmod,call=call,ABsvec=as.data.frame(ABsvec.AB),ABsvec.coef=data.frame(ABsvec.coef),
              ABsvec.det=data.frame(ABsvec.det),
              ABsvec.res=data.frame(ABsvec.res),
              ABsvec.fit=data.frame(ABsvec.fit),
              svec.irf.AB=svec.irf.AB,
              svec.fevd.AB=data.frame(svec.fevd.AB),time.execution=time.execution)
{ channel<-paste("result.",n,sep="")
  END<-Sys.time();time.execution<-END-START
  result=list(svec=result,channel=channel,"time execution"=time.execution)
  class(result)<-"svecchannel"
}
#   return(result=list(svec=result,channel=channel,"time execution"=time.execution))
#
# }

assign(paste("svec.channel",n,sep=""),result)

if(n==16){save.image(Dir.svar)}
