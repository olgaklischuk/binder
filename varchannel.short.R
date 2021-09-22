library(vars)
library(readr)
#load####
if(Sys.info()[4]=="MacBook-Air-Ulia.local"){comp="Mac"}else{comp="Asus"}
if(comp=="Mac"){disk="~/OneDrive/"}else{disk="C:/Users/Asus/OneDrive/"}
if(comp=="Asus"){
  OneDrive<-"C:/Users/Asus/OneDrive/Computer/"}else{OneDrive<-"~/OneDrive/Computer/"}
if(comp=="Mac"){Dir.svar<-paste(disk,"Computer/I/svar.code/data/2var.and.svar.RData",sep="")}else{Dir.svar<-"C:/Users/Asus/OneDrive/Computer/I/svar.code/data/2var.and.svar.RData"}
load(Dir.svar)
var=c(TRUE,FALSE)[1];i=4;if(var==TRUE){if(comp=="Mac"){load(paste(disk,"Computer/I/svar.code/data/var.",i,".RData",sep=""))}else{load(paste(disk,"Computer/I/svar.code/data/var.",i,".RData",sep=""))}}else{if(comp=="Mac"){load(paste(disk,"Computer/I/svar.code/data/var.and.svar.RData",sep=""))}else{load(paste(disk,"Computer/I/svar.code/data/var.and.svar.RData",sep=""))}}
var=c(TRUE,FALSE)[1];if(comp=="Mac"){save.image(paste(disk,"Computer/I/svar.code/data/var.",i,".RData",sep=""))}else{save.image(paste("C:/Users/Asus/OneDrive/Computer/I/svar.code/data/var.",i,".RData",sep=""))}
rm(list=ls())
freq=c("monthly","quarterly")[1]
##transforming into numeric####
for(i in c("jd1","jd2","jd3")[3]){for(j in 1:ncol(get(paste("channel",i,sep="")))){#get(paste("channel",i,sep=""))[,j][[1]]
  channeljd3[,j][[1]]=as.numeric(get(paste("channel",i,sep=""))[,j][[1]])}}
channeljd1[,6][[1]]<-as.numeric(channeljd1[,1][[1]])

##model.irf.fevd####137,153#####
if(freq=="monthly"){freq=2}else{freq=1}
for(l in c(1:2)){l
first.loop=c(TRUE,FALSE)[l]
if(first.loop==TRUE){T<-c(43,131)[freq]}else{T<-c(52,157)[freq]}
for(i in c("jd1","jd2","jd3","jd4","jd4.1","jd5","jd5.1","jd5.2","jd6","jd7")[5]){n<-nrow(na.omit(get(paste("channel",i,sep=""))[1:T,]));if(n<10){warning(paste("channel",i,": ","Stop! Number of observations less then needed to satisfying degrees of freedom.",sep=""))}else{
try(assign(paste("Var.lag.number",l,i,sep="."),VARselect(na.omit(get(paste("channel",i,sep=""))[1:n,]),lag.max = 1,type="trend")))
}}
time.start<-Sys.time();for(i in c("jd1","jd2","jd3","jd4", "jd4.1","jd5","jd5.1","jd5.2","jd6","jd7")[5]){if(i=="NBU"){j<-"NBU"}else{j<-parse_number(i)}; n<-nrow(na.omit(get(paste("channel",i,sep=""))[1:T,]))
  try(assign(x = paste("varchannel",l,j,sep="."),value=VAR(na.omit(get(paste("channel",i,sep=""))[1:T,]),p = get(paste("Var.lag.number",l,i,sep="."))$selection[1],type = "trend",season=NULL, exogen = NULL,  ic = c("AIC", "HQ", "SC", "FPE"))))}
time.end<-Sys.time()-time.start;time.end

for(i in c(1:4,4.1,5,5.1,5.2,6,7)[5]){li<-paste(l,i,sep=".")
try(assign(pst("Causality.",li),causality(get(paste("varchannel",li,sep=".")), names(getp("varchannel.",li)$varresult)[1], boot=TRUE, boot.runs=1000)))#varchannel1$y)
j<-ifelse(getp("varchannel.",li)$type=="both",2,ifelse(getp("varchannel.",li)$type=="trend"| getp("varchannel.",li)$type=="const",1,0));p<-getp("varchannel.",li)$p;k<-getp("varchannel.",li)$K
assign(pst("sample.model.",li),list(j=j,p=p,K=k));getp("sample.model.",li)
k1<-get(paste("sample.model",li,sep="."))$K
N<-k1
amat.1<-matrix(NA,N,N)
amat.1[lower.tri(amat.1,diag=FALSE)]<-0
bmat.1<-matrix(NA,N,N)
bmat.1[upper.tri(bmat.1,diag=TRUE)]<-0
# try(assign(pst("identify.",li),identify(getp("varchannel.",li),getp("amat.",1),getp("bmat.",1))))
# assign(pst("coef.",li),getp("identify.",li)$coef.var);assign(pst("sigma.",li),getp("identify.",li)$sigma)
assign(pst("ynames.",li),names(getp("varchannel.",li)$varresult))
assign(pst("table.var",li),data.frame(cbind(data.frame((var.coef<-(var.coef<-summary(getp("varchannel.",li)))$varresult[[k1]]$coefficients))[1:(getp("varchannel.",li)$p*getp("varchannel.",li)$K)+j,],var.cor<-(var.cor<-data.frame((var.cor<-(var.cor<-summary(getp("varchannel.",li))))$corres))[1:(getp("varchannel.",li)$p*getp("varchannel.",li)$K)+j,])))

# assign(pst("irf.",li,".1"),irf(getp("varchannel.",li), impulse = names(getp("varchannel",li)$varresult)[im], response = NULL, n.ahead = 11, ortho = TRUE,
#                               cumulative = FALSE, boot = TRUE, ci = 0.95, runs = 100, seed = NULL))
try(assign(pst("vardecomp.",li,".1"),fevd(getp("varchannel.",li), n.ahead = 12)))
for(i.2 in 1:N){
try(assign(paste("vardecomp.",li,".1.",i.2,sep=""),data.frame(get(pst("vardecomp.",li,".1"))[[i.2]])))}
}
  }
    #in vardecomp first number is a model number, second one is impulse number
##lm##########
endogen4<-model.matrix(data=channeljd4,~0+channeljd4[[c(colnames(channeljd4))[1]]]+channeljd4[[c(colnames(channeljd4))[2]]]+channeljd4[[c(colnames(channeljd4))[3]]]+channeljd4[[c(colnames(channeljd4))[4]]]+channeljd4[[c(colnames(channeljd4))[5]]]+channeljd4[[c(colnames(channeljd4))[6]]]+channeljd4[[c(colnames(channeljd4))[7]]]+channeljd4[[c(colnames(channeljd4))[8]]]+channeljd4[[c(colnames(channeljd4))[9]]])
colnames(endogen4)<-colnames(channeljd4)

model4.5<-lm(data=channeljd4,channeljd4$credit.in.foreign.currency[8:153]~endogen4)
model4.6<-lm(data=channeljd4,channeljd4$deposit.in.foreign.currency[8:153]~endogen4)
model4.8<-lm(data=channeljd4,channeljd4$Official.exch.rate[8:153]~endogen4)

summary(model4.5)
summary(model4.6)
summary(model4.8)

ncol(get(paste("channeljd",i,sep="")))->nvar
for( i in 1:7){ncol(get(paste("channeljd",i,sep="")))->nvar;if(ncol(get(paste("channeljd",i,sep="")))==9) {assign(paste("endogen", i, sep=""), model.matrix(data=get(paste("channeljd",i,sep="")),~0+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[1]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[2]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[3]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[4]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[5]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[6]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[7]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[8]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[9]]]))}else{
  if(nvar==8) {assign(paste("endogen", i, sep=""), model.matrix(data=get(paste("channeljd",i,sep="")),~0+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[1]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[2]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[3]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[4]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[5]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[6]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[7]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[8]]]))
  }else{if(nvar==7) {assign(paste("endogen", i, sep=""), model.matrix(data=get(paste("channeljd",i,sep="")),~0+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[1]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[2]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[3]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[4]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[5]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[6]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[7]]]))
         }
      }else{if(nvar==6) {assign(paste("endogen", i, sep=""), model.matrix(data=get(paste("channeljd",i,sep="")),~0+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[1]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[2]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[3]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[4]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[5]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[6]]]))
  }else{if(nvar==2) {assign(paste("endogen", i, sep=""), model.matrix(data=get(paste("channeljd",i,sep="")),~0+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[1]]]+get(paste("channeljd",i,sep=""))[[c(colnames(get(paste("channeljd",i,sep=""))))[2]]]))
        }
      }
    }
  }
}
  for(i in 1:7){for(j in 1:ncol(get(paste("channeljd",i,sep="")))->nvar) {assign(paste("model",i,j,sep="."),lm(data=get(paste("channeljd", i, sep="")),get(paste("channeljd", i, sep=""))[j,8:153]~get(paste("endogen",i))))}}

  #############

try(assign("vardecomp.1.5.1.1",fevd(varchannel.1.5.1,n.ahead = 12)))
vardecomp.1.1.1.6<-vardecomp.1.1.1.6*100#REER shock transmits
vardecomp.1.2.1.6<-vardecomp.1.2.1.6*100#REER shock transmits
vardecomp.1.3.1.6<-vardecomp.1.3.1.6*100#REER shock transmits
vardecomp.1.4.1.6<-vardecomp.1.4.1.6*100#REER shock transmits
vardecomp.1.5.1.3<-format(vardecomp.1.5.1.3*100,digits=2)#REER shock transmits
vardecomp.1.5.1.5<-format(vardecomp.1.5.1.5*100,digits=2)#Sovereign defoult risk premium transmits
vardecomp.1.5.1.1.4<-format(vardecomp.1.5.1.1.4*100,justify = "centre",digits=2)
vardecomp.1.5.1.1.8<-format(vardecomp.1.5.1.1.8*100,justify = "centre",digits=2)
vardecomp.1.5.2.1.3<-format(vardecomp.1.5.2.1.3*100,digits=2)#REER shock transmits
vardecomp.1.5.2.1.5<-format(vardecomp.1.5.2.1.5*100,digits=2)#Sovereign defoult risk premium transmits
vardecomp.1.6.1.1<-format(vardecomp.1.6.1.1*100,justify = "centre",digits=2)
vardecomp.1.7.1.1<-format(vardecomp.1.7.1.1*100,justify = "centre",digits=2)

vardecomp.2.1.1.6<-vardecomp.2.1.1.6*100#REER shock transmits
vardecomp.2.2.1.6<-vardecomp.2.2.1.6*100#REER shock transmits
vardecomp.2.3.1.6<-vardecomp.2.3.1.6*100#REER shock transmits
vardecomp.2.4.1.6<-vardecomp.2.4.1.6*100#REER shock transmits
vardecomp.2.5.1.3<-format(vardecomp.2.5.1.3*100,digits=2) # 2 model asset prices=1.02-17.3
vardecomp.2.5.1.5<-format(vardecomp.2.5.1.5*100,digits=2)#
#asset.price shock transmission after second bootstrap in 5.1 channel
vardecomp.2.5.1.1.4<-format(vardecomp.2.5.1.1.4*100,justify = "centre",digits=2)
vardecomp.2.5.1.1.8<-format(vardecomp.2.5.1.1.8*100,justify = "centre",digits=2)
vardecomp.2.6.1.1<-format(vardecomp.2.6.1.1*100,justify = "centre",digits=2)
vardecomp.2.7.1.1<-format(vardecomp.2.7.1.1*100,justify = "centre",digits=2)
cor1<-data.frame(cor(na.omit(transmission[,c("bond.yields.h","Official.exch.rate","REER","NEER")])))

setwd(paste(disk,"Computer/I/svar.code/result2",sep=""));WriteXLS::WriteXLS(vardecomp.1.1.1.6,"Vardecomp.channel.1.firstboot.of 6 impulse.xlsx",SheetNames = "vardecomp.1.1.1.6")
WriteXLS::WriteXLS(vardecomp.1.2.1.6,"Vardecomp.channel.2.firstboot.of 6 impulse.xlsx",SheetNames = "vardecomp.1.2.1.6")
WriteXLS::WriteXLS(vardecomp.1.3.1.6,"Vardecomp.channel.3.firstboot.of 6 impulse.xlsx",SheetNames = "vardecomp.1.3.1.6")
WriteXLS::WriteXLS(vardecomp.1.4.1.6,"Vardecomp.channel.4.firstboot.of 6 impulse.xlsx",SheetNames = "vardecomp.1.4.1.6")
WriteXLS::WriteXLS(vardecomp.1.5.1.3,"Vardecomp.channel.5.firstboot.of 3 impulse.xlsx",SheetNames = "vardecomp.1.5.1.3")
WriteXLS::WriteXLS(vardecomp.1.5.1.1.3,"Vardecomp.channel.5.1.firstboot.of 3 impulse.xlsx",SheetNames = "vardecomp.1.5.1.1.3")
WriteXLS::WriteXLS(vardecomp.1.5.1.1.4,"Vardecomp.channel.5.1.firstboot.of 4 impulse.xlsx",SheetNames = "vardecomp.1.5.1.1.4")
WriteXLS::WriteXLS(vardecomp.1.5.1.1.8,"Vardecomp.channel.5.1.firstboot.of 8 impulse.xlsx",SheetNames = "vardecomp.1.5.1.1.8")
WriteXLS::WriteXLS(vardecomp.1.6.1.1,"Vardecomp.channel.6.1.firstboot.of 1 impulse.xlsx",SheetNames = "vardecomp.1.6.1.1")
WriteXLS::WriteXLS(vardecomp.1.7.1.1,"Vardecomp.channel.7.1.firstboot.of 1 impulse.xlsx",SheetNames = "vardecomp.1.7.1.1")
WriteXLS::WriteXLS(cor1,"Correlation between bond.yields and Official.exch.rate.xlsx",SheetNames = "correlation")

setwd(paste(disk,"Computer/I/svar.code/result2",sep=""));WriteXLS::WriteXLS(vardecomp.2.1.1.6,"Vardecomp.channel.1.secondboot.of 6 impulse.xlsx",SheetNames = "vardecomp.2.1.1.6")
WriteXLS::WriteXLS(vardecomp.2.2.1.6,"Vardecomp.channel.2.secondboot.of 6 impulse.xlsx",SheetNames = "vardecomp.2.2.1.6")
WriteXLS::WriteXLS(vardecomp.2.3.1.6,"Vardecomp.channel.3.secondboot.of 6 impulse.xlsx",SheetNames = "vardecomp.2.3.1.6")
WriteXLS::WriteXLS(vardecomp.2.4.1.6,"Vardecomp.channel.4.secondboot.of 6 impulse.xlsx",SheetNames = "vardecomp.2.4.1.6")
WriteXLS::WriteXLS(vardecomp.2.5.1.3,"Vardecomp.channel.5.secondboot.of 3 impulse.xlsx",SheetNames = "vardecomp.2.5.1.3")
WriteXLS::WriteXLS(vardecomp.2.5.1.1.3,"Vardecomp.channel.5.1.secondtboot.of 3 impulse.xlsx",SheetNames = "vardecomp.2.5.1.1.3")
WriteXLS::WriteXLS(vardecomp.2.5.1.1.4,"Vardecomp.channel.5.1.secondboot.of 4 impulse.xlsx",SheetNames = "vardecomp.2.5.1.1.4")
WriteXLS::WriteXLS(vardecomp.2.5.1.1.8,"Vardecomp.channel.5.1.secondboot.of 8 impulse.xlsx",SheetNames = "vardecomp.2.5.1.1.8")
WriteXLS::WriteXLS(vardecomp.2.6.1.1,"Vardecomp.channel.6.1.secondboot.of 1 impulse.xlsx",SheetNames = "vardecomp.2.6.1.1")
WriteXLS::WriteXLS(vardecomp.2.7.1.1,"Vardecomp.channel.7.1.secondboot.of 1 impulse.xlsx",SheetNames = "vardecomp.2.7.1.1")

#tests####
for(i in c(1:5,"5.1","5.2",6:7)[4])
{
  assign(pst("archlm.test.treatment",i,".1"),arch.test(getp("varchannel.",li), lags.single = 16, lags.multi = 5,
                                                       multivariate.only = FALSE))
  assign(pst("jarque.berra.test.treatment",i,".1"),normality.test(getp("varchannel.",li), multivariate.only = FALSE))
  assign(pst("portmanteau.test.treatment",i,".1"),serial.test(getp("varchannel.",li), lags.pt = 16, lags.bg = 5,
                                                              type = "PT.asymptotic"))
  assign(pst("stability.test.treatment",i,".1"),stability(getp("varchannel.",li), type = "OLS-CUSUM",
                                                          h = 0.15, dynamic = FALSE, rescale = TRUE))}

  i=="NBU"
  assign(pst("archlm.test.treatment",i,".1"),arch.test(getp("varchannel",i), lags.single = 16, lags.multi = 5,
                                                       multivariate.only = FALSE))
  assign(pst("jarque.berra.test.treatment",i,".1"),normality.test(getp("varchannel",i), multivariate.only = FALSE))
  assign(pst("portmanteau.test.treatment",i,".1"),serial.test(getp("varchannel",i), lags.pt = 16, lags.bg = 5,
                                                              type = "PT.asymptotic"))
  assign(pst("stability.test.treatment",i,".1"),stability(getp("varchannel",i), type = "OLS-CUSUM",
                                                          h = 0.15, dynamic = FALSE, rescale = TRUE))
for(i in c(1:12,14:16,"15.extension"))
{
  assign(pst("archlm.test.treatment",i,".1"),arch.test(getp("varchannel",i), lags.single = 16, lags.multi = 5,
                                                       multivariate.only = FALSE))
  assign(pst("jarque.berra.test.treatment",i,".1"),normality.test(getp("varchannel",i), multivariate.only = FALSE))
  assign(pst("portmanteau.test.treatment",i,".1"),serial.test(getp("varchannel",i), lags.pt = 16, lags.bg = 5,
                                                              type = "PT.asymptotic"))
  assign(pst("stability.test.treatment",i,".1"),stability(getp("varchannel",i), type = "OLS-CUSUM",
                                                          h = 0.15, dynamic = FALSE, rescale = TRUE))}

  for(test in c("archlm","jarque.berra","portmanteau","stability")[1:4]){i="NBU";if(!test=="stability"){print(get(paste(test,".test.treatment",i,".1",sep="")))}else{plot.new();plot(get(paste(test,".test.treatment",i,".1",sep="")))}}
  for(test in c("archlm","jarque.berra","portmanteau","stability")[1:4]){for(i in c(1:12,14:16,"16.extension")){if(!test=="stability"){print(get(paste(test,".test.treatment",i,".1",sep="")))}else{plot.new();plot(get(paste(test,".test.treatment",i,".1",sep="")))}}}
  for(test in c("archlm","jarque.berra","portmanteau","stability")[1:4]){i="NBU";if(!test=="stability"){
          if(test=="archlm"){for(uni in c(paste(c(rep("arch",2),rep("jb",2)),c(".uni",".multi"),sep=""),"serial")[1:2]){for(k in 1:getp("varchannel",i)$K){
            assign(paste(test,uni,i,sep=""),cbind(get(paste(test,".test.treatment",i,".1",sep=""))[[uni]][[k]][[1]],(get(paste(test,".test.treatment",i,".1",sep=""))[[uni]][[k]][[3]])))}}
              }else{if(test=="jarque.berra"){for(uni in c(paste(c(rep("arch",2),rep("jb",2)),c(".uni",".multi"),sep=""),"serial")[3:4]){for(k in 1:getp("varchannel",i)$K){assign(paste(test,uni,i,sep=""),cbind(get(paste(test,".test.treatment",i,".1",sep=""))[[uni]][[k]][[1]],(get(paste(test,".test.treatment",i,".1",sep=""))[[uni]][[k]][[3]])))}}}
                }else{for(uni in "serial"){for(k in 1:getp("varchannel",i)$K){assign(paste(test,uni,i,sep=""),cbind(get(paste(test,".test.treatment",i,".1",sep=""))[[uni]][[k]][[1]],(get(paste(test,".test.treatment",i,".1",sep=""))[[uni]][[k]][[3]])))}}}
                  }else{for(uni in "stability"){for(k in 1:getp("varchannel",i)$K){assign(paste(test,i,sep=""),data.frame(get(paste(test,".test.treatment",i,".1",sep=""))[["stability"]][[k]][["process"]]))}}}}
  for(i in c(1:12, 14:16,"15.extension"))
          {for(test in c("archlm","jarque.berra","portmanteau","stability")){if(!test=="stability"){
            if(test=="archlm"){for(uni in c(paste(c(rep("arch",2),rep("jb",2)),c(".uni",".multi"),sep=""),"serial")[1:2]){for(k in 1:getp("varchannel",i)){
              assign(paste(test,uni,i,sep=""),cbind(get(paste(test,".test.treatment",i,".1",sep=""))[[uni]][[k]][[1]],(get(paste(test,".test.treatment",i,".1",sep=""))[[uni]][[k]][[3]])))}
                }}else{if(test=="jarque.berra"){for(uni in c(paste(c(rep("arch",2),rep("jb",2)),c(".uni",".multi"),sep=""),"serial")[3:4]){for(k in 1:getp("varchannel",i)){assign(paste(test,uni,i,sep=""),cbind(get(paste(test,".test.treatment",i,".1",sep=""))[[uni]][[k]][[1]],(get(paste(test,".test.treatment",i,".1",sep=""))[[uni]][[k]][[3]])))}}
                  }else{for(uni in "serial"){for(k in 1:getp("varchannel",i)$K){assign(paste(test,uni,i,sep=""),cbind(get(paste(test,".test.treatment",i,".1",sep=""))[[uni]][[k]][[1]],(get(paste(test,".test.treatment",i,".1",sep=""))[[uni]][[k]][[3]])))}}
                    }}else{for(uni in "serial"){for(k in 1:getp("varchannel",i)$K){assign(paste(test,i,sep=""),data.frame(get(paste(test,".test.treatment",i,".1",sep=""))[["stability"]][[k]][["process"]]))}}}}}}
#end####
if(var==TRUE){ i<-2
if(comp=="Mac"){save.image(paste(disk,"Computer/I/svar.code/data/var.",i,".RData",sep=""))}else{save.image(paste("C:/Users/Asus/OneDrive/Computer/I/svar.code/data/var.",i,".RData",sep=""))}
}else{
if(comp=="Mac"){save.image(paste(disk,"Computer/I/svar.code/data/var.and.svar.RData",sep=""))}else{save.image("C:/Users/Asus/OneDrive/Computer/I/svar.code/data/var.and.svar.RData")}}

if(comp=="Mac"){Dir.svar<-paste(disk,"Computer/I/svar.code/data/var.and.svar.RData",sep="")}else{Dir.svar<-"C:/Users/Asus/OneDrive/Computer/I/svar.code/data/var.and.svar.RData"}
save.image(Dir.svar)

rm(list=ls())
q(save="no",1,FALSE)

###Upload########
library(readxl)
database="mtm.1"
if(!exists("transmission")){
if(database=="mtm.1"|database=="MTM"){n1<-2; n2<-159+4}else{n2=53}
if(database=="MTM"){
oneDrive<-TRUE;if(oneDrive==FALSE){metadata<-read_excel("J:/.../Monetary transmission mechanism in Ukraine.xlsx",
                                                        sheet = "Metadata",skip=0)}else{metadata<-read_excel(paste(strsplit(OneDrive,split="/Computer")[[1]][1],"/Thesis Ph.D (2)/Statistical appendicies/Monetary transmission mechanism in Ukraine compare.xlsx",sep=""),
                                                                                                             sheet = "Metadata",skip=0)}}else{
if(database=="mtm.1"){
oneDrive<-TRUE
if(oneDrive==FALSE){
  transmission <- readxl::read_excel("J:/.../Monetary transmission mechanism in Ukraine.xlsx",
                                     sheet = "Sheet1",skip=0)[1:153,1:159]}else{ #Metadata#Sheet1#Sheet1..
                                       transmission <- readxl::read_excel(paste(disk,"Thesis Ph.D (2)/Statistical appendicies/Monetary transmission mechanism in Ukraine compare.xlsx",sep=""),
                                                                                sheet = "Sheet1")[1:n2,]}}else{
if(oneDrive==FALSE){
  transmission <- readxl::read_excel("J:/... 2.xlsx",
                                     sheet = "Sheet3.2",skip=0)[1:n2,1:372]}else{ #Metadata#Sheet1#Sheet1..
                                       transmission <- readxl::read_excel(paste(disk,
                                       "Thesis Ph.D (2)/Statistical appendicies/?????????? ????????????? ???????? 2.xlsx",sep=""),sheet="Sheet3.2")[1:n2,1:339]}
                                         }
                                        }

transmission<-transmission[1:n2,];
if(database=="mtm.2"){colnames(transmission)[c(78,126,120:121,87,88,111)]<-c("deposit.in.foreign.currency","Budget.deficit","net.intervention","reserves","bond.yields","Sovereign.Risk.Premium","Direct.investment,.net")}else{
  transmission<-transmission[,-128]
transmission<-transmission[,-c(136:138)]
colnames(transmission[,98])<-"deposit.in.foreign.currency.change"
colnames(transmission[,78])<-"deposit.in.foreign.currency.volume"
colnames(transmission[,131])<-"Budget.deficit"
colnames(transmission)[89]<-"bond.yields"
colnames(transmission)[90]<-"Sovereign.Risk.Premium"
}
transmission[["period"]]<-as.Date(transmission[,1][[1]],origin="1899-12-30")

##short form of data channels#########
form=c("short","long")[2];boot=1
if(form=="short"){if(database=="mtm.1"){
j=0
channeljd1<- transmission[1:(n2-j), c("cpi.a","Interest.gap","Direct.investment,.net","Placed.state.contingent.bonds","Balance.of.payment",#"Budget.deficit",
                                       "REER")]#7  lag=4
channeljd2<- transmission[1:(n2-j), c("cpi.a","Interest.gap","Direct.investment,.net","Placed.state.contingent.bonds","Balance.of.payment",#"Budget.deficit",
                                       "NEER")]#7  lag=4
channeljd3<- transmission[1:n2, c("deflator","Interest.gap","Direct.investment,.net","Bonds.issued.by.non-deposit.corporations","State.Contingent.Bonds",#"Budget.deficit",
                                  "REER")]#6  lag=4
channeljd4<- transmission[1:n2, c("Discount.rate" ,"real.rate","Interest.gap",#"Interest.rate.differential",
                                 "credit.of.corporations","credit.of.households","Construction","asset.price","REER")]#8  lag=2
channeljd5<- transmission[1:n2,c("bond.yields.h","UFOTCTS",c("Official.exch.rate.per.uah","REER")[2],"asset.price","Sovereign.Risk.Premium.h")]
channeljd6<- transmission[1:n2,c("Reer.volality1_.ln","Balance.of.payment","Exp","Imp","Direct.investment,.net","reserves,.mln..usd","net.intervention,.mln.usd")];colnames(channeljd6)<-c("Reer.volality_.ln",	"Dln.Balance.of.payment",	"DlnExp",	"DlnImp",	"Direct.investment_.net",	"Dlnreserves_.mln..usd",	"Dlnnet.intervention_.mln..usd")
channeljd7<- transmission[1:n2,c("REER","Deposit,.%","Credit,.%","deposit.in.foreign.currency","credit.in.foreign.currency")];colnames(channeljd7)<-c("REER",	"Dln.Deposit_..",	"Dln.Credit_..",	"dlndep_for_curr",	"dlncredit_for_curr")
for(m in c(2:4,6:7)){assign(names(channeljd6[,m]),dln(na.omit(channeljd6[,m][[1]]),1))}
for(m in names(channeljd6)[c(2:4,6:7)]){show(get(paste(m)))}
for(m in c(2:5)){assign(names(channeljd7[,m]),dln(na.omit(channeljd7[,m][[1]]),1))}
for(m in names(channeljd7)[c(2:5)]){show(get(paste(m)))}
channeljd6[4:153,c(2:4,6:7)]<-c(Dln.Balance.of.payment, DlnExp, DlnImp, Dlnreserves_.mln..usd, Dlnnet.intervention_.mln..usd)
channeljd7[3:153,c(2:5)]<-c(Dln.Deposit_..,Dln.Credit_..,dlndep_for_curr,dlncredit_for_curr)
channeljd6<- channeljd6[4:153,]
channeljd7<- channeljd7[4:153,]
}else{

  # for(i in c("bond.yields.h",	"UFOTCTS",	"Official.exch.rate",	"NEER",	"ln(reer volality)",	"Bonds.in.portfolioes",	"Bonds.issued.in.Repo",	"Sovereign.Risk.Premium.h")){
#   transmission[,paste("dln",i,sep="")]<-c(rep(NA,153-length(log2(diff(na.omit(transmission[,i]))))+1),log2(diff(na.omit(transmission[,i]))))
# }
if(database=="mtm.2"){if(boot==1){channeljd5.1<-transmission[,c("dlnbond.yields",	"dlnUFOTCTS",	"dlnOfficial.exch.rate",	"NEER",	"Reer.volality1_.ln",	"Bonds.in.portfolioes_.mln..uah",	"dln.Bonds.issued.in.Repo",	"dln.Sovereign.Risk.Premium.h", c("cpi.a","ccpi.a")[1])]}else{
channeljd5.1<-transmission[,c("dlnbondyields",	"dlnUFOTCTS",	"DlnOfficial exch rate per 100 usd",	"NEER",	"Reer volality_ ln",	"dln Bonds in portfolioes_ mln. uah",	"dln Bonds issued in Repo",	"dln UA Sovereign Risk Premium", c("cpi.a","ccpi.a")[1])]
channeljd5.2<-transmission[,c("bond yields",	"UFOTCTS",	"Official exch rate per 100 usd",	"NEER",	"REER",	"Bonds in portfolioes_ mln. uah",	"Bonds issued in Repo",	"UA Sovereign Risk Premium")]
# for(i in 1:ncol(channeljd5)){channeljd5[[i]][]<-diffinv(channeljd5[[i]],differences=1)[2:153]}#154 rows
                                  }
                              }
                         }

####first model data channels##########
}else{model=1;svar<-NULL
channelNBU <-transmission[1:n2, c("Discount.rate" ,"Deviation.from.overnight.rate" ,
                                "Weighed.rate.of.refinancing.",
                                "Deposit.rate" ,
                                "Real.GDP.gap","pi.e","NEER","net.intervention","reserves")]
channeljd1 <- transmission[1:n2, c("Discount.rate" ,"real.rate","Interest.gap",#"Interest.rate.differential",
                                   "credit.of.corporations","credit.of.households","Construction","asset.price","REER")]#8  lag=2
channeljd2 <- transmission[1:n2, c("npl","bank.liquidity","Real.GDP","Real.GDP.gap","cpi.a","ppi.a","ccpi","rule.rate")]#8  lag=1

channeljd3 <- transmission[1:n2, c("Discount.rate", "d(ln(M3_a))","rr",#"Average.refinancing.rate",
                                     "tender.rate","overnight.rate","repo.rate","Weighed.interbank.rate","bond.yields")]#8  lag=2
channeljd4 <- transmission[1:n2, c("Sovereign.Risk.Premium.h","Interest.trend",
                                   "Savings","credit.in.national.currency","credit.in.foreign.currency", "deposit.in.national.currency","deposit.in.foreign.currency",
                                   "Official.exch.rate","cpi.a")];#df4 #7 df=15*14*2 #n=7 lag=3
channeljd4.1 <- transmission[1:n2, c("Money.stock",
                                   "credit.change","credit.in.national.currency","credit.in.foreign.currency","deposit.change", "deposit.in.national.currency","deposit.in.foreign.currency",
                                   "Official.exch.rate","cpi.a")];#df4 #7 df=15*14*2 #n=7 lag=3

channeljd5 <- transmission[1:n2, c("Sovereign.Risk.Premium.h","Interest.trend",
                                   "Savings","credit.in.national.currency","credit.in.foreign.currency",
                                   "Real.GDP","cpi.a")];#df4 #7 df=15*14*2 #n=7 lag=3
channeljd6 <- transmission[1:n2, c("REER","asset.price","Construction","Savings","credit.change","npl","exch.pos.loss.risk.m","bank.loss.risk")]#8  lag=4
channeljd7 <- transmission[1:n2, c("REER","pi.e","trade.balance",c("imported.inflation","Real.GDP")[2],"food.prices","administered.prices","ppi.a","ccpi",c("rule.rate","Interest.rate.differential")[2])]#9  lag=1
channeljd8 <- transmission[1:n2, c("Real.GDP","Real.GDP.gap","cpi.a","ppi.a","deflator","Discount.rate","Interest.gap","Monetary.policy.shock")]#8  lag=2
channeljd9 <- transmission[1:n2, c("deflator","Interest.gap","Direct.investment,.net","Bonds.issued.by.non-deposit.corporations","State.Contingent.Bonds",#"Budget.deficit",
                                  "REER")]#6  lag=4
channeljd10 <- transmission[1:n2, c("pi.e","REER","asset.price","Construction","Savings","Consumption","credit.change",
                                   "Wages","shares","Real.GDP","Real.GDP.gap","cpi.a","Discount.rate")[1:9]]#13-4  lag=0
channeljd11 <-transmission[1:n2, c("Wages","shares","Consumption","REER","cpi.a","Discount.rate")]#13-4  lag=0
if(!is.null(svar)){
if(model==1){channeljd11<-cbind(svarchannel1$ABsvar.fit$REER,channeljd2[1:150+3,"npl"]);colnames(channeljd11)<-c("REER","npl")
channeljd12<-cbind(svarchannel3$ABsvar.fit$bond.yields.h,channeljd4[5:150+3,"Sovereign.Risk.Premium.h"]);colnames(channeljd12)<-c("bond.yields.h","Sovereign.Risk.Premium.h")
channeljd14<-cbind(svarchannel6$ABsvar.fit$rule.rate,channeljd7[5:150+3,"Real.GDP"]);colnames(channeljd14)<-c("rule.rate","Real.GDP")
channeljd15<-transmission[1:n2, c("exch.pos.loss.risk.m","REER")]}else{
channeljd11<-cbind(svarchannel.2.1$ABsvar.fit$REER,channeljd2[1:150+3,"npl"]);colnames(channeljd11)<-c("REER","npl")
channeljd12<-cbind(svarchannel.2.3$ABsvar.fit$bond.yields.h,channeljd4[1:150+3,"Sovereign.Risk.Premium.h"]);colnames(channeljd12)<-c("bond.yields.h","Sovereign.Risk.Premium.h")
channeljd14<-cbind(svarchannel.2.6$ABsvar.fit$rule.rate,channeljd7[1:150+3,"Real.GDP"]);colnames(channeljd14)<-c("rule.rate","Real.GDP")
channeljd15<- transmission[1:n2, c("exch.pos.loss.risk.m","REER")]}}
Samples.sizes<-data.frame(nrow(channelNBU),nrow(channeljd1),nrow(channeljd2),nrow(channeljd3),
                          nrow(channeljd4),nrow(channeljd5),nrow(channeljd6),
                          nrow(channeljd7),nrow(channeljd8),nrow(channeljd9))
namess<-c("exch.pos.loss.risk.m","bank.loss.risk","capital.risk","npl","bank.liquidity")
normativ<-transmission[1:n2,c("exch.pos.loss.risk.m","bank.loss.risk","capital.risk","npl","bank.liquidity")]
Normativ<-model.matrix(data=normativ,~0+normativ[["exch.pos.loss.risk.m"]]+normativ[["bank.loss.risk"]]+normativ[["capital.risk"]]+normativ[["npl"]]+normativ[["bank.liquidity"]]);colnames(Normativ)<-c(namess[1:5])
REER<-transmission[1:135,"REER"];REER.B.CRISIS<-ifelse(exp(REER) > 0&exp(REER) < 1, ifelse(exp(REER) < 0.5,1,.5), 0);REER.B.revaluation<-ifelse(exp(REER) >1, 1, 0)}
}
######cleanced data##################################################
which(is.na(channeljd1),arr.ind=TRUE);which(is.na(channeljd2),arr.ind=TRUE);which(is.na(channeljd3),arr.ind=TRUE);which(is.na(channeljd4),arr.ind=TRUE)
which(is.na(channeljd5),arr.ind=TRUE);which(is.na(channeljd6),arr.ind=TRUE);which(is.na(channeljd7),arr.ind=TRUE);which(is.na(channeljd8),arr.ind=TRUE);which(is.na(channeljd9),arr.ind=TRUE)
####
channeljd6<-transmission[1:n2,c("Monetary.policy.shock","UFOTCTS",c("Official.exch.rate.per.uah","REER")[2],"asset.price","Sovereign.Risk.Premium.h")]
channeljd7<-transmission[1:n2,c("bond.yields.h","Official.exch.rate")]
#end####
if(var==TRUE){ i<-4
if(comp=="Mac"){save.image(paste(disk,"Computer/I/svar.code/data/var.",4,".RData",sep=""))}else{save.image(paste("C:/Users/Asus/OneDrive/Computer/I/svar.code/data/var.",i,".RData",sep=""))}
}else{
  if(comp=="Mac"){save.image(paste(disk,"Computer/I/svar.code/data/var.and.svar.RData",sep=""))}else{save.image("C:/Users/Asus/OneDrive/Computer/I/svar.code/data/var.and.svar.RData")}}

if(comp=="Mac"){Dir.svar<-paste(disk,"Computer/I/svar.code/data/var.and.svar.RData",sep="")}else{Dir.svar<-"C:/Users/Asus/OneDrive/Computer/I/svar.code/data/var.and.svar.RData"}
save.image(Dir.svar)

rm(list=ls())
q(save="no",1,FALSE)

