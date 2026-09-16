# source("C:\\Users\\cnaber\\Documentos\\Arquivos\\consultoria\\CENPE_UFC\\programas\\TRI\\simulacao\\Testlet\\Simulando Testlet.r")

# Removing digital residuals
rm(list = ls(all.names = TRUE))

set.seed(4445)

# R packages
library(mirt)
library(readODS)
library(readr)
library(msm)
library(xtable)

# mudar o diretório abaixo para indicar onde está o arquivo
# "Aux Func IRT.r"
source("C:\\Users\\cnaber\\Documentos\\Arquivos\\consultoria\\CENPE_UFC\\programas\\Aux Func IRT.r")

# saving folder
file.save <- "C:\\Users\\cnaber\\Documentos\\Arquivos\\consultoria\\CENPE_UFC\\programas\\TRI\\simulacao\\Testlet\\simulacoes\\"

# necessary inputs
n <- 3000
sigma2tau <- 0.8

# loading necessary files
# item parameters and testlet structure indication
m_inf_item <- read_ods("C:\\Users\\cnaber\\Documentos\\Arquivos\\consultoria\\CENPE_UFC\\programas\\TRI\\simulacao\\Testlet\\Item_Parameters.ods")

head(m_inf_item)
nI <- nrow(m_inf_item)
xtable(m_inf_item)

# mV
mV <- matrix(1,n,nI)
vn <- apply(mV,2,sum)

# Item parameters
a<-m_inf_item$a
b <- m_inf_item$b
d <- -a*b
c <- m_inf_item$c
testlet <- m_inf_item$testlet
#
ma <- cbind(a,a*testlet)

# Latent traits values
m_Sigma_theta_tau <- rbind(cbind(1,0),cbind(0,sigma2tau))
vtheta <- rnorm(n,0,1)

# Testlet Random effects values
vtau <- rnorm(n,0,sqrt(sigma2tau))
  
# Latent matrix required by mirt 
mTheta <- cbind(vtheta,vtau)

# Testlet structure

# Itens 4, 5, 6, 7
v_ind_testlet <-testlet

# Response simulation
dataset<-simdata(a=ma,d=d,itemtype="dich",
                 guess=c,Theta=mTheta)
mY <- dataset
vescore<-apply(mY*mV,1,sum)
# xtable(head(mY))

# Model fit

# factor structure specification
specific<-c(rep(NA,3),rep(1,4),rep(NA,13))
## model specification
##                  PRIOR = (1-20, a1, 
##                          lnorm,-0.2058759,0.6), 
##                          (1-20, d, norm,0,10),
##                         (1-20, g, norm,-1.3,0.7)
#model_testlet <- "G = 1 - 20
#                  CONSTRAIN = (1,a1),(2,a1),
#                  (3,a1),(4,a1,a2),(5,a1,a2),
#                  (6,a1,a2),(7,a1,a2),(8,a1),
#                  (9,a1),(10,a1),(11,a1),
#                  (12,a1),(13,a1),(14,a1),
#                  (15,a1),(16,a1),(17,a1),
#                  (18,a1),(19,a1),(20,a1)
#                 COV = S1*S1"
#
#
#PRIOR = (1-20, a1, 
#         lnorm,-0.2058759,0.6),
#(4-7, a2,lnorm,-0.2058759,
# 0.6),
#(1-20, d, norm,0,10),
#(1-20, g, beta,5,15)
#model_testlet <- mirt.model("G = 1 - 20
#                  CONSTRAIN = (1,a1),(2,a1),
#                  (3,a1),(4,a1,a2),(5,a1,a2),
#                  (6,a1,a2),(7,a1,a2),(8,a1),
#                  (9,a1),(10,a1),(11,a1),
#                  (12,a1),(13,a1),(14,a1),
#                  (15,a1),(16,a1),(17,a1),
#                  (18,a1),(19,a1),(20,a1)
#                  COV = S1*S1")
#
model_testlet <- mirt.model('G = 1 - 20
                  PRIOR = (1-20, a1, 
                  lnorm,-0.2058759,0.6),
                  (4-7, a2,lnorm,-0.2058759,0.6),
                  (1-20, d, norm,0,10),
                  (1-20, g, norm,-1.3,0.7)
                  CONSTRAIN = (4,a1,a2),(5,a1,a2),
                  (6,a1,a2),(7,a1,a2)
                  COV = S1*S1')
#
#simmod <- bfactor(mY, specific, model_testlet,
#                  itemtype='3PL')
simmod <- bfactor(data=mY,model=specific, 
                  model2=model_testlet,
                  itemtype='3PL',SE=T)

#summary(simmod)
coef(simmod)
#coef(simmod,IRTpars=TRUE)
#
#coef(simmod)[4]
#coef(simmod)[10]
#coef(simmod,IRTpars=TRUE)[4]
#coef(simmod)[5]
#coef(simmod,IRTpars=TRUE)[5]
#coef(simmod)[6]
#coef(simmod,IRTpars=TRUE)[6]
#coef(simmod)[7]
#coef(simmod,IRTpars=TRUE)[7]

# Parameter estimates
# Item parameter
resultpar <- as.data.frame(coef(simmod, printSE=T,
                                simply=T)) 
indu <- 5*seq(1,nI,1)
indc <- indu-1
indb<- indc-1
inda <- indb-2
#
mSE<-est.se.IRTpar.KPLmodel(simmod,nI,model='3PL')
  

# dificuldade
eb <- as.numeric(resultpar[1,indb]) # estimativa
epb <- mSE[,2] # erro-padrão
# discriminação
ea <- as.numeric(resultpar[1,inda]) # estimativa
epa <- mSE[,1] # erro-padrão
# acerto casual
ec <-as.numeric(resultpar[1,indc]) # estimativa
epc <- mSE[,3] # erro-padrão
#
eb <- -eb/ea
ec<- 1/(1+exp(-ec))
  
# Testelet random effects variance(s)
eeta<-coef(simmod)$GroupPars

# Item parameters
pdf(file=paste(file.save,sep="","ParItens.pdf"))
par(mfrow=c(2,2))
plot(a,ea,cex=1.2,
     cex.lab=1.2,cex.main=1.2,
     xlab="verdadeiro",ylab="estimativa",
     main="discriminação",
     col="black",
     pch=19)
abline(0,1,lwd=2,lty=2,col="gray")
plot(b,eb,cex=1.2,
     cex.lab=1.2,cex.main=1.2,
     xlab="verdadeiro",ylab="estimativa",
     main="dificuldade",
     col="black",
     pch=19)
abline(0,1,lwd=2,lty=2,col="gray")
plot(c,ec,cex=1.2,cex.lab=1.2,cex.main=1.2,
     xlab="verdadeiro",ylab="estimativa",
     main="acerto casual",
     col="black",
     pch=19)
abline(0,1,lwd=2,lty=2,col="gray")
dev.off()

# Latent traits and Testlet random effects
mthetatau <- fscores(simmod,method='EAP',
                      full.scores.SE=TRUE)
pdf(file=paste(file.save,sep="","TLEAT.pdf"))
par(mfrow=c(2,2))
plot(vtheta,mthetatau[,1],cex=1.2,
     cex.lab=1.2,cex.main=1.2,
     xlab="verdadeiro",ylab="estimativa",
     main="traço latente",
     col="black",
     pch=19)
abline(0,1,lwd=2,lty=2,col="gray")
plot(vtau,mthetatau[,2],cex=1.2,
     cex.lab=1.2,cex.main=1.2,
     xlab="verdadeiro",ylab="estimativa",
     main="efeitos aleatórios teslet",
     col="black",
     pch=19)
abline(0,1,lwd=2,lty=2,col="gray")
#
boxplot(cbind(vtheta,mthetatau[,1]),
        main="traços latentes",
        names=c("simulado","estimado"))
boxplot(cbind(vtau,mthetatau[,2]),
        main="efeitos aleatórios teslet",
        names=c("simulado","estimado"))
dev.off()

# Testlet random effects variance
pdf(file=paste(file.save,sep="","VarEAT.pdf"))
par(mfrow=c(1,1))
plot(1,sigma2tau,cex=1.2,pch=19,col="black",
     xlim=c(0.9,1.3),axes=F,
     ylab="estimativa",xlab="quantidade",
     ylim=c(round(min(sigma2tau,eeta[,5])-0.20,2),
            round(max(sigma2tau,eeta[,5])+0.20,2)),
     main=paste("verdadeiro = ",round(sigma2tau,2),
                ", estimado=",round(eeta[1,5],2)))
plotCI(1.2,eeta[1,5],li=eeta[2,5],ui=eeta[3,5],
       add=TRUE,cex=1.2,pch=19,col="gray")
#lines(1.2,eeta[1,5],cex=1.2,pch=19,col="gray",
#      type="p")
axis(1,at=c(1,1.2),
     labels=c("valor verdadeiro","estimtiva"))
axis(2,at=seq(round(min(sigma2tau,eeta[,5])-0.20,2),
            round(max(sigma2tau,eeta[,5])+0.20,2),
            by=0.1))
dev.off()
#
pdf(file=paste(file.save,sep="","ParItensEIC.pdf"))
par(mfrow=c(2,2))
ez=qnorm(0.975)
plotCI(ea,ui=ea+ez*epa,li=ea-ez*epa,cex=1.2,
       cex.lab=1.2,cex.main=1.2,
       xlab="item",ylab="estimativa",
       main="discriminação",
       col="black",
       pch=19)
lines(a,col="gray",type="p",cex=1.2,pch=17)
plotCI(eb,ui=eb+ez*epb,li=eb-ez*epb,cex=1.2,
       cex.lab=1.2,cex.main=1.2,
       xlab="item",ylab="estimativa",
       main="dificuldade",
       col="black",
       pch=19,xaxt="n")
lines(b,col="gray",type="p",cex=1.2,pch=17)
plotCI(ec,ui=ec+ez*epc,li=ec-ez*epc,cex=1.2,
       cex.lab=1.2,cex.main=1.2,
       xlab="item",ylab="estimativa",
       main="acerto casual",
       col="black",
       pch=19,xaxt="n")
lines(c,col="gray",type="p",cex=1.2,pch=17)
dev.off()

# Ajuste
# Think about
pdf(file=paste(file.save,sep="","CCI.pdf"))
for (i in 1:nI)
{
par(mfrow=c(1,1))
titulo <- paste("CCI para o item : ",sep="",i)
grafico <- plot(simmod,type="trace",
                which.item=i)
grafico$main<-titulo
plot(grafico)  #print(itemfit(resultML3P, empirical.plot=i, 
#              Theta=matrix(rthetaML3P)))
#cat("\n") 
}
dev.off()
#
# Infit
pdf(file=paste(file.save,sep="","Infit.pdf"))
par(mfrow=c(1,1))
infout<-itemfit(simmod,fit_stats="infit")
plot(infout[,4],
     pch=19,xlab="item",ylab="infit",
     ylim=c(min(0,infout[,4]),max(2.5,infout[,4])))
#axis(1,1:nI,labels=nomesitens,las=2,cex=0.5)
abline(h=0.5,lwd=2,lty=3,col="blue")
abline(h=1.5,lwd=2,lty=2,col="blue")
abline(h=2,lwd=2,lty=3,col="blue")
abline(h=1,lwd=2,lty=2,col="blue")
dev.off()
#
# Outfit
pdf(file=paste(file.save,sep="","Outfit.pdf"))
par(mfrow=c(1,1))
plot(infout[,2],
     pch=19,xlab="item",ylab="outfit",
     ylim=c(min(0,infout[,2]),max(2.5,infout[,2])))
#axis(1,1:nI,labels=nomesitens,las=2,cex=0.5)
abline(h=0.5,lwd=2,lty=3,col="blue")
abline(h=1.5,lwd=2,lty=2,col="blue")
abline(h=2,lwd=2,lty=3,col="blue")
abline(h=1,lwd=2,lty=2,col="blue")
dev.off()
#
# Residuos para verificação da independência local
auxres<-residuals(simmod,type="LD",table=TRUE)
#
auxnpar <- nI*(nI-1)/2
RMSEA <- matrix(0,auxnpar)
for (i in 1:auxnpar)
{
  rtable <- auxres[[i]]  
  rtableO <-rtable$Obs
  rtableE <-rtable$Exp
  ptableO <-rtableO/sum(rtableO)
  ptableE <-rtableE/sum(rtableE)
  RMSEA[i]<-sqrt(sum((ptableO-ptableE)^2))
}
#
pdf(file=paste(file.save,sep="","RQEQMIL.pdf"))
par(mfrow=c(1,1))
plot(RMSEA,xlab="pares de itens",
     ylab="RQEQMIL",pch=19,
     cex=1.2,cex.main=1.2,cex.lab=1.2,
     ylim=c(0,1))
abline(h=0.05,lwd=2,lty=2,col="green")
abline(h=0.10,lwd=2,lty=2,col="green")
dev.off()
result_prop<-itemfit(simmod,return.tables = TRUE)
#itemfit(simmod,S_X2.tables=T)[[1]]
#itemfit(simmod)
pdf(file=paste(file.save,sep="","S_X2.pdf"))
par(mfrow=c(1,1))
plot(itemfit(simmod,fit_stats="S_X2")[,5],ylim=c(0,1),pch=19)
abline(h=0.05,col="gray",lwd=2,lty=2)
abline(h=0.90,col="gray",lwd=2,lty=2)
dev.off()
#
pdf(file=paste(file.save,sep="","ProObsEsp.pdf"))
for (i in 1:nI)
{
plot(result_prop$O[[i]]/vn[i],
     result_prop$E[[i]]/vn[i],
     xlab="proporção observada",
     ylab="proporção esperada",pch=19,
     cex=1.2,main=paste("Item : ",i))
abline(0,1,col="gray",lwd=2,lty=2)
}
dev.off()
