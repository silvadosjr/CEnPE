# Para executar este programa basta executar o comando
# "source" abaixo, com o devido diretório
# source("C:\\Users\\cnaber\\Documentos\\Arquivos\\consultoria\\CENPE_UFC\\programas\\TRI\\simulacao\\um grupo\\unidimensional\\SimUUMRG.r")

# limpando a área de trabalho
rm(list = ls(all.names = TRUE))

# carregando os pacotes e funções necessários
library(mirt)
library(data.table)
library(readODS)
library(plotrix)
library(psych)
library(car)
# mudar o diretório abaixo para indicar onde está o arquivo
# "Aux Func IRT.r"
source("C:\\Users\\cnaber\\Documentos\\Arquivos\\consultoria\\CENPE_UFC\\programas\\Aux Func IRT.r")

# fixando a semente aleatória
#set.seed(4142)

## diretórios

# carregar dados
# mudar o diretório para carregar o arquivo com os 
# parâmetros dos itens, veja abaixo
arquivo.carregar <- "C:\\Users\\cnaber\\Documentos\\Arquivos\\consultoria\\CENPE_UFC\\dados\\"

# simulaçãoescolher onde salvar resultados
arquivo.salvar <- "C:\\Users\\cnaber\\Documentos\\Arquivos\\consultoria\\CENPE_UFC\\programas\\TRI\\simulacao\\um grupo\\unidimensional\\result\\MRG\\"

## dados de entrada
# matriz com os parâmetros dos itens
# somente para simulação
mzeta<-read_ods(path=paste(arquivo.carregar,
                           sep="","modelo RG.ods"),
                sheet = 1,
                col_names = TRUE)
n <- 1000 # número de respondentes
ncat<-5
#n<-5000
nI <- nrow(mzeta) # número de itens
vtheta<- c(scale(rnorm(n))) # traços latentes
#
# visualizando os traços latentes simulados
# somente para simulação
pdf(file=paste(arquivo.salvar,sep="","TracosLatentesV.pdf"))
par(mfrow=c(1,2))
hist(vtheta,xlab="traço latente",ylab="densidade",main="",probability=TRUE)
boxplot(vtheta,ylab="traço latente")
dev.off()

# Simulando respostas de um modelo RG
# um único grupo
#
# somente para a simulação
va<- c(mzeta[,2]$a) # discriminação
mb<- as.matrix(mzeta[,3:(ncat+1)]) # dificuldade
ma<- matrix(va,nI,ncat-1)
md<- -va*mb # intercepto
#
# duas formas, ela pode ser indicada a parte  
# (0: se não há resposta e 1: se há resposta)
# ou indicando na matrix mY (acima) a não resposta
# com algumas letra ou número diferente (do gabarito
# ou do 0/1)
mV<-matrix(1,n,nI) # indicação de não resposta
#
# matriz de respostas (NA indica não resposta)
# esta tem de ser carregada, por exemplo
# usando o comando "fread"  (prefiro esse, principalmente 
# de a base de dados estiver em .csv), "read.table"
# read_ods etc..., pode ser tanto uma matriz binária como uma
# matriz com as alternativas (A,B,etc) + o gabarito ou
# as respostas binários (0: incorreto e 1: correto)
# posso fazer um exemplo
mY <- simdata(a=va,d=md,N=n,
              Theta=cbind(vtheta),itemtype='graded')
head(mY)
mYmirt <-mY
mYmirt[mV==0] <- NA
#
pdf(file=paste(arquivo.salvar,sep="","TracosLatentesVEscoreO.pdf"))
par(mfrow=c(1,1))
vescore <- apply(mY*mV,1,sum)
plot(vescore,vtheta,xlab="escore",ylab="traço latente",
     cex=1.2,cex.lab=1.2,cex.main=1.2)
dev.off()
#
# Estimação dos parâmetros dos itens
# Dimensão dos traços latente (F1: uma Dimensão)
# PRIOR: prioris
# 1-30 (indica os itens para os quais devem ser
# assumidos os fatores e prioris)
#model.prior <- mirt.model('F1 = 1-30
#                          PRIOR = (1-30, a1, norm,1,0.5), 
#                          (1-30, d, norm,0,3),
#                          (1-30, g, norm,-1.2,1.2)
#                          LBOUND=(1-30,a1,0)')
#
#model.prior <-mirt.model('F1 = 1-30
#                          PRIOR = (1-30, a1, 
#                          lnorm,-0.2058759,0.6), 
#                          (1-30, d1, norm,0,40),
#                         (1-30, d2, norm,0,40),
#                          (1-30, d3, norm,0,40),
#                         (1-30, d4, norm,0,40)')

model.prior <-mirt.model('F1 = 1-30
                          PRIOR = (1-30, a1, 
                          lnorm,-0.2058759,0.6)')

#
#model.prior <- mirt.model('F1 = 1-30
#                          PRIOR = (1-30, a1, lnorm,-0.2058759,0.7761264), 
#                          (1-30, d, norm,0,4),
#                          (1-30, g, norm,-1.274977,0.1854789)')

# Estimação dos par. dos itens
# mY: base de dados (tem que colocar NA para não resposta)
# model: Dimensão e prioris
# itemtype: Modelo - 3PL
# method: (Pseudo) Algoritmo EM
# SE: calcular o erro-padrão
resultMRG <- mirt(mY,model=model.prior,
                   itemtype='graded',
                   SE=TRUE,method="EM")
resultMRG
#extract.mirt(resultML3P,"G2")
#extract.mirt(resultML3P,"CFI")
#extract.mirt(resultML3P,c("G2","CFI","TLI"))

# Resultados das estimativas dos parâmetros dos itens
# extrair os parâmetros dos itens
resultMRGparite <- as.data.frame(coef(resultMRG,IRTpars=TRUE,
                                       printSE=T,simply=T))
#
# índices úteis para salvar os resultados
# relativos aos parâmetros dos itens
# necessários para gerar alguns gráficos
# abaixo
inda <- (ncat)*seq(1,nI,1)-ncat+1
indb <- matrix(0,ncat-1,nI)
for (i in 1:(ncat-1))
{
indb[i,] <-inda+i   
}
#
# dificuldade
eb <- epb <- matrix(0,ncat-1,nI)
for (i in 1: (ncat-1))
{
eb[i,] <- as.matrix(resultMRGparite[1,indb[i,]]) # estimativa
epb[i,] <- as.numeric(resultMRGparite[2,indb[i,]]) # erro-padrão
}

# discriminação
ea <- as.numeric(resultMRGparite[1,inda]) # estimativa
epa <- as.numeric(resultMRGparite[2,inda]) # erro-padrão
#
# gráficos de dispersão: valores verdadeiros x estimativas
# só no caso de Estimação
pdf(file=paste(arquivo.salvar,sep="","ParItensEDisp.pdf"))
par(mfrow=c(2,3))
plot(va,ea,xlab="valor verdadeiro",ylab="estimativa",main="discriminação",pch=19,cex=1.2,cex.main=1.2,cex.lab=1.2)
abline(0,1,lwd=2,col="blue")
for (i in 1:(ncat-1))
{
plot(mb[,i],eb[i,],xlab="valor verdadeiro",ylab="estimativa",
     main=bquote(paste("dificuldade: b",.(i))),
     pch=19,cex=1.2,cex.main=1.2,cex.lab=1.2)
abline(0,1,lwd=2,col="blue")
}
dev.off()
#
# IC's, valores verdadeiros e estimativas pontuais
# para os dado reais, nao teremos os valores verdadeiros
# comando "abline", abaixo
pdf(file=paste(arquivo.salvar,sep="","ParItensEIC.pdf"))
par(mfrow=c(2,3))
ez=qnorm(0.975)
plotCI(ea,ui=ea+ez*epa,li=ea-ez*epa,pch=19,cex=1.2,
       cex.lab=1.2,cex.main=1.2,
       xlab="item",ylab="estimativa",main="discriminação")
lines(va,type="p",pch=19,col="red",cex=1.2)
abline(0.6,0,lwd=2,lty=2,col="gray")
#
for (i in 1:(ncat-1))
{
plotCI(eb[i,],ui=eb[i,]+ez*epb[i,],
       li=eb[i,]-ez*epb[i,],pch=19,cex=1.2,
       cex.lab=1.2,cex.main=1.2,
       xlab="item",ylab="estimativa",
       main=bquote(paste("dificuldade: b",.(i))))
lines(mb[,i],type="p",pch=19,col="red",cex=1.2)
abline(0,0,lwd=2,lty=2,col="gray")
}
dev.off()
#
## Resultados das estimativas dos traços latentes
#
# dispersão/histograma/boxplot 
# entre estimativas e verdadeiros valores
# resultML3P: objetivo com a estimativa dos parâmetros
# dos itens
# mehtod: EAP - Esperança a Posteriori
# full.scores.SE: erros-padrão das estimativas
# os comandos com "vtheta" abaixo, são apenas no caso de
# valores simulados
rthetaMRG <- fscores(resultMRG,method='EAP',
                      full.scores.SE=TRUE)[,1]
pdf(file=paste(arquivo.salvar,sep="","TracosLatentesE.pdf"))
par(mfrow=c(2,2))
hist(vtheta,probability=TRUE,xlab="traço latente",
     ylab="densidade",main="traços latentes",nclass=12,
     cex.lab=1.2,cex.main=1.2,border="blue",col=NULL)
hist(rthetaMRG,probability=TRUE,xlab="traço latente",
     ylab="densidade",main="traços latentes",
     nclass=12,add=TRUE,border="green",col=NULL)
boxplot(c(vtheta,rthetaMRG)~c(rep("verdadeiro",n),
                               rep("EAP",n)),
        cex=1.2,cex.main=1.2,cex.lab=1.2,
        main="traço latente",xlab="tipo",ylab="valor")
plot(vtheta,rthetaMRG,pch=19,xlab="verdadeiro",
     ylab="estimativa",cex=1.2,cex.lab=1.2,cex.main=1.2)
abline(0,1,lwd=2,col="blue")
# qqplot (normalidade padrão)
qqPlot(c(scale(rthetaMRG)),xlab="quantil N(0,1)",
       dist="norm",mean=0,sd=1,col.lines="blue",grid="FALSE",
         ylab="quantil do traço latente (padronizado)",
         cex=1.2,pch=19)
dev.off()

## Mecanismos de ajuste/estatístiicas de comparação de modelos

## Itens

# gráficos de ajuste
# proporções esperadas e observadas
# itemplot(resultML3P,1,type="infoSE")
# resultML3P: estimativa dos parâmetros dos itens
# rthetaML3P: estimativa dos traços latentes

pdf(file=paste(arquivo.salvar,sep="","CCIPOE.pdf"))
for (i in 1:nI)
{
par(mfrow=c(1,1))
plot(itemfit(resultMRG,group.bins = 10,empirical.plot = i,
        Theta=matrix(rthetaMRG)))
#print(itemfit(resultML3P, empirical.plot=i, 
#              Theta=matrix(rthetaML3P)))
#cat("\n") 
}
dev.off()
#
#itemfit(resultML3P)
#
# estatístiicas de ajuste do modelo
#
# resultML3P: estimativa dos parâmetros dos itens
pdf(file=paste(arquivo.salvar,sep="","S_X2item.pdf"))
# S_X2
par(mfrow=c(1,1))
plot(itemfit(resultMRG,fit_stats="S_X2")[,4],ylim=c(0,1),
     pch=19,xlab="item",ylab="RQEQM_S_X2")
abline(h=0.05,lwd=2,lty=2,col="blue")
abline(h=0.10,lwd=2,lty=3,col="blue")
dev.off()
#
# X2
# resultML3P: estimativa dos parâmetros dos itens
pdf(file=paste(arquivo.salvar,sep="","X2item.pdf"))
par(mfrow=c(1,1))
plot(itemfit(resultMRG,fit_stats="X2")[,4],ylim=c(0,1),
     pch=19,xlab="item",ylab="RQEQM_X2")
abline(h=0.05,lwd=2,lty=2,col="blue")
abline(h=0.10,lwd=2,lty=3,col="blue")
dev.off()
#
# G2
# resultML3P: estimativa dos parâmetros dos itens
pdf(file=paste(arquivo.salvar,sep="","G2item.pdf"))
par(mfrow=c(1,1))
plot(itemfit(resultMRG,fit_stats="G2")[,4],ylim=c(0,1),
     pch=19,xlab="item",ylab="RQEQM_G2")
abline(h=0.05,lwd=2,lty=2,col="blue")
abline(h=0.10,lwd=2,lty=3,col="blue")
dev.off()
# 
# PV_Q1
# resultML3P: estimativa dos parâmetros dos itens
pdf(file=paste(arquivo.salvar,sep="","PV_Q1item.pdf"))
par(mfrow=c(1,1))
plot(itemfit(resultMRG,fit_stats="PV_Q1")[,4],ylim=c(0,1),
     pch=19,xlab="item",ylab="RQEQM_PV_Q1")
abline(h=0.05,lwd=2,lty=2,col="blue")
abline(h=0.10,lwd=2,lty=3,col="blue")
dev.off()
#
# Infit
# resultML3P: estimativa dos parâmetros dos itens
pdf(file=paste(arquivo.salvar,sep="","infititem.pdf"))
par(mfrow=c(1,1))
infout<-itemfit(resultMRG,fit_stats="infit")
plot(infout[,4],
     pch=19,xlab="item",ylab="infit",
     ylim=c(min(0,infout[,4]),max(2.5,infout[,4])))
abline(h=0.5,lwd=2,lty=3,col="blue")
abline(h=1.5,lwd=2,lty=2,col="blue")
abline(h=2,lwd=2,lty=3,col="blue")
abline(h=1,lwd=2,lty=2,col="blue")
dev.off()
#
# Outfit
pdf(file=paste(arquivo.salvar,sep="","outfititem.pdf"))
par(mfrow=c(1,1))
plot(infout[,2],
     pch=19,xlab="item",ylab="outfit",
     ylim=c(min(0,infout[,2]),max(2.5,infout[,2])))
abline(h=0.5,lwd=2,lty=3,col="blue")
abline(h=1.5,lwd=2,lty=2,col="blue")
abline(h=2,lwd=2,lty=3,col="blue")
abline(h=1,lwd=2,lty=2,col="blue")
dev.off()

# Resíduos para verificação da independência local
# resultML3P: estimativa dos parâmetros dos itens
auxresMRG<-residuals(resultMRG,type="LD",table=TRUE)
#
auxnpar <- nI*(nI-1)/2
RMSEA <- matrix(0,auxnpar)
for (i in 1:auxnpar)
{
  rtable <- auxresMRG[[i]]  
  rtableO <-rtable$Obs
  rtableE <-rtable$Exp
  ptableO <-rtableO/sum(rtableO)
  ptableE <-rtableE/sum(rtableE)
  RMSEA[i]<-sqrt(sum((ptableO-ptableE)^2))
}
#
pdf(file=paste(arquivo.salvar,sep="","RQEQMIL.pdf"))
par(mfrow=c(1,1))
plot(RMSEA,xlab="pares de itens",
     ylab="RQEQMIL",pch=19,
     cex=1.2,cex.main=1.2,cex.lab=1.2,
     ylim=c(0,1))
abline(h=0.05,lwd=2,lty=2,col="green")
abline(h=0.10,lwd=2,lty=2,col="green")
dev.off()

#pvalorX2 <- matrix(0,auxnpar)
#pvalorG2 <-matrix(0,auxnpar)
#for (i in 1:auxnpar)
#{
#  rtable <- auxresML3P[[i]]  
#  rtableO <-rtable$Obs
#  rtableE <-rtable$Exp
#  X2 <- sum((rtableO-rtableE)^2/rtableE)
#  G2 <- -2*sum(rtableO*log(rtableE/rtableO))
#  pvalorX2[i]<- 1-pchisq(X2,df=1)
#  pvalorG2[i] <- 1-pchisq(G2,df=1)  
#}
#pdf(file=paste(arquivo.salvar,sep="","ILX2item.pdf"))
#par(mfrow=c(1,1))
#plot(pvalorX2,xlab="pares de itens",
#     ylab="p-valor (estatístiica X2)",pch=19,
#     cex=1.2,cex.main=1.2,cex.lab=1.2)
#abline(h=0.05,lwd=2,lty=2)
#dev.off()
#
#pdf(file=paste(arquivo.salvar,sep="","ILG2item.pdf"))
#par(mfrow=c(1,1))
#plot(pvalorG2,xlab="pares de itens",
#     ylab="p-valor (estatístiica G2)",pch=19,
#     cex=1.2,cex.main=1.2,cex.lab=1.2)
#abline(h=0.05,lwd=2,lty=2)
#dev.off()
#
# Resíduos componente do desvio
# essa função fora criada por mim
# ea: estimativa dos parâmetros de discriminação
# eb: estimativa dos parâmetros de dificuldade
# ec: estimativa dos parâmetros de "acerto casual"
# rthetaML3P: estimativa dos traços latentes

#mP <- prob.tri.MRD(ea,eb,ec,rthetaML3P,1)$m.P
#mrescomdesv<-calc.res.dev.dic.IRT(mY,mV,mP,rthetaML3P,ea,eb,ec)
#
#pdf(file=paste(arquivo.salvar,sep="","RCDItem.pdf"))
#for(i in 1:nI)
#{
#par(mfrow=c(1,2))
#mrescomdesvitem <- mrescomdesv$mresdevitem
#plot(mrescomdesvitem[is.na(mYmirt[,i])==FALSE,i],xlab="indivíduo",
#     ylab="Resíduo componente do desvio",
#     main=paste("item:",sep="",i),
#     ylim=c(min(-3,mrescomdesvitem[is.na(mYmirt[,i])==FALSE,i]),
#            max(3,mrescomdesvitem[is.na(mYmirt[,i])==FALSE,i])),pch=19)
#abline(h=-2,lwd=2,lty=2,col="blue")
#abline(h=0,lwd=2,lty=2,col="blue")
#abline(h=2,lwd=2,lty=2,col="blue")
#
#plot(mP[is.na(mYmirt[,i])==FALSE,i],
#     mrescomdesvitem[is.na(mYmirt[,i])==FALSE,i],xlab="indivíduo",
#     ylab="Resíduo componente do desvio",
#     main=paste("item:",sep="",i),
#     ylim=c(min(-3,mrescomdesvitem[is.na(mYmirt[,i])==FALSE,i]),
#            max(3,mrescomdesvitem[is.na(mYmirt[,i])==FALSE,i])),pch=19)
#abline(h=-2,lwd=2,lty=2,col="blue")
#abline(h=0,lwd=2,lty=2,col="blue")
#abline(h=2,lwd=2,lty=2,col="blue")
#}
#dev.off()
#
#
## indivíduos
# resultML3P: estimativa dos parâmetros dos itens
pdf(file=paste(arquivo.salvar,sep="","outfitindiv.pdf"))
resindiv<-personfit(resultMRG)
par(mfrow=c(1,1))
plot(resindiv[,1],
     pch=19,xlab="indivíduo",ylab="outfit",
     ylim=c(min(0,resindiv[,1]),max(2.5,resindiv[,1])))
abline(h=0.5,lwd=2,lty=3,col="blue")
abline(h=1.5,lwd=2,lty=2,col="blue")
abline(h=2,lwd=2,lty=3,col="blue")
abline(h=1,lwd=2,lty=2,col="blue")
dev.off()
#
pdf(file=paste(arquivo.salvar,sep="","infitindiv.pdf"))
par(mfrow=c(1,1))
plot(resindiv[,3],
     pch=19,xlab="indivíduo",ylab="infit",
     ylim=c(min(0,resindiv[,3]),max(2.5,resindiv[,3])))
abline(h=0.5,lwd=2,lty=3,col="blue")
abline(h=1.5,lwd=2,lty=2,col="blue")
abline(h=2,lwd=2,lty=3,col="blue")
abline(h=1,lwd=2,lty=2,col="blue")
dev.off()
#
pdf(file=paste(arquivo.salvar,sep="","zhindiv.pdf"))
par(mfrow=c(1,1))
plot(resindiv[,5],
     pch=19,xlab="indivíduo",ylab="zh",
     ylim=c(min(-3,resindiv[,5]),max(3,resindiv[,5])))
abline(h=-2,lwd=2,lty=2,col="blue")
abline(h=0,lwd=2,lty=2,col="blue")
abline(h=2,lwd=2,lty=2,col="blue")
dev.off()

