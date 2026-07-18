# =======================================================
# 📦 1) PACOTES E PREPARO DO AMBIENTE
# =======================================================
setwd('~/GitHub/CEnPE')

# Lista de pacotes necessários
pacotes <- c(
  "mirt", "data.table", "car", "xtable", "readODS", "e1071", "haven",
  "foreign", "dplyr", "plyr", "openxlsx", "plotrix", "readxl", "tidyr",
  "readr", "stringr", "ggplot2", "reshape2", "here"
)

# Instala pacotes ausentes
novos_pacotes <- pacotes[!(pacotes %in% installed.packages()[, "Package"])]
if (length(novos_pacotes)) install.packages(novos_pacotes)

# Carrega pacotes
invisible(lapply(pacotes, require, character.only = TRUE))

# =======================================================
# 📁 CAMINHOS DO PROJETO
# =======================================================

# O pacote here() está fixando a raiz em:
# C:/Users/Usuário/OneDrive/Documentos/GitHub/CEnPE
root_dir <- here::here()

# Pasta principal do projeto Piauí 2026
proj_dir <- file.path(root_dir, "Piaui", "2026")

# Pasta dos dados
dados_dir <- file.path(proj_dir, "Dados")

# Pasta principal de saída dos resultados
folder <- "ResultadosTRI_2EM_LP"

# Subpasta específica desta análise
subfolder <- "Simulado_I"

# Diretório final de saída
result_dir <- file.path(proj_dir, folder, subfolder)

# Cria a pasta de resultados, caso não exista
dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

# Carrega funções auxiliares do IRT
source(file.path(root_dir, "Piaui", "Aux Func IRT.r"))

# Conferência dos caminhos
cat("\n==============================\n")
cat("Caminhos utilizados\n")
cat("==============================\n")
cat("Raiz do projeto:     ", root_dir, "\n")
cat("Projeto Piauí 2026:  ", proj_dir, "\n")
cat("Pasta dos dados:     ", dados_dir, "\n")
cat("Pasta dos resultados:", result_dir, "\n")
cat("==============================\n\n")

# =======================================================
# 🧱 2) ENTRADA DE DADOS E PRÉ-PROCESSAMENTO
# =======================================================

# 2.1) Leitura do banco RDS já higienizado em etapa anterior
base <- readRDS(file.path(dados_dir, "df_1ª_SIMULA_2ª_SÉRIE.RDS"))
colnames(base) <- trimws(colnames(base))

# 2.2) Filtra disciplina Língua Portuguesa
base <- base %>%
  dplyr::filter(disciplina_descricao == "LÍNGUA PORTUGUESA")

# 2.3) Elimina itens/colunas específicas, se necessário
suppressWarnings({
  base <- base %>%
    dplyr::select(-dplyr::any_of(c("rpa_021", "rp_021")))
})

# 2.4) Extrai gabarito: colunas "rp_*", exceto "rpa_*"
gabar_cols <- base %>%
  dplyr::select(dplyr::starts_with("rp")) %>%
  dplyr::select(-dplyr::starts_with("rpa"))

vgabr <- as.character(gabar_cols[1, ])
rm(gabar_cols)

# 2.5) Mantém apenas avaliados e apenas respostas do tipo "rpa_*"
base <- base %>%
  dplyr::filter(fl_avaliado == 1) %>%
  dplyr::select(matricula, dplyr::starts_with("rpa_"))

# 2.6) Matriz de respostas brutas
mY <- as.data.frame(base[, -1])

# Distribuição de frequências dos caracteres observados
mY_tab <- apply(mY, 2, table)

# 2.7) Convenção de "não apresentado"
# "9" codifica item não apresentado.
# mV = 1 se item apresentado; 0 se não apresentado.
mV <- 1 - (mY == 9)

# 2.8) Corrige itens: matriz binária mYc
# 1 = acerto; 0 = erro
mYc <- corrigeItens(mY, vgabr)

# 2.9) Versão com NA para itens não apresentados
mYcNA <- mYc
mYcNA[mV == 0] <- NA

# 2.10) Nomes dos itens
auxnomesitens <- colnames(mYc)
nomesitens <- substring(auxnomesitens, 5)

n <- nrow(mYc)
nI <- ncol(mYc)

opcoes <- c("-", "*", "A", "B", "C", "D", "E")
num_alter <- 5



# =======================================================
# 📌 4. CARREGAMENTO DAS INFORMAÇÕES DOS ITENS
# =======================================================


InfoItens<-read_xlsx(file.path(dados_dir,'gabarito_2serie_lp 1ª SIMULA.xlsx'),sheet = 'LP-SimI')

# Eliminando itens

InfoItens<- InfoItens %>% filter(Item!='rpa_021')


# # # Constantes de Transformação
#alfa = 55.8923279
#beta = 249.964381

v_item_conhecido <- !is.na(InfoItens$CONHECIDO)
# 
InfoItens <- InfoItens %>% filter(CONHECIDO=='Prova25' | CONHECIDO=='SimII')
# 
# InfoItens$JULHO <-paste0('rpa_0',InfoItens$JULHO)
# 
# InfoItens<- InfoItens %>% arrange(JULHO)



# =======================================================
# 📌 5. ESTIMAÇÃO DO MODELO TRI (3PL) COM PRIORS
# =======================================================

# Definição do modelo com distribuições a priori
model.prior <- mirt.model(paste0('F1 = 1-', nI, ' 
                         PRIOR = (1-', nI, ', a1, lnorm, -0.2058759, .6),
                                 (1-', nI, ', d, norm, 0, 10),
                                 (1-', nI, ', g, norm, -1.3, 0.7)'))



# Estimando o modelo inicial (3PL) usando o método EM
resultML3Paux <- mirt(mYcNA,model=model.prior,itemtype='3PL',
                      SE=TRUE,method="EM",pars='values')



# =======================================================
# 📌 6. FIXAÇÃO DE PARÂMETROS CONHECIDOS
# =======================================================

# Identificar quais itens possuem parâmetros conhecidos
vitem <- 1:nI

#v_item_conhecido <- auxnomesitens%in%InfoItens$Item

# Criar matriz indicadora de parâmetros fixados
m_ind_par_fix <- matrix(0, nI, 4)
v_ind_index_par_fix <- vitem[v_item_conhecido==1]
m_ind_par_fix[v_ind_index_par_fix, ] <- 1

# Vetor para indicar ao `mirt` quais parâmetros devem ser fixados
v_ind_par_fix <- c(t(m_ind_par_fix))
v_ind_par_fix<- c(v_ind_par_fix)
v_ind_index_par_fix_2 <- which(v_ind_par_fix==1)

# Criar matriz com os parâmetros fixados (a, b, c), mais uma coluna extra.
mzeta_par_item_fix <- data.frame(InfoItens %>% select(aSAEB,bSAEB,c), 1)

# Ajustar escala dos parâmetros
mu <- 250
sigma <- 50
mzeta_par_item_fix[, 1] <- mzeta_par_item_fix[, 1] * sigma
mzeta_par_item_fix[, 2] <- ((mzeta_par_item_fix[, 2] - mu) / sigma)

mzeta_par_item_fix[,2] <- -mzeta_par_item_fix[,1]*mzeta_par_item_fix[,2]

# =======================================================
# 📌 7. REESTIMAÇÃO DO MODELO COM PARÂMETROS FIXADOS
# =======================================================

# Parâmetros dos itens a serem fixados
resultML3Paux$est[v_ind_index_par_fix_2] <- FALSE
# Devemos estimar os parâmetros populacionais
resultML3Paux$est[c(4*nI+1,4*nI+2)] <- TRUE
# Atribuindo os valores dos parâmetros dos itens a
# serem fixados
resultML3Paux$value[v_ind_index_par_fix_2] <- 
  c(t(as.matrix(mzeta_par_item_fix)))
# resultML3Paux$value[] <- c(mutheta,sqrt(psitheta))
begin<-Sys.time()

# Ajuste final do modelo, agora considerando os itens conhecidos fixos
resultML3P <- mirt(mYcNA, model = model.prior, itemtype = '3PL', 
                   SE = TRUE, method = "EM", pars=resultML3Paux,technical = list(NCYCLES=2000))

end<-Sys.time()

end-begin

# =======================================================
# 📌 8. EXTRAÇÃO E EXIBIÇÃO DOS RESULTADOS
# =======================================================

# Extrair os coeficientes dos itens estimados
resultML3Pparite <- as.data.frame(coef(resultML3P, IRTpars = TRUE, printSE = TRUE, simply = TRUE))

#
# Indices uteis para salvar os resultados
# relativos aos parametros dos itens
# necess?rios para gerar alguns gr?ficos
# abaixo
indu <- 4*seq(1,nI,1)
indc <- indu-1
indb<- indc-1
inda <- indb-1
#
# dificuldade
eb <- as.numeric(resultML3Pparite[1,indb]) # estimativa
epb <- as.numeric(resultML3Pparite[2,indb]) # erro-padr?o
# discrimina??o
ea <- as.numeric(resultML3Pparite[1,inda]) # estimativa
epa <- as.numeric(resultML3Pparite[2,inda]) # erro-padr?o
# acerto casual
ec <-as.numeric(resultML3Pparite[1,indc]) # estimativa
epc <- as.numeric(resultML3Pparite[2,indc]) # erro-padr?o

# Estimativas pontuais dos par. os itens
eeta<-coef(resultML3P)$GroupPars
#


pdf(file=paste(result_dir,sep="","/ParItensE.pdf"))
par(mfrow=c(2,2))
plot(ea,cex=1.2,
     cex.lab=1.2,cex.main=1.2,
     xlab="item",ylab="estimativa",
     main="discriminacao",
     col=ifelse(vitem %in% c(v_ind_index_par_fix),
                "green","black"),
     pch=ifelse(vitem %in% c(v_ind_index_par_fix),
                17,19),xaxt="n")
axis(1,1:nI,labels=nomesitens,las=2,cex=0.5)
abline(0.6/sqrt(eeta[1,2]),0,lwd=2,lty=2,col="red")
abline(4.0/sqrt(eeta[1,2]),0,lwd=2,lty=2,col="blue")
plot(eb,cex=1.2,
     cex.lab=1.2,cex.main=1.2,
     xlab="item",ylab="estimativa",
     main="dificuldade",
     col=ifelse(vitem %in% c(v_ind_index_par_fix),
                "green","black"),
     pch=ifelse(vitem %in% c(v_ind_index_par_fix),
                17,19),xaxt="n")
axis(1,1:nI,labels=nomesitens,las=2,cex=0.5)
abline(eeta[1,1]*sqrt(eeta[1,2]),0,lwd=2,lty=2,col="blue")
plot(ec,cex=1.2,
     cex.lab=1.2,cex.main=1.2,
     xlab="item",ylab="estimativa",
     main="acerto casual",
     col=ifelse(vitem %in% c(v_ind_index_par_fix),
                "green","black"),
     pch=ifelse(vitem %in% c(v_ind_index_par_fix),
                17,19),xaxt="n")
axis(1,1:nI,labels=nomesitens,las=2,cex=0.5)
abline(0.20,0,lwd=2,lty=2,col="red")
abline(0.25,0,lwd=2,lty=2,col="orange")
abline(0,0,lwd=2,lty=2,col="blue")
dev.off()

# IC's, valores verdadeiros e estimativas pontuais
# para os dado reais, nao teremos os valores verdadeiros
# comando "abline", abaixo
pdf(file=paste(result_dir,sep="","/ParItensEIC.pdf"))
par(mfrow=c(2,2))
ez=qnorm(0.975)
plotCI(ea,ui=ea+ez*epa,li=ea-ez*epa,cex=1.2,
       cex.lab=1.2,cex.main=1.2,
       xlab="item",ylab="estimativa",
       main="discriminacao",
       col=ifelse(vitem %in% c(v_ind_index_par_fix),
                  "green","black"),
       pch=19,xaxt="n")
axis(1,1:nI,labels=nomesitens,las=2,cex=0.5)
abline(0.6/sqrt(eeta[1,2]),0,lwd=2,lty=2,col="red")
abline(4.0/sqrt(eeta[1,2]),0,lwd=2,lty=2,col="blue")
plotCI(eb,ui=eb+ez*epb,li=eb-ez*epb,cex=1.2,
       cex.lab=1.2,cex.main=1.2,
       xlab="item",ylab="estimativa",
       main="dificuldade",
       col=ifelse(vitem %in% c(v_ind_index_par_fix),
                  "green","black"),
       pch=19,xaxt="n")
axis(1,1:nI,labels=nomesitens,las=2,cex=0.5)
abline(eeta[1,1]*sqrt(eeta[1,2]),0,lwd=2,lty=2,col="blue")
plotCI(ec,ui=ec+ez*epc,li=ec-ez*epc,cex=1.2,
       cex.lab=1.2,cex.main=1.2,
       xlab="item",ylab="estimativa",
       main="acerto casual",
       col=ifelse(vitem %in% c(v_ind_index_par_fix),
                  "green","black"),
       pch=19,xaxt="n")
axis(1,1:nI,labels=nomesitens,las=2,cex=0.5)
abline(0.20,0,lwd=2,lty=2,col="red")
abline(0.25,0,lwd=2,lty=2,col="orange")
abline(0,0,lwd=2,lty=2,col="blue")
dev.off()


# Estimacao dos tracos latentes
rthetaML3P <- fscores(resultML3P,method='EAP',
                      full.scores.SE=TRUE)[,1]


mu <- 250
sigma <- 50

rthetaML3PT <- rthetaML3P*sigma+mu

pdf(file=paste(result_dir,sep="","/TracosLatentesET.pdf"))
par(mfrow=c(2,2))
hist(rthetaML3PT,probability=TRUE,xlab="traco latente",
     ylab="densidade",main="tracos latentes",
     nclass=12,col=NULL)
boxplot(rthetaML3PT,
        cex=1.2,cex.main=1.2,cex.lab=1.2,
        main="traco latente",xlab="tipo",ylab="valor")
# qqplot (normalidade padr?o)
qqPlot(c(scale(rthetaML3PT)),xlab="quantil N(0,1)",
       dist="norm",mean=0,sd=1,col.lines="blue",grid="FALSE",
       ylab="quantil do traco latente (padronizado)",
       cex=1.2,pch=19)
dev.off()



# Densidade e parametros de dificuldade
estbtransf <- sigma*eb + mu
auxdensy <- density(rthetaML3PT)$y
auxdensx <- density(rthetaML3PT)$x
#
pdf(file=paste(result_dir,sep="/TracosLatentesETPDI",".pdf"))
par(mfrow=c(1,1))
plot(density(rthetaML3PT),xlab="escala",
     ylab="densidade",cex=1.2,cex.lab=1.2,main="",
     lwd=2,
     xlim=c(min(auxdensx,estbtransf),
            max(auxdensx,estbtransf)),
     ylim=c(0,max(auxdensy+0.001)),col=(2))
lines(estbtransf,rep(0,nI),type="p",cex=1.2,
      col=ifelse(vitem %in% c(v_ind_index_par_fix),
                 "green","black"),
      pch=ifelse(vitem %in% c(v_ind_index_par_fix),
                 17,19),xaxt="n")
dev.off()


## CCI's ajustadas

conhecido<-ifelse(v_item_conhecido,'Sim','Nao')

pdf(file=paste(result_dir,sep="","/CCIPOE.pdf"))
for(i in 1:nI)
{
  par(mfrow=c(1,1))
  titulo <- paste("CCI para o item : ",sep="",auxnomesitens[i],' - Fixado:',conhecido[i])
  grafico <- itemfit(resultML3P,group.bins = 10,empirical.plot = i,
                     Theta=matrix(rthetaML3P))
  grafico$main<-titulo
  plot(grafico)  #print(itemfit(resultML3P, empirical.plot=i, 
  #              Theta=matrix(rthetaML3P)))
  #cat("\n") 
}
dev.off()



pdf(file=paste(result_dir, sep="", "/IICPOE.pdf"))
for (i in 1:nI) {
  par(mfrow=c(1,1))
  titulo <- paste("CI para o item : ",sep="",auxnomesitens[i],' - Fixado:',conhecido[i])
  
  # Gerando valores de Theta
  theta_vals <- seq(-4, 4, length.out = 100)
  
  # Extraindo o item específico
  item_i <- extract.item(resultML3P, i)
  
  # Calculando a informação do item
  info_vals <- iteminfo(item_i, Theta = theta_vals)
  
  # Criando o gráfico
  plot(theta_vals, info_vals, type="l", main=titulo, xlab="Theta", ylab="Informação", col="blue", lwd=2)
}
dev.off()


# Parametros populacionais
# Sem transformacao
eeta<-coef(resultML3P)$GroupPars
emutheta <- eeta[,1]
epsitheta <- eeta[,2]
eptheta <- sqrt(epsitheta)
#
# Erros-padr?o
epmutheta<-resultML3Pparite[2,4*nI+1]
eppsitheta<-resultML3Pparite[2,4*nI+2]
epdptheta <- c(sqrt(((eppsitheta^2)/2)*(1/eptheta[1]))) # m?todo delta
#
# Transformando para a escala SAEB
emuthetaT <- as.numeric(mu+sigma*emutheta[1])
epsithetaT <- as.numeric((sigma^2)*epsitheta[1])
edpthetaT <- as.numeric(sqrt(epsithetaT))
#
# erros-padr?o
epmuthetaT <-  sigma*epmutheta
eppsimuthetaT <-  (sigma^2)*eppsitheta
epdpmuthetaT <-  (sigma)*epdptheta
#
# Intervalos de confian?a
ez<-qnorm(0.975)
ICmutheta <- c(emuthetaT-ez*epmuthetaT,
               emuthetaT+ez*epmuthetaT)
ICdptheta <- c(edpthetaT-ez*epdpmuthetaT,
               edpthetaT+ez*epdpmuthetaT)


pdf(file=paste(result_dir,sep="","/PPE.pdf"))
par(mfrow=c(1,2))
plotCI(emutheta[1],li=emutheta[2],ui=emutheta[3],
       pch=19,cex=1.2,cex.lab=1.2,cex.main=1.2,
       xlab="",ylab="estimativa",
       main="media dos tracos latentes",
       xaxt="n")
#
plotCI(sqrt(epsitheta[1]),li=sqrt(epsitheta[2]),
       ui=sqrt(epsitheta[3]),
       pch=19,cex=1.2,cex.lab=1.2,cex.main=1.2,
       xlab="",ylab="estimativa",
       main="desvio-padrao dos tracos latentes",
       xaxt="n")
dev.off()

pdf(file=paste(result_dir,sep="","/PPET.pdf"))
par(mfrow=c(1,2))
plotCI(emuthetaT[1],li=ICmutheta[1],ui=ICmutheta[2],
       pch=19,cex=1.2,cex.lab=1.2,cex.main=1.2,
       xlab="",ylab="estimativa",
       main="media dos tracos latentes",
       xaxt="n")
#
plotCI(edpthetaT[1],li=ICdptheta[1],ui=ICdptheta[2],
       pch=19,cex=1.2,cex.lab=1.2,cex.main=1.2,
       xlab="",ylab="estimativa",
       main="desvio-padrao dos tracos latentes",
       xaxt="n")
dev.off()



## base de Dados incluindo Proficiencias individuais estimadas




base$Theta<-rthetaML3PT


write.xlsx(base,paste(result_dir,sep="/BaseRespTheta_2EM_LP_S1",".xlsx"),rowNames=T)


## Percentuais de alunos por classe dos tra?cos latentes na escala SAEB - TRI


corte<-seq(90,350,by=25)



corte<-c(0,25,50,100,125,150,175,200,225,250,275,300,325,350,1000)

thetaInt<-cut(rthetaML3PT,corte,right = F)

freqTheta<-table(thetaInt)

freqTheta<-cbind(freqTheta)

freqTheta<-cbind(freqTheta,round((freqTheta/length(rthetaML3PT))*100,3))

colnames(freqTheta)<-c("N de alunos", "Percentual")

require(xtable)

freqTheta<-as.data.frame(freqTheta)

apply(freqTheta,2,sum)

write.xlsx(freqTheta,paste(result_dir,sep="/DistriAlunosClasseLP2serie_S1",".xlsx"),rowNames=T)

freqTheta<-format(freqTheta,decimal.mark=",")

xtable(freqTheta)



## Tabela com estimativa dos parametros dos itens

conhecido<-ifelse(v_item_conhecido,'Sim','Nao')

TabItens<-data.frame(Item=nomesitens,a=ea,b=eb,aSAEB=ea/sigma,bSAEB=sigma*eb+mu,c=ec,SAEB=conhecido)

#TabItens[,2:6]<-round(TabItens[,-c(1,7)],3)

write.xlsx(TabItens,paste(result_dir,sep="/EstItens_2EM_LP_S1",".xlsx"),rowNames=F)

TabItens<-format(TabItens,decimal.mark=",")

xtable(TabItens,digits = 3)


## Medidas descritivas: proficiencias

med_resu <- c(mean(rthetaML3PT),sd(rthetaML3PT),
              100*sd(rthetaML3PT)/mean(rthetaML3PT),
              min(rthetaML3PT),
              quantile(rthetaML3PT,0.25),
              quantile(rthetaML3PT,0.5),
              quantile(rthetaML3PT,0.75),
              max(rthetaML3PT),
              skewness(rthetaML3PT,type = 1),
              kurtosis(rthetaML3PT,type = 1)+3)

names(med_resu)<-c("media","dp","cv(%)","min.","1o Q",
                   "med.","3oQ","max.","ca",
                   "curt.")


write.xlsx(as.data.frame(med_resu),paste(result_dir,sep="/ResumoTheta_2EM_LP_S1",".xlsx"),rowNames=T)

round(med_resu,3)



EstPopThetaT<-data.frame(est=emuthetaT,IC_Inf=ICmutheta[1],IC_Sup=ICmutheta[2],estDP=edpthetaT,IC_InfDP=ICdptheta[1],IC_SupDP=ICdptheta[2])

EstPopTheta<-data.frame(est=emutheta[1],IC_Inf=emutheta[2],IC_Sup=emutheta[3],estDP=eptheta[1],IC_InfDP=eptheta[2],IC_SupDP=eptheta[3])

EstPop<-round(rbind(EstPopThetaT,EstPopTheta),3)


rownames(EstPop)<-c("SAEB","(0,1)")

write.xlsx(rbind(EstPop),paste(result_dir,sep="/ResumoThetaIC_2EM_LP_S1",".xlsx"),rowNames=T)


#xtable(EstPop)




#============= Estatisticas de ajuste do modelo ==============================#
#
# resultML3P: estimativa dos parametros dos itens
pdf(file=paste(result_dir,sep="","/S_X2item.pdf"))

# X2
# resultML3P: estimativa dos parametros dos itens
pdf(file=paste(result_dir,sep="","/X2item.pdf"))
par(mfrow=c(1,1))
plot(itemfit(resultML3P,fit_stats="X2",na.rm = F)[,4],ylim=c(0,1),
     pch=19,xlab="item",ylab="RQEQM_X2",xaxt="n")
axis(1,1:nI,labels=nomesitens,las=2,cex=0.5)
abline(h=0.05,lwd=2,lty=2,col="blue")
abline(h=0.10,lwd=2,lty=3,col="blue")
dev.off()
#
# G2
pdf(file=paste(result_dir,sep="","/G2item.pdf"))
par(mfrow=c(1,1))
plot(itemfit(resultML3P,fit_stats="G2",na.rm = F)[,4],ylim=c(0,1),
     pch=19,xlab="item",ylab="RQEQM_G2",xaxt="n")
axis(1,1:nI,labels=nomesitens,las=2,cex=0.5)
abline(h=0.05,lwd=2,lty=2,col="blue")
abline(h=0.10,lwd=2,lty=3,col="blue")
dev.off()
# 
# PV_Q1
pdf(file=paste(result_dir,sep="","/PV_Q1item.pdf"))
par(mfrow=c(1,1))
plot(itemfit(resultML3P,fit_stats="PV_Q1",na.rm = F)[,4],ylim=c(0,1),
     pch=19,xlab="item",ylab="RQEQM_PV_Q1",xaxt="n")
axis(1,1:nI,labels=nomesitens,las=2,cex=0.5)
abline(h=0.05,lwd=2,lty=2,col="blue")
abline(h=0.10,lwd=2,lty=3,col="blue")
dev.off()
#
# Infit
# resultML3P: estimativa dos par?metros dos itens
pdf(file=paste(result_dir,sep="","/infititem.pdf"))
par(mfrow=c(1,1))
infout<-itemfit(resultML3P,fit_stats="infit",na.rm = F)
plot(infout[,4],
     pch=19,xlab="item",ylab="infit",
     ylim=c(min(0,infout[,4]),max(2.5,infout[,4])),xaxt="n")
axis(1,1:nI,labels=nomesitens,las=2,cex=0.5)
abline(h=0.5,lwd=2,lty=3,col="blue")
abline(h=1.5,lwd=2,lty=2,col="blue")
abline(h=2,lwd=2,lty=3,col="blue")
abline(h=1,lwd=2,lty=2,col="blue")
dev.off()
#
# Outfit
pdf(file=paste(result_dir,sep="","/outfititem.pdf"))
par(mfrow=c(1,1))
plot(infout[,2],
     pch=19,xlab="item",ylab="outfit",
     ylim=c(min(0,infout[,2]),max(2.5,infout[,2])),xaxt="n")
axis(1,1:nI,labels=nomesitens,las=2,cex=0.5)
abline(h=0.5,lwd=2,lty=3,col="blue")
abline(h=1.5,lwd=2,lty=2,col="blue")
abline(h=2,lwd=2,lty=3,col="blue")
abline(h=1,lwd=2,lty=2,col="blue")
dev.off()

# Residuos para verificacao da independencia local
# resultML3P: estimativa dos parametros dos itens
auxresML3P<-residuals(resultML3P,type="LD",table=TRUE)
#
auxnpar <- nI*(nI-1)/2
RMSEA <- matrix(0,auxnpar)
for (i in 1:auxnpar)
{
  rtable <- auxresML3P[[i]]  
  rtableO <-rtable$Obs
  rtableE <-rtable$Exp
  ptableO <-rtableO/sum(rtableO)
  ptableE <-rtableE/sum(rtableE)
  RMSEA[i]<-sqrt(sum((ptableO-ptableE)^2))
}
#
pdf(file=paste(result_dir,sep="","/RQEQMIL.pdf"))
par(mfrow=c(1,1))
plot(RMSEA,xlab="pares de itens",
     ylab="RQEQMIL",pch=19,
     cex=1.2,cex.main=1.2,cex.lab=1.2,
     ylim=c(0,1))
abline(h=0.05,lwd=2,lty=2,col="green")
abline(h=0.10,lwd=2,lty=2,col="green")
dev.off()
