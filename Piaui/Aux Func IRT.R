# adaptada da funçao analizarItems do pacote itan 
# (https://cran.r-project.org/web/packages/itan/index.html)

library(plyr)


#### Atualizado em 10/0/2025

# Gera matrizes de respostas binárias
# respostas podem ou não conter NA, tudo o que for diferente
# do gabarito, assumir que é 0
# NA é considerado zero (0)
corrigeItens <-function (respostas, gabarito) 
{
#  gabarito <- validarClave(respostas, gabarito)
  respostas[is.na(respostas)] <- 0
  output <- matrix(NA, nrow(respostas), ncol(respostas))
  colnames(output) <- colnames(respostas)
  rownames(output) <- rownames(respostas)
  for (i in 1:ncol(respostas)) {
    output[,i] <- (respostas[,i] == gabarito[i]) * 1
  }
  return(output)
}

# Cria matriz indicadora de itens apresentados
# criaMatrizV <- function()


# Calcula os escores brutos e padronizados
# respCorrigidas - matriz de zeros e uns
# mV matriz indicadora
# já calcula o escore padronizado conciderando item não apresentado

calculaEscore<-function (respCorrigidas,mV=1) 
{
  vescoreb<-apply(respCorrigidas*mV, 1, sum)
  vescorep<-vescoreb/apply(mV, 1, sum)
  result <- list(vescoreb=vescoreb,vescorep=vescorep)
  return(result)
}

# Dificuldade e discriminação por item
# respcor - matriz de zeros e uns
# mV matriz indicadora
# escores - escores brutos ou padronizados
itemana <- function(respcor, mV, escores, prop = 0.27)
{
  
  p <- apply(respcor, 2, sum)
  aux <- apply(mV,2,sum)
  dific<-p<- p/aux
  if (prop >= 0.5) {
    warning("The proportion can not be higher than 0.5. Using default
value (0.27).")
    prop <- 0.27
  }
  nItens <- ncol(respcor)
#  escores <- apply(respcor, 1, sum)
  disc <- Dc2 <- Dc1 <- rep(0,nItens)
  #
  for (i in 1:nItens)
  {
  nGrupos <- round(length(mV[mV[,i]==1,i]) * prop)
  data <- cbind(respcor[mV[,i]==1,i], 
                escores = escores[mV[,i]==1])
  dataaux <- cbind(mV[mV[,i]==1,i], 
                   escores = escores[mV[,i]==1])
  data <- data[order(data[, "escores"]), ]
  dataaux <- dataaux[order(dataaux[, "escores"]), ]
  grupoInferior <- data[1:nGrupos, -ncol(data)]
  grupoSuperior <- data[nrow(data):(nrow(data) - nGrupos +
                                      1), -ncol(data)]
  grupoInferioraux <- dataaux[1:nGrupos, -ncol(dataaux)]
  grupoSuperioraux <- dataaux[nrow(data):(nrow(dataaux) - nGrupos +
                                            1), -ncol(dataaux)]
  
  Dc1[i] <- sum(grupoInferior)/
    sum(grupoInferioraux)
  Dc2[i] <- sum(grupoSuperior)/
    sum(grupoSuperioraux)
disc[i] <- Dc2[i]-Dc1[i]
  }
  result <- data.frame(cbind(dific=dific,Dc1=Dc1,Dc2=Dc2,
                 disc=disc))
  colnames(result)<-c("Dific.","PropG1","ProG2","Disc.")
#cbind()
return(result)
}

# Dificuldade e discriminação por alternativa dentro de
# cada item
# respostas - matriz com as alternativas indicadas
# mV matriz indicadora
# escores - escores brutos ou padronizados
# só considera o gabarito, como as opções
itemanalt <- function(respostas, mV, escores, answerKey, 
                      nalt=4, 
                      prop = 0.27)
{
 opcoes <- LETTERS[1:nalt]
 nItens <- ncol(respostas)
 mresult <- array(NA,c(nItens,4,nalt))
 rownames(mresult)<-colnames(respostas)
 colnames(mresult) <- c("Dific.","PropG1","ProG2","Disc.")
 mDific <-mDisc<- matrix(NA,nItens,nalt)
 rownames(mDific) <- rownames(mDisc) <- colnames(respostas)
 colnames(mDific) <- colnames(mDisc) <- opcoes
 
  for (a in 1:nalt)
  {
    auxrespcor <- corrigeItens(respostas, 
                               rep(opcoes[a],nItens))
    aux <- itemana(auxrespcor,mV,escores,prop)
    
    mresult[,,a] <- as.matrix(aux)  
    #
    mDific[,a] <- mresult[,1,a]  
    mDisc[,a] <- mresult[,4,a]  
  }
 answerKey<- matrix(answerKey,nItens,1)
 colnames(answerKey)<-"Key"
 mDisc<-cbind(as.data.frame(mDisc),cbind(answerKey))
 mDific<-cbind(as.data.frame(mDific),cbind(answerKey))
 metamresult<-list(mresult=mresult,mDific=mDific,mDisc=mDisc)
 return(metamresult) 
}
  
# Dificuldade por alternativa (considerando respostas
# diferentes do gabarito, supondo que toda reposta
# diferente do gabarito é recordada com o mesmo 
# símbolo)

#dific_alter <- function(mY,mV,num_alter){
#
#naltmax <- max(num_alter)  
#mdific <- apply(mY,2,table)
#opcoes <- LETTERS[1:naltmax]
#mcont <- apply(mV,2,table)
#nItens <- ncol(mY)
#ncolunas <- length(opcoes)+1
#aux_mresult <- ldply(mdific, rbind)[,-1]
#mresult <- matrix(NA,nItens,naltmax+1)
#
#nom_itens <- names(mdific)
#nom_alter <- colnames(aux_mresult)
#mresult <- matrix(unlist(aux_mresult),
#                         nItens,ncolunas)/
#  matrix(mcont,nItens,ncolunas)
#mresult<- mresult[,-1]
#mresult <- mresult[,order(nom_alter)]
#mresult<-data.frame(mresult)
#colnames(mresult) <-nom_alter
#rownames(mresult) <-nom_itens
#for(i in 1:nItens)
#{
#mresult[i,(1:(num_alter[i]+1))] <- mdific[[i]]   
#}

#return(mresult)  

#}

#disc_alter <- function(mY,mV,num_alter){
#}

#itemanalt <- function(respostas, mV, escores, answerKey, nalt=4, 
#prop = 0.27)
# Dificuldade e discriminação por alternativa dentro de
# cada item
# respostas - matriz com as alternativas indicadas
# mV matriz indicadora
# escores - escores brutos ou padronizados
# considera o gabarito e o que mais se quiser, como as opções
# "*", "-" etc
itemanaltNR <- function(respostas, mV, escores, answerKey, 
                      nalt=4,opcoes, 
                      prop = 0.27)
{
#  opcoes <- LETTERS[1:nalt]
  nItens <- ncol(respostas)
  mresult <- array(NA,c(nItens,4,nalt))
  rownames(mresult)<-colnames(respostas)
  colnames(mresult) <- c("Dific.","PropG1","ProG2","Disc.")
  mDific <-mDisc<- matrix(NA,nItens,nalt)
  rownames(mDific) <- rownames(mDisc) <- colnames(respostas)
  colnames(mDific) <- colnames(mDisc) <- opcoes
  
  for (a in 1:nalt)
  {
    auxrespcor <- corrigeItens(respostas, 
                               rep(opcoes[a],nItens))
    aux <- itemana(auxrespcor,mV,escores,prop)
    
    mresult[,,a] <- as.matrix(aux)  
    #
    mDific[,a] <- mresult[,1,a]  
    mDisc[,a] <- mresult[,4,a]  
  }
  answerKey<- matrix(answerKey,nItens,1)
  colnames(answerKey)<-"Key"
  mDisc<-cbind(as.data.frame(mDisc),cbind(answerKey))
  mDific<-cbind(as.data.frame(mDific),cbind(answerKey))
  metamresult<-list(mresult=mresult,mDific=mDific,mDisc=mDisc)
  return(metamresult) 
}


# adaptada da funçãoo analizarDistractores do pacote itan 
# (https://cran.r-project.org/web/packages/itan/index.html)

# ver a questão do NA, esta função 
# não está totalmente reformulada
# Na verdade, não está funcionando como deveria
# tem de finalizá-la
analizingDistractors<- function (answers, answerkey,
                                 scores,
                                 noptions = 4, 
                                 proportion = 0.27, 
                                 frequency = TRUE) 
{
  nItens <- ncol(answers)
  options <- LETTERS[1:noptions]
  if (proportion >= 0.5) {
    warning("The proportion can not be higher or equal to 0.5. Using the default value (0.27).")
    proportion <- 0.27
  }
  #scores <- apply(respCorrect, 1, sum)
  #answerkey <- validarClave(answers, answerkey)
  for (i in 1:nItens) {
    levels(answers[, i]) <- ifelse(options == answerkey[i], 
                                   paste("*", options, 
                                         sep = ""), 
                                   paste(" ", options,sep = ""))
  }
  for (i in 1:nItens) {
    nGroups <- round(nrow(answers) * proportion)
    respCorrect <- corrigeItens(answers, answerkey)
    data <- cbind(answers, sco = scores)
    data <- data[order(data[, "sco"]), ]
    gInf <- data[1:nGroups, -ncol(data)]
    gSup <- data[nrow(data):(nrow(data) - nGroups + 1), -ncol(data)]
    output <- list()
    
    output[[i]] <- rbind(gSup = table(factor(gSup[,i],levels=options)), 
                         gInf = table(factor(gInf[,i],levels=options)))
    #output[[i]] <- rbind(gSup = table(gSup[, i]), gInf = table(gInf[,i]))
    if (!frequency) {
      output[[i]] <- output[[i]]/nGroups
    }
  }
  names(output) <- colnames(answers)
  return(output)
}

# adaptada da fun??o pBis do pacote itan 
# (https://cran.r-project.org/web/packages/itan/index.html)

#cpBis<-function (answers, answerkey, correcctionSco = TRUE, nalt = 4) 
#{
#answerkey <- validarClave(answers, answerkey)
#  answCorrect <- corregirItems(answers, answerkey)
#  options <- LETTERS[1:nalt]
#  nItems <- ncol(answers)
#  output <- matrix(NA, nItems, nalt)
#  rownames(output) <- colnames(answers)
#  colnames(output) <- options
#  for (i in 1:nItems) {
#    for (a in 1:nalt) {
#      tmp <- ifelse(answers[, i] == options[a], 1, 
#                    0)
#      tmp[is.na(tmp)] <- 0
#      if (correcctionSco) {
#        output[i, a] = cor(tmp, rowMeans(answCorrect[, 
#                                                        -i], na.rm = TRUE) * (ncol(answCorrect) - 
#                                                                                1), use = "pairwise.complete.obs")
#      }
#      else {
#        output[i, a] = cor(tmp, answCorrect, use = "pairwise.complete.obs")
#      }
#    }
#  }
#output <- round(output)
#  altern<-colnames(output)
#  vpBis<-matrix(0,I)
#  for (i in 1:I)
#  {
#    vpBis[i] <- as.numeric(output[i,altern==answerkey[i]][1])
#  }
#  output <- as.data.frame(output)
#  answerkey <- as.data.frame(t(answerkey), stringsAsFactors = FALSE)
#  names(answerkey) <- "Key"
#  output <- cbind(output, answerkey)
#  result <- list(output=output,vpBis=vpBis)
#  return(result)
#}

# deixa o NA como outra alternativa
# O mais apropriado ? 
# Faz por item
cpBisold<-function(answers, mV, answerkey, dealNA, dichot, 
                nalt = 4) 
{
  # dealNA : "include" (put zero in the place of NA) 
  # (considers NA as another alternative), 
  # "exclude" (exclude the observation), in this case, 
  # the matrix "answers"
  # should contain NA instead another letter for 
  # the non reponse
  # dichot : TRUE - if the data set correspond to a original 
  # dichotomous items
  # and FALSE in case of multuple choice test
  #
  #answerkey <- validarClave(answers, answerkey)
  if (dichot=="TRUE")
  {
    answCorrect <- answers
  }
  else if (dichot == "FALSE")
  {
    answCorrect <- corrigeItens(answers, answerkey)
  }
  if (dealNA=="include")
  {
    answCorrect[is.na(mV)]<-0
    score<-as.numeric(rowSums (answCorrect, na.rm = FALSE))
  }
  
  else if (dealNA=="exclude")
  {
    answCorrect[mV==0]<-NA
    score<-as.numeric(rowSums (answCorrect, na.rm = TRUE))
  }
  if (dichot == "FALSE"){
  options <- LETTERS[1:nalt]
  }
  else if (dichot == "TRUE")
  {options <-1}
  
  nItems <- ncol(answers)
  output <- matrix(NA, nItems, nalt)
  rownames(output) <- colnames(answers)
  colnames(output) <- options
  for (i in 1:nItems) {
    for (a in 1:nalt) {
      if (dealNA=="include"){
        if (dichot == "TRUE")
        {
          tmp <- answers[,i]  
        }
        else if (dichot=="FALSE")
        {
          tmp <- ifelse(answers[, i] == options[a], 1, 
                        0)
        }
        tmp[is.na(tmp)] <- 0
        
      }
      else if (dealNA=="exclude")
      {
        if (dichot == "TRUE")
        {
          tmp <- answers[,i]  
        }
        else if(dichot == "FALSE")
        {
          tmp <- ifelse(answers[,i] == options[a], 1, 
                        0)
        }
      #  tmp[is.na(answers[,i])]<-NA
      }
      output[i, a] = cor(tmp,score, use = "complete.obs")
    }
  }
  #output <- round(output)
  altern<-colnames(output)
  vpBis<-matrix(0,nItems)
  for (i in 1:nItems)
  {
    vpBis[i] <- as.numeric(output[i,altern==answerkey[i]][1])
  }
  output <- as.data.frame(output)
  answerkey <- as.data.frame(as.matrix(answerkey,nalt,1), stringsAsFactors = FALSE)
  colnames(answerkey) <- "Key"
  # output <- cbind(output, answerkey)
  output <- cbind(output, cbind(answerkey))
  result <- list(output=output,vpBis=vpBis)
  return(result)
}

# faz por item, ou seja, se tiver NA em apenas alguns itens
# só se "elimina" a resposta (indivídu) para esses itens
cpBis<-function(answers, mV, answerkey, dealNA, dichot) 
{
  # dealNA : "include" (put zero in the place of NA) 
  # (considers NA as another alternative), 
  # "exclude" (exclude the observation), in this case, 
  # the matrix "answers"
  # should contain NA instead another letter for 
  # the non reponse
  # dichot : TRUE - if the data set correspond to a original 
  # dichotomous items
  # and FALSE in case of multuple choice test
  #
  #answerkey <- validarClave(answers, answerkey)
  if (dichot=="TRUE")
  {
    answCorrect <- answers
    answCorrect[mV==0]<-0
    score<-calculaEscore(answCorrect,mV)$vescoreb 
    
  }
  else if (dichot == "FALSE")
  {
    answCorrect <- corrigeItens(answers, answerkey)
    score<-calculaEscore(answCorrect,mV)$vescoreb 
    
  }
  if (dealNA=="include")
  {
    answCorrect[mV==0]<-0
 #   score<-as.numeric(rowSums (answCorrect, na.rm = FALSE))
  }
  
  else if (dealNA=="exclude")
  {
    answCorrect[mV==0]<-NA
  }
  nItems <- ncol(answers)
  output <- matrix(NA, nItems, 1)
  rownames(output) <- answerkey
#  colnames(output) <- options
  for (i in 1:nItems) {
      output[i] = cor(answCorrect[,i],score, use = "complete.obs")
  }
  #output <- round(output)
  altern<-colnames(output)
#  vpBis<-matrix(0,nItems)
#  for (i in 1:nItems)
#  {
#    vpBis[i] <- as.numeric(output[i,altern==answerkey[i]][1])
#  }
  output <- as.data.frame(output)
  colnames(output) <- "pBis"
  answerkey <- as.data.frame(as.matrix(answerkey,1,1), stringsAsFactors = FALSE)
  colnames(answerkey) <- "Key"
  # output <- cbind(output, answerkey)
  output <- cbind(output, cbind(answerkey))
 # result <- list(output=output,vpBis=vpBis)
  result <- list(output=output)
  return(result)
}

# deixa a não resposta, devidamente identificada,
# como outra alternativa

cpBisNR<-function(answers, mV, answerkey, dealNA, options,nalt = 4) 
{
  # a revisa
  # deal.na : "include" (put zero in the place of NA) 
  # (considers NA as another alternative), 
  # "exclude" (exclude the observation), in this case, 
  # the matrix "answers"
  # should contain NA instead another letter for 
  # the non reponse
  # dichot : TRUE - if the data set correspond to a original 
  # dichotomous items
  # and FALSE in case of multuple choice test
  #
  #answerkey <- validarClave(answers, answerkey)
  answCorrect <- corrigeItens(answers, answerkey)
  
  if (dealNA=="include")
  {
    answers[mV==1]<-0
    score<-as.numeric(rowSums (answCorrect, na.rm = FALSE))
  }
  
  else if (dealNA=="exclude")
  {
    answers[mV==0]<-NA
    score<-as.numeric(rowSums (answCorrect, na.rm = TRUE))
  }
  nItems <- ncol(answers)
  output <- matrix(NA, nItems, nalt)
  rownames(output) <- colnames(answers)
  colnames(output) <- options
  for (i in 1:nItems) {
    for (a in 1:nalt) {
      if (dealNA=="include"){
          tmp <- ifelse(answers[, i] == options[a], 1, 
                        0)
        tmp[is.na(tmp)] <- 0
        
      }
      else if (dealNA=="exclude")
      {
          tmp <- ifelse(answers[,i] == options[a], 1, 
                        0)
        }
      output[i, a] = cor(tmp,score, use = "complete.obs")
    }
  }
  altern<-colnames(output)
  vpBis<-matrix(0,nItems)
  for (i in 1:nItems)
  {
    vpBis[i] <- as.numeric(output[i,altern==answerkey[i]][1])
  }
  output <- as.data.frame(output)
  answerkey <- as.data.frame(as.matrix(answerkey,nalt,1), stringsAsFactors = FALSE)
  colnames(answerkey) <- "Key"
  output <- cbind(output, cbind(answerkey))
  result <- list(output=output,vpBis=vpBis)
  return(result)
}

# calcula a correlação biserial para cada alternativa
# mproesc (correct answer proption)

cBisold <- function(mpBis,mproesc,nItens,answerkey)
{
  mproesc <- cbind(mproesc)
  mpronesc <- 1-mproesc
  #  nalt<-length(answerkey)
  altern <- LETTERS[1:nalt]
  
  if (ncol(mpBis)>=2)
  {
    mqpronesc <- apply(mpronesc,2,qnorm,mean=0,sd=1)
    h <- apply(mqpronesc,2,dnorm,mean=0,sd=1)
  }
  
  else if (ncol(mpBis)==1)
  {
    mqpronesc <- qnorm(mpronesc,mean=0,sd=1)
    h <- dnorm(mqpronesc)
  }
  mcBis <- as.matrix(mpBis*sqrt(mpronesc*mproesc)/h)
  #
  vcBis<-matrix(0,nItens)
  #
  if (ncol(mpBis)==1){
    vcBis <- mcBis
  }
  #
  if (ncol(mpBis)>=2){
    for (i in 1:nItens)
    {
      vcBis[i] <- as.numeric(mcBis[i,altern==answerkey[i]][1])
    }
  }
  answerkey <- as.data.frame(answerkey, 
                             stringsAsFactors = FALSE)
  names(answerkey) <- "Key"
  output <- cbind(mcBis, answerkey)
  result <- list(output=output,vcBis=vcBis)
  return(result)  
}

# calcula a correlação biserial para cada alternativa
# mproesc (correct answer proption)

cBis <- function(mpBis,mproesc,answerkey)
{
  mproesc <- cbind(mproesc)
  mpBis<-cbind(mpBis)
  mpronesc <- 1-mproesc
#  altern <- LETTERS[1:nalt]
  
  if (ncol(mpBis)>=2)
  {
    mqpronesc <- apply(mpronesc,2,qnorm,mean=0,sd=1)
    h <- apply(mqpronesc,2,dnorm,mean=0,sd=1)
  }
  
  else if (ncol(mpBis)==1)
  {
    mqpronesc <- qnorm(mpronesc,mean=0,sd=1)
    h <- dnorm(mqpronesc)
  }
  mcBis <- as.matrix(mpBis*sqrt(mpronesc*mproesc)/h)
  #
  answerkey <- as.data.frame(answerkey, 
                             stringsAsFactors = FALSE)
  #names(answerkey) <- "Key"
  #names(mcBis)<-"cBis"
  output <- cbind(mcBis, answerkey)
  colnames(output)<-c("cBis","Key")
  result <- list(output=output)
  return(result)  
}

# calcula a correlação biserial para cada alternativa
# mproesc (correct answer proption)

cBisNR <- function(mpBis,mproesc,answerkey)
{
  mpronesc <- 1-mproesc
  mqpronesc <- apply(mpronesc,2,qnorm,mean=0,sd=1)
  nItems <- nrow(mpronesc)
  h <- dnorm(mqpronesc)
  #
  mcBis <- as.matrix(mpBis*sqrt(mpronesc*mproesc)/h)
  #
  #answerkey <- as.data.frame(answerkey, 
  #                           stringsAsFactors = FALSE)
  names(answerkey) <- "Key"
  output <- cbind(mcBis)
  vcBis<-matrix(0,nItems)
  altern<-colnames(output)
  for (i in 1:nItems)
  {
    vcBis[i] <- as.numeric(output[i,altern==answerkey[i]][1])
  }
  answerkey <- as.data.frame(answerkey,stringsAsFactors = FALSE)
  output <- cbind(mcBis, answerkey)
  result <- list(output=output,vcBis=vcBis)
  return(result)  
}

cBisNRold <- function(mpBis,mproesc,nItens,answerkey,
                   altern,nalt=4)
{
  mpronesc <- 1-mproesc
  #  nalt<-length(answerkey)
  #  altern <- LETTERS[1:nalt]
  
  if (ncol(mpBis)>=2)
  {
    mqpronesc <- apply(mpronesc,2,qnorm,mean=0,sd=1)
    h <- apply(mqpronesc,2,dnorm,mean=0,sd=1)
  }
  
  else if (ncol(mpBis)==1)
  {
    mqpronesc <- qnorm(mpronesc,mean=0,sd=1)
    h <- dnorm(mqpronesc)
  }
  mcBis <- as.matrix(mpBis*sqrt(mpronesc*mproesc)/h)
  #
  vcBis<-matrix(0,nItens)
  #
  if (ncol(mpBis)==1){
    vcBis <- mcBis
  }
  #
  if (ncol(mpBis)>=2){
    for (i in 1:nItens)
    {
      vcBis[i] <- as.numeric(mcBis[i,altern==answerkey[i]][1])
    }
  }
  answerkey <- as.data.frame(answerkey, 
                             stringsAsFactors = FALSE)
  names(answerkey) <- "Key"
  output <- cbind(mcBis, answerkey)
  result <- list(output=output,vcBis=vcBis)
  return(result)  
}

agiM<-function (respuestas, clave, nGrupos = 4, nOpciones = 4) 
{
  #clave <- validarClave(respuestas, clave)
  opciones <- LETTERS[1:nOpciones]
  respCorregidas <- corregirItens(respuestas, clave)
  #pBiserial <- pBis(respuestas, clave, nOpciones = nOpciones)[, 
  #                                                            -(nOpciones + 1)]
  pBiserial<-round((cpBis(respuestas, clave, "include",
                          "FALSE", nalt = nOpciones)$output)[, 
                                                             -(nOpciones + 1)],2)
  puntajes <- calcularPuntajes(corregirItens(respuestas, clave))
  scoreGroups <- cut(puntajes, breaks = nGrupos)
  sgLevels <- levels(scoreGroups)
  lowerLimits <- as.numeric(sub("\\((.+),.*", "\\1", sgLevels))
  upperLimits <- as.numeric(sub("[^,]*,([^]]*)\\]", "\\1", 
                                sgLevels))
  sgMeans <- rowMeans(cbind(lowerLimits, upperLimits))
  limites <- unique(c(lowerLimits, upperLimits))
  sgIndexes <- vector("list", nGrupos)
  for (j in 1:nGrupos) {
    sgIndexes[[j]] = which(scoreGroups == sgLevels[j])
  }
  tmp <- matrix(nrow = nOpciones, ncol = nGrupos)
  colnames(tmp) <- sgLevels
  rownames(tmp) <- opciones
  plots <- vector("list", ncol(respuestas))
  datos <- vector("list", ncol(respuestas))
  nItems <- ncol(respuestas)
  for (i in 1:nItems) {
    for (g in 1:nGrupos) {
      for (o in 1:nOpciones) {
        tmp[o, g] = length(which(respuestas[sgIndexes[[g]], 
                                            i] == opciones[o]))/length(sgIndexes[[g]])
      }
    }
    proportions <- as.data.frame(t(tmp))
    names(proportions) <- ifelse(opciones == clave[, i], 
                                 paste(c("*"), names(proportions), c(" ("), pBiserial[i, 
                                                                                      ], c(")"), sep = ""), paste(c(" "), names(proportions), 
                                                                                                                  c(" ("), pBiserial[i, ], c(")"), sep = ""))
    df <- cbind(proportions, sgMeans)
    datos[[i]] <- df
    df <- melt(data = df, id.vars = "sgMeans")
    plots[[i]] <- ggplot(df, aes_string(x = "sgMeans", y = "value", 
                                        colour = "variable")) + geom_line() + geom_point() + 
      labs(title = paste("Item ", i), x = "Grupo de puntaje", 
           y = "Proporcion de estudiantes", colour = "Alternativa (pBis)") + 
      theme(legend.position = c(0.12, 0.75), legend.text = element_text(size = 11, 
                                                                        face = "bold", hjust = 0.5)) + scale_x_continuous(limits = c(min(limites), 
                                                                                                                                     max(limites)), breaks = round(limites, 1)) + scale_y_continuous(limits = c(0, 
                                                                                                                                                                                                                1))
  }
  return(list(plots, datos))
}


# modelos probitos para respostas dicotômicas

prob.tri.probito.MRD <- function(v.a,v.b,v.c,v.theta)
  
  # v.a : vetor com os par?metros de discrimina??o
  # v.b : vetor com os par?metros de dificuldade
  # v.c : vetor com os par?metros de acerto ao acaso
  # v.theta : vetor com os tra?os latentes
  # D : escalar com o par?metro de modifica??o da fun??o de
  #     liga??o.
  
{
  
  ######################################################
  # assegura que os vetores de par?metro ser?o vetores
  # coluna
  
  v.a <- cbind(v.a)
  
  v.b <- cbind(v.b)
  
  v.c <- cbind(v.c)
  
  v.theta <- cbind(v.theta)
  
  n <- nrow(v.theta)
  
  I <- nrow(v.b)
  
  # matriz com os valores do par?metro de discrimina??o
  
  m.a <- kronecker(matrix(1,nrow(v.theta),1),t(v.a))
  
  
  # matriz com os valores do par?metro de discrimina??o
  
  m.b <- kronecker(matrix(1,nrow(v.theta),1),t(v.b))
  
  
  # matriz com os valores do par?metro de discrimina??o
  
  m.c <- kronecker(matrix(1,nrow(v.theta),1),t(v.c))
  
  
  # matriz com os valores dos tra?os latentes
  
  m.theta <- matrix(v.theta,nrow(v.theta),nrow(v.b))
  
  
  # matriz vom os os preditores aleat?rios
  
  
  #m.predito <- m.a*m.theta - m.b # (n x I)
  
  m.predito <- m.a*(m.theta - m.b) # (n x I)
  
  # matriz com as probabilidades de resposta correta
  
  
  m.P.c <- matrix(pnorm(c(m.predito)),n,I) # (n x I)
  
  
  m.P <-  m.c + (1 - m.c)*m.P.c
  
  
  # matriz com as probabilidades de resposta incorreta
  
  m.Q <- 1 - m.P
  
  ##################################
  # deixa os resultados dispon?veis
  
  result.prob.tri.probito.MRD <- list(m.P=m.P,m.Q=m.Q,m.a=m.a,m.b=m.b,m.c=m.c,m.predito=m.predito)
  
  return(result.prob.tri.probito.MRD)
  
}



# modelos logísticos para respostas dicotômicas

prob.tri.MRD <- function(v.a,v.b,v.c,v.theta,D)
  
  # v.a : vetor com os par?metros de discrimina??o
  # v.b : vetor com os par?metros de dificuldade
  # v.c : vetor com os par?metros de acerto ao acaso
  # v.theta : vetor com os tra?os latentes
  # D : escalar com o par?metro de modifica??o da fun??o de
  #     liga??o.
  
{
  
  ######################################################
  # assegura que os vetores de par?metros ser?o vetores
  # coluna
  
  v.a <- cbind(v.a)
  
  v.b <- cbind(v.b)
  
  v.c <- cbind(v.c)
  
  v.theta <- cbind(v.theta)
  
  
  # matriz com os valores do par?metro de discrimina??o
  
  m.a <- kronecker(matrix(1,nrow(v.theta),1),t(v.a))
  
  
  # matriz com os valores do par?metro de discrimina??o
  
  m.b <- kronecker(matrix(1,nrow(v.theta),1),t(v.b))
  
  
  # matriz com os valores do par?metro de discrimina??o
  
  m.c <- kronecker(matrix(1,nrow(v.theta),1),t(v.c))
  
  
  # matriz com os valores dos tra?os latentes
  
  m.theta <- matrix(v.theta,nrow(v.theta),nrow(v.b))
  
  
  # matriz com as probabilidades de resposta correta
  
  
  m.P <-  m.c + (1 - m.c)/(1 + exp(-D*(m.a)*(m.theta - m.b)))
  
  # matriz com as probabilidades de resposta incorreta
  
  m.Q <- 1 - m.P
  
  ##################################
  # deixa os resultados dispon?veis
  
  result.prob.tri.dic <- list(m.P=m.P,m.Q=m.Q,m.a=m.a,m.b=m.b,m.c=m.c)
  
  return(result.prob.tri.dic)
  
}


prob.tri.Mult.MRD <- function(m.a,v.b,v.c,m.theta,D)
  # v.a : vetor com os par?metros de discrimina??o
  # v.b : vetor com os par?metros de dificuldade
  # v.c : vetor com os par?metros de acerto ao acaso
  # v.theta : vetor com os tra?os latentes
  # D : escalar com o par?metro de modifica??o da fun??o de
  #     liga??o.
  
{
  ######################################################
  # assegura que os vetores de par?metros ser?o vetores
  # coluna
  #v.a <- cbind(v.a)
  v.b <- cbind(v.b)
  v.c <- cbind(v.c)
  #v.theta <- cbind(v.theta)
  
  n <- nrow(m.theta)
  I <- nrow(v.b)
  # matriz com os valores do par?metro de discrimina??o
  #m.a <- kronecker(matrix(1,nrow(v.theta),1),t(v.a))
  # matriz com os valores do par?metro de discrimina??o
  m.b <- kronecker(matrix(1,nrow(m.theta),1),t(v.b))
  # matriz com os valores do par?metro de discrimina??o
  m.c <- kronecker(matrix(1,nrow(m.theta),1),t(v.c))
  # matriz com os valores dos tra?os latentes
  # m.theta <- matrix(v.theta,nrow(v.theta),nrow(v.b))
  
  # matriz vom os os preditores aleat?rios
  m.predito <- m.theta%*%t(m.a) - m.b # (n x I)
  # matriz com as probabilidades de resposta correta
  #m.P.c <- matrix(pnorm(c(m.predito)),n,I) # (n x I)
  m.P.c <- 1/(1 + exp(-D*m.predito))
  
  m.P <-  m.c + (1 - m.c)*m.P.c
  # matriz com as probabilidades de resposta incorreta
  m.Q <- 1 - m.P
  ##################################
  # deixa os resultados dispon?veis
  result.prob.tri.probito.MRD <- list(m.P=m.P,m.Q=m.Q,m.a=m.a,m.b=m.b,m.c=m.c,m.predito=m.predito)
  return(result.prob.tri.probito.MRD)
  
}


prob.tri.gen.MRD <- function(v.a,v.b,v.c,v.d,v.theta,spec,...)
  
  # v.a : vetor com os par?metros de discrimina??o
  # v.b : vetor com os par?metros de dificuldade
  # v.c : vetor com os par?metros de acerto ao acaso
  # v.theta : vetor com os tra?os latentes
  # D : escalar com o par?metro de modifica??o da fun??o de
  #     liga??o.
  
{
  
  ######################################################
  # assegura que os vetores de par?metros ser?o vetores
  # coluna
  G  <- get(paste("p", spec, sep = ""), mode = "function")  
  
  v.a <- cbind(v.a)
  
  v.b <- cbind(v.b)
  
  v.c <- cbind(v.c)
  
  v.d <- cbind(v.d)
  
  v.theta <- cbind(v.theta)
  
  n <- nrow(v.theta)
  
  I <- nrow(v.b)
  
  # matriz com os valores do par?metro de discrimina??o
  
  m.a <- kronecker(matrix(1,nrow(v.theta),1),t(v.a))
  
  
  # matriz com os valores do par?metro de discrimina??o
  
  m.b <- kronecker(matrix(1,nrow(v.theta),1),t(v.b))
  
  
  # matriz com os valores do par?metro de discrimina??o
  
  m.c <- kronecker(matrix(1,nrow(v.theta),1),t(v.c))
  
  
  # matriz com os valores dos tra?os latentes
  
  m.theta <- matrix(v.theta,nrow(v.theta),nrow(v.b))
  
  
  # matriz vom os os preditores aleat?rios
  
  
  m.predito <- m.a*(m.theta - m.b) # (n x I)
  
  
  # matriz com as probabilidades de resposta correta
  
  
  #m.P.c <- matrix(pnorm(c(m.predito)),n,I) # (n x I)
  
  m.P.c <-c(G(m.predito,...))
  
  m.P <-  m.c + (1 - m.c)*m.P.c
  
  
  # matriz com as probabilidades de resposta incorreta
  
  m.Q <- 1 - m.P
  
  ##################################
  # deixa os resultados dispon?veis
  
  result.prob.tri.probito.MRD <- list(m.P=m.P,m.Q=m.Q,m.a=m.a,m.b=m.b,m.c=m.c,m.predito=m.predito)
  
  return(result.prob.tri.probito.MRD)
  
}

#################################################################################################
###########################################################################################################
#####################  TRI #########################################3

library(plotrix)

# Generating points and quadrature weights for a given densityu
## input
# nqp: number of quadrature points
# spec: type of distribution, eg "norm","t", "sn"
# llimit: lower limit of the quadrature points
# ulimit: upper limit of the quadrature points
## output
# vqp: quadrature points
# vdens: density evaluated at the quadrature points
# vqw: quadrature weights
# vinc: incremente used for the quadrature points
gen.points.weight.quad<-function(nqp,spec,llimit,ulimit,...)
{
  G  <- get(paste("d", spec, sep = ""), mode = "function")  
  vqp <- c(seq(llimit,ulimit,length.out = nqp))#c(G(nT,...))
  vdens <-c(G(vqp,...))
  vinc <- rep((ulimit - llimit)/(nqp - 1),nqp)
  vqw <-  c(vdens*vinc)
  result<-list(vqp=vqp,vdens=vdens,vqw=vqw,vinc=vinc)
  return(result)
}

#gen.points.weight.quad(10,"norm",-4,4,mean=0,sd=1)

# Generating the weighted likelihoods (by the quadrature weights) 
# (dichotomous items)
## input
# mP: matrix with the correct response evaluated at the quadrature points
# mY: matrix with the responses
# vqw: vector with the quadrature points
# mV: matrix with the response indicator
## output
# mlogliksubQw: matrix with the log-likelihood per subject (probability of the 
# observed subject pattern) for each quadrature points (n x nqp)
# mLiksubQw = exp(mlogliksubQw)
# vsLthetaQw = apply(mLiksubQw,1,sum)
# msLthetaqw = matrix(vsLthetaQw,nrow(vsLthetaQw),ncol(mLiksubQw)) (n x nqp)
gen.weight.lik.dic.items<-function(mP,mY,vqw,mV)
{
  #mQ <- 1-mP
  # matrix with the quadrature weights
  mqw <- t(matrix(vqw,length(vqw),nrow(mY)))
  # matrix with the subject log-likelihood (n x nqp)
  mlogliksubQw <- as.matrix(mV*mY)%*%log(t(mP)) + as.matrix(mV*(1 - mY))%*%log(1 - t(mP)) + log((mqw)) # (n x q)
  # matrix with the subject ikelihood (n x nqp)
  mLiksubQw <- exp(mlogliksubQw)
  # vector with the sum of the Likelihood per quadrature points 
  vsLthetaQw <- cbind(apply(mLiksubQw,1,sum))
  # matrix with the the sum of the weighted likelihoods
  msLthetaqw <- matrix(vsLthetaQw,nrow(vsLthetaQw),ncol(mLiksubQw))
  result <- list(mlogliksubQw=mlogliksubQw,mLiksubQw=mLiksubQw,
                 vsLthetaQw=vsLthetaQw,msLthetaqw=msLthetaqw)
  return(result)
}

# Generates the so-called artificial data
#
## input
# mLiksubQw = matrix with the likelihood per subject (probability of the 
# observed subject pattern) for each quadrature points (n x nqp)
# vsLthetaQw = apply(mLiksubQw,1,sum)
# msLthetaqw = matrix(vsLthetaQw,nrow(vsLthetaQw),ncol(mLiksubQw)) (n x nqp)
# mY: matrix with the responses
# mV: matrix with the response indicator
## output
# vfbil: expected number of subjects with latent trait around the value of 
# each quadrature point (nqp x 1)
# dfbil:expected proportion of subjects with latent trait around the value of 
# each quadrature point (nqp x 1)
# mrbil: expected number of subjects latent trait around the value of 
# each quadrature point that answer correctly eack item (nqp x vI)
# pbil: expected proprotion of subjects latent trait around the value of 
# each quadrature point that answer correctly eack item (nqp x vI)
gen.art.data.dic.item <-function(mLiksubQw,msLthetaqw,mY,mV)
{
  # Generating the artificial data (dichotomous items)
  # matrix witht the expected number of subjects 
  # along the quadrature points (n x nqp)
  mfbil <- mLiksubQw/msLthetaqw 
  # (q x 1)
  vfbil <- cbind(apply(mfbil,2,sum))
  dfbil <- vfbil/sum(vfbil)
  # (q x I)
  mrbil <- (t(mfbil))%*%as.matrix(mY*mV) # (q x I)
  #
  pbil <- mrbil/matrix(vfbil,nrow(vfbil),ncol(mrbil))
  result<-list(vfbil=vfbil,dfbil=dfbil,mrbil=mrbil,pbil=pbil)
}

################################################
################################################


# Generating replicas calculating the predicted scores distributions,
# predicted probability of correct response and residualdeviance

## input
# va,vb,vc,vc : vector with the item parameter values
# vtheta: vector with the latentr traits
# nrep: number of replications
#
## output
# mYR: matrix with the simulated responses

gen.rep.dic.IRT<-function(va,vb,vc,vd,vtheta,nrep)
{
  vI <- length(va)
  n<- length(vtheta)
  mP<-prob.tri.MRD(cbind(va),cbind(vb),cbind(vc),cbind(vtheta),1)$m.P
  mU<-cbind(replicate(vI,runif(n)))
  mY <- as.matrix(ifelse(mP>=mU,1,0))
  mYR <- rbind(1,mY)
  #
  for (r in 2:nrep)
  {
    mP<-prob.tri.MRD(cbind(va),cbind(vb),cbind(vc),cbind(vtheta),1)$m.P
    mU<-cbind(replicate(vI,runif(n)))
    mY <- as.matrix(ifelse(mP>=mU,1,0))
    mYR <- cbind(mYR,rbind(r,mY))
  }
  return(mYR)    
  #mYR[,mYR[1,]==r]  
}

gen.rep.dic.Mult.IRT<-function(ma,vb,vc,mtheta,D,nrep)
{
  vI <- nrow(ma)
  n<- nrow(mtheta)
  M<- ncol(mtheta)
  mP<-prob.tri.Mult.MRD(ma,vb,vc,mtheta,D)$m.P
  mU<-cbind(replicate(vI,runif(n)))
  mY <- as.matrix(ifelse(mP>=mU,1,0))
  mYR <- rbind(1,mY)
  #
  for (r in 2:nrep)
  {
    mP<-prob.tri.Mult.MRD(ma,vb,vc,mtheta,D)$m.P
    
    mU<-cbind(replicate(vI,runif(n)))
    mY <- as.matrix(ifelse(mP>=mU,1,0))
    mYR <- cbind(mYR,rbind(r,mY))
  }
  return(mYR)    
}


# Estimate item parameters and latent trait
# based on the replicas

## input:
# model: latent trait dimension (in the most basice versions)
# or it can be a more informative argument as
#model<- mirt.model('F = 1-22,
#                          PRIOR = (1-22, a, lnorm, .2, .2), 
#                          (1-22, b, norm, 0, 1),
#                          (1-22, c, beta, 1,5)')
# number.item.par.basic: number of bnasic IRT parameters a,b,c,d
# number.item.par: number of IRT paramaters related to the IRF
# irt.model: IRF name, e.g., c(rep('old1PL',vI))
# customItems: additional nomenclature list(old1PL=P.old1PLfunc)
# method: item parameter estimation method, e.g. "EM"
# theta.method: latent trait estimation methods, "ML"
#
# output
# mPRtheta: correct response probability based on the latent traits
# mPRqp: correct response probability based on the quadrature points
estimate.item.theta.replica.dic.IRT <-function(mYR,model,irt.model,
                                               customItems,method,theta.method,
                                               number.item.par,
                                               number.item.par.basic,vqp,nrep,vI)
{
  
  ind.item <- matrix(0,number.item.par,vI)
  aux.ind <- 0
  for (i in 0 : (number.item.par-1))
  {
    ind.item[number.item.par-i,] <- number.item.par*seq(1,vI,1)-aux.ind
    aux.ind <- aux.ind + 1 
  }
  #mzeta <- matrix(0,vI,number.item.par)
  n<- nrow(mYR)-1
  mYaux <- mYR[2:(n+1),mYR[1,]==1]
  colnames(mYaux) <- c(paste("i",seq(1:vI),sep=""))
  resultmirt <- mirt(mYaux,model,irt.model,customItems=customItems,
                     SE=TRUE,method=method)
  resultaux <- as.data.frame(coef(resultmirt,printSE=T,simply=T))
  if(number.item.par.basic == 1)
  {
    va <- rep(1,vI)
    vb <- as.numeric(resultaux[1,ind.item[1,]])
    vc <- rep(0,vI)
    vd <- rep(0,vI)
  } 
  else if(number.item.par.basic == 2)
  {
    va <- as.numeric(resultaux[1,ind.item[1,]])
    vb <- as.numeric(resultaux[1,ind.item[2,]])
    vc <- rep(0,vI)
    vd <- rep(0,vI)
  }
  else if(number.item.par.basic == 3)
  {
    va <- as.numeric(resultaux[1,ind.item[1,]])
    vb <- as.numeric(resultaux[1,ind.item[2,]])
    vc <- as.numeric(resultaux[1,ind.item[3,]])
    vd <- rep(0,vI)
  }
  else if (number.item.par.basic == 4)
  {
    
  }
  rtheta <- fscores(resultmirt,method=theta.method,full.scores.SE=TRUE)[,1]
  #
  # Based on theta
  mPa1 <- prob.tri.MRD(cbind(va),cbind(vb),cbind(vc),cbind(rtheta),1)$m.P
  mPRtheta<- rbind(1,mPa1)
  #
  # Based on quadrature points
  mPa2 <- prob.tri.MRD(cbind(va),cbind(vb),cbind(vc),cbind(vqp),1)$m.P
  mPRqp <- rbind(1,mPa2)
  
  for (r in 2:nrep)
    
  {
    mYaux <- mYR[2:(n+1),mYR[1,]==r]
    colnames(mYaux) <- c(paste("i",seq(1:vI),sep=""))
    resultmirt <- mirt(mYaux,model,irt.model,customItems=customItems,
                       SE=TRUE,method=method)
    resultaux <- as.data.frame(coef(resultmirt,printSE=T,simply=T))
    if(number.item.par.basic == 1)
    {
      va <- rep(1,vI)
      vb <- as.numeric(resultaux[1,ind.item[1,]])
      vc <- rep(0,vI)
      vd <- rep(0,vI)
    } 
    else if(number.item.par.basic == 2)
    {
      va <- as.numeric(resultaux[1,ind.item[1,]])
      vb <- as.numeric(resultaux[1,ind.item[2,]])
      vc <- rep(0,vI)
      vd <- rep(0,vI)
    }
    else if(number.item.par.basic == 3)
    {
      va <- as.numeric(resultaux[1,ind.item[1,]])
      vb <- as.numeric(resultaux[1,ind.item[2,]])
      vc <- as.numeric(resultaux[1,ind.item[3,]])
      vd <- rep(0,vI)
    }
    else if (number.item.par.basic == 4)
    {
      
    }
    rtheta <- fscores(resultmirt,method=theta.method,full.scores.SE=TRUE)[,1]
    #
    # Based on theta
    mPa1 <- prob.tri.MRD(cbind(va),cbind(vb),cbind(vc),cbind(rtheta),1)$m.P
    mPRtheta <- cbind(mPRtheta,rbind(r,mPa1))
    #
    # Based on quadrature points
    mPa2 <- prob.tri.MRD(cbind(va),cbind(vb),cbind(vc),cbind(vqp),1)$m.P
    mPRqp <- cbind(mPRqp,rbind(r,mPa2))
    
    
  }
  
  result <- list(mPRtheta=mPRtheta,mPRqp=mPRqp)
  
}

# plotting the estimated density using artificial data
#
## input:
# vqw: quadrature weights
# vqp: quadrature points
# vinc: between quadrature points increment
# plotdens: if "TRUE" plot an estimated density, based on the quadrature points
# spec: type of distribution, eg "norm","t", "sn"
# type: "parametric" or "non-parametric" density of the latent traits
# based on the quadrature points

plot.est.dens.lat.trait <-function(vqw,vqp,dfbil,vinc,plotdens,spec,type,
                                   col="gray",cex.axis=1.2,cex.lab=1.2,
                                   cex.main=1.2,...)
{
  G  <- get(paste("d", spec, sep = ""), mode = "function")  
  if (type == "parametric")
  {
    vdens <-c(G(vqp,...))
  }
  else if (type=="non-parametric")
  {
    y<-density(vqp,n=length(vqp))$y 
  }
  edens <- c(dfbil/abs(vinc))
  aux<-barplot(edens,names.arg=round(vqp,1),col=col,cex.axis=cex.axis,
               cex.lab=cex.lab,cex.main=cex.main,xlab="quadrature points",
               ylab="density (quadrature weights)")
  if (plotdens == "TRUE")
  {
    if (type == "parametric")
    {
      lines(aux,vdens,lwd=2,lty=2)  
    }
    else if (type=="non-parametric")
    {
      lines(aux,y,lwd=2,lty=2)  
    }
  }
  return(edens)
}

# Generates samples of the artificial data
# 
## input:
# mYR: matrix with the simulated response matrices
# mPRqp: matrix with the probability of correct response, based on mYR, and the
# quadrature points
# vqw: quadrature weights
# nqp: quadrature points
# q1, q2: limits for the confidence intervals for the expected proportion
# of correct response, based on the artificial data
#
## output:
# mpbilR: matrix with the replicas of the the expected proportion
# of correct response, based on the artificial data
# mpbilmed: median of mpbilR, per replicas (for each item and each quadrature points)  
# mpbilLLCI: lower limit of the CI of mpbilR, per replicas (for each item and each quadrature points)  
# mpbilULCI: upper limit of the CI of mpbilR, per replicas (for each item and each quadrature points)  
# auxitempc: vector of indexes to be used in the function plot.prob.CCI.art.data.dic.item
calc.sample.art.data.dic.item <- function(mYR,mPRqp,mV,vqw,nqp,q1,q2)
{
  nrep <- max(mYR[1,])
  n<- nrow(mYR)-1
  vI <- ncol(mV)
  vauxitem <- c(kronecker(seq(1:vI),matrix(1,nqp,1)))
  mpbilR <- matrix(0,nrep,length(vauxitem))
  #
  for (r in 1:nrep)
  {
    mYaux <- mYR[2:(n+1),mYR[1,]==r]
    #mYaux <- mYaux[2:(n+1),]
    mPqpaux <- mPRqp[2:(nqp+1),mPRqp[1,]==r]
    #mPqpaux <- mP[2:(n+1),]
    #
    resultLik <- gen.weight.lik.dic.items(mPqpaux,mYaux,vqw,mV)
    mLiksubQw <- resultLik$mLiksubQw
    msLthetaqw <- resultLik$msLthetaqw
    resultAD <-gen.art.data.dic.item(mLiksubQw,msLthetaqw,mYaux,mV)
    pbil <- resultAD$pbil
    mpbilR[r,] <- c(pbil)
    #
  }
  
  auxitempc <- c(kronecker(seq(1:vI),c(matrix(1,nqp,1))))
  
  mpbilmed <- apply(mpbilR,2,quantile,0.5)
  mpbilLLCI <- apply(mpbilR,2,quantile,q1)
  mpbilULCI <- apply(mpbilR,2,quantile,q2)
  
  result<-list(mpbilR=mpbilR,mpbilmed=mpbilmed,mpbilLLCI=mpbilLLCI,
               mpbilULCI=mpbilULCI,auxitempc=auxitempc)
  
  return(result)
  
}

# Building the plots with observed (artificial data) and predict (ICC curve)
# per item

## input
# resultMIRT: object with the mirt fit
# item: number of the item to be plotted
# mpbilmed, mpbilLLCI,mpbilULCI, auxitempc: 
# see function calc.sample.art.data.dic.item
# vqp: quadrature points

plot.prob.CCI.art.data.dic.item <- function(resultMIRT,item,mpbilmed,
                                            mpbilLLCI,mpbilULCI,
                                            auxitempc,vqp)
{
  plt <- itemplot(resultMIRT,item,type="trace",facet_items=FALSE) # curva característica do item (cci)
  plot(plt$panel.args[[1]]$x,plt$panel.args[[1]]$y,type="l",col="blue",
       cex.axis=1.2,cex=1.2,cex.lab=1.2,xlab="latent trait",
       ylab="probability of correct response",main=paste("Item:",sep="",item),
       ylim=c(0,1))
  #
  mpbilmeda<- mpbilmed[auxitempc==item]
  mpbilLLCIa<- mpbilLLCI[auxitempc==item]
  mpbilULCIa<- mpbilULCI[auxitempc==item]
  #
  plotCI(vqp,mpbilmeda,li=mpbilLLCIa,ui=mpbilULCIa,
         col="black",pch=19,cex=1.2,add=TRUE,lwd=2)
  #lines(vqp,mpbilmeda,col="",lwd=2,lty=2)
  
  #    lines(vqp,pbil[,item],pch=19,type="p")
}

# Calculating the standardized residual deviance by item and by persons
# Residual deviance
## input
# mY: matrix with the responses
# mV: matrix with the indicator response
# mP: matrix with the probability of correct response
#
## output:
# mresdevitem: deviance residual per item
# mresdevsubj: deviance residual per subject

calc.res.dev.dic.IRT <-function(mY,mV,mP,rtheta,va,vb,vc)
{
  
  mP[mP==0]<-0.001
  mP[mP==1]<-0.999
  mresdev <- mV*((-sqrt(2*abs(log(1-mP)))*(1-mY)) + sqrt(2*abs(log(mP)))*(mY))
  mresdevitem <- matrix(0,nrow(mY),ncol(mY))
#  mresdevsubj <- matrix(0,nrow(mY),ncol(mY))
  mPV <- mP*mV
  vI<- ncol(mY)
    
  for (i in 1 : vI)
  {
    mXtheta <- cbind(1,-rtheta)
    mVtheta <- mPV[,i]*(1-mPV[,i])#diag(mP[,1]*(1-mP[,1]))
    mHtheta <- mXtheta%*%solve(t(mXtheta)%*%diag(mVtheta)%*%mXtheta)%*%t(mXtheta)
    mhtheta <- mVtheta*diag(mHtheta)
    mresdevitem[,i] <- mV[,i]*mresdev[,i]/sqrt(1-mhtheta)#solve(sqrt(diag(1,n,n)-mhtheta))%*%mresdev
  }
  
#  for (j in 1:n)
#  {
#    # necessary to adpat for one and three parameter models?
#   mXtheta <- cbind(va,-va*vb)
#    mVtheta <- mPV[j,]*(1-mPV[j,])#diag(mP[,1]*(1-mP[,1]))
#    mHtheta <- mXtheta%*%solve(t(mXtheta)%*%diag(mVtheta)%*%mXtheta)%*%t(mXtheta)
#    mhtheta <- mVtheta*diag(mHtheta)
#    mresdevsubj[j,] <- mV[j,]*mresdev[j,]/sqrt(1-mhtheta)#solve(sqrt(diag(1,n,n)-mhtheta))%*%mresdev
#  }
  # Take with the not presented item when plot the residuals by item
 # result <- list(mresdevitem=mresdevitem,mresdevsubj=mresdevsubj)
  result <- list(mresdevitem=mresdevitem)
  return(result)
}


# Calculates and plots the (observed) score distributions
#
## input
# mY: matrix with the responses
# mV: matrix with the indicator response
# type: "absolute" and "relative" (respectively, absolute and relative frequency)
# plot.score: "TRUE" (plot the score distribution), "FALSE", does not plot
#
## output:
# values: observed scores
# ofreq: observed frequency for observed scores
# vscoreO: score value for each subject
# vscore0F:score value for each subject (in "factor" format)

calc.plot.o.score.dist<-function(mY,mV,type,plot.score="TRUE")
{
  mYV <- mY*mV
  vscoreO <- apply(mYV,1,sum)
  auxlev <- seq(0,ncol(mYV))
  vscore0F <- factor(vscoreO,levels=auxlev)
  result <-table(vscore0F)
  ofreq<-as.vector(result[])
  values <- as.numeric(names(result))
  if (plot.score == "TRUE")
  {
    if (type == "absolute")
    {
      plot(values,ofreq,pch=19,type="b",xlab="score",ylab="frequency",cex=1.2,
           cex.lab=1.2,cex.main=1.2)
    }
    else if (type == "relative")
    {
      plot(values,ofreq/sum(ofreq),pch=19,type="b",xlab="score",ylab="frequency",cex=1.2,
           cex.lab=1.2,cex.main=1.2)
    }
  }
  else if (plot.score == "FALSE")
  {
  }
  result<-list(values=values,ofreq=ofreq,vscoreO=vscoreO,vscore0F=vscore0F)
  return(result)
}


# Calculating the predicted and observed scores distributions

# Generates the plots of observed and predicted scores distribution
#
## input:
# mY: matrix of (observed) responses
# mYR:matrix with the simualted responses 
# mV: indicator response matrix
# nrep: number of replications
# type: "absolute" e "relative" (absolute and relative frequencies will be plotted
# respectively)
# q1,q2: quantiles for the precited score distribution

plot.obs.predic.scor.dist <- function(mY,mYR,mV,nrep,type,q1=0.025,q2=0.975)
{
  result <- calc.plot.o.score.dist(mY,mV,"relative","FALSE")
  ofreq <- result$ofreq
  values <- result$values
  n <- nrow(mY)
  vI <- ncol(mY)
  #
  mescorepred <- matrix(0,nrep,(vI+1))
  #
  for (r in 1:nrep)
    
  {
    mYaux <- mYR[2:(n+1),mYR[1,]==r]
    resultaux<-calc.plot.o.score.dist(mYaux,mV,"relative","FALSE")
    mescorepred[r,]<-resultaux$ofreq
  }
  
  mescorepredMED <- apply(mescorepred,2,quantile,0.5)
  mescorepredLLCI <- apply(mescorepred,2,quantile,q1)
  mescorepredULCI <- apply(mescorepred,2,quantile,q2)
  
  if (type == "absolute")
  {
    plot(values,ofreq,pch=19,type="b",xlab="score",ylab="frequency",
         cex=1.2,cex.lab=1.2,cex.main=1.2,lwd=2,ylim=c(0,max(mescorepredULCI)))
    lines(values,mescorepredMED,col="gray",lwd=2,lty=2,type="b",pch=17,cex=1.2)
    lines(values,mescorepredLLCI,col="gray",lwd=2,lty=2,type="b",pch=15,cex=1.2)
    lines(values,mescorepredULCI,col="gray",lwd=2,lty=2,type="b",pch=15,cex=1.2)
  }
  else if (type == "relative")
  {
    plot(values,ofreq/sum(ofreq),pch=19,type="b",xlab="score",ylab="frequency",
         cex=1.2,cex.lab=1.2,cex.main=1.2,lwd=2,ylim=c(0,max(mescorepredULCI/sum(mescorepredMED))))
    lines(values,mescorepredMED/sum(mescorepredMED),col="gray",lwd=2,lty=2,type="b",pch=17,cex=1.2)
    lines(values,mescorepredLLCI/sum(mescorepredMED),col="gray",lwd=2,lty=2,type="b",pch=15,cex=1.2)
    lines(values,mescorepredULCI/sum(mescorepredMED),col="gray",lwd=2,lty=2,type="b",pch=15,cex=1.2)
  }
  
  #
  result<-list(mescorepred=mescorepred,mescorepredMED=mescorepredMED,
               mescorepredLLCI=mescorepredLLCI,
               mescorepredULCI=mescorepredULCI)
  return(result)
}

# Calculating the observed proportion of correct response
# per item

# Calculating the predicted and observed probabilities of correct response per item

## input:
# mY: response matrix
# mV: indicator response matrix
# vscore0F:score value for each subject (in "factor" format)
#
## output:
# mprocorrecres: matrix with the observed proportion of correc response
# per item, based on the observed scores

calc.o.prop.correc.response.item<-function(mY,mV,vscore0F)
{
  nulevels <- nlevels(vscore0F)
  #mprocorrecres <- matrix(0,ncol(mY),ncol(mY)+1)
  mprocorrecres <- matrix(0,ncol(mY),nulevels)
  #
  mYV <- mY*mV
  for (i in 1:ncol(mY))
  {
    mprocorrecres[i,] <- c(by(mYV[,i],vscore0F,sum))/c(by(mV[,i],vscore0F,sum))  
  }
  mprocorrecres[is.na(mprocorrecres)]<-0
  return(mprocorrecres)
}

## input
# mprocorrecres: see function calc.o.prop.correc.response.item
# item: item to plot the proportions
# ll: lower limit for the observed score distribution to be considered 
# in the plot 
# ul: upper limit for the observed score distribution to be considered
# in the plot

plot.o.prop.correc.response.item<-function(mprocorrecres,item,ll,ul)
  
{
  xaxis <- seq(ll,ul)
  mprocorrecresaux <- mprocorrecres[,ll:ul]
  plot(mprocorrecresaux[item,],cex.main=1.2,cex.lab=1.2,cex=1.2,pch=19,
       xlab="score",ylab="proportion of correct response",
       main=paste("Item :",sep=""," ",item),type="b",lwd=2) 
}

# calculates the proportion of correct response by item based on the
# scores

## input:
# mYR: simulated response matrices
# mV: indicator response matrix
# nrep: number of replications
# q1, q2: quantiles for the confience intervals
#
## output:
# mpropredcorrecres: response of the proportion of correct response
# based on the simulated responses
# mpropredcorrecresMED: median of the proportion of correct response
# mpropredcorrecresLLCI: lower limit of the confidence interval
# of the proportion of correct response
# mpropredcorrecresULCI: upper limit of the confidence interval
# of the proportion of correct response

calc.obs.pred.prop.correc.response.item<-function(mYR,mV,nrep,q1=0.025,q2=0.975)
{
  vI <- ncol(mV)
  n<- nrow(mV)
  mpropredcorrecres <- matrix(0,vI*(vI+1),nrep)
  #
  for (r in 1:nrep)
  {
    mYaux <- mYR[2:(n+1),mYR[1,]==r] 
    resultaux<-calc.plot.o.score.dist(mYaux,mV,"absolute","FALSE")
    vscore0F <- resultaux$vscore0F
    resultaux1<-calc.o.prop.correc.response.item(mYaux,mV,vscore0F)
    mprocorrecres<-resultaux1
    mpropredcorrecres[,r] <- c(t(mprocorrecres))
  }
  #
  mpropredcorrecresMED <- apply(mpropredcorrecres,1,quantile,0.5)
  mpropredcorrecresLLCI <- apply(mpropredcorrecres,1,quantile,q1)
  mpropredcorrecresULCI <- apply(mpropredcorrecres,1,quantile,q2)
  #
  auxinditem <- kronecker(seq(1:vI),c(matrix(1,1,vI+1)))
  #
  result<-list(mpropredcorrecres=mpropredcorrecres,
               auxinditem=auxinditem,mpropredcorrecresMED=mpropredcorrecresMED,
               mpropredcorrecresLLCI=mpropredcorrecresLLCI,
               mpropredcorrecresULCI=mpropredcorrecresULCI)
  
  return(result)
  
}

# plots the simulated model-based proportion of correc response 
# 
## input
# mprocorrecres, mpropredcorrecresMED, mpropredcorrecresLLCI,
# mpropredcorrecresULCI: see function calc.obs.pred.prop.correc.response.item
# item: item to plot the proportions
# ll: lower limit for the observed score distribution to be considered 
# in the plot 
# ul: upper limit for the observed score distribution to be considered
# in the plot
plot.obs.pred.prop.correc.response.item<-function(mprocorrecres,
                                                  mpropredcorrecresMED,
                                                  mpropredcorrecresLLCI,
                                                  mpropredcorrecresULCI,
                                                  auxinditem,item,
                                                  ll,ul)
{
  vscore <- seq(1,ncol(mprocorrecres))
  vscore <- vscore[ll:ul]
  mprocorrecresaux <- mprocorrecres[,ll:ul]
  plot(vscore,mprocorrecresaux[item,],cex.main=1.2,cex.lab=1.2,cex=1.2,pch=19,
       xlab="score",ylab="proportion of correct response",
       main=paste("Item :",sep=""," ",item),type="b",lwd=2,ylim=c(0,1)) 
  #
  mpropredcorrecresMEDa1 <- mpropredcorrecresMED[auxinditem==item]
  mpropredcorrecresMEDa2 <- mpropredcorrecresMEDa1[ll:ul]
  mpropredcorrecresLLCIa1 <- mpropredcorrecresLLCI[auxinditem==item]
  mpropredcorrecresLLCIa2 <- mpropredcorrecresLLCIa1[ll:ul]
  mpropredcorrecresULCIa1 <- mpropredcorrecresULCI[auxinditem==item]
  mpropredcorrecresULCIa2 <- mpropredcorrecresULCIa1[ll:ul]
  #
  plotCI(vscore,mpropredcorrecresMEDa2,li=mpropredcorrecresLLCIa2,
         ui=mpropredcorrecresULCIa2,col="gray",pch=17,lwd=2,lty=2,
         cex=1.2,add=TRUE)
  lines(vscore,mpropredcorrecresMEDa2,col="gray",lwd=2,lty=2)
  
  #lines(mpropredcorrecresMEDa2,type="b",cex=1.2,pch)
  #lines(mpropredcorrecresLLCI2,type="b",cex=1.2)
  #lines(mpropredcorrecresULCI2,type="b",cex=1.2)
  
}


# plots the simulated model-based proportion of correc response 
# 
## input
# mprocorrecres, mpropredcorrecresMED, mpropredcorrecresLLCI,
# mpropredcorrecresULCI: see function calc.obs.pred.prop.correc.response.item
# item: item to plot the proportions
# ll: lower limit for the observed score distribution to be considered 
# in the plot 
# ul: upper limit for the observed score distribution to be considered
# in the plot
plot.obs.pred.prop.correc.response.item.MGM<-function(mprocorrecres,
                                                      mpropredcorrecresMED,
                                                      mpropredcorrecresLLCI,
                                                      mpropredcorrecresULCI,
                                                      auxinditem,item,
                                                      ll,ul,labelp)
{
  vscore <- seq(1,ncol(mprocorrecres))
  vscore <- vscore[ll:ul]
  mprocorrecresaux <- mprocorrecres[,ll:ul]
  plot(vscore,mprocorrecresaux[item,],cex.main=1.2,cex.lab=1.2,cex=1.2,pch=19,
       xlab="score",ylab="proportion of correct response",
       main=paste("Item :",sep=""," ",labelp),type="b",lwd=2,ylim=c(0,1)) 
  #
  mpropredcorrecresMEDa1 <- mpropredcorrecresMED[auxinditem==item]
  mpropredcorrecresMEDa2 <- mpropredcorrecresMEDa1[ll:ul]
  mpropredcorrecresLLCIa1 <- mpropredcorrecresLLCI[auxinditem==item]
  mpropredcorrecresLLCIa2 <- mpropredcorrecresLLCIa1[ll:ul]
  mpropredcorrecresULCIa1 <- mpropredcorrecresULCI[auxinditem==item]
  mpropredcorrecresULCIa2 <- mpropredcorrecresULCIa1[ll:ul]
  #
  plotCI(vscore,mpropredcorrecresMEDa2,li=mpropredcorrecresLLCIa2,
         ui=mpropredcorrecresULCIa2,col="gray",pch=17,lwd=2,lty=2,
         cex=1.2,add=TRUE)
  lines(vscore,mpropredcorrecresMEDa2,col="gray",lwd=2,lty=2)
  
  #lines(mpropredcorrecresMEDa2,type="b",cex=1.2,pch)
  #lines(mpropredcorrecresLLCI2,type="b",cex=1.2)
  #lines(mpropredcorrecresULCI2,type="b",cex=1.2)
  
}

# Generate plots with the CCI and the artifical data (rbil)
# without confience intervals
#
## input:
# resultMIRT: object with mirt fit
# vqp: quadrature points
# pbil: expecet proportion of correct response based on the quadrature points
# item: item to plot the figure
# plot: temporalily disabled
# filesave: temporalily disabled

gen.plot.item.fit.AD <- function(resultMIRT,vqp,pbil,item,plot,filesave)
{
  
  plt <- itemplot(resultMIRT,item,type="trace",facet_items=FALSE) # curva característica do item (cci)
  plot(plt$panel.args[[1]]$x,plt$panel.args[[1]]$y,type="l",col="blue",
       cex.axis=1.2,cex=1.2,cex.lab=1.2,xlab="latent trait",
       ylab="probability of correct response",main=paste("Item:",sep="",item))
  lines(vqp,pbil[,item],pch=19,type="p")
  #
  if (plot == "all")
    # save all plots in a pdf file
  {
    pdf(file=filesave)
    for (i in 1:ncol(pbil))
    {
      plt <- itemplot(resultMIRT,i,type="trace",facet_items=FALSE) # curva característica do item (cci)
      plot(plt$panel.args[[1]]$x,plt$panel.args[[1]]$y,type="l",col="blue",
           cex.axis=1.2,cex=1.2,cex.lab=1.2,xlab="latent trait",ylab="probability of correct response")
      lines(vqp,pbil[,i],pch=19,type="p",main=paste("Item:",sep="",item))
    }
    dev.off()
  }
  
  
}

gen.plot.item.fit.AD.2 <- function(mzeta,vqp,pbil,item,plot,filesave)
{
  
  mP<-prob.tri.MRD(cbind(mzeta[,1][item]),cbind(mzeta[,2][item]),
                   cbind(mzeta[,3][item]),cbind(vqp),1)$m.P
  plot(vqp,mP,type="l",col="blue",
       cex.axis=1.2,cex=1.2,cex.lab=1.2,xlab="latent trait",
       ylab="probability of correct response",main=paste("Item:",sep="",item))
  lines(vqp,pbil[,item],pch=19,type="p")
  #
  if (plot == "all")
    # save all plots in a pdf file
  {
    pdf(file=filesave)
    for (i in 1:ncol(pbil))
    {
      plt <- itemplot(resultMIRT,i,type="trace",facet_items=FALSE) # curva característica do item (cci)
      plot(plt$panel.args[[1]]$x,plt$panel.args[[1]]$y,type="l",col="blue",
           cex.axis=1.2,cex=1.2,cex.lab=1.2,xlab="latent trait",ylab="probability of correct response")
      lines(vqp,pbil[,i],pch=19,type="p",main=paste("Item:",sep="",item))
    }
    dev.off()
  }
  
  
}

# Calculatin p-values related do the observed and predicted scores
# distributions

p.value.obs.pred.score.dist <-function(mY,mV,mYR1,mYR2,nrep)
  
{
  
  ofreqO <- calc.plot.o.score.dist(mY,mV,"absolute",plot.score="FALSE")$ofreq
  ofreqO[ofreqO==0]<-0.01
  #  
  m.scores <- matrix(0,nrep,2)
  for (r in 1:nrep)
  {
    mY1 <- mYR1[2:(n+1),mYR1[1,]==r] 
    mY2 <- mYR2[2:(n+1),mYR2[1,]==r]
    ofreqP <-calc.plot.o.score.dist(mY1,mV,"absolute",plot.score="FALSE")$ofreq
    ofreqOP <-calc.plot.o.score.dist(mY2,mV,"absolute",plot.score="FALSE")$ofreq
    ofreqP[ofreqP==0] <- 0.01
    ofreqOP[ofreqOP==0] <- 0.01
    disc.measure.1 <- sum((ofreqO-ofreqP)^2/(ofreqP))
    disc.measure.2 <- sum((ofreqOP-ofreqP)^2/(ofreqP))
    m.scores[r,1:2] <- c(disc.measure.1,disc.measure.2)
  }
  
  pvalue <- mean(as.numeric(m.scores[,2]>= m.scores[,1]))
  return(pvalue)
  
}

# Calculatin p-values related do the observed and predicted proportion
# of correct response per item by score

p.value.obs.pred.prop.correc.item <-function(mY,mV,mYR1,mYR2,nrep)
  
{
  
  auxinditem <- kronecker(seq(1:vI),c(matrix(1,1,vI+1)))
  vI <- ncol(mY)
  vscore0F<-calc.plot.o.score.dist(mY,mV,type,"FALSE")$vscore0F
  mprocorrecres<-calc.o.prop.correc.response.item(mY,mV,vscore0F)#$mprocorrecres
  vpropcorrecO <- c(t(mprocorrecres))
  vpropcorrecO[vpropcorrecO==0]<-0.01
  m.prop.correc.1 <- matrix(0,nrep,vI)
  m.prop.correc.2 <- matrix(0,nrep,vI)
  
  for (r in 1:nrep)
  {
    mY1 <- mYR1[2:(n+1),mYR1[1,]==r] 
    mY2 <- mYR2[2:(n+1),mYR2[1,]==r]
    vscore0F1<-calc.plot.o.score.dist(mY1,mV,type,"FALSE")$vscore0F
    mprocorrecres1<-calc.o.prop.correc.response.item(mY1,mV,vscore0F)#$mprocorrecres
    vscore0F2<-calc.plot.o.score.dist(mY2,mV,type,"FALSE")$vscore0F
    mprocorrecres2<-calc.o.prop.correc.response.item(mY2,mV,vscore0F)#$mprocorrecres
    vpropcorrecO1 <- c(t(mprocorrecres1))
    vpropcorrecO2 <- c(t(mprocorrecres2))
    vpropcorrecO1[vpropcorrecO1==0]<-0.01
    vpropcorrecO2[vpropcorrecO2==0]<-0.01
    disc.measure.1 <- c(by((vpropcorrecO-vpropcorrecO1)^2/vpropcorrecO1,auxinditem,sum))
    disc.measure.2 <- c(by((vpropcorrecO2-vpropcorrecO1)^2/vpropcorrecO1,auxinditem,sum))
    m.prop.correc.1[r,] <- disc.measure.1 
    m.prop.correc.2[r,] <-disc.measure.2 
  }
  
  pvalue <- apply(matrix(as.numeric(m.prop.correc.2>= m.prop.correc.1),nrep,vI),2,mean)
  plot(pvalue,xlab="item",ylab="p-value",pch=19,cex=1.2,cex.lab=1.2,cex.main=1.2)
  return(pvalue)
  
}

# generates the predicted distribution of the deviance residuals

calc.conf.band.res.dev.dic.IRT <- function(mYR,mV,mPRtheta,nrep,rtheta,va,vb,
                                           vc)
{
  vI <- length(va) 
  n <- length(rtheta)
  #numrespitem <- c(apply(mV,2,sum))
  mresdevitemaux <- matrix(0,n*vI,nrep)
  auxitemres <- kronecker(c(seq(1:vI)),c(matrix(1,1,n)))
  #auxitemres <- matrix(1,1,numrespitem[1])#matrix(0,1,sum(numrespitem))
  
  #for (i in 2:vI)
  #{
  
  #auxitemres <- c(auxitemres,matrix(i,1,numrespitem[i]))
  
  #}
  for (r in 1:nrep)
  {
    mYaux <- mYR[2:(n+1),mYR[1,]==r] 
    #mY2 <- mYR2[2:(n+1),mYR2[1,]==r]
    mPthetaaux <- mPRtheta[2:(n+1),mPRtheta[1,]==r] 
    #mPtheta2 <- mPRtheta2[2:(n+1),mPRtheta2[1,]==r]
    mresdevitemaux[,r]<-c(apply(calc.res.dev.dic.IRT(mYaux,mV,mPthetaaux,rtheta,va,vb,vc)$mresdevitem,2,sort))
    #mresdevitem2<-calc.res.dev.dic.IRT(mY2,mV,mP2,rtheta,va,vb,vc)$mresdevitem
  }
  
  mresdevitemmed <- apply(mresdevitemaux,1,quantile,0.5)
  #mresdevitemLLCI <- apply(mresdevitem,1,quantile,q1)
  #mresdevitemULCI <- apply(mresdevitem,1,quantile,q2)
  mresdevitemaux1<- t(apply(mresdevitemaux,1,sort))
  mresdevitemLLCI <- (mresdevitemaux1[,2]+mresdevitemaux1[,3])/2
  mresdevitemULCI <- (mresdevitemaux1[,nrep-1]+mresdevitemaux1[,nrep-2])/2
  
  result<- list(mresdevitemmed=mresdevitemmed,mresdevitemLLCI=mresdevitemLLCI,
                mresdevitemULCI=mresdevitemULCI)
  return(result)
  
}

# generates the qq plot with confidence bands of the deviance residuals


plot.qq.plot.conf.bands<-function(mresdevitem,
                                  mresdevitemmed,mresdevitemLLCI,
                                  mresdevitemULCI,item)
{
  
  # auxescitem <- auxitemres[auxitemres==item]
  mresdevitemitem <- (mresdevitem[,item])
  mresdevitemmeditem <- (mresdevitemmed[auxitemres==item])
  mresdevitemLLCIitem <- (mresdevitemLLCI[auxitemres==item])
  mresdevitemULCIitem <- (mresdevitemULCI[auxitemres==item])
  aux <- range(mresdevitemitem,mresdevitemLLCIitem,mresdevitemULCIitem) 
  qqnorm(mresdevitemitem,xlab="standard normal quantile",
         ylab="standardized deviance residual", ylim=aux, 
         pch=16, main="",cex=1.1,cex.axis=1.1,cex.lab=1.1)
  par(new=T)
  qqnorm(mresdevitemLLCIitem,axes=F,xlab="",ylab="",type="l",ylim=aux,lty=1, main="")
  par(new=T)
  qqnorm(mresdevitemULCIitem,axes=F,xlab="",ylab="", type="l",ylim=aux,lty=1, main="")
  par(new=T)
  qqnorm(mresdevitemmeditem,axes=F,xlab="",ylab="",type="l",ylim=aux,lty=2, main="")
  
}



gen.diag.mult.tetrachoric.IRT <- function(mYR,mY,Mmax,nrep,q1,q2)
  
{
  
  oeigen <- eigen(tetrachoric(mY)$rho)$values
  meigen <- matrix(0,nrep,Mmax)  
  for (r in 1:nrep)
  {
    mYaux <- mYR[2:(n+1),mYR[1,]==r]
    mYauxNA <- mYaux
    mYauxNA[mV==0]<-NA  
    #mcor<-tetrachoric(mYauxNA)$rho  
    meigen[r,]<- (eigen(tetrachoric(mYauxNA)$rho)$values)[1:Mmax]
  }
  meigenmed <- apply(meigen,2,quantile,0.5)
  meigenLLCI <- apply(meigen,2,quantile,q1)
  meigenULCI <- apply(meigen,2,quantile,q2)
  #
  plot(seq(1,Mmax,1),oeigen[1:Mmax],xlab="dimension",ylab="eigenvalue",
       pch=19,cex=1.2,cex.lab=1.2,cex.axis=1.2,col="gray",
       ylim=c(min(c(meigenLLCI)),max(c(meigenULCI))))
  plotCI(meigenmed,li=meigenLLCI,ui=meigenULCI,add=TRUE,pch=17,cex=1.2)
  #
}



probtri.transf.MRN<-function(vtheta,vmi,mzeta,I)
{
  ##############################################################
  #=============================================================
  # vtheta : vetor com os tra?os latentes
  # vmi : n?mero de categorias
  # mzetae : matriz com os valores dos parametros transformados (sum(vmi) x 2)
  # parametriza??o de Bock and Aitkin
  # I : n?mero de itens
  #=============================================================
  ##############################################################
  
  ##################################
  # Garante que as quantidades
  # sejam das dimensões convenientes
  
  vtheta <- cbind(vtheta) # vetor com os tra?os latentes
  vmi <- cbind(vmi) # vetor com o n?mero de categorias
  # de cada item
  mzeta <- cbind(mzeta)
  
  #################
  # cria a matriz B
  B <- cbind(1,vtheta)
  i <- 1 # inicializa o contador
  catpr <- 1 # vetor auxiliar que delimita índices de matrizes
  
  #############################################################
  ### matriz utilizada no cálculo das probabilidades de escolha
  #############################################################
  mZ <- exp(B%*%(t(mzeta[catpr : (catpr + vmi[i] - 1),])))
  
  ##############################
  # soma as linhas da matriz m.Z
  vsomaZ <- cbind(apply(mZ,1,sum))
  
  #####################################
  # matriz com as somas das linhas de Z
  msomaZ <- matrix(vsomaZ,nrow(vsomaZ),ncol(mZ))
  
  ######################################################
  # inicializa??o da matriz de probabilidades de escolha
  mP <- mZ / msomaZ
  
  #########################
  # atualiza??o do contador
  catpr <- catpr + vmi[i]
  
  ##########################################################################
  # la?o para construir a matriz de porbabilidades de escolha (n x sum(v.m))
  # só entra no la?o se o n?mero de itens for maior do que 1
  
  if (I > 1)
  {
    for(i in 2:I)
    {
      #########################################
      # matriz com as probabilidades de escolha
      mZ <- exp(B%*%(t(mzeta[catpr : (catpr + vmi[i] - 1),])))
      
      ##############################
      # soma as linhas da matriz m.Z
      vsomaZ <- cbind(apply(mZ,1,sum))
      
      #####################################
      # matriz com as somas das linhas de Z
      msomaZ <- matrix(vsomaZ,nrow(vsomaZ),ncol(mZ))
      
      ######################################################
      # atualiza??o da matriz de probabilidades de escolha
      mP <- cbind(mP,mZ/msomaZ)
      
      ##########################
      # atualiza??o do contador
      catpr <- catpr + vmi[i]
      
      ##################################
    }# for : para a constru??o do la?o
    # para os itens
    ##################################
    
    #####################################
  }# if : para verificar se existem mais
  # do que um item
  #####################################
  
  ##################################
  ## deixa os resultados dispon?veis
  resultprobtriorigMRN<-list(mP=mP,vsomaZ=vsomaZ)
  
  return(resultprobtriorigMRN)
  
  ############################
} # fun??o : probtri.orig.MRN
############################


gera.veross.MRN <- function(mY,mV,mP,vAl,I,vmi,escveros)
  
{
  
  ##############################################
  #=============================================
  # m.Y : matriz com as respostas dos indivíduos
  # m.P : matriz com as probabilidades de escolha
  #  Al : matriz com os pesos de quadratura 
  #       (quando amnbos os par?metros dos itens e 
  #        tra?os latentes s?o desconhecidos) 
  # v.mi : vetor com o n?mero de categorias de 
  #        cada item
  # esc.veros : define a verossimilhança por indivíduo
  #             utilizada
  #             1 - verossimilhança genuína
  #             2 - verossimilhança ponderada pelos
  #                 pontos de quadratura
  #=============================================
  ##############################################
  
  
  #############################################
  # garante que o vetor de pontos de quadratura
  # seja coluna
  vAl <- cbind(vAl)
  
  ###################################
  if (escveros == 1) # verossimilhança sem os pontos de 
    # quadratura
    ###################################
  
  {
    
    ##################################################
    # matriz com as logverossimilhanças por indivíduo
    # m.P precisa ter o n?mero q de linhas
    # se os par?metros dos itens forem desconhecidos
    # as verossimilhaças por indivíduos encontram-se 
    # na diagonal princiapal
    
    ################
    ## por indivíduo
    ################
    mlnthetamrn <- (mV*mY)%*%log(t(mP)) # (n x n)
    
    ################################################
    # vetor com as logverossimilhanças por indivíduo
    vlnthetamrn <- cbind(diag(mlnthetamrn)) # (n x 1)
    
    ##############################################
    # matriz com as verossimilhanças por indivíduo
    mLthetamrn <- exp(mlnthetamrn)
    
    #############################################
    # vetor com as verossimilhanças por indivíduo
    vLthetamrn <- cbind(diag(mLthetamrn))
    
    ###################################
    ###################################
    
    ############
    ## por item
    ############
    
    ############################################
    # matriz com as logverossimilhanças por item
    # so precisa no MCMC
    mlnitemmrn <- (t(mY*mV))%*%log(mP) #(I x I)
    
    ########################################
    # vetor com as logverossimilhanças
    # por item
    vlnitemmrn <- cbind(diag(mlnitemmrn))# (I x 1)
    
    ##########################################
    # matriz com as verossimilhanças por item
    
    mLitemmrn <- exp(mlnitemmrn)
    
    ################################
    # vetor com as verossimilhanças
    # por item
    
    ###################################
    # este vetor tem as componentes por
    # categoria
    vLitemmrn <- cbind(diag(mLitemmrn))
    
    ####################################
    ####################################
    # La?o para a constru??o do vetor
    # de log-verossimilhanças por item
    
    ################################
    # inicializa??o do contador
    auxitem <- 1
    
    #####################################
    # matriz para armazenar as verossi-
    # lhanças por item
    vlnitemitemmrn <- matrix(0,I,1)
    
    for (i in 1 : I)
    {
      ###########################
      # preenchimento da matriz
      vlnitemitemmrn[i] <- sum(vlnitemmrn[auxitem :(auxitem + vmi[i] - 1)])
      auxitem <- auxitem + vmi[i]
      
      ###########################
      # atualiza??o do contador
      
      ################################
      # vetor com a logverossimilhança
      # por item
      
      ##############################
    } # for para a constru??o
    # do vetor de verossimilhanças
    # por categoria
    ##############################
    
    vLitemitemmrn <- exp(vlnitemitemmrn)
    
    #############################
  }# escolha da verossimilhança
  # sem os pontos de quadratura
  #############################
  
  
  ################################
  else # matriz com as verossimilhanças
    # com pontos de quadratura
    ################################
  
  {
    
    ##################################################
    # matriz com as logverossimilhanças por indivíduo
    # m.P precisa ter o n?mero q de linhas
    # se os par?metros dos itens forem desconhecidos
    
    ########################################
    # matriz com os pontos de quadratura
    mAl <- t(matrix(vAl,nrow(vAl),nrow(mY))) #(n x q)
    
    ##################################################
    # matriz com as logverossimilhanças por indivíduo
    # m.P precisa ter o n?mero ter q linhas
    # se os par?metros dos itens forem desconhecidos
    mlnthetamrn <- ((mV*mY)%*%log(t(mP))) + log((mAl)) # (n x q)
    
    ##################################################
    # matriz com as logverossimilhanças por indivíduo
    mLthetamrn <- exp(mlnthetamrn) # (n x q)
    
    #############################################
  }# verossimilhança com os pontos de quadratura
  #############################################
  
  
  ##################################
  ## deixa os resultados dispon?veis
  ##################################
  
  ###########################
  if (escveros == 1 )# verossimilhanças genuínas
    ###########################
  
  {
    resultgeraverossmrn<-list(vlnthetamrn=vlnthetamrn,vLthetamrn=vLthetamrn,
                              vlnitemitemmrn=vlnitemitemmrn,
                              vLitemitemmrn=vLitemitemmrn)
    return(resultgeraverossmrn)
    
  } # if : verossimilhanças genuínas
  
  
  else # verossimilhança ponderada (indivíduo)
    
    
  {
    
    resultgeraverossmrn<-list(mlnthetamrn=mlnthetamrn,mLthetamrn=mLthetamrn)
    
    return(resultgeraverossmrn)
    
    
    #######################################
  } # verossimilhança ponderada (indivíduo)
  #######################################
  
  ##########################
}# fun??o : gera.veross.MRN
##########################





gera.dados.art.MRN <- function(mY,mV,mLthetamrnAl)
  
{
  
  # m.Y : matriz com as respostas dos indivíduos
  # m.Ltheta.mrn : matriz com as logverossimilhanças
  #                por indivíduo (ponderada pelos pontos
  #                de quadratura)
  
  
  ## calcula os valores esperados
  ## que compõem os dados artificiais
  
  
  # vetor com as somas das verossimilhanças
  
  vsLthetaAl <- cbind(apply(mLthetamrnAl,1,sum)) # (n x 1)
  
  ############################################################
  # n?mero esperado de indivíduos
  # através dos valores das habilidades (pontos de quadratura)
  ############################################################
  
  
  ##################
  ## matriz auxiliar
  ## gj(thetal)
  
  
  ##################################
  # matriz auxiliar com as somas
  # das verossimilhanças ponderadas
  
  
  msLthetaAl <- matrix(vsLthetaAl,nrow(vsLthetaAl),ncol(mLthetamrnAl)) # (n x q)
  
  mfbil <- mLthetamrnAl/msLthetaAl #(n x q)
  
  
  ##################
  ## valor esperado
  
  vfbil <- cbind(apply(mfbil,2,sum)) # (1 x q)
  
  
  ############################################################
  # n?mero esperado de indivíduos em cada alternativa
  # através dos valores das habilidades (pontos de quadratura)
  ############################################################
  
  mrbil <- (t(mfbil))%*%(mY*mV)
  
  dfbil <- vfbil/sum(vfbil)
  # verificar 
  pbil <- mrbil/matrix(vfbil,nrow(vfbil),ncol(mrbil))
  
  ##################################
  ## deixa os resultados dispon?veis
  ##################################
  
  
  resultgeradadosartMRN <- list(mfbil=mfbil,vfbil=vfbil,
                                mrbil=mrbil,dfbil=dfbil,
                                pbil=pbil)
  
  return(resultgeradadosartMRN)
  
}


transf.resp.anskey.binary <-function(mchoice,vanswer,vncat)
{
  vI <- ncol(mchoice)
  n<-nrow(mchoice) 
  
  manswer <- t(matrix(vanswer,vncat[1],n))
  
  mYD <- matrix(as.numeric(manswer== replicate(vncat[1],mchoice[[1]])),n,vncat[1])
  for (i in 2:vI)
  {
    manswer <- t(matrix(vanswer,vncat[i],n))
    mYD <- cbind(mYD,matrix(as.numeric(manswer== replicate(vncat[i],mchoice[[i]])),n,vncat[i]))
  }
  
  return(mYD)
  
}

##############################################################
##############################################################
### Cria matriz auxiliar responsável pela transforma??o linear
### nos par?metros (restrições lineares)
##############################################################
##############################################################


cria.matriz.aux.MRN<-function(vmi)
  
{
  
  ## v.mi : vetor com o n?mero de categorias
  ## os contrastes s?o desvios com restri??o
  ## considerando a primeira catergoria como referência
  
  # cria a matriz Si
  Si <- rbind(matrix(1,1,vmi-1),(diag(-1,vmi-1,vmi-1)))
  
  # cria a matriz Ai
  Ai <- diag(1,vmi,vmi) - matrix(1/vmi,vmi,vmi)
  
  # cria a matriz Ti
  Ti <- solve(t(Si)%*%Si)%*%(t(Si))%*%Ai
  
  # deixa disponível em forma de objeto os resultados
  matriz.aux<-list(Si=Si,Ai=Ai,Ti=Ti)
  
  return(matriz.aux)
}


##########################################################
## Calcula a Probabilidade de escolha a cada categoria
## utilizando a reparametriza??o : par?metros irrestritos
##########################################################

probtri.repar.MRN<-function(vtheta,vmi,mGamma,I)
  
{
  
  #==========================================================
  # v.theta : vetor com os tra?os latentes
  # v.mi : n?mero de categorias
  # m.Gamma : matriz com os valores dos parametros irrestritos
  #           componente 1 : dificuldade (intercepto)
  #           componente 2 : discrimina??o (inclina??o)
  #==========================================================
  
  ##################################
  # Garante que as quantidades
  # sejam das dimensões convenientes
  
  vtheta <- cbind(vtheta) # vetor com os tra?os latentes
  
  vmi <- cbind(vmi) # vetor com o n?mero de categorias
  # de cada item
  
  mGamma <- cbind(mGamma)
  
  ##################
  # cria a matriz B
  
  B <- cbind(1,vtheta)
  
  i <- 1 # inicializa o contador
  
  catpr <- 1 # vetor auxiliar que delimita índices de matrizes
  
  
  ##################################
  ## cria??o das matrizes auxiliares
  
  matauxmrn <- cria.matriz.aux.MRN(vmi[i])
  Ti <- matauxmrn$Ti
  
  #############################################################
  ### matriz utilizada no cálculo das probabilidades de escolha
  #############################################################
  
  #################################
  # A parte exponencial do modelo
  mZ <- exp(B%*%(t(mGamma[catpr : (catpr + vmi[i] - 2),])) %*% Ti)
  
  ##############################
  # soma as linhas da matriz m.Z
  vsomaZ <- cbind(apply(mZ,1,sum))
  
  #####################################
  # matriz com as somas das linhas de Z
  msomaZ <- matrix(vsomaZ,nrow(vsomaZ),ncol(mZ))
  
  #######################################################
  # inicializa??o da matriz de probabilidades de escolha
  mP <- mZ / msomaZ
  
  #########################
  # atualiza??o do contador
  catpr <- catpr + vmi[i] - 1
  
  ###########################################################################
  # la?o para construir a matriz de porbabilidades de escolha (n x sum(v.m))
  
  if (I > 1)
    
  {
    
    for(i in 2:I)
      
    {
      
      ##########################
      # cria matrizes auxiliares
      matauxmrn <- cria.matriz.aux.MRN(vmi[i])
      Ti <- matauxmrn$Ti
      
      
      #########################################
      # matriz com as probabilidades de escolha
      mZ <- exp(B%*%(t(mGamma[catpr : (catpr + vmi[i] - 2),])) %*% Ti)
      
      #############################
      # soma as linhas da matriz m.Z
      vsomaZ <- cbind(apply(mZ,1,sum))
      
      #####################################
      # matriz com as somas das linhas de Z
      msomaZ <- matrix(vsomaZ,nrow(vsomaZ),ncol(mZ))
      
      ######################################################
      # atualiza??o da matriz de probabilidades de escolha
      mP <- cbind(mP,(mZ / msomaZ))
      
      #########################
      # atualiza??o do contador
      catpr <- catpr + vmi[i] - 1
      
      ####################
    } #for : para os itens
    ####################
    
    #########################
  }# if : para verificar se 
  #      existe mais do que
  #      um item
  #########################
  
  ## deixa os resultados dispon?veis
  resultprobtrireparMRN<-list(mP=mP,vsomaZ=vsomaZ)
  return(resultprobtrireparMRN)
  
  ############################
}# fun??o : probtri.repar.MRN
############################


par.ori.transf.MRN <- function(mvGamma,mcovGamma,I,vmi,covmatrix)
  
{
  
  
  ########################################################
  #=======================================================
  #
  # mv.Gamma : matriz com os valores dos par?metros irres-
  #            tritos (sum(v.mi) x 2)
  #            coluna 1 - dificuldade (delta)
  #            coluna 2 - discrimina??o (alpha)
  # mcov.Gamma : matriz com as matrizes de covari?ncia 
  #              dos par?metros de cada item (sum(v.mi) x 
  #              v.mi)
  # m.vi : matriz com o n?mero de alternativas de cada item
  #========================================================
  #########################################################
  
  
  
  #####################################################
  #### matrizes preenchidas com zeros para a constru??o
  #### dos par?metros transformados e originais
  
  
  ########################
  # valores dos par?metros
  mvzetaa <- matrix(0,sum(vmi),2) # par?metros transformados
  mvzeta <- matrix(0,sum(vmi),2) # par?metros originais
  
  if (covmatrix == 1)
  {
    ########################
    # matriz de covari?ncias
    mcovzetaa <- matrix(0,2*sum(vmi),2*(max(vmi)))    
    mcovzeta <- matrix(0,2*sum(vmi),2*(max(vmi)))    
    ########################
    # matriz com os erros-padr?o
    mepzetaa<- matrix(0,sum(vmi),2) # (sum(v.mi) x 2)
    mepzeta<- matrix(0,sum(vmi),2) # (sum(v.mi) x 2)
  }
  
  ####################
  # zera os contadores
  catpm1 <- catpm2 <- catpm3 <- catpm4 <- 1
  
  for (i in 1:I)
    
  {
    
    ##########################
    # cria matrizes auxiliares
    matriz.aux.mrn <- cria.matriz.aux.MRN(vmi[i])
    Si <- matriz.aux.mrn$Si
    Ai <- matriz.aux.mrn$Ai
    Ti <- matriz.aux.mrn$Ti
    
    #############################################
    # matriz dos par?metros irrestritos do item i 
    mGammai <- cbind(mvGamma[catpm2 : (catpm2 + vmi[i] - 2),])
    
    if (covmatrix ==1)
    {
      #############################################################
      # matriz de covari?ncias dos par?metros irrestritos do item i      
      mcovGammai <- cbind(mcovGamma[catpm3 : (catpm3 + 2*(vmi[i]-1) - 1),])
    }
    
    #############################################
    # matriz dos par?metros transformados do item
    mvzetaai <- t((t(mGammai))%*%(t(Si))%*%(ginv(Si%*%(t(Si))))) # (mi x 2)
    
    ######################################################################
    # matriz responsável pela transforma??o : irrestritos -> transformados
    mtransf <- Ai%*%(t(Ti))%*%(solve(Ti%*%(t(Ti))))
    
    if (covmatrix ==1)
    {
      #################################################################
      # Matriz de covari?ncia dos par?metros transformados
      # desenvolvimento no artigo da estima??o dos par?metros dos itens
      idenz <- diag(1,2,2) # matriz auxiliar (identidade 2 x 2)
      auxtransf <- t(Si)%*%ginv(Si %*% t(Si)) # componente da transforma??o
      auxtrasnf <- kronecker(auxtransf,idenz) # para realizar o Jacobiano
      mcovzetaai <- (t(auxtrasnf)) %*% mcovGammai %*% (auxtrasnf)
      ###########################################
      # Erros-padr?o dos par?metros trasnformados
      ###########################################
      epzetaai <- cbind(sqrt(diag(mcovzetaai))) # (2*v.mi x 1)
    }
    
    #################################################### 
    ####################################################
    # Obten??o dos resultados dos par?metros originais #
    ####################################################
    ####################################################
    
    ########################################
    ### Estimativas dos par?metros originais
    # Componente 1 : dificuldade
    # Componente 2 : discrimina??o
    mvzetai <- cbind((-(mvzetaai[,1]/mvzetaai[,2])),mvzetaai[,2]) 
    
    if (covmatrix ==1)
    {
      ###############################
      #### Matriz de covari?ncia ####
      idenaux <- diag(1,vmi[i],vmi[i]) # matriz identidade
      zeroaux <- matrix(0,vmi[i],vmi[i]) # matriz de zeros
      
      ############################################
      # Componentes da matriz Jacoabiana
      Dkappa <- diag(mvzetaai[,1]/((mvzetaai[,2])^2)) # matriz diagonal kappa
      Drho <- diag(-(1/mvzetaai[,1])) # matriz diagonal rho
      
      ############################
      # concatena??o das matrizes
      
      ###################################
      # Matriz Delta Z
      Deltaz <- rbind(cbind(Dkappa,Drho),cbind(zeroaux,idenaux))
      mcovzetai <- Delta.z%*%(mcovzetaai)%*%(t(Deltaz))
      
      ###########################################
      # Erros-padr?o dos par?metros originais
      ###########################################
      epzetai <- cbind(sqrt(diag(mcovzetai))) # (2*v.mi x 1)
      
      ########################################################
      # preeenchimento da matriz com os valores dos par?metros
      # e da matriz de  covari?ncias
      ########################################################
    }
    
    ########################
    # valores dos par?metros
    mvzetaa[catpm1 :(catpm1 + vmi[i] - 1),] <- mvzetaai
    mvzeta[catpm1 :(catpm1 + vmi[i] - 1),] <- mvzetai
    #
    ########################
    # valores da matriz de
    # covari?ncia
    if (covmatrix ==1)
    {
      mcovzetaa[catpm4 :(catpm4 + 2*vmi[i] - 1),] <- mcovzetaai
      mcovzeta[catpm4 :(catpm4 + 2*vmi[i] - 1),] <- mcovzetai
      ##############################
      # erros-padr?o dos par?metros
      # originais e transformados
      
      mepzetaa[catpm1 :(catpm1 + vmi[i] - 1),]<- 
        cbind(rbind(epzetaai[1 : (vmi[i])]),rbind(epzetaai[(vmi[i] + 1) : (nrow(epzetaai))])) # (sum(v.mi) x 2)
      
      mepzeta[catpm1 :(catpm1 + vmi[i] - 1),]<- 
        cbind(rbind(epzetai[1 : (vmi[i])]),rbind(epzetai[(vmi[i] + 1) : (nrow(epzetai))])) # (sum(v.mi) x 2)
    }
    
    ### atualiza??o dos contadores
    catpm4 <- catpm4 + 2*vmi[i]
    catpm3 <- catpm3 + 2*(vmi[i] - 1)
    catpm2 <- catpm2 + vmi[i] - 1
    catpm1 <- catpm1 + vmi[i]
    
  } # la?o
  
  
  ###################################
  ### deixa os resultados dispon?veis
  if (covmatrix ==1)
  {
    resultparoritransfMRN <- list(mvzeta=mvzeta,mvzetaa=mvzetaa,mepzetaa=mepzetaa,mepzeta=mepzeta,mcovzetaa=mcovzetaa,mcovzeta=mcovzeta)
  }
  resultparoritransfMRN <- list(mvzeta=mvzeta,mvzetaa=mvzetaa)
  
  return(resultparoritransfMRN)
  
  
  ###########################
}# fun??o par.ori.tranf.MRN
###########################


###########################################################
###########################################################
###########################################################
###########################################################
##=========================================================
##=========================================================
## Gera respostas segundo o Modelo de Resposta Nominal
##=========================================================
##=========================================================
###########################################################
###########################################################
###########################################################
###########################################################


gera.resp.MRN <- function(n,I,vmi,mezeta,mutheta,vartheta,asstheta,
                          vthetasimul,tiposimultheta,typepar)
  
{
  
  ###########################################
  #==========================================
  # n : n?mero de indivíduos submetidos ao
  #     instrumento de medida
  # I : n?mero de itens do instrumento de 
  #     medida
  # v.mi : vetor com as o n?mero de categorias
  #        de cada item
  # me.zeta : matriz com os valores dos parâ-
  #           metros originais
  #           componente 1 : par?metro de difi-
  #                          culdade
  #           componente 2 : par?metro de dis-
  #                          crimina??o
  # mu.theta : m?dia da distribui??o latente
  # var.theta : vari?ncia da distribui??o latente
  # ass.theta : assimetria da distribui??o latente
  #
  # v.theta.simul : vetor com os tra?os latentes
  #                 simulados anteriormente
  #
  # tipo.simul.theta : tipo de tra?os latentes
  #                    simulados
  #                    1 - simulados dentro da fun??o
  #                    2 - simulados anteriormente
  #==========================================
  ###########################################            
  
  if (typepar == "Original")
  {
    ######################################
    # gera??o dos par?metros transformados
    mezetae <- cbind(-(mezeta[,1] * mezeta[,2]),mezeta[,2])
  }
  
  if (typepar=="Transformed")
  {
    mezetae <- mezeta
  }
  
  if (typepar=="Original" | typepar=="Transformed")
  {
    ###########################
    # inicializa??o do contador
    i <- 1
    
    ####################################
    # gera??o dos par?metros irrestritos
    catcpg <- 1
    
    #################################
    # cria??o das matrizes auxiliares
    Si <- (criamatriz.aux.MRN(vmi[i]))$Si
    
    #######################################
    # incializa??o da matriz de estimativa
    # dos par?metros irrestritos
    meGamma <- (t(mezetae[catcpg : (catcpg + vmi[i] - 1),])) %*% Si # (2 x (v.mi-1))
    
    #################################
    # atualiza??o dos contadores
    catcpg <- catcpg + vmi[i] 
    
    #################################
    # la?o para a cria??o da matriz
    # com os par?metros irrestritos
    #===============================
    
    for (i in 2 : I)
      
    {
      
      #################################
      # cria??o das matrizes auxiliares
      Si <- (cria.matriz.aux.MRN(vmi[i]))$Si
      
      #########################
      # concatenaçao horizontal
      meGamma <- cbind(meGamma,(t(mezetae[catcpg : (catcpg + vmi[i]- 1),])) %*% Si) # ( 2 x sum(v.mi-1))
      catcpg <- catcpg + vmi[i]
      
      ######################################
    } # for : cria??o da matriz com as es-
    # timativas dos par?metros irrestritos
    ###################################### 
  }
  
  if (typepar=="Irrestricted")
    
  {
    meGamma<-mezetae <- mezeta
  }
  
  #############################
  # gera??o dos tra?os latentes
  # distribui??o normal assimétrica
  if (tiposimultheta == 1) # simuala??o dentro da fun??o
  {
    vthetasim <- cbind(rsn(n,mutheta,sqrt(vartheta),asstheta))
  }
  
  else # simual??o externa
    
  {
    
  }
  
  vthetasim <- vthetasimul # recebe tra?os latentes simulados
  
  
  ################################
  # Gera??o das probabilidades de
  # escolha
  
  #############################
  # transposi??o da matriz
  # de par?metros irrestritos
  if(typepar == "Original" | typepar == "Transformed")
  {
    meGamma <- t(meGamma)
  }
  
  ## matriz com as probabilidades de escolha
  mP<-probtri.repar.MRN(vthetasim,vmi,meGamma,I)$mP
  
  ###########################
  # Gera??o das respostas
  ## probabilidade de escolha acumulada
  mPac <- matrix(0,n,sum(vmi))
  
  #############################
  # inicializa?o dos contadores
  i <- 1 # para os itens
  caty <- 1 # para as categorias de cada item
  auxy <- 1 # para as categorias dentro de cada item
  
  ############################################
  ## Inicializa??o das matrizes de prob. acum.
  ## e de escolha
  
  ###################################################
  # pega as colunas referentes a um determinado item
  # relacionada a uma determinada categoria
  mPilp <- mP[,caty:(caty + vmi[i] - 1)]
  
  for (s in caty:(caty + vmi[i] - 1))
    
  {
    
    ##################################
    # calcula a probabiliade acumulada
    mPac[,s] <- apply(cbind(mPilp[,1:auxy]),1,sum)
    
    #####################
    # atualiza o contador
    auxy <- auxy + 1
    
    ######################################
  }# for : para a matriz de probabilidade 
  # de escolha e acumulada
  ######################################
  
  ######################
  # Matriz de respostas
  
  #########################
  # simula??o de uniformes
  vU <- cbind(runif(n,0,1)) # vetor com n?meros uniformes
  auxum <- matrix(1,1,vmi[i]) # matriz auxiliar de um's
  
  ###########################################
  mU <- kronecker(auxum,vU) # matriz com os n?meros uniformes por
  # coluna (as colunas têm os mesmos n?meros)
  ###########################################
  
  #######################################
  #######################################
  # matriz com as escolhas (alternativas)
  
  mesc <- apply(mU >= mPac[,caty : (caty + vmi[i] - 1)],2,as.numeric) # vale zero até chegar na alternativa escolhida
  Mesc <- cbind(apply(mesc,1,sum)) + 1 # apresenta o n?mero da alternativa escolhida
  
  #############################
  # matriz de respostas geradas
  auxdois <- matrix(1,n,1) # matriz n x 1 de um's
  
  ##############################################
  # vale 1 na categoria excolhida e 0 nas demais
  
  ####################################
  # matriz com as colunas representando
  # a alternativa escolhida
  mMesc <- matrix(Mesc,nrow(Mesc),vmi[i])
  mYgera <- apply((kronecker(auxdois,rbind(seq(1,vmi[i]))) == mMesc),2,as.numeric)
  mRMesc <- Mesc
  
  #########################
  # atualiza??o do contador
  caty <- caty + vmi[i]
  
  #############################################
  #############################################
  # La?o para constru??o da matriz de respostas
  #############################################
  #############################################
  
  for (i in 2:I)
  {
    auxy <- 1 # zera o contador para a matriz de probabilidade acumulada
    
    ###################################################
    # pega as colunas referentes a um determinado item
    # relacionada a uma determinada categoria
    
    mPilp <- mP[,caty:(caty + vmi[i] - 1)]
    
    for (s in caty:(caty + vmi[i] - 1))
    {
      
      ##################################
      # calcula a probabiliade acumulada
      mPac[,s] <- apply(cbind(mPilp[,1:auxy]),1,sum)
      
      #####################
      # atualiza o contador
      auxy <- auxy + 1
      
      ######################################
    }# for : para a matriz de probabilidade 
    # de escolha e acumulada
    ######################################
    
    ######################
    # Matriz de respostas
    
    #########################
    # simula??o de uniformes
    vU <- cbind(runif(n,0,1)) # vetor com n?meros uniformes
    auxum <- matrix(1,1,vmi[i]) # matriz auxiliar de um's
    
    ###########################################
    mU <- kronecker(auxum,vU) # matriz com os n?meros uniformes por
    # coluna (as colunas têm os mesmos n?meros)
    ###########################################
    
    #######################################
    #######################################
    # matriz com as escolhas (alternativas)
    mesc <- apply(mU >= mPac[,caty : (caty + vmi[i] - 1)],2,as.numeric) # vale zero até chegar na alternativa escolhida
    Mesc <- cbind(apply(mesc,1,sum)) + 1 # apresenta o n?mero da alternativa escolhida
    
    #############################
    # matriz de respostas geradas
    auxdois <- matrix(1,n,1) # matriz n x 1 de um's
    
    ##############################################
    # vale 1 na categoria excolhida e 0 nas demais
    
    ####################################
    # matriz com as colunas representando
    # a alternativa escolhida
    mMesc <- matrix(Mesc,nrow(Mesc),vmi[i])
    mYgera <- cbind(mYgera,apply((kronecker(auxdois,rbind(seq(1,vmi[i]))) == mMesc),2,as.numeric))
    caty <- caty + vmi[i] # atualiza??o do contador
    mRMesc <- cbind(mRMesc,Mesc)
    
    ###################################################
  } # for : la?o para a constru??o da matriz, por item
  ###################################################
  
  
  ######################################
  # disponibiliza os resultados da fun??o
  
  ###################################################
  ###################################################
  #==================================================
  #       Lista de resultados dispon?veis
  #================================+=================
  # me.zeta : matriz com os par?metros originais
  #           coluna 1 : discrimina??o
  #           coluna 2 : dificuldade
  # me.zetae : matriz com os par?metros transformados
  #           coluna 1 : discrimina??o
  #           coluna 2 : dificuldade
  # me.Gamma : matriz com os par?metros irrestritos 
  #           coluna 1 : discrimina??o
  #           coluna 2 : dificuldade
  # m.Y.gera : matriz com as escolhas das alternativas
  #            de cada indivíduo
  # m.P.ac : matriz com a probabilidade de escolha acu-
  #          mulada
  #####################################################
  #####################################################
  
  resultgerarespMRN <- list(mezeta=mezeta,mezetae=mezetae,meGamma=meGamma,vthetasim=vthetasim,mYgera=mYgera,mP=mP,mPac=mPac,mRMesc=mRMesc)
  return(resultgerarespMRN) # disponibiliza??o
  
  #################################
} # término da fun??o gera.resp.MRN
#################################

# 

get.iest.NRM <- function(rmirt,vI,vncat,mcov=0)
{
  
  mest <- coef(rmirt,simply=TRUE,printSE=TRUE)
  mestpara <-msepara <- matrix(0,vI,ncat-1)
  mestpard <-msepard<- matrix(0,vI,ncat-1)
  
  for (i in 1:vI)
    
  {
    mestpara[i,] <- as.numeric(mest[[i]][1,1:(vncat[i]-1)])
    mestpard[i,] <- as.numeric(mest[[i]][1,vncat[i]:(2*(vncat[i]-1))])
    msepara[i,] <- as.numeric(mest[[i]][2,1:(vncat[i]-1)])
    msepard[i,] <- as.numeric(mest[[i]][2,vncat[i]:(2*(vncat[i]-1))])
  }
  
  mezeta <- cbind(c(t(mestpard)),c(t(mestpara))) # (cih/dih,aih), cih+aih\theta
  msezeta <- cbind(c(t(msepard)),c(t(msepara))) # (cih/dih,aih), cih+aih\theta
  
  if (mean(abs(mcov)) !=0)
  {
    mcovzeta <- matrix(0,2*(sum(vncat-1)),2*(max(vncat)-1))
    aux1 <-1
    aux2 <-2*(vncat[1]-1) 
    mcovzeta[aux1:aux2,1:(2*(vncat[1]-1))] <- mcov[aux1:aux2,aux1:aux2] # cih/dih
    for (i in 2 : vI)
    {
      aux1 <- aux1 + 2*(vncat[i]-1)
      aux2 <- aux1 + 2*(vncat[i]-1)-1 
      mcovzeta[aux1:aux2,1:(2*(vncat[1]-1))] <- mcov[aux1:aux2,aux1:aux2] # cih/dih
    }
    
  }
  
  if (mean(abs(mcov))!=0)
  {
    result <- list(mezeta=mezeta,msezeta=msezeta,mcovzeta=mcovzeta)  
  }
  else{
    result <- list(mezeta=mezeta,msezeta=msezeta)  
  }
  
  return(result)
  
}


# Plot the probabilities of choosing each category
# for the nominal reponse model without confidence
# intervals

plot.prob.CCI.art.data.NRM.item <- function(mPqp,pbil,vqp,vI,vncat,filesave)
  
{
  auxcat1<-1
  #auxcat2<-ncat[1]  
  pdf(file=filesave)
  for (i in 1:vI)
  {
    aux1 <- round(vncat[i]/2)
    aux2 <- vncat[i]-aux1
    #
    par(mfrow = c(aux1,aux2))#,mai = c(1, 0.1, 0.1, 0.1)) 
    #
    plot(vqp,mPqp[,auxcat1],typ="l",ylim=c(0,1),main=paste("item:",i,", category:",1),
         cex=1.2,cex.lab=1.1,cex.main=1.2,lwd=2,col="gray",
         ylab=expression(paste("P(",theta,")")),xlab="latent trait")
    lines(vqp,pbil[,auxcat1],type="p",cex=1.2,pch=19)
    # 
    for (k in 2:vncat[i])
    {
      auxcat1 <- auxcat1 + 1
      plot(vqp,mPqp[,auxcat1],typ="l",ylim=c(0,1),main=paste("item:",i,", category:",k),
           cex=1.2,cex.lab=1.1,cex.main=1.2,lwd=2,col="gray",
           ylab=expression(paste("P(",theta,")")),xlab="latent trait")
      lines(vqp,pbil[,auxcat1],type="p",cex=1.2,pch=19)
    }
    text(5,10,labels=paste("Item",i))
    auxcat1 <- auxcat1 + 1
  }
  dev.off()
  
  
}

# Calcula a propor??o observada de escolha de cada categoria
#
calculate.proportion.NRM.item<-function(mYD,escore,lie,lse)
{
  # Calculating the proportions of choosing each category per escore value
  n <- nrow(mYD)
  pesccat <- (apply(mYD[escore==lie,]*mVD[escore==lie,],2,sum)/apply(mVD[escore==lie,],2,sum))
  #
  for (r in (lie+1):lse)
  {
    pesccat <- rbind(pesccat,c(apply(mYD[escore==r,]*mVD[escore==r,],2,sum)/apply(mVD[escore==r,],2,sum)))
  }
  #
  pesccat<-c(pesccat)
  return(pesccat)
}


plot.obs.proportion.NRM.item<-function(pesccat,vI,vncat,lie,lse,filesave)
{
  # Ploting the proportions
  a1<- 1
  a2 <- lse
  nesc <- a2-a1+1
  pdf(file=filesave)
  for (i in 1:vI)
  {
    aux1 <- round(vncat[i]/2)
    aux2 <- vncat[i]-aux1
    par(mfrow = c(aux1,aux2))#,mai = c(1, 0.1, 0.1, 0.1)) 
    for(j in 1:vncat[i])
    {
      plot(seq(lie,lse,1),pesccat[a1:a2],type="b",ylim=c(0,1),pch=17,lwd=2,col="gray",
           cex=1.2,cex.axis=1.2,cex.main=1.2,xlab="observed score", 
           ylab="proportion of choosing",main=paste("Item:",i,", Category:",j))
      a1 <- a1+nesc
      a2 <- a2+nesc
    }
  }
  dev.off()
}

calculate.proportion.pred.NRM.item<-function(mYDR,escore,lie,lse)
{
  nrep <- max(mYDR[1,])
  n<- nrow(mYDR)-1
  #
  mYDaux <- mYDR[2:(n+1),mYDR[1,]==1]
  pesccat <- (apply(mYDaux[escore==lie,]*mVD[escore==lie,],2,sum)/apply(mVD[escore==lie,],2,sum))
  #
  for (k in (lie+1):lse)
  {
    pesccat <- rbind(pesccat,c(apply(mYDaux[escore==k,]*mVD[escore==k,],2,sum)/apply(mVD[escore==k,],2,sum)))
  }
  #
  mpesccat<-c(pesccat)
  
  for (r in 2:nrep)
    
  {
    # propor??o predita de escolha por categoria
    mYDaux <- mYDR[2:(n+1),mYDR[1,]==r]
    pesccat <- (apply(mYDaux[escore==lie,]*mVD[escore==lie,],2,sum)/apply(mVD[escore==lie,],2,sum))
    #
    for (k in (lie+1):lse)
    {
      pesccat <- rbind(pesccat,c(apply(mYDaux[escore==k,]*mVD[escore==k,],2,sum)/apply(mVD[escore==k,],2,sum)))
    }
    mpesccat<-rbind(mpesccat,c(pesccat))
    
  }
  
  return(mpesccat)
}


plot.obs.pred.proportion.NRM.item<-function(pesccat,mpesccat,vI,vncat,lie,lse,q1,q2,filesave)
{
  # calculating the necessary quantities
  mpesccatmed <- apply(mpesccat,2,quantile,0.5)
  mpesccatLIIC <- apply(mpesccat,2,quantile,q1)
  mpesccatLSIC <- apply(mpesccat,2,quantile,q2)
  #
  # Ploting the proportions
  a1<- 1
  a2 <- lse
  nesc <- a2-a1+1
  pdf(file=filesave)
  for (i in 1:vI)
  {
    aux1 <- round(vncat[i]/2)
    aux2 <- vncat[i]-aux1
    par(mfrow = c(aux1,aux2))#,mai = c(1, 0.1, 0.1, 0.1)) 
    for(j in 1:vncat[i])
    {
      plot(seq(lie,lse,1),pesccat[a1:a2],type="b",ylim=c(0,1),pch=17,lwd=2,col="gray",
           cex=1.2,cex.axis=1.2,cex.main=1.2,xlab="observed score", 
           ylab="proportion of choosing",main=paste("Item:",i,", Category:",j))
      plotCI(seq(lie,lse,1),mpesccatmed[a1:a2],li=mpesccatLIIC[a1:a2],
             ui=mpesccatLSIC[a1:a2],add=TRUE,pch=19,cex=1.2)
      a1 <- a1+nesc
      a2 <- a2+nesc
    }
  }
  dev.off()
}

estimate.item.theta.replica.NRM.IRT <-function(mYNR,modelname,customItems,
                                               inames,method,vI,vncat)
{
  nrep <- max(mYNR[1,])
  n<- nrow(mYNR)-1
  mYNRaux <- mYNR[2:(n+1),mYNR[1,]==1]
  colnames(mYNRaux) <-inames
  resultR <- mirt(mYNRaux,1, modelname, customItems=customItems,
                  SE=TRUE,method=method)#,empiricalhist=TRUE)
  # Armazenando as estimativas dos par?metros (parametriza??o mirt)
  mest <-coef(resultR,simply=TRUE) #as.data.frame(coef(resultM1,simply=TRUE)$i01)[1,]
  mestpara <- matrix(0,vI,ncat-1)
  mestpard <- matrix(0,vI,ncat-1)
  for (i in 1:vI)
  {
    mestpara[i,] <- as.numeric(mest[[i]][1,1:(ncat-1)])
    mestpard[i,] <- as.numeric(mest[[i]][1,ncat:(2*(ncat-1))])
  }
  mzetaIB <- cbind(1,c(t(mestpard)),c(t(mestpara))) # (cih,aih), cih+aih\theta
  
  for (r in 2:nrep)
  {
    mYNRaux <- mYNR[2:(n+1),mYNR[1,]==r]
    colnames(mYNRaux) <-inames
    resultR <- mirt(mYNRaux,1, modelname, customItems=customItems,
                    SE=TRUE,method=method)#,empiricalhist=TRUE)
    # Armazenando as estimativas dos par?metros (parametriza??o mirt)
    mest <-coef(resultR,simply=TRUE) #as.data.frame(coef(resultM1,simply=TRUE)$i01)[1,]
    mestpara <- matrix(0,vI,ncat-1)
    mestpard <- matrix(0,vI,ncat-1)
    for (i in 1:vI)
    {
      mestpara[i,] <- as.numeric(mest[[i]][1,1:(ncat-1)])
      mestpard[i,] <- as.numeric(mest[[i]][1,ncat:(2*(ncat-1))])
    }
    mzetaIB <- rbind(mzetaIB,cbind(r,c(t(mestpard)),c(t(mestpara)))) # (cih,aih), cih+aih\theta
  }
  
  return(mzetaIB)
  
}


# Plot the probabilities of choosing each category
# for the nominal reponse model with confidence
# intervals

plot.prob.CCI.CI.art.data.NRM.item <- function(mPY,stheta,vqp,vqw,mzetaIB,
                                               vI,vncat,q1,q2,
                                               filesave)
  
{
  # Calculating the prediction of the probabilties
  nqp <-length(vqp)
  nrep <- max(mzetaIB[,1])
  mYDaux <- mYDR[2:(n+1),mYDR[1,]==1]
  mzetaIBaux <- mzetaIB[mzetaIB[,1]==1,2:3]
  mPqp<-probtri.repar.MRN(vqp,vncat,mzetaIBaux,vI)$mP
  # Verossimilhança em fun??o dos pontos de quadratura
  resultvMRN<-gera.veross.MRN(mYDaux,mVD,mPqp,vqw,vI,vncat,2)
  mLthetamrn<-resultvMRN$mLthetamrn
  # Dados artificiais
  resultAD<-gera.dados.art.MRN(mYDaux,mVD,mLthetamrn)
  pbil <- resultAD$pbil # (probabilidade de respota correta em fun??o dos tras?os latentes) 
  mpbil <- c(pbil)
  #
  for (r in 2:nrep)
  {
    mYDaux <- mYDR[2:(n+1),mYDR[1,]==r]
    mzetaIBaux <- mzetaIB[mzetaIB[,1]==r,2:3]
    mPqp<-probtri.repar.MRN(vqp,vncat,mzetaIBaux,vI)$mP
    # Verossimilhança em fun??o dos pontos de quadratura
    resultvMRN<-gera.veross.MRN(mYDaux,mVD,mPqp,vqw,vI,vncat,2)
    mLthetamrn<-resultvMRN$mLthetamrn
    # Dados artificiais
    resultAD<-gera.dados.art.MRN(mYDaux,mVD,mLthetamrn)
    pbil <- resultAD$pbil # (probabilidade de respota correta em fun??o dos tras?os latentes) 
    mpbil <- rbind(mpbil,c(pbil))
  }
  #
  mpbilMed <- apply(mpbil,2,quantile,0.5)
  mpbilLIIC <- apply(mpbil,2,quantile,q1)
  mpbilLSIC <- apply(mpbil,2,quantile,q2)
  #
  auxcat1<-1
  #auxcat2<-ncat[1]  
  a1<-1
  a2<-nqp
  pdf(file=filesave)
  for (i in 1:vI)
  {
    aux1 <- round(vncat[i]/2)
    aux2 <- vncat[i]-aux1
    #
    par(mfrow = c(aux1,aux2))#,mai = c(1, 0.1, 0.1, 0.1)) 
    #
    plot(stheta,mPY[,auxcat1],typ="l",ylim=c(0,1),main=paste("Item:",i,", category:",1),
         cex=1.2,cex.lab=1.2,cex.main=1.2,lwd=2,col="gray",
         ylab=expression(paste("P(",theta,")")),xlab="latent trait")
    plotCI(vqp,mpbilMed[a1:a2],li=mpbilLIIC[a1:a2],ui=mpbilLSIC[a1:a2],add=TRUE,
           pch=19,cex=1.2)
    # 
    for (k in 2:vncat[i])
    {
      a1 <- a1 + nqp
      a2 <- a2 + nqp
      auxcat1 <- auxcat1 + 1
      plot(stheta,mPY[,auxcat1],typ="l",ylim=c(0,1),main=paste("Item:",i,", category:",k),
           cex=1.2,cex.lab=1.1,cex.main=1.2,lwd=2,col="gray",
           ylab=expression(paste("P(",theta,")")),xlab="latent trait")
      plotCI(vqp,mpbilMed[a1:a2],li=mpbilLIIC[a1:a2],ui=mpbilLSIC[a1:a2],add=TRUE,
             pch=19,cex=1.2)
    }
    auxcat1 <- auxcat1 + 1
    a1 <- a1 + nqp
    a2 <- a2 + nqp
  }
  dev.off()
  
}

res.item.par.dic.mirt <- function(resultmirt,nI,nparitem)
{

  mestitem <- mLIICitem<-mLSICitem<- matrix(0,nI,nparitem)
  for (i in 1 : nI){
    result <- coef(resultmirt)[[i]]
    mestitem[i,]<- result[1,1:nparitem]
    mLIICitem[i,] <- result[2,1:nparitem]
    mLSICitem[i,] <- result[3,1:nparitem]
    }  
  mestitem[is.na(mestitem)]<-0
  mLIICitem[is.na(mLIICitem)]<-0
  mLSICitem[is.na(mLSICitem)]<-0
  #
  resultfunc <- list(mestitem=mestitem,mLIICitem=mLIICitem,
                     mLSICitem=mLSICitem)
  return(resultfunc)
  
}

#est.par.ori.mtri.dic <-function()
#{
#  for(i in 1:nI)
 # {
#  }
 # mDelta <- rbind(cbind(diag(D+1),0),
#                  c(rep(0,D+1),exp(-g)/(1+exp(-g))^2))
#}

est.se.IRTpar.KPLmodel <- function(fit,nI,
                                   model='3PL')
{
  
  if (model=="3PL")
  {
    parvec <- extract.mirt(fit, 'parvec')
    n_pars = 3*nI
    vcov <- vcov(fit)
    mSE <- matrix(nrow = nI, ncol = 3)
    for(i in c(seq(1, n_pars, by = 3)))
    {
      j <- seq(i,i+2)
      ad <- parvec[j]
      v <- vcov[j, j]
      k = (i+2)/3
      mSE[k,] <- deltamethod(list(~x1, ~-x2/x1, ~1/(1+exp(-x3))), ad, v)
    }    
    return(mSE)
  } # 3PL
  
}

#########################
#########################
# DAEIMA algorithms
#

vP.MLKP.repar.theta<- function(param,vtheta)
{
  a<-param[1]
  d<-param[2]
  c<-param[3]
  u<-param[4]
  mP <- c+(u-c)*(1/(1+exp(-(a*vtheta+d))))
  mP <- ifelse(mP<=0.0000000000001,
               0.000000000001,mP)
  mP <- ifelse(mP>=0.9999999999,
               0.999999999,mP)
  return(mP)
}

minus.loglik.MLKP.repar.theta<- 
  function(mY,mV,param,vtheta)
{
  mP<-vP.MLKP.repar.theta(param,vtheta)
  logl<- sum(mV*(mY*log(mP)+(1-mY)*log(1-mP)))
  return(-logl)
}
