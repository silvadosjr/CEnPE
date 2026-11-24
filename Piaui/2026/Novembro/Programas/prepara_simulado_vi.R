library(readxl)
root <- normalizePath('Piaui/2026/Novembro', winslash='/')
dados <- file.path(root, 'Dados')
base <- read_excel(file.path(dados, 'df - 2ª SÉRIE 6º SIMULA.xlsx'))
names(base) <- trimws(names(base))
stopifnot(all(sprintf('rpa_%03d',1:26) %in% names(base)))
audit <- list()
for (sigla in c('lp','mt')) {
  disciplina <- if(sigla=='lp') 'LÍNGUA PORTUGUESA' else 'MATEMÁTICA'
  info <- read_excel(file.path(dados,paste0('gabarito_2serie_',sigla,' 6ª SIMULA.xlsx')))
  names(info)[toupper(names(info))=='CONHECIDO'] <- 'Conhecido'
  stopifnot(identical(as.integer(info$Item),1:26))
  b <- base[base$disciplina_descricao==disciplina,]
  stopifnot(nrow(b)>0)
  rp <- sprintf('rp_%03d',1:26)
  for(j in 1:26) stopifnot(all(!is.na(b[[rp[j]]])),all(b[[rp[j]]]==info$Gabarito[j]))
  conhecido <- !is.na(info$Conhecido) & nzchar(trimws(info$Conhecido))
  stopifnot(all(is.finite(as.matrix(info[conhecido,c('aSAEB','bSAEB','c')]))))
  stopifnot(all(info$aSAEB[conhecido]>0),all(info$c[conhecido]>=0 & info$c[conhecido]<1))
  # A calibração anterior usa aSAEB e bSAEB como referência. As colunas a e b
  # contêm arredondamentos; preservar os parâmetros SAEB fornecidos.
  diferencas <- data.frame(Item=info$Item[conhecido],delta_a=info$a[conhecido]-50*info$aSAEB[conhecido],delta_bSAEB=info$bSAEB[conhecido]-(250+50*info$b[conhecido]))
  aval <- b[b$fl_avaliado==1,]
  stopifnot(!anyDuplicated(aval$matricula))
  respostas <- as.matrix(aval[,sprintf('rpa_%03d',1:26)])
  stopifnot(!anyNA(respostas),all(respostas %in% c(LETTERS[1:5],'-','*','9')))
  audit[[sigla]] <- list(linhas=nrow(b),avaliados=nrow(aval),itens=26,ancoras=info$Item[conhecido],respostas=as.list(table(respostas)),gabarito_conferido=TRUE,diferencas_parametros=diferencas)
  cat('\n',disciplina,'\n'); print(info); print(audit[[sigla]])
}
saveRDS(base,file.path(dados,'df_6ª_SIMULA_2ª_SÉRIE.RDS'))
dir.create(file.path(root,'Relatorios/qa'),recursive=TRUE,showWarnings=FALSE)
jsonlite::write_json(audit,file.path(root,'Relatorios/qa/auditoria_entrada.json'),pretty=TRUE,auto_unbox=TRUE)
