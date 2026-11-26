library(mirt)
p <- 'Piaui/2026/Novembro/ResultadosTRI_2EM_MT/Simulado_VI'
m <- readRDS(file.path(p,'modelo_3PL.rds'))
v <- mod2values(m)
q <- v[v$item=='rpa_006' & v$name %in% c('a1','d','g'),]
stopifnot(nrow(q)==3,all(q$est),isTRUE(m@OptimInfo$converged))
stopifnot(!'rpa_025' %in% v$item)
fixed <- c(7,8,13,14,15,16,17,18,19,20)
for (i in fixed) {
  z <- v[v$item==sprintf('rpa_%03d',i) & v$name %in% c('a1','d','g'),]
  stopifnot(nrow(z)==3,all(!z$est))
}
capture.output(q,file=file.path(p,'auditoria_item6_livre.txt'))
print(q[,c('item','name','value','est')])
