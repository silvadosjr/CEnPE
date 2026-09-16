# =============================================================================
# MODELO MULTIDIMENSIONAL 3PL: duas dimensoes, um grupo
# =============================================================================
# Entrada: modelo 3P Multidimensional.ods (item, a1, a2, b, c).
# Saidas: resultados/Multidimensional, ao lado deste script; veja README.md.
# Execute o arquivo inteiro com source(..., encoding = "UTF-8") ou Rscript.
# Dependencias: install.packages(c("mirt", "readODS"))
# Nao limpa o ambiente, nao muda getwd() e nao exige Aux Func IRT.R.

# 1. Caminhos e configuracao ----------------------------------------------------
localizar_pasta <- function() {
  # Resolve caminhos com acentos mesmo quando o terminal inicia no locale C.
  locale_anterior <- Sys.getlocale("LC_CTYPE")
  if (.Platform$OS.type == "windows" && identical(locale_anterior, "C")) {
    on.exit(Sys.setlocale("LC_CTYPE", locale_anterior), add = TRUE)
    Sys.setlocale("LC_CTYPE", ".UTF-8")
  }
  normalizar <- function(x) enc2utf8(normalizePath(x, mustWork = TRUE))
  arquivos <- Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
  if (length(arquivos)) return(dirname(normalizar(tail(arquivos, 1)[[1]])))
  argumento <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(argumento)) return(dirname(normalizar(sub("^--file=", "", argumento[1]))))
  candidatas <- c(getwd(), file.path(getwd(),
    "Experimentos com diferentes modelos da TRI", "Modelo Multidimensional"))
  encontradas <- candidatas[file.exists(file.path(candidatas, "modelo 3P Multidimensional.ods"))]
  if (!length(encontradas)) stop("Execute o arquivo inteiro usando source() ou Rscript.")
  normalizar(encontradas[1])
}
pasta_script <- localizar_pasta()
arquivo_entrada <- file.path(pasta_script, "modelo 3P Multidimensional.ods")
pasta_saida <- file.path(pasta_script, "resultados", "Multidimensional")
n <- 2000L
rho <- 0.8                           # Correlacao verdadeira entre F1 e F2.
semente <- 4142L                     # Ativa a semente sugerida no original.
D <- 2L                             # Este experimento foi definido para duas dimensoes.

pacotes <- c("mirt", "readODS")
faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes)) stop("Pacotes indisponiveis nesta biblioteca do R. Execute: install.packages(c(",
  paste(sprintf('"%s"', faltantes), collapse = ", "), "))")
if (!file.exists(arquivo_entrada)) stop("Entrada nao encontrada: ", arquivo_entrada)
if (length(n) != 1L || !is.finite(n) || n <= D || n != floor(n)) stop("n deve ser inteiro e maior que 2.")
if (length(rho) != 1L || !is.finite(rho) || abs(rho) >= 1) stop("rho deve estar entre -1 e 1, excluindo os extremos.")
dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)
set.seed(semente)

# 2. Funcoes auxiliares --------------------------------------------------------
validar_entrada <- function(dados) {
  colunas <- c("item", "a1", "a2", "b", "c")
  if (!all(colunas %in% names(dados))) stop("Colunas exigidas: item, a1, a2, b, c.")
  dados <- as.data.frame(dados[, colunas, drop = FALSE])
  dados <- dados[rowSums(!is.na(dados)) > 0L, , drop = FALSE]
  if (nrow(dados) < 3L || anyNA(dados) || anyDuplicated(dados$item)) {
    stop("Exigem-se pelo menos tres itens, identificadores unicos e dados completos.")
  }
  parametros <- dados[, c("a1", "a2", "b", "c")]
  if (!all(vapply(parametros, is.numeric, logical(1))) || any(!is.finite(as.matrix(parametros)))) {
    stop("Parametros devem ser numericos e finitos.")
  }
  if (any(dados$c < 0 | dados$c >= 1) || any(rowSums(dados[, c("a1", "a2")]^2) == 0)) {
    stop("Exigem-se 0 <= c < 1 e ao menos uma carga nao nula por item.")
  }
  if (dados$a2[1] != 0 || dados$a1[1] <= 0) {
    stop("A estrutura original exige primeiro item com a1 > 0 e a2 = 0.")
  }
  rownames(dados) <- NULL
  dados
}
simular_tracos <- function(n, Psi) {
  # Gera normais e impoe media amostral zero e covariancia amostral exata Psi.
  # Equivale ao branqueamento/recoloracao do original, usando apenas R base.
  z <- scale(matrix(rnorm(n * ncol(Psi)), n), center = TRUE, scale = FALSE)
  z %*% solve(chol(stats::cov(z))) %*% chol(Psi)
}
salvar_csv <- function(tabela, nome) {
  utils::write.csv(tabela, file.path(pasta_saida, nome), row.names = FALSE, fileEncoding = "UTF-8")
}
salvar_pdf <- function(nome, desenho) {
  grDevices::pdf(file.path(pasta_saida, nome), width = 10, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(desenho)
}
grafico_ic <- function(tabela, titulo) {
  plot(seq_len(nrow(tabela)), tabela$estimativa, pch = 19,
    ylim = range(c(tabela$verdadeiro, tabela$ic95_inferior, tabela$ic95_superior, tabela$estimativa), finite = TRUE),
    xlab = "Posicao do item", ylab = "Estimativa e IC 95%", main = titulo)
  ok <- is.finite(tabela$ic95_inferior) & is.finite(tabela$ic95_superior) & !tabela$fixo
  arrows(which(ok), tabela$ic95_inferior[ok], which(ok), tabela$ic95_superior[ok],
    angle = 90, code = 3, length = 0.04)
  points(seq_len(nrow(tabela)), tabela$verdadeiro, pch = 17, col = "red")
  legend("topleft", c("Estimado", "Verdadeiro"), pch = c(19, 17), col = c("black", "red"), bty = "n", cex = 0.8)
}
grafico_indice <- function(valores, rotulo, eixo, referencias) {
  if (!any(is.finite(valores))) stop("Indice indisponivel: ", rotulo)
  plot(valores, pch = 19, xlab = eixo, ylab = rotulo,
    ylim = range(c(valores, referencias), finite = TRUE))
  abline(h = referencias, col = "blue", lty = 2)
}

# 3. Entrada e simulacao --------------------------------------------------------
mzeta <- validar_entrada(readODS::read_ods(arquivo_entrada, sheet = 1))
nI <- nrow(mzeta)
nomes_itens <- paste0("Item_", mzeta$item)
ma <- as.matrix(mzeta[, c("a1", "a2")])
# ATENCAO: a coluna b e usada como -d, preservando a convencao do original.
# Nao e a dificuldade unidimensional -d/a nem a dificuldade multidimensional
# -d/sqrt(a1^2+a2^2). Por isso os graficos identificam d como INTERCEPTO.
vd <- -mzeta$b
vc <- mzeta$c
mPsi <- matrix(c(1, rho, rho, 1), D, D, dimnames = list(c("F1", "F2"), c("F1", "F2")))
mtheta <- simular_tracos(n, mPsi)
colnames(mtheta) <- c("F1", "F2")
# Modelo compensatorio: uma dimensao pode compensar a outra no preditor linear.
# P(Y_ij=1 | theta_i) = c_j + (1-c_j)*plogis(a1_j*theta1_i + a2_j*theta2_i + d_j).
mY <- mirt::simdata(a = ma, d = vd, N = n, guess = vc, Theta = mtheta, itemtype = "3PL")
colnames(mY) <- nomes_itens
# Respostas completas: 0 = erro, 1 = acerto; escore de 0 a nI.
vescore <- rowSums(mY)
if (any(vapply(seq_len(nI), function(i) length(unique(mY[, i])) != 2L, logical(1)))) {
  stop("Algum item nao apresentou as duas categorias. Revise n e os parametros.")
}
salvar_csv(data.frame(individuo = seq_len(n), mY), "respostas_simuladas.csv")

# 4. Estimacao -----------------------------------------------------------------
# Preserva a especificacao original: F1 em todos os itens, F2 do segundo ao ultimo.
# SOMENTE a2 do primeiro item e fixado em zero. Os demais zeros da planilha sao
# valores verdadeiros da simulacao, nao restricoes adicionais do ajuste.
# Variancias de F1/F2 fixas em 1; medias fixas em 0; correlacao estimada.
# Prioris normais: cargas N(1,1), d N(0,3), logit(g) N(-1,1), parametros media/DP.
# A combinacao de cargas cruzadas e correlacao livre exige cautela na orientacao
# dos fatores. As prioris regularizam o ajuste; convergencia nao prova identificacao.
sintaxe_modelo <- sprintf(paste0(
  "F1 = 1-%d\nF2 = 2-%d\n",
  "PRIOR = (1-%d, a1, norm, 1, 1), (2-%d, a2, norm, 1, 1),\n",
  "        (1-%d, d, norm, 0, 3), (1-%d, g, norm, -1, 1)\n",
  "LBOUND = (1-%d, a1, -0.5), (2-%d, a2, -0.5)\nCOV = F1*F2"),
  nI, nI, nI, nI, nI, nI, nI, nI)
model.prior <- mirt::mirt.model(sintaxe_modelo)
resultML3P <- mirt::mirt(mY, model = model.prior, itemtype = "3PL", SE = TRUE,
  method = "EM", TOL = 0.0001)
saveRDS(resultML3P, file.path(pasta_saida, "modelo_Multidimensional.rds"))
writeLines(sintaxe_modelo, file.path(pasta_saida, "especificacao_modelo.txt"))
writeLines(capture.output(sessionInfo()), file.path(pasta_saida, "sessionInfo.txt"))
if (!isTRUE(mirt::extract.mirt(resultML3P, "converged"))) {
  stop("O modelo nao convergiu. Objeto salvo; revise o ajuste antes de interpreta-lo.")
}
print(resultML3P)

# 5. Parametros, erros-padrao e intervalos --------------------------------------
# Extracao por nomes, sem indices fixos nem funcao auxiliar externa.
# Com printSE=TRUE, g e apresentado na escala logit; transforma-se para c.
coef_ep <- mirt::coef(resultML3P, printSE = TRUE, rawug = TRUE, simplify = FALSE)
valores <- mirt::mod2values(resultML3P)
zcrit <- qnorm(0.975)
parametros_itens <- do.call(rbind, lapply(seq_len(nI), function(i) {
  co <- coef_ep[[nomes_itens[i]]]
  if (!"SE" %in% rownames(co)) stop("Erros-padrao indisponiveis; revise o ajuste salvo.")
  estimativa <- co["par", c("a1", "a2", "d", "logit(g)")]
  ep <- co["SE", c("a1", "a2", "d", "logit(g)")]
  linha <- valores[valores$item == nomes_itens[i], ]
  fixo <- !linha$est[match(c("a1", "a2", "d", "g"), linha$name)]
  inferior <- estimativa - zcrit * ep
  superior <- estimativa + zcrit * ep
  # IC para c na escala logit, transformado para permanecer entre 0 e 1.
  estimativa[4] <- plogis(estimativa[4])
  inferior[4] <- plogis(inferior[4])
  superior[4] <- plogis(superior[4])
  ep[4] <- estimativa[4] * (1 - estimativa[4]) * ep[4]
  ep[fixo] <- inferior[fixo] <- superior[fixo] <- NA_real_
  data.frame(item = nomes_itens[i], parametro = c("a1", "a2", "d", "c"), fixo = fixo,
    verdadeiro = c(ma[i, ], vd[i], vc[i]), estimativa = as.numeric(estimativa),
    erro_padrao = as.numeric(ep), ic95_inferior = as.numeric(inferior), ic95_superior = as.numeric(superior))
}))
if (any(!is.finite(parametros_itens$erro_padrao[!parametros_itens$fixo]))) {
  stop("Erros-padrao nao finitos em parametros livres. Revise o modelo salvo.")
}
# Com variancias unitarias, COV_21 coincide com a correlacao entre F1 e F2.
ecor <- coef_ep$GroupPars["par", "COV_21"]
ep_cor <- coef_ep$GroupPars["SE", "COV_21"]
if (!is.finite(ecor) || abs(ecor) >= 1 || !is.finite(ep_cor)) stop("Correlacao ou erro-padrao invalido.")
# IC aproximado via transformacao de Fisher e metodo delta, respeitando (-1,1).
ep_fisher <- ep_cor / (1 - ecor^2)
correlacao <- data.frame(verdadeiro = rho, amostral_simulado = cor(mtheta)[1, 2],
  estimativa = ecor, erro_padrao = ep_cor,
  ic95_inferior = tanh(atanh(ecor) - zcrit * ep_fisher),
  ic95_superior = tanh(atanh(ecor) + zcrit * ep_fisher))
escores <- mirt::fscores(resultML3P, method = "EAP", full.scores.SE = TRUE)
rthetaML3P <- escores[, c("F1", "F2"), drop = FALSE]
tracos_latentes <- data.frame(individuo = seq_len(n), escore = vescore,
  F1_verdadeiro = mtheta[, "F1"], F1_EAP = escores[, "F1"], F1_EP = escores[, "SE_F1"],
  F2_verdadeiro = mtheta[, "F2"], F2_EAP = escores[, "F2"], F2_EP = escores[, "SE_F2"])
salvar_csv(parametros_itens, "parametros_itens.csv")
salvar_csv(correlacao, "correlacao_dimensoes.csv")
salvar_csv(tracos_latentes, "tracos_latentes.csv")
resultado <- list(modelo = resultML3P, entrada = mzeta, respostas = mY,
  parametros = parametros_itens, correlacao = correlacao, tracos = tracos_latentes,
  configuracao = list(n = n, rho = rho, semente = semente, Psi = mPsi), sintaxe = sintaxe_modelo)
saveRDS(resultado, file.path(pasta_saida, "resultado_Multidimensional.rds"))

# 6. Graficos dos valores simulados e dos parametros ----------------------------
salvar_pdf("TracosLatentesV.pdf", {
  par(mfrow = c(1, 2))
  for (j in seq_len(D)) hist(mtheta[, j], probability = TRUE, main = paste("Dimensao", j),
    xlab = "Traco verdadeiro", ylab = "Densidade")
  par(mfrow = c(1, 1))
  boxplot(mtheta, ylab = "Traco verdadeiro", xlab = "Dimensao")
  plot(mtheta[, 1], mtheta[, 2], pch = 19, xlab = "F1", ylab = "F2")
})
salvar_pdf("TracosLatentesVEscoresO.pdf", {
  par(mfrow = c(1, 2))
  for (j in seq_len(D)) plot(vescore, mtheta[, j], pch = 19,
    xlab = "Escore total", ylab = "Traco verdadeiro", main = paste("Dimensao", j))
})
salvar_pdf("ParPopEIC.pdf", {
  plot(1, ecor, ylim = c(-1, 1), xlim = c(0.5, 1.5), xaxt = "n", pch = 19,
    xlab = "", ylab = "Correlacao e IC 95%", main = "F1 e F2")
  arrows(1, correlacao$ic95_inferior, 1, correlacao$ic95_superior, angle = 90, code = 3, length = 0.1)
  points(1, rho, col = "red", pch = 17)
  legend("topleft", c("Estimada", "Verdadeira"), pch = c(19, 17), col = c("black", "red"), bty = "n")
})
rotulos <- c(a1 = "Discriminacao a1", a2 = "Discriminacao a2", d = "Intercepto d = -b da planilha", c = "Acerto casual c")
salvar_pdf("ParItensEDisp.pdf", {
  par(mfrow = c(2, 2))
  for (nome in names(rotulos)) {
    tab <- parametros_itens[parametros_itens$parametro == nome, ]
    plot(tab$verdadeiro, tab$estimativa, pch = 19, xlab = "Verdadeiro", ylab = "Estimativa", main = rotulos[[nome]])
    abline(0, 1, col = "blue")
  }
})
salvar_pdf("ParItensEIC.pdf", {
  par(mfrow = c(2, 2))
  for (nome in names(rotulos)) grafico_ic(parametros_itens[parametros_itens$parametro == nome, ], rotulos[[nome]])
})

# 7. Comparacao dos tracos verdadeiros e EAP ------------------------------------
salvar_pdf("TracosLatentesEHist.pdf", {
  par(mfrow = c(1, 2))
  for (j in seq_len(D)) {
    quebras <- pretty(range(c(mtheta[, j], rthetaML3P[, j])), n = 12)
    h1 <- hist(mtheta[, j], breaks = quebras, plot = FALSE)
    h2 <- hist(rthetaML3P[, j], breaks = quebras, plot = FALSE)
    plot(h1, freq = FALSE, border = "blue", ylim = c(0, max(h1$density, h2$density)),
      xlab = "Traco latente", ylab = "Densidade", main = paste("Dimensao", j))
    plot(h2, freq = FALSE, border = "darkgreen", add = TRUE)
    legend("topleft", c("Verdadeiro", "EAP"), col = c("blue", "darkgreen"), lty = 1, bty = "n")
  }
})
salvar_pdf("TracosLatentesEBP.pdf", {
  par(mfrow = c(1, 2))
  for (j in seq_len(D)) boxplot(list(Verdadeiro = mtheta[, j], EAP = rthetaML3P[, j]), main = paste("Dimensao", j))
})
salvar_pdf("TracosLatentesEDisp.pdf", {
  par(mfrow = c(1, 2))
  for (j in seq_len(D)) {
    plot(mtheta[, j], rthetaML3P[, j], pch = 19, xlab = "Verdadeiro", ylab = "EAP", main = paste("Dimensao", j))
    abline(0, 1, col = "blue")
  }
})
salvar_pdf("TracosLatentesEQQplot.pdf", {
  par(mfrow = c(1, 2))
  for (j in seq_len(D)) {
    valores_qq <- as.numeric(scale(rthetaML3P[, j]))
    qqnorm(valores_qq, main = paste("EAP padronizado - dimensao", j))
    qqline(valores_qq, col = "blue")
  }
})

# 8. Diagnosticos de itens, pares e respondentes --------------------------------
# S_X2 requer respostas completas. O PDF apresenta RMSEA, nao o p-valor.
ajuste_sx2 <- mirt::itemfit(resultML3P, fit_stats = "S_X2")
salvar_csv(ajuste_sx2, "ajuste_itens_S_X2.csv")
salvar_pdf("S_X2item.pdf", {
  grafico_indice(ajuste_sx2$RMSEA.S_X2, "RMSEA (S_X2)", "Item", c(0, 0.05, 0.10, 1))
})
infout <- mirt::itemfit(resultML3P, fit_stats = "infit", Theta = rthetaML3P)
salvar_csv(infout, "ajuste_itens_infit_outfit.csv")
for (indice in c("infit", "outfit")) salvar_pdf(paste0(indice, "item.pdf"), {
  grafico_indice(infout[[indice]], indice, "Item", c(0, 0.5, 1, 1.5, 2, 2.5))
})
# Preserva a formula do original. E distancia euclidiana entre proporcoes,
# nao o RMSEA convencional. As esperadas integram as DUAS dimensoes do modelo.
auxresML3P <- mirt::residuals(resultML3P, type = "LD", tables = TRUE)
distancias <- vapply(auxresML3P, function(tabela) {
  po <- tabela$Obs / sum(tabela$Obs)
  pe <- tabela$Exp / sum(tabela$Exp)
  sqrt(sum((po - pe)^2))
}, numeric(1))
independencia_local <- data.frame(par = names(auxresML3P), distancia = distancias)
salvar_csv(independencia_local, "independencia_local.csv")
salvar_pdf("RQEQMIL.pdf", {
  grafico_indice(distancias, "Distancia entre proporcoes", "Par de itens", c(0, 0.05, 0.10, 1))
})
resindiv <- mirt::personfit(resultML3P, Theta = rthetaML3P)
salvar_csv(data.frame(individuo = seq_len(n), resindiv), "ajuste_individuos.csv")
for (indice in c("outfit", "infit", "Zh")) salvar_pdf(paste0(tolower(indice), "indiv.pdf"), {
  referencias <- if (indice == "Zh") c(-3, -2, 0, 2, 3) else c(0, 0.5, 1, 1.5, 2, 2.5)
  grafico_indice(resindiv[[indice]], indice, "Individuo", referencias)
})
resultado$diagnosticos <- list(S_X2 = ajuste_sx2, infit_outfit = infout,
  independencia_local = independencia_local, individuos = resindiv)
saveRDS(resultado, file.path(pasta_saida, "resultado_Multidimensional.rds"))
message("Analise concluida. Resultados em: ", pasta_saida)
