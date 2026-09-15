# =============================================================================
# MODELO DE RESPOSTA GRADUAL (MRG): simulacao e recuperacao de parametros
# =============================================================================
# Entrada: modelo RG.ods, primeira planilha, colunas item, a, b1, ..., b4.
# Saidas: PDFs, tabelas CSV e objeto RDS na subpasta resultados/MRG.
# Execute o arquivo inteiro com source(..., encoding = "UTF-8") ou Rscript.
# A pasta do script e detectada automaticamente; nao e necessario usar setwd().
# Dependencias (instalar uma vez): install.packages(c("mirt", "readODS", "plotrix"))
# O script nao limpa o ambiente global nem depende de Aux Func IRT.R.

# 1. Localizacao e configuracao -------------------------------------------------
localizar_pasta <- function() {
  # source() registra o caminho em ofile; Rscript fornece --file=.
  arquivos <- Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
  if (length(arquivos)) {
    return(dirname(normalizePath(tail(arquivos, 1)[[1]], mustWork = TRUE)))
  }
  argumento <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(argumento)) {
    return(dirname(normalizePath(sub("^--file=", "", argumento[1]), mustWork = TRUE)))
  }
  # Alternativa para executar interativamente a partir da pasta ou do repositorio.
  candidatas <- c(getwd(), file.path(getwd(),
    "Experimentos com diferentes modelos da TRI", "Modelo de resposta gradual"))
  encontradas <- candidatas[file.exists(file.path(candidatas, "modelo RG.ods"))]
  if (!length(encontradas)) stop("Execute o arquivo inteiro usando source() ou Rscript.")
  normalizePath(encontradas[1], mustWork = TRUE)
}

pasta_script <- localizar_pasta()
arquivo_entrada <- file.path(pasta_script, "modelo RG.ods")
pasta_saida <- file.path(pasta_script, "resultados", "MRG")
n <- 1000L                       # Numero de respondentes simulados.
ncat <- 5L                       # Categorias ordenadas 0, 1, 2, 3 e 4.
semente <- 4142L                 # Reprodutibilidade na mesma versao do R/pacotes.

pacotes <- c("mirt", "readODS", "plotrix")
faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes)) {
  stop("Instale os pacotes ausentes: install.packages(c(",
       paste(sprintf('"%s"', faltantes), collapse = ", "), "))")
}
if (!file.exists(arquivo_entrada)) stop("Entrada nao encontrada: ", arquivo_entrada)
dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)
set.seed(semente)

# Abre e fecha cada PDF mesmo se ocorrer um erro durante o desenho.
salvar_pdf <- function(nome, desenho) {
  grDevices::pdf(file.path(pasta_saida, nome), width = 10, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(desenho)
}
salvar_csv <- function(tabela, nome) {
  utils::write.csv(tabela, file.path(pasta_saida, nome), row.names = FALSE,
                   fileEncoding = "UTF-8")
}
grafico_indice <- function(valores, rotulo, eixo, referencias) {
  if (!any(is.finite(valores))) stop("Indice sem valores finitos: ", rotulo)
  plot(valores, pch = 19, xlab = eixo, ylab = rotulo,
       ylim = range(c(valores[is.finite(valores)], referencias)))
  abline(h = referencias, col = "blue", lty = 2)
}

# 2. Parametros verdadeiros e validacao da entrada -------------------------------
mzeta <- as.data.frame(readODS::read_ods(arquivo_entrada, sheet = 1))
nomes_b <- paste0("b", seq_len(ncat - 1L))
nomes_parametros <- c("a", nomes_b)
if (!all(c("item", nomes_parametros) %in% names(mzeta))) {
  stop("A planilha deve conter: item, ", paste(nomes_parametros, collapse = ", "))
}
mzeta <- mzeta[, c("item", nomes_parametros), drop = FALSE]
# Ignora apenas linhas inteiramente vazias (a planilha tem linhas finais formatadas).
mzeta <- mzeta[rowSums(!is.na(mzeta)) > 0L, , drop = FALSE]
if (anyNA(mzeta) || anyDuplicated(mzeta$item)) stop("Itens ausentes, repetidos ou parametros NA.")
if (!all(vapply(mzeta[nomes_parametros], is.numeric, logical(1)))) {
  stop("Os parametros a e b devem ser numericos.")
}
va <- mzeta$a                    # Discriminacao: inclinacao de cada item.
mb <- as.matrix(mzeta[nomes_b])   # Limiares ordenados entre categorias.
nI <- nrow(mzeta)                # Numero de itens lido da planilha, sem fixar 30.
if (nI < 2L || any(!is.finite(c(va, mb))) || any(va <= 0) ||
    any(apply(mb, 1, function(x) any(diff(x) <= 0)))) {
  stop("Exigem-se >= 2 itens, a > 0 e limiares finitos estritamente crescentes.")
}

# 3. Simulacao -----------------------------------------------------------------
# Normal padrao, depois centrada e escalada para media 0 e desvio amostral 1.
vtheta <- as.numeric(scale(rnorm(n)))
# No mirt: P(Y >= k | theta) = logistic(a * theta + d_k), com d_k = -a*b_k.
# A probabilidade de cada categoria e a diferenca de probabilidades acumuladas.
md <- -sweep(mb, 1, va, "*")
mY <- mirt::simdata(a = va, d = md, N = n,
                    Theta = matrix(vtheta), itemtype = "graded")
colnames(mY) <- paste0("Item_", mzeta$item)
# Mascara de observacao: 1 = observado; 0 = ausente. Aqui nao ha ausencias.
mV <- matrix(1L, n, nI)
mYmirt <- mY
mYmirt[mV == 0] <- NA
vescore <- rowSums(mYmirt, na.rm = TRUE)
# Evita estimar silenciosamente menos limiares se uma categoria nao for sorteada.
if (any(vapply(seq_len(nI), function(i)
  !setequal(unique(stats::na.omit(mYmirt[, i])), 0:(ncat - 1L)), logical(1)))) {
  stop("Algum item nao apresentou todas as categorias; revise n e os parametros.")
}
salvar_pdf("TracosLatentesV.pdf", {
  par(mfrow = c(1, 2))
  hist(vtheta, probability = TRUE, xlab = "Traco latente verdadeiro", main = "", ylab = "Densidade")
  boxplot(vtheta, ylab = "Traco latente verdadeiro")
})
salvar_pdf("TracosLatentesVEscoreO.pdf", {
  plot(vescore, vtheta, xlab = "Escore total observado", ylab = "Traco latente verdadeiro")
})

# 4. Estimacao dos itens --------------------------------------------------------
# Um fator; prior lognormal para a (meanlog e sdlog, nao media/desvio de a).
# Mantem a prior do original. Com prior, o ajuste e por maximizacao a posteriori.
model.prior <- mirt::mirt.model(sprintf(
  "F1 = 1-%d\nPRIOR = (1-%d, a1, lnorm, -0.2058759, 0.6)", nI, nI))
resultMRG <- mirt::mirt(mYmirt, model = model.prior, itemtype = "graded",
                       SE = TRUE, method = "EM")
if (!isTRUE(mirt::extract.mirt(resultMRG, "converged"))) {
  stop("O ajuste nao convergiu. Revise o modelo antes de interpretar as estimativas.")
}
print(resultMRG)
# Extrai cada item e cada parametro pelo nome, preservando os erros-padrao.
# IRTpars transforma interceptos em limiares; EPs sao transformados pelo metodo delta.
coef_itens <- mirt::coef(resultMRG, IRTpars = TRUE, printSE = TRUE,
                          simplify = FALSE)[colnames(mYmirt)]
estimativas <- t(vapply(coef_itens, function(x) x["par", nomes_parametros],
                        numeric(ncat)))
erros_padrao <- t(vapply(coef_itens, function(x) x["SE", nomes_parametros],
                         numeric(ncat)))
if (any(!is.finite(erros_padrao))) warning("Ha erros-padrao indisponiveis; revise a identificacao do ajuste.")
verdadeiros <- as.matrix(mzeta[nomes_parametros])
z <- qnorm(0.975)
parametros_itens <- do.call(rbind, lapply(seq_len(ncat), function(j) {
  data.frame(item = mzeta$item, parametro = nomes_parametros[j],
    verdadeiro = verdadeiros[, j], estimativa = estimativas[, j],
    erro_padrao = erros_padrao[, j],
    ic95_inferior = estimativas[, j] - z * erros_padrao[, j],
    ic95_superior = estimativas[, j] + z * erros_padrao[, j])
}))
salvar_pdf("ParItensEDisp.pdf", {
  par(mfrow = c(ceiling(ncat / 3), 3))
  for (j in seq_len(ncat)) {
    plot(verdadeiros[, j], estimativas[, j], pch = 19,
         xlab = "Valor verdadeiro", ylab = "Estimativa", main = nomes_parametros[j])
    abline(0, 1, col = "blue", lwd = 2)
  }
})
salvar_pdf("ParItensEIC.pdf", {
  par(mfrow = c(ceiling(ncat / 3), 3))
  for (j in seq_len(ncat)) {
    plotrix::plotCI(estimativas[, j],
      ui = estimativas[, j] + z * erros_padrao[, j],
      li = estimativas[, j] - z * erros_padrao[, j],
      pch = 19, xlab = "Posicao do item", ylab = "Estimativa e IC 95%",
      main = nomes_parametros[j])
    points(verdadeiros[, j], col = "red", pch = 19)
  }
})

# 5. Estimacao dos tracos latentes ----------------------------------------------
# EAP: media da distribuicao posterior; preserva tambem o erro-padrao individual.
escores <- mirt::fscores(resultMRG, method = "EAP", full.scores.SE = TRUE)
rthetaMRG <- escores[, "F1"]
tracos_latentes <- data.frame(individuo = seq_len(n), verdadeiro = vtheta,
  estimativa_EAP = rthetaMRG, erro_padrao = escores[, "SE_F1"], escore = vescore)
salvar_pdf("TracosLatentesE.pdf", {
  par(mfrow = c(2, 2))
  limites <- range(c(vtheta, rthetaMRG))
  hist(vtheta, probability = TRUE, xlim = limites, border = "blue",
       xlab = "Traco latente", ylab = "Densidade", main = "Verdadeiro (azul) e EAP (verde)")
  hist(rthetaMRG, probability = TRUE, add = TRUE, border = "green")
  boxplot(list(Verdadeiro = vtheta, EAP = rthetaMRG), ylab = "Traco latente")
  plot(vtheta, rthetaMRG, pch = 19, xlab = "Verdadeiro", ylab = "EAP")
  abline(0, 1, col = "blue")
  qqnorm(as.numeric(scale(rthetaMRG)), main = "QQ normal do EAP padronizado")
  qqline(as.numeric(scale(rthetaMRG)), col = "blue")
})

# Salva os resultados centrais antes dos diagnosticos mais demorados.
salvar_csv(parametros_itens, "parametros_itens.csv")
salvar_csv(tracos_latentes, "tracos_latentes.csv")
salvar_csv(data.frame(individuo = seq_len(n), mYmirt), "respostas_simuladas.csv")
saveRDS(list(modelo = resultMRG, entrada = mzeta, respostas = mYmirt,
             parametros = parametros_itens, tracos = tracos_latentes,
             semente = semente), file.path(pasta_saida, "resultado_MRG.rds"))
writeLines(capture.output(sessionInfo()), file.path(pasta_saida, "sessionInfo.txt"))

# 6. Ajuste dos itens -----------------------------------------------------------
# Proporcoes observadas versus previstas, por categoria e faixa de theta.
salvar_pdf("CCIPOE.pdf", {
  for (i in seq_len(nI)) {
    print(mirt::itemfit(resultMRG, group.bins = 10, empirical.plot = i,
                        Theta = matrix(rthetaMRG)))
  }
})
# RMSEA de cada estatistica, extraido pelo nome. Linhas sao referencias visuais,
# nao regras universais de aceitacao. As tabelas guardam tambem os p-valores.
# S_X2 exige respostas completas; a simulacao padrao atende essa condicao.
ajustes_itens <- list()
for (estatistica in c("S_X2", "X2", "G2", "PV_Q1")) {
  ajuste <- mirt::itemfit(resultMRG, fit_stats = estatistica)
  ajustes_itens[[estatistica]] <- ajuste
  salvar_csv(ajuste, paste0("ajuste_itens_", estatistica, ".csv"))
  salvar_pdf(paste0(estatistica, "item.pdf"), {
    grafico_indice(ajuste[[paste0("RMSEA.", estatistica)]],
                   paste0("RMSEA (", estatistica, ")"), "Item", c(0, 0.05, 0.10, 1))
  })
}
infout <- mirt::itemfit(resultMRG, fit_stats = "infit")
salvar_csv(infout, "ajuste_itens_infit_outfit.csv")
for (indice in c("infit", "outfit")) {
  salvar_pdf(paste0(indice, "item.pdf"), {
    grafico_indice(infout[[indice]], indice, "Item", c(0, 0.5, 1, 1.5, 2, 2.5))
  })
}

# 7. Independencia local --------------------------------------------------------
# Compara tabelas de frequencias observadas/esperadas para cada par de itens.
# Preserva a medida do original: sqrt(sum((p_observada - p_esperada)^2)).
# Trata-se de distancia euclidiana entre proporcoes, NAO do RMSEA convencional.
auxresMRG <- mirt::residuals(resultMRG, type = "LD", tables = TRUE)
distancias_pares <- vapply(auxresMRG, function(tabela) {
  po <- tabela$Obs / sum(tabela$Obs)
  pe <- tabela$Exp / sum(tabela$Exp)
  sqrt(sum((po - pe)^2))
}, numeric(1))
rotulos_pares <- names(auxresMRG)
if (is.null(rotulos_pares)) rotulos_pares <- as.character(seq_along(distancias_pares))
salvar_csv(data.frame(par = rotulos_pares, distancia = distancias_pares),
           "independencia_local.csv")
salvar_pdf("RQEQMIL.pdf", {
  grafico_indice(distancias_pares, "Distancia entre proporcoes", "Par de itens", c(0, 0.05, 0.10, 1))
})

# 8. Ajuste dos respondentes ----------------------------------------------------
resindiv <- mirt::personfit(resultMRG)
salvar_csv(data.frame(individuo = seq_len(n), resindiv), "ajuste_individuos.csv")
for (indice in c("outfit", "infit", "Zh")) {
  salvar_pdf(paste0(tolower(indice), "indiv.pdf"), {
    referencias <- if (indice == "Zh") c(-3, -2, 0, 2, 3) else c(0, 0.5, 1, 1.5, 2, 2.5)
    grafico_indice(resindiv[[indice]], indice, "Individuo", referencias)
  })
}
message("Analise concluida. Resultados em: ", pasta_saida)
