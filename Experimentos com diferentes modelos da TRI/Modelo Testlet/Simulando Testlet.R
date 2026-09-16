# =============================================================================
# MODELO TESTLET 3PL: simulacao e recuperacao de parametros
# =============================================================================
# Entrada: Item_Parameters.ods, primeira aba, colunas a, b, c e testlet.
# Saidas: resultados/Testlet, dentro da pasta deste script (veja README.md).
# Execute o arquivo inteiro com source(..., encoding = "UTF-8") ou Rscript.
# Dependencias: install.packages(c("mirt", "readODS"))
# A analise preserva o experimento original: um grupo, um testlet e duas dimensoes.
# Nao limpa o ambiente global, nao muda getwd() e nao exige Aux Func IRT.R.

# 1. Caminhos e configuracao ----------------------------------------------------
localizar_pasta <- function() {
  # Alguns terminais iniciam o R no locale C, que corrompe caminhos com acentos.
  # Usa UTF-8 apenas durante a resolucao e restaura o locale do usuario ao sair.
  locale_anterior <- Sys.getlocale("LC_CTYPE")
  if (.Platform$OS.type == "windows" && identical(locale_anterior, "C")) {
    on.exit(Sys.setlocale("LC_CTYPE", locale_anterior), add = TRUE)
    Sys.setlocale("LC_CTYPE", ".UTF-8")
  }
  normalizar <- function(x) enc2utf8(normalizePath(x, mustWork = TRUE))
  arquivos <- Filter(Negate(is.null), lapply(sys.frames(), function(x) x$ofile))
  if (length(arquivos)) {
    return(dirname(normalizar(tail(arquivos, 1)[[1]])))
  }
  argumento <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(argumento)) {
    return(dirname(normalizar(sub("^--file=", "", argumento[1]))))
  }
  candidatas <- c(getwd(), file.path(getwd(),
    "Experimentos com diferentes modelos da TRI", "Modelo Testlet"))
  encontradas <- candidatas[file.exists(file.path(candidatas, "Item_Parameters.ods"))]
  if (!length(encontradas)) stop("Execute o arquivo inteiro usando source() ou Rscript.")
  normalizar(encontradas[1])
}

pasta_script <- localizar_pasta()
arquivo_entrada <- file.path(pasta_script, "Item_Parameters.ods")
pasta_saida <- file.path(pasta_script, "resultados", "Testlet")
n <- 3000L                          # Numero de respondentes.
sigma2tau <- 0.8                     # Variancia verdadeira do efeito testlet.
semente <- 4445L                     # Semente preservada do original.

pacotes <- c("mirt", "readODS")
faltantes <- pacotes[!vapply(pacotes, requireNamespace, logical(1), quietly = TRUE)]
if (length(faltantes)) {
  stop("Instale os pacotes ausentes: install.packages(c(",
       paste(sprintf('"%s"', faltantes), collapse = ", "), "))")
}
if (!file.exists(arquivo_entrada)) stop("Entrada nao encontrada: ", arquivo_entrada)
if (length(n) != 1L || !is.finite(n) || n < 2 || n != as.integer(n)) {
  stop("n deve ser um inteiro maior que 1.")
}
if (length(sigma2tau) != 1L || !is.finite(sigma2tau) || sigma2tau <= 0) {
  stop("sigma2tau deve ser positiva e finita.")
}
dir.create(pasta_saida, recursive = TRUE, showWarnings = FALSE)
set.seed(semente)

# 2. Funcoes auxiliares locais -------------------------------------------------
salvar_pdf <- function(nome, desenho) {
  grDevices::pdf(file.path(pasta_saida, nome), width = 10, height = 7)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(desenho)
}
salvar_csv <- function(tabela, nome) {
  utils::write.csv(tabela, file.path(pasta_saida, nome), row.names = FALSE,
                   fileEncoding = "UTF-8")
}
grafico_ic <- function(estimativa, inferior, superior, verdadeiro, titulo) {
  limites <- range(c(estimativa, inferior, superior, verdadeiro), finite = TRUE)
  plot(seq_along(estimativa), estimativa, pch = 19, ylim = limites,
       xlab = "Item", ylab = "Estimativa e IC 95%", main = titulo)
  validos <- is.finite(inferior) & is.finite(superior) & superior > inferior
  arrows(which(validos), inferior[validos], which(validos), superior[validos],
         angle = 90, code = 3, length = 0.04)
  points(seq_along(verdadeiro), verdadeiro, col = "red", pch = 17)
  legend("topleft", c("Estimativa", "Verdadeiro"), pch = c(19, 17),
         col = c("black", "red"), bty = "n", cex = 0.8)
}
validar_entrada <- function(dados) {
  obrigatorias <- c("a", "b", "c", "testlet")
  if (!all(obrigatorias %in% names(dados))) {
    stop("A planilha deve conter as colunas a, b, c e testlet.")
  }
  dados <- as.data.frame(dados[, obrigatorias, drop = FALSE])
  # A planilha contem linhas finais formatadas, mas sem parametros.
  dados <- dados[rowSums(!is.na(dados)) > 0L, , drop = FALSE]
  if (!all(vapply(dados, is.numeric, logical(1))) ||
      anyNA(dados) || any(!is.finite(as.matrix(dados)))) {
    stop("Todas as colunas devem ser numericas e completas, sem valores infinitos.")
  }
  if (any(dados$a <= 0) || any(dados$c < 0 | dados$c >= 1)) {
    stop("Exigem-se a > 0 e 0 <= c < 1.")
  }
  if (!all(dados$testlet %in% c(0, 1)) ||
      sum(dados$testlet == 1) < 3L || sum(dados$testlet == 0) < 3L) {
    stop("Este script exige um testlet (0/1), com >= 3 itens dentro e >= 3 fora.")
  }
  rownames(dados) <- NULL
  dados
}
# A matriz vcov usa rotulos como a1.16.17 para parametros ligados por igualdade.
# Localiza o numero do parametro (mod2values), sem supor blocos de tamanho fixo.
indice_vcov <- function(numero, matriz) {
  ids <- strsplit(colnames(matriz), ".", fixed = TRUE)
  encontrados <- which(vapply(ids, function(x) as.character(numero) %in% x[-1L], logical(1)))
  if (length(encontrados) != 1L) stop("Parametro nao localizado univocamente em vcov: ", numero)
  encontrados
}
ep_dificuldade <- function(a, d, cov_ad) {
  # b = -d/a: o metodo delta inclui a covariancia entre a e d.
  gradiente <- c(d / a^2, -1 / a)
  variancia <- as.numeric(t(gradiente) %*% cov_ad %*% gradiente)
  if (!is.finite(variancia) || variancia < -1e-10) stop("Variancia invalida para b.")
  sqrt(max(0, variancia))
}

# 3. Leitura e simulacao --------------------------------------------------------
m_inf_item <- validar_entrada(readODS::read_ods(arquivo_entrada, sheet = 1))
nI <- nrow(m_inf_item)
nomes_itens <- paste0("Item_", seq_len(nI))
a <- m_inf_item$a
b <- m_inf_item$b
acerto_casual <- m_inf_item$c         # Evita criar uma variavel chamada c.
testlet <- m_inf_item$testlet
itens_testlet <- which(testlet == 1)
d <- -a * b
ma <- cbind(a, a * testlet)

# theta ~ N(0, 1), tau ~ N(0, sigma2tau), independentes na populacao.
# Todos os itens dependem de theta; so os itens testlet dependem de tau.
# P(Y_ij = 1 | theta_i, tau_i) =
# c_j + (1-c_j)*plogis(a_j*(theta_i + testlet_j*tau_i - b_j)).
vtheta <- rnorm(n)
vtau <- rnorm(n, sd = sqrt(sigma2tau))
mTheta <- cbind(G = vtheta, S1 = vtau)
mY <- mirt::simdata(a = ma, d = d, N = n, itemtype = "dich",
                    guess = acerto_casual, Theta = mTheta)
colnames(mY) <- nomes_itens
# Experimento com respostas completas: 0 = erro, 1 = acerto.
# Dados reais com ausencias exigem NA e revisao dos diagnosticos (sobretudo S_X2).
vescore <- rowSums(mY)
if (any(vapply(seq_len(nI), function(i) length(unique(mY[, i])) < 2L, logical(1)))) {
  stop("Algum item tem respostas constantes; revise n e os parametros.")
}

# 4. Ajuste do modelo Testlet ---------------------------------------------------
# NA indica item sem fator especifico; 1 indica participacao no testlet S1.
specific <- ifelse(testlet == 1, 1L, NA_integer_)
# Mantem as prioris do original. Em g, a normal se aplica a logit(g).
# As prioris a1/a2 sao preservadas, inclusive nos parametros ligados por igualdade.
# CONSTRAIN impoe a1=a2 em cada item testlet. Assim, a variancia de S1 e estimavel.
# G tem variancia 1; G e S1 tem medias 0 e covariancia 0.
prior_a2 <- paste(sprintf("(%d, a2, lnorm, -0.2058759, 0.6)", itens_testlet), collapse = ", ")
igualdades <- paste(sprintf("(%d, a1, a2)", itens_testlet), collapse = ", ")
sintaxe_modelo <- sprintf(paste0(
  "G = 1-%d\n",
  "PRIOR = (1-%d, a1, lnorm, -0.2058759, 0.6), %s,\n",
  "        (1-%d, d, norm, 0, 10), (1-%d, g, norm, -1.3, 0.7)\n",
  "CONSTRAIN = %s\nCOV = S1*S1"), nI, nI, prior_a2, nI, nI, igualdades)
model_testlet <- mirt::mirt.model(sintaxe_modelo)
simmod <- mirt::bfactor(data = mY, model = specific, model2 = model_testlet,
                        itemtype = "3PL", SE = TRUE)
# Salva o objeto bruto antes de extrair resultados, inclusive se nao convergir.
saveRDS(simmod, file.path(pasta_saida, "modelo_Testlet.rds"))
writeLines(capture.output(sessionInfo()), file.path(pasta_saida, "sessionInfo.txt"))
writeLines(sintaxe_modelo, file.path(pasta_saida, "especificacao_modelo.txt"))
if (!isTRUE(mirt::extract.mirt(simmod, "converged"))) {
  stop("O modelo nao convergiu. Objeto salvo; revise o ajuste antes de interpreta-lo.")
}
print(simmod)

# 5. Parametros e erros-padrao --------------------------------------------------
# Com printSE=TRUE, g vem na escala logit. A conversao para c e explicita.
# IRTpars=TRUE nao transforma os itens com duas cargas; calculamos b=-d/a1.
coef_ep <- mirt::coef(simmod, printSE = TRUE, rawug = TRUE, simplify = FALSE)
valores_modelo <- mirt::mod2values(simmod)
matriz_vcov <- mirt::vcov(simmod)
if (is.null(colnames(matriz_vcov)) || any(!is.finite(matriz_vcov))) {
  stop("Matriz de covariancias indisponivel; nao e possivel calcular os ICs.")
}
z <- qnorm(0.975)
parametros_itens <- do.call(rbind, lapply(seq_len(nI), function(i) {
  co <- coef_ep[[nomes_itens[i]]]
  if (!all(c("par", "SE") %in% rownames(co))) stop("Erros-padrao indisponiveis.")
  ai <- co["par", "a1"]
  di <- co["par", "d"]
  gi <- co["par", "logit(g)"]
  ci <- plogis(gi)
  linha <- valores_modelo[valores_modelo$item == nomes_itens[i], ]
  numeros <- linha$parnum[match(c("a1", "d"), linha$name)]
  posicoes <- vapply(numeros, indice_vcov, integer(1), matriz = matriz_vcov)
  se_b <- ep_dificuldade(ai, di, matriz_vcov[posicoes, posicoes, drop = FALSE])
  estimativa <- c(ai, -di / ai, ci)
  ep <- c(co["SE", "a1"], se_b, ci * (1 - ci) * co["SE", "logit(g)"])
  inferior <- estimativa - z * ep
  superior <- estimativa + z * ep
  # Para c, transforma o IC da escala logit: limites permanecem entre 0 e 1.
  inferior[3] <- plogis(gi - z * co["SE", "logit(g)"])
  superior[3] <- plogis(gi + z * co["SE", "logit(g)"])
  data.frame(item = nomes_itens[i], testlet = testlet[i], parametro = c("a", "b", "c"),
             verdadeiro = c(a[i], b[i], acerto_casual[i]), estimativa = estimativa,
             erro_padrao = ep, ic95_inferior = inferior, ic95_superior = superior)
}))
if (any(!is.finite(as.matrix(parametros_itens[, 4:8])))) {
  stop("Estimativas ou erros-padrao nao finitos. Revise o ajuste salvo.")
}
# COV_22 e a variancia de S1, extraida pelo nome em vez de pela quinta coluna.
variancia_estimada <- coef_ep$GroupPars["par", "COV_22"]
ep_variancia <- coef_ep$GroupPars["SE", "COV_22"]
if (!is.finite(variancia_estimada) || variancia_estimada <= 0 ||
    !is.finite(ep_variancia) || ep_variancia < 0) {
  stop("Variancia do testlet ou seu erro-padrao invalido.")
}
# IC aproximado na escala log, para evitar limites negativos de variancia.
variancia_testlet <- data.frame(testlet = "S1", verdadeiro = sigma2tau,
  estimativa = variancia_estimada, erro_padrao = ep_variancia,
  ic95_inferior = exp(log(variancia_estimada) - z * ep_variancia / variancia_estimada),
  ic95_superior = exp(log(variancia_estimada) + z * ep_variancia / variancia_estimada))

# 6. Habilidade geral e efeito testlet por pessoa -------------------------------
# EAP = media posterior; os erros-padrao condicionam nos parametros estimados.
mthetatau <- mirt::fscores(simmod, method = "EAP", full.scores.SE = TRUE)
tracos_latentes <- data.frame(individuo = seq_len(n), escore = vescore,
  theta_verdadeiro = vtheta, theta_EAP = mthetatau[, "G"],
  theta_EP = mthetatau[, "SE_G"], tau_verdadeiro = vtau,
  tau_EAP = mthetatau[, "S1"], tau_EP = mthetatau[, "SE_S1"])
salvar_csv(data.frame(individuo = seq_len(n), mY), "respostas_simuladas.csv")
salvar_csv(parametros_itens, "parametros_itens.csv")
salvar_csv(variancia_testlet, "variancia_testlet.csv")
salvar_csv(tracos_latentes, "tracos_latentes.csv")
resultado <- list(modelo = simmod, entrada = m_inf_item, respostas = mY,
  parametros = parametros_itens, variancia = variancia_testlet, tracos = tracos_latentes,
  configuracao = list(n = n, sigma2tau = sigma2tau, semente = semente),
  sintaxe_modelo = sintaxe_modelo)
saveRDS(resultado, file.path(pasta_saida, "resultado_Testlet.rds"))

# 7. Graficos de recuperacao dos parametros ------------------------------------
salvar_pdf("ParItens.pdf", {
  par(mfrow = c(2, 2))
  for (nome in c("a", "b", "c")) {
    tabela <- parametros_itens[parametros_itens$parametro == nome, ]
    plot(tabela$verdadeiro, tabela$estimativa, pch = 19,
         xlab = "Verdadeiro", ylab = "Estimativa", main = paste("Parametro", nome))
    abline(0, 1, lty = 2, col = "gray")
  }
})
salvar_pdf("ParItensEIC.pdf", {
  par(mfrow = c(2, 2))
  for (nome in c("a", "b", "c")) {
    tabela <- parametros_itens[parametros_itens$parametro == nome, ]
    grafico_ic(tabela$estimativa, tabela$ic95_inferior, tabela$ic95_superior,
               tabela$verdadeiro, paste("Parametro", nome))
  }
})
salvar_pdf("TLEAT.pdf", {
  par(mfrow = c(2, 2))
  plot(vtheta, mthetatau[, "G"], pch = 19, xlab = "Verdadeiro", ylab = "EAP", main = "Habilidade geral")
  abline(0, 1, lty = 2, col = "gray")
  plot(vtau, mthetatau[, "S1"], pch = 19, xlab = "Verdadeiro", ylab = "EAP", main = "Efeito testlet")
  abline(0, 1, lty = 2, col = "gray")
  boxplot(list(Simulado = vtheta, Estimado = mthetatau[, "G"]), main = "Habilidade geral")
  boxplot(list(Simulado = vtau, Estimado = mthetatau[, "S1"]), main = "Efeito testlet")
})
salvar_pdf("VarEAT.pdf", {
  limites <- range(c(sigma2tau, variancia_testlet$ic95_inferior, variancia_testlet$ic95_superior))
  plot(c(1, 2), c(sigma2tau, variancia_estimada), ylim = limites, xlim = c(0.5, 2.5),
       xaxt = "n", pch = 19, xlab = "", ylab = "Variancia", main = "Variancia do efeito testlet")
  axis(1, at = c(1, 2), labels = c("Verdadeira", "Estimada e IC 95%"))
  arrows(2, variancia_testlet$ic95_inferior, 2, variancia_testlet$ic95_superior,
         angle = 90, code = 3, length = 0.08)
})

# 8. Diagnosticos de ajuste -----------------------------------------------------
# Curvas CONDICIONAIS: fixa tau em -DP, 0 e +DP estimados, variando theta.
# Fora do testlet, as tres curvas coincidem. Nao sao curvas marginais em tau.
salvar_pdf("CCI.pdf", {
  grade_theta <- seq(-4, 4, length.out = 201)
  cortes_tau <- c(-1, 0, 1) * sqrt(variancia_estimada)
  for (i in seq_len(nI)) {
    item <- mirt::extract.item(simmod, i)
    probabilidades <- vapply(cortes_tau, function(tau) {
      mirt::probtrace(item, Theta = cbind(grade_theta, rep(tau, length(grade_theta))))[, 2]
    }, numeric(length(grade_theta)))
    matplot(grade_theta, probabilidades, type = "l", lty = 1:3,
            col = c("blue", "black", "red"), ylim = c(0, 1),
            xlab = "Habilidade geral (theta)", ylab = "Probabilidade de acerto",
            main = paste("CCI condicional -", nomes_itens[i]))
    legend("topleft", c("tau = -DP", "tau = 0", "tau = +DP"), lty = 1:3,
           col = c("blue", "black", "red"), bty = "n")
  }
})
infout <- mirt::itemfit(simmod, fit_stats = "infit", Theta = mthetatau[, c("G", "S1")])
salvar_csv(infout, "ajuste_itens_infit_outfit.csv")
for (indice in c("infit", "outfit")) {
  nome_pdf <- if (indice == "infit") "Infit.pdf" else "Outfit.pdf"
  salvar_pdf(nome_pdf, {
    valores <- infout[[indice]]
    plot(valores, pch = 19, xlab = "Item", ylab = indice,
         ylim = range(c(0, 2.5, valores), finite = TRUE))
    abline(h = c(0.5, 1, 1.5, 2), col = "blue", lty = 2)
  })
}
# Preserva a distancia do original: nao se trata do RMSEA convencional.
# O modelo ja inclui a dependencia induzida por tau nas frequencias esperadas.
auxres <- mirt::residuals(simmod, type = "LD", tables = TRUE)
distancias <- vapply(auxres, function(tabela) {
  po <- tabela$Obs / sum(tabela$Obs)
  pe <- tabela$Exp / sum(tabela$Exp)
  sqrt(sum((po - pe)^2))
}, numeric(1))
independencia_local <- data.frame(par = names(auxres), distancia = distancias)
salvar_csv(independencia_local, "independencia_local.csv")
salvar_pdf("RQEQMIL.pdf", {
  plot(distancias, pch = 19, xlab = "Par de itens", ylab = "Distancia entre proporcoes", ylim = c(0, 1))
  abline(h = c(0.05, 0.10), col = "green", lty = 2)
})
# S_X2: salva estatistica, graus de liberdade, RMSEA e p-valor.
# O PDF mostra explicitamente o p-valor; referencias graficas nao sao regras universais.
ajuste_sx2 <- mirt::itemfit(simmod, fit_stats = "S_X2")
salvar_csv(ajuste_sx2, "ajuste_itens_S_X2.csv")
salvar_pdf("S_X2.pdf", {
  plot(ajuste_sx2$p.S_X2, ylim = c(0, 1), pch = 19,
       xlab = "Item", ylab = "p-valor S_X2 (sem correcao por multiplicidade)")
  abline(h = 0.05, col = "gray", lty = 2)
})
result_prop <- mirt::itemfit(simmod, fit_stats = "S_X2", return.tables = TRUE)
proporcoes <- do.call(rbind, lapply(seq_len(nI), function(i) {
  obs <- result_prop$O[[i]]
  esp <- result_prop$E[[i]]
  celulas <- which(is.finite(obs) & is.finite(esp), arr.ind = TRUE)
  data.frame(item = nomes_itens[i], faixa = rownames(obs)[celulas[, 1]],
    categoria = colnames(obs)[celulas[, 2]], observado = obs[celulas], esperado = esp[celulas],
    proporcao_observada = obs[celulas] / n, proporcao_esperada = esp[celulas] / n)
}))
salvar_csv(proporcoes, "proporcoes_observadas_esperadas.csv")
salvar_pdf("ProObsEsp.pdf", {
  for (i in seq_len(nI)) {
    tabela <- proporcoes[proporcoes$item == nomes_itens[i], ]
    plot(tabela$proporcao_observada, tabela$proporcao_esperada, pch = 19,
         xlab = "Proporcao observada (celula / n)", ylab = "Proporcao esperada (celula / n)",
         main = nomes_itens[i])
    abline(0, 1, col = "gray", lty = 2)
  }
})
# Atualiza o RDS com os diagnosticos apenas apos finalizar todas as etapas.
resultado$diagnosticos <- list(infit_outfit = infout, S_X2 = ajuste_sx2,
  independencia_local = independencia_local, proporcoes = proporcoes, tabelas_S_X2 = result_prop)
saveRDS(resultado, file.path(pasta_saida, "resultado_Testlet.rds"))
message("Analise concluida. Resultados em: ", pasta_saida)
