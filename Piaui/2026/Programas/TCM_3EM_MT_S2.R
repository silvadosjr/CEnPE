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
folder <- "ResultadosTCM_3EM_MT"

# Subpasta específica desta análise
subfolder <- "Simulado_II"

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

#saveRDS(df_piaui_setembro,here('df_piaui_setembro.RDS'))

# =======================================================
# 🧱 2) ENTRADA DE DADOS E PRÉ-PROCESSAMENTO
# =======================================================

# 2.1) Leitura do banco RDS já higienizado em etapa anterior
base <- readRDS(file.path(dados_dir, "df_2ª_SIMULA_3ª_SÉRIE.RDS"))
colnames(base) <- trimws(colnames(base))

# 2.2) Filtra disciplina Matemática
base <- base %>%
  dplyr::filter(disciplina_descricao == "MATEMÁTICA")

# 2.3) Elimina itens/colunas específicas, se necessário
# suppressWarnings({
#   base <- base %>%
#     dplyr::select(-dplyr::any_of(c("rpa_024", "rp_024")))
# })

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
# 🧮 3) ESCORES TCT E RELATÓRIO RESUMO
# =======================================================

# 3.1) Escores brutos
vescore <- calculaEscore(mYc, mV)$vescoreb

TabEscores <- data.frame(
  matricula = base$matricula,
  Escores = vescore
)

openxlsx::write.xlsx(
  TabEscores,
  file.path(result_dir, "EscoresBrutos_Aluno.xlsx"),
  rowNames = FALSE
)

# 3.2) Histograma e boxplot
pdf(file.path(result_dir, "histbpescores.pdf"))
par(mfrow = c(1, 2))

hist(
  vescore,
  prob = TRUE,
  xlab = "escore",
  ylab = "densidade",
  main = "",
  cex = 1.2,
  cex.lab = 1.2
)

boxplot(
  vescore,
  ylab = "escore",
  cex = 1.2,
  cex.lab = 1.2,
  cex.main = 1.2
)

dev.off()

# 3.3) Resumo descritivo dos escores
med_resu <- c(
  mean(vescore, na.rm = TRUE),
  sd(vescore, na.rm = TRUE),
  100 * sd(vescore, na.rm = TRUE) / mean(vescore, na.rm = TRUE),
  min(vescore, na.rm = TRUE),
  quantile(vescore, 0.25, na.rm = TRUE),
  quantile(vescore, 0.50, na.rm = TRUE),
  quantile(vescore, 0.75, na.rm = TRUE),
  max(vescore, na.rm = TRUE),
  e1071::skewness(vescore, na.rm = TRUE),
  e1071::kurtosis(vescore, na.rm = TRUE) + 3
)

names(med_resu) <- c(
  "media", "dp", "cv(%)", "min.", "1o Q", "med.",
  "3oQ", "max.", "ca", "curt."
)

openxlsx::write.xlsx(
  data.frame(t(med_resu)),
  file.path(result_dir, "ResumoEscores.xlsx"),
  rowNames = TRUE
)


# =======================================================
# 📊 4) TCT: DIFICULDADE, DISCRIMINAÇÃO E CORRELAÇÕES
# =======================================================

# 4.1) Dificuldade e discriminação
resitemana <- itemana(mYc, mV, vescore)

vdific <- resitemana[, 1]
vdisc <- resitemana[, 4]

pdf(file.path(result_dir, "dificdiscitems.pdf"))
par(mfrow = c(1, 2))

plot(
  vdific,
  pch = 19,
  xlab = "",
  ylab = "dificuldade",
  cex = 1.1,
  xaxt = "n"
)
axis(1, 1:nI, labels = nomesitens, las = 2, cex.axis = 0.7)
abline(h = 0.5, col = "green", lwd = 2, lty = 2)

plot(
  vdisc,
  pch = 19,
  xlab = "",
  ylab = "discriminacao",
  cex = 1.1,
  xaxt = "n"
)
axis(1, 1:nI, labels = nomesitens, las = 2, cex.axis = 0.7)
abline(h = 0.2, col = "green", lwd = 2, lty = 2)
abline(h = 0.0, col = "red", lwd = 2, lty = 2)

dev.off()

# 4.2) Correlações ponto-bisserial e bisserial
vcpBis <- cpBis(
  mYcNA,
  mV,
  vgabr,
  dealNA = "exclude",
  dichot = TRUE
)$output$pBis

vcBis <- cBis(vcpBis, vdific, vgabr)$output$cBis

MedidasTCM <- data.frame(
  Item = nomesitens,
  Dificuldade = vdific,
  Discriminacao = vdisc,
  CBisserial = vcBis,
  CPBisserial = vcpBis
)

openxlsx::write.xlsx(
  MedidasTCM,
  file.path(result_dir, "MedidasTCM_Item.xlsx"),
  rowNames = FALSE
)

pdf(file.path(result_dir, "corpbisbisitems.pdf"))
par(mfrow = c(1, 2))

plot(
  vcpBis,
  pch = 19,
  xlab = "",
  ylab = "correlacao ponto-bisserial",
  cex = 1.1,
  xaxt = "n"
)
axis(1, 1:nI, labels = nomesitens, las = 2, cex.axis = 0.7)
abline(h = 0.2, col = "green", lwd = 2, lty = 2)
abline(h = 0.0, col = "red", lwd = 2, lty = 2)

plot(
  vcBis,
  pch = 19,
  xlab = "",
  ylab = "correlacao bisserial",
  cex = 1.1,
  xaxt = "n"
)
axis(1, 1:nI, labels = nomesitens, las = 2, cex.axis = 0.7)
abline(h = 0.2, col = "green", lwd = 2, lty = 2)
abline(h = 0.0, col = "red", lwd = 2, lty = 2)

dev.off()


# =============================================================
# 🧩 5) TCT: GRUPOS DE ESCORE E GRÁFICOS ASSOCIADOS
# =============================================================

# 5.1) Definição das faixas de escore
breaks_5 <- unique(
  quantile(
    vescore,
    probs = seq(0, 1, by = 0.2),
    na.rm = TRUE
  )
)

if (length(breaks_5) < 6) {
  eps <- 1e-8
  breaks_5 <- seq(
    min(vescore, na.rm = TRUE) - eps,
    max(vescore, na.rm = TRUE) + eps,
    length.out = 6
  )
}

grupo <- cut(
  vescore,
  breaks = breaks_5,
  labels = c("G1", "G2", "G3", "G4", "G5"),
  include.lowest = TRUE,
  right = TRUE
)

# 5.2) Percentual de acertos por grupo
Respostas_grupo <- data.frame(mYc, grupo)
mV_grupo <- data.frame(mV, grupo)

Respostas_grupo[mV_grupo == 0] <- NA
Respostas_grupo$grupo <- factor(
  Respostas_grupo$grupo,
  levels = c("G1", "G2", "G3", "G4", "G5")
)

proporcao_acertos <- Respostas_grupo %>%
  dplyr::group_by(grupo) %>%
  dplyr::summarise(
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = ~ mean(.x, na.rm = TRUE)
    )
  )

proporcao_acertos <- as.data.frame(t(proporcao_acertos[, -1]))
colnames(proporcao_acertos) <- c("G1", "G2", "G3", "G4", "G5")
rownames(proporcao_acertos) <- nomesitens

openxlsx::write.xlsx(
  proporcao_acertos,
  file.path(result_dir, "PropAcertosGrupo.xlsx"),
  rowNames = TRUE
)

proporcao_acertos[] <- lapply(proporcao_acertos, as.numeric)
prop_mat <- as.matrix(proporcao_acertos)

# 5.3) Gráfico do percentual de acertos por grupo
pdf(file.path(result_dir, "graficos_proporcao_acertos.pdf"))

for (i in 1:nI) {
  barplot(
    prop_mat[i, ],
    names.arg = c("G1", "G2", "G3", "G4", "G5"),
    main = nomesitens[i],
    col = "lightblue",
    ylim = c(0, 1),
    ylab = "Proporção de Acertos",
    xlab = "Grupo (faixas de escore)"
  )
}

dev.off()

cat("Arquivo PDF salvo:", file.path(result_dir, "graficos_proporcao_acertos.pdf"), "\n")

# 5.4) Percentual de escolhas por alternativa × grupo
mY_NA <- mY
mY_NA[mY == 9] <- NA

m_prop_escolha <- matrix(0, 5 * nI, length(opcoes))

for (i in 1:nI) {
  for (j in 1:5) {
    
    idx_g <- which(grupo == levels(grupo)[j])
    
    aux1 <- table(
      factor(
        mY_NA[idx_g, i],
        levels = opcoes
      )
    )
    
    aux2 <- sum(mV[idx_g, i], na.rm = TRUE)
    
    if (aux2 > 0) {
      m_prop_escolha[(i - 1) * 5 + j, ] <- 100 * as.numeric(aux1) / aux2
    } else {
      m_prop_escolha[(i - 1) * 5 + j, ] <- 0
    }
  }
}

m_prop_escolhaDF <- data.frame(
  Item = rep(nomesitens, each = 5),
  Grupo = rep(paste0("G", 1:5), nI),
  m_prop_escolha,
  gabarito = rep(vgabr, each = 5)
)

colnames(m_prop_escolhaDF) <- c("Item", "Grupo", opcoes, "gabarito")

openxlsx::write.xlsx(
  m_prop_escolhaDF,
  file.path(result_dir, "PropEscolhaAlternativaGrupo.xlsx"),
  rowNames = FALSE
)

# 5.5) Gráficos do percentual de escolhas por alternativa × grupo
pdf(file.path(result_dir, "graficos_percentuais_escolha.pdf"))

itens_unicos <- unique(m_prop_escolhaDF$Item)

for (item in itens_unicos) {
  
  dados_item <- subset(m_prop_escolhaDF, Item == item)
  
  dados_long <- reshape2::melt(
    dados_item,
    id.vars = c("Item", "Grupo", "gabarito"),
    variable.name = "Alternativa",
    value.name = "Percentual"
  )
  
  p <- ggplot(
    dados_long,
    aes(
      x = Grupo,
      y = Percentual,
      color = Alternativa,
      group = Alternativa
    )
  ) +
    geom_line(linewidth = 1.2) +
    geom_text(aes(label = Alternativa), vjust = -0.5, size = 3) +
    labs(
      title = paste(
        "Percentual de Escolha -",
        item,
        "- Gabarito:",
        unique(dados_item$gabarito)
      ),
      x = "Grupo (faixas de escore)",
      y = "Percentual de Escolha"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10)
    )
  
  print(p)
}

dev.off()

cat("Arquivo PDF salvo:", file.path(result_dir, "graficos_percentuais_escolha.pdf"), "\n")


# =======================================================
# 🅰️ 6) TCT POR ALTERNATIVA
# =======================================================

mY_aux <- mY
mY_aux[mY_aux == 9] <- NA
colnames(mY_aux) <- nomesitens

mitemanaltNR <- itemanaltNR(
  mY_aux,
  mV,
  vescore,
  vgabr,
  nalt = length(opcoes),
  opcoes = opcoes
)

# Zera para NA em categorias inexistentes
for (k in 1:length(opcoes)) {
  mitemanaltNR$mresult[, , k][mitemanaltNR$mresult[, , k] == 0] <- NA
}

mitemanaltNR$mDific[mitemanaltNR$mDific == 0] <- NA
mitemanaltNR$mDisc[mitemanaltNR$mDisc == 0] <- NA

# 6.1) Dificuldade por alternativa
mDificNR <- mitemanaltNR$mDific
mDificNR[, 1:length(opcoes)] <- round(
  100 * mDificNR[, 1:length(opcoes)],
  3
)

mDificNRDF <- data.frame(mDificNR, num_alter)

colnames(mDificNRDF) <- c(
  "Branco", "Rasura", "A", "B", "C", "D", "E",
  "Gabarito", "n_alt"
)

openxlsx::write.xlsx(
  mDificNRDF,
  file.path(result_dir, "mDificNRDF.xlsx"),
  dec = ".",
  rowNames = TRUE
)

# 6.2) Discriminação por alternativa
mDiscNR <- mitemanaltNR$mDisc

mDiscNRDF <- data.frame(
  round(mDiscNR[, -(length(opcoes) + 1)], 3),
  Key = mDiscNR$Key,
  num_alter
)

colnames(mDiscNRDF) <- c(
  "Branco", "Rasura", "A", "B", "C", "D", "E",
  "Gabarito", "n_alt"
)

openxlsx::write.xlsx(
  mDiscNRDF,
  file.path(result_dir, "mDiscNRDF.xlsx"),
  dec = ".",
  rowNames = TRUE
)

# 6.3) Correlação ponto-bisserial por alternativa
mcpBisNR <- cpBisNR(
  mY_aux,
  mV,
  vgabr,
  dealNA = "exclude",
  opcoes,
  nalt = length(opcoes)
)$output

mcpBisNRDF <- data.frame(
  mcpBisNR[, -(length(opcoes) + 1)],
  key = mcpBisNR[, length(opcoes) + 1],
  num_alter
)

colnames(mcpBisNRDF) <- c(
  "Branco", "Rasura", "A", "B", "C", "D", "E",
  "Gabarito", "n_alt"
)

openxlsx::write.xlsx(
  mcpBisNRDF,
  file.path(result_dir, "mcpBisNRDF.xlsx"),
  dec = ".",
  rowNames = TRUE
)

# 6.4) Correlação bisserial por alternativa
mDificNRaux <- mDificNR[, -(length(opcoes) + 1)] / 100

mcBisNR <- cBisNR(
  mcpBisNRDF[, 1:7],
  mDificNRaux,
  vgabr
)$output

mcBisNRDF <- data.frame(
  mcBisNR[, -(length(opcoes) + 1)],
  key = mcBisNR$answerkey,
  num_alter
)

colnames(mcBisNRDF) <- c(
  "Branco", "Rasura", "A", "B", "C", "D", "E",
  "Gabarito", "n_alt"
)

openxlsx::write.xlsx(
  mcBisNRDF,
  file.path(result_dir, "mcBisNRDF.xlsx"),
  dec = ".",
  rowNames = TRUE
)


# =======================================================
# ✅ 7) CHECKLIST FINAL
# =======================================================

cat("\n====================================================\n")
cat("Análise TCT concluída.\n")
cat("Arquivos salvos em:\n")
cat(result_dir, "\n")
cat("====================================================\n\n")

cat("Arquivos gerados:\n")
cat("1) EscoresBrutos_Aluno.xlsx\n")
cat("2) histbpescores.pdf\n")
cat("3) ResumoEscores.xlsx\n")
cat("4) dificdiscitems.pdf\n")
cat("5) MedidasTCM_Item.xlsx\n")
cat("6) corpbisbisitems.pdf\n")
cat("7) PropAcertosGrupo.xlsx\n")
cat("8) graficos_proporcao_acertos.pdf\n")
cat("9) PropEscolhaAlternativaGrupo.xlsx\n")
cat("10) graficos_percentuais_escolha.pdf\n")
cat("11) mDificNRDF.xlsx\n")
cat("12) mDiscNRDF.xlsx\n")
cat("13) mcpBisNRDF.xlsx\n")
cat("14) mcBisNRDF.xlsx\n")
cat("====================================================\n")