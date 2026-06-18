# =======================================================
# 📦 1) PACOTES E PREPARO DO AMBIENTE
# =======================================================

# Diretório de trabalho (ajuste conforme necessário)
setwd('C:/Users/Usuário/OneDrive/Documentos/CEnPE/Piauí/Setembro/')

# Lista de pacotes necessários
pacotes <- c(
  "mirt","data.table","car","xtable","readODS","e1071","haven",
  "foreign","dplyr","plyr","openxlsx","plotrix","readxl","tidyr",
  "readr","stringr","ggplot2","reshape2","here"
)

# Instala pacotes ausentes
novos_pacotes <- pacotes[!(pacotes %in% installed.packages()[,"Package"])]
if (length(novos_pacotes)) install.packages(novos_pacotes)

# Carrega pacotes
invisible(lapply(pacotes, require, character.only = TRUE))

# Carrega funções auxiliares do IRT (suas funções autorais)
source(file.path('C:/Users/Usuário/OneDrive/Documentos/CEnPE/Piauí',"Aux Func IRT.r"))

# Pasta de saída dos resultados
folder    <- "ResultadosTCM_3EM_MT"
file.save <- here(folder)
dir.create(folder, showWarnings = FALSE)

#saveRDS(df_piaui_setembro,here('df_piaui_setembro.RDS'))

# =======================================================
# 🧱 2) ENTRADA DE DADOS E PRÉ-PROCESSAMENTO
# =======================================================

# 2.1) Leitura do banco RDS já higienizado em etapa anterior
base <- readRDS(here('df_piaui_setembro.RDS'))
colnames(base) <- trimws(colnames(base))

# 2.2) Filtra disciplina Matemática (ajuste o nome se necessário)
base <- base %>% dplyr::filter(disciplina_descricao == 'MATEMÁTICA')

# 2.3) Elimina itens/colunas específicas (ex.: erro/campo duplicado)
#     OBS: ajuste os nomes conforme a realidade do banco
suppressWarnings({
  base <- base %>% dplyr::select(-dplyr::any_of(c("rpa_024","rp_024"))) # O item 24 foi anulado
})

# 2.4) Extrai gabarito (colunas "rp_*", exceto "rpa_*")
#     Aqui usamos "starts_with" e removemos "rpa_" para evitar operador lógico dentro de select().
gabar_cols <- base %>% dplyr::select(starts_with("rp")) %>% dplyr::select(-starts_with("rpa"))
vgabr      <- as.character(gabar_cols[1, ])    # gabarito = primeira linha das colunas rp_*
rm(gabar_cols)

# 2.5) Mantém apenas avaliados e apenas respostas do tipo "rpa_*"
base <- base %>%
  dplyr::filter(fl_avaliado == 1) %>%
  dplyr::select(matricula, dplyr::starts_with("rpa_"))

# 2.6) Matriz de respostas brutas (apenas itens)
mY <- as.data.frame(base[,-1])

## Distribuição de frequências de caracteres observados

mY_tab<-apply(mY,2,table)


# 2.7) Convenção de "não apresentado" (mV) e "não resposta"
#     Neste projeto, "9" codifica "item não apresentado". Vamos criar a matriz mV:
#     mV = 1 se item apresentado, 0 se NÃO apresentado.
mV <- 1 - (mY == 9)

# 2.8) Corrige itens -> matriz binária mYc (1=acerto, 0=erro),
#     com 0 também para itens não observados (mas apresentados).
#     A função corrigeItens precisa do gabarito (vgabr).
mYc <- corrigeItens(mY, vgabr)  # (função autoral)

# 2.9) Versão com NA para itens não apresentados
mYcNA <- mYc
mYcNA[mV == 0] <- NA

# 2.10) Nomes dos itens mais limpos
auxnomesitens <- colnames(mYc)
nomesitens    <- substring(auxnomesitens, 5)  # remove "rpa_"
n   <- nrow(mYc)   # nº indivíduos
nI  <- ncol(mYc)   # nº itens
opcoes <- c("-", "*", "A", "B", "C", "D", "E") # inclui branco "-" e nulo "*"
num_alter <- 5      # nº máximo de alternativas válidas (A–E)



# =======================================================
# 🧮 3) ESCORES TCT (ESCORE BRUTO) E RELATÓRIO RESUMO
# =======================================================

# 3.1) Escores brutos (soma de acertos por pessoa)
vescore <- calculaEscore(mYc, mV)$vescoreb  # (função autoral)
TabEscores <- data.frame(matricula = base$matricula, Escores = vescore)
openxlsx::write.xlsx(TabEscores, here(folder,'EscoresBrutos_Aluno.xlsx'), rowNames = FALSE)

# 3.2) Resumo descritivo dos escores + figuras (histograma e boxplot)
pdf(here(folder,"histbpescores.pdf"))
par(mfrow = c(1, 2))
hist(vescore, prob = TRUE, xlab = "escore", ylab = "densidade", main = "", cex = 1.2, cex.lab = 1.2)
boxplot(vescore, ylab = "escore", cex = 1.2, cex.lab = 1.2, cex.main = 1.2)
dev.off()

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
  e1071::kurtosis(vescore, na.rm = TRUE)+3
)
names(med_resu) <- c("media","dp","cv(%)","min.","1o Q","med.","3oQ","max.","ca","curt.")

openxlsx::write.xlsx(data.frame(t(med_resu)), here(folder, "ResumoEscores.xlsx"), rowNames = TRUE)


# =======================================================
# 📊 4) TCT: GRAUS DE DIFICULDADE, DISCRIMINAÇÃO E CORRELAÇÕES
# =======================================================
# (Gráficos solicitados para TCT)

# 4.1) Dificuldade (proporção de acertos) e Discriminação (por item)
resitemana <- itemana(mYc, mV, vescore)  # (função autoral)
vdific <- resitemana[,1]   # proporção de acertos (índice de dificuldade)
vdisc  <- resitemana[,4]   # índice de discriminação

pdf(here(folder,"dificdiscitems.pdf"))
par(mfrow = c(1, 2))

# (a) Dificuldade (proporção de acertos por item)
plot(vdific, pch = 19, xlab = "", ylab = "dificuldade",
     cex = 1.1, xaxt = "n")
axis(1, 1:nI, labels = nomesitens, las = 2, cex.axis = 0.7)
abline(h = 0.5, col = "green", lwd = 2, lty = 2)

# (b) Discriminação por item
plot(vdisc, pch = 19, xlab = "", ylab = "discriminacao",
     cex = 1.1, xaxt = "n")
axis(1, 1:nI, labels = nomesitens, las = 2, cex.axis = 0.7)
abline(h = 0.2, col = "green", lwd = 2, lty = 2)
abline(h = 0.0, col = "red",   lwd = 2, lty = 2)
dev.off()

# 4.2) Correlações ponto-bisserial (pBis) e bisserial (cBis)
#     Usamos a versão com NA para itens não apresentados (mYcNA).
vcpBis <- cpBis(mYcNA, mV, vgabr, dealNA = "exclude", dichot = TRUE)$output$pBis
vcBis  <- cBis(vcpBis, vdific, vgabr)$output$cBis

MedidasTCM <- data.frame(
  Item = nomesitens,
  Dificuldade = vdific,
  Discriminacao = vdisc,
  CBisserial = vcBis,
  CPBisserial = vcpBis
)
openxlsx::write.xlsx(MedidasTCM, here(folder, "MedidasTCM_Item.xlsx"), rowNames = FALSE)

pdf(here(folder,"corpbisbisitems.pdf"))
par(mfrow = c(1, 2))

# (c) Correlação ponto-bisserial
plot(vcpBis, pch = 19, xlab = "", ylab = "correlacao ponto-bisserial", cex = 1.1, xaxt = "n")
axis(1, 1:nI, labels = nomesitens, las = 2, cex.axis = 0.7)
abline(h = 0.2, col = "green", lwd = 2, lty = 2)
abline(h = 0.0, col = "red",   lwd = 2, lty = 2)

# (d) Correlação bisserial
plot(vcBis, pch = 19, xlab = "", ylab = "correlacao bisserial", cex = 1.1, xaxt = "n")
axis(1, 1:nI, labels = nomesitens, las = 2, cex.axis = 0.7)
abline(h = 0.2, col = "green", lwd = 2, lty = 2)
abline(h = 0.0, col = "red",   lwd = 2, lty = 2)
dev.off()


# =============================================================
# 🧩 5) TCT: GRUPOS DE ESCORE (G1–G5) E GRÁFICOS ASSOCIADOS
# =============================================================
# (i) gráfico do percentual de acertos por faixa de escores
# (ii) gráfico do percentual de escolhas por alternativa × faixas de escore

# 5.1) Definição das faixas de escore (quintis por decis pares)
#     • G1: 0º–20º percentil
#     • G2: 20º–40º
#     • G3: 40º–60º
#     • G4: 60º–80º
#     • G5: 80º–100º
# Observação: usamos 'include.lowest = TRUE' e 'unique()' p/ evitar cortes duplicados.
breaks_5 <- unique(quantile(vescore, probs = seq(0, 1, by = 0.2), na.rm = TRUE))
if (length(breaks_5) < 6) {
  # Em caso de empates extremos, forçamos pequenos deltas
  eps <- 1e-8
  breaks_5 <- seq(min(vescore, na.rm = TRUE) - eps,
                  max(vescore, na.rm = TRUE) + eps,
                  length.out = 6)
}
grupo <- cut(
  vescore, breaks = breaks_5,
  labels = c("G1","G2","G3","G4","G5"),
  include.lowest = TRUE, right = TRUE
)

# 5.2) Percentual de acertos por grupo (para cada item)
Respostas_grupo <- data.frame(mYc, grupo)
mV_grupo        <- data.frame(mV,  grupo)
# Itens não apresentados em cada pessoa → NA para não distorcer médias
Respostas_grupo[mV_grupo == 0] <- NA
Respostas_grupo$grupo          <- factor(Respostas_grupo$grupo, levels = c("G1","G2","G3","G4","G5"))

proporcao_acertos <- Respostas_grupo %>%
  dplyr::group_by(grupo) %>%
  dplyr::summarise(dplyr::across(everything(), ~ mean(.x, na.rm = TRUE)))
proporcao_acertos <- as.data.frame(t(proporcao_acertos[,-1]))
colnames(proporcao_acertos) <- c("G1","G2","G3","G4","G5")
rownames(proporcao_acertos) <- nomesitens

openxlsx::write.xlsx(proporcao_acertos, here(folder,'PropAcertosGrupo.xlsx'), rowNames = TRUE)

# garanta que as colunas sejam numéricas
proporcao_acertos[] <- lapply(proporcao_acertos, as.numeric)

# transforme em matriz (linhas = itens, colunas = G1..G5)
prop_mat <- as.matrix(proporcao_acertos)


# 5.3) Gráfico (PDF) do percentual de acertos por faixa (um gráfico por item)
pdf(here(folder,'graficos_proporcao_acertos.pdf'))
for (i in 1:nI) {
  barplot(prop_mat[i,],
          names.arg = c('G1','G2','G3','G4','G5'),
          main = nomesitens[i],
          col = "lightblue",
          ylim = c(0, 1),
          ylab = "Proporção de Acertos",
          xlab = "Grupo (faixas de escore)")
}
dev.off()

cat("Arquivo PDF salvo: graficos_proporcao_acertos.pdf\n")

# 5.4) Percentual de escolhas por alternativa × grupo
#     Constrói matriz com percentuais das categorias: "-", "*", A, B, C, D, E
mY_NA <- mY
mY_NA[mY == 9] <- NA      # usa NA para itens não apresentados

m_prop_escolha <- matrix(0, 5 * nI, length(opcoes))
for (i in 1:nI) {
  # grupos já definidos pelos mesmos quintis
  for (j in 1:5) {
    idx_g <- which(grupo == levels(grupo)[j])
    aux1  <- table(factor(mY_NA[idx_g, i], levels = opcoes))   # contagem por alternativa
    aux2  <- sum(mV[idx_g, i], na.rm = TRUE)                   # nº de apresentados no grupo
    if (aux2 > 0) {
      m_prop_escolha[(i - 1) * 5 + j, ] <- 100 * as.numeric(aux1) / aux2
    } else {
      m_prop_escolha[(i - 1) * 5 + j, ] <- 0
    }
  }
}
m_prop_escolhaDF <- data.frame(
  Item     = rep(nomesitens, each = 5),
  Grupo    = rep(paste0("G", 1:5), nI),
  m_prop_escolha,
  gabarito = rep(vgabr, each = 5)
)
colnames(m_prop_escolhaDF) <- c("Item","Grupo", opcoes, "gabarito")
openxlsx::write.xlsx(m_prop_escolhaDF, here(folder,'PropEscolhaAlternativaGrupo.xlsx'), rowNames = FALSE)

# 5.5) Gráficos (PDF) do percentual de escolhas por alternativa × grupo (um por item)
pdf(here(folder,"graficos_percentuais_escolha.pdf"))
itens_unicos <- unique(m_prop_escolhaDF$Item)
for (item in itens_unicos) {
  dados_item <- subset(m_prop_escolhaDF, Item == item)
  dados_long <- reshape2::melt(
    dados_item,
    id.vars = c("Item","Grupo","gabarito"),
    variable.name = "Alternativa", value.name = "Percentual"
  )
  # Traço por alternativa; rótulos com código da alternativa
  p <- ggplot(dados_long, aes(x = Grupo, y = Percentual, color = Alternativa, group = Alternativa)) +
    geom_line(linewidth = 1.2) +
    geom_text(aes(label = Alternativa), vjust = -0.5, size = 3) +
    labs(
      title = paste("Percentual de Escolha -", item, "- Gabarito:", unique(dados_item$gabarito)),
      x = "Grupo (faixas de escore)", y = "Percentual de Escolha"
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
cat("Arquivo PDF salvo: graficos_percentuais_escolha.pdf\n")


# =======================================================
# 🅰️ 6) TCT POR ALTERNATIVA (incluindo Branco/Nulo)
# =======================================================
# Dificuldade e discriminação por alternativa (considerando "-" e "*" como categorias)
# Aqui usamos mitemanaltNR() para obter medidas por alternativa.

mY_aux <- mY
mY_aux[mY_aux == 9] <- NA     # garante NA para não apresentados
colnames(mY_aux) <- nomesitens

mitemanaltNR <- itemanaltNR(
  mY_aux, mV, vescore, vgabr,
  nalt = length(opcoes), opcoes = opcoes
)

# Zera → NA (para categorias inexistentes em certos itens)
for (k in 1:length(opcoes)) {
  mitemanaltNR$mresult[,,k][mitemanaltNR$mresult[,,k] == 0] <- NA
}
mitemanaltNR$mDific[mitemanaltNR$mDific == 0] <- NA
mitemanaltNR$mDisc [mitemanaltNR$mDisc  == 0] <- NA

# Dificuldade (proporção de escolha) por alternativa (%)
mDificNR <- mitemanaltNR$mDific
mDificNR[, 1:length(opcoes)] <- round(100 * mDificNR[, 1:length(opcoes)], 3)
mDificNRDF <- data.frame(mDificNR, num_alter)
colnames(mDificNRDF) <- c('Branco','Rasura','A','B','C','D','E','Gabarito','n_alt')
openxlsx::write.xlsx(mDificNRDF, here(folder,"mDificNRDF.xlsx"), dec='.', rowNames = TRUE)

# Discriminação por alternativa
mDiscNR <- mitemanaltNR$mDisc
mDiscNRDF <- data.frame(round(mDiscNR[, -(length(opcoes)+1)], 3), Key = mDiscNR$Key, num_alter)
colnames(mDiscNRDF) <- c('Branco','Rasura','A','B','C','D','E','Gabarito','n_alt')
openxlsx::write.xlsx(mDiscNRDF, here(folder,"mDiscNRDF.xlsx"), dec='.', rowNames = TRUE)

# Correlações ponto-bisserial e bisserial por alternativa
mcpBisNR <- cpBisNR(mY_aux, mV, vgabr, dealNA = "exclude",opcoes, nalt = length(opcoes))$output
mcpBisNRDF <- data.frame(mcpBisNR[, -(length(opcoes)+1)], key = mcpBisNR[, length(opcoes)+1], num_alter)
colnames(mcpBisNRDF) <- c('Branco','Rasura','A','B','C','D','E','Gabarito','n_alt')
openxlsx::write.xlsx(mcpBisNRDF, here(folder,"mcpBisNRDF.xlsx"), dec='.', rowNames = TRUE)

mDificNRaux  <- mDificNR[, -(length(opcoes)+1)]/100   # converte %→proporção
mcBisNR      <- cBisNR(mcpBisNRDF[,1:7], mDificNRaux, vgabr)$output
mcBisNRDF    <- data.frame(mcBisNR[, -(length(opcoes)+1)], key = mcBisNR$answerkey, num_alter)
colnames(mcBisNRDF) <- c('Branco','Rasura','A','B','C','D','E','Gabarito','n_alt')
openxlsx::write.xlsx(mcBisNRDF, here(folder,"mcBisNRDF.xlsx"), dec='.', rowNames = TRUE)

# (Opcional) Versões para LaTeX via xtable (apenas para inspeção rápida)
# print(xtable(mDificNRDF, digits = 3))
# print(xtable(mDiscNRDF , digits = 3))
# print(xtable(mcpBisNRDF, digits = 3))
# print(xtable(mcBisNRDF , digits = 3))


# =======================================================
# ✅ 7) CHECKLIST DO QUE O SCRIPT ENTREGA (TCT)
# =======================================================
# 1) Gabarito do item (vgabr) salvo em planilhas que incluem a coluna "Gabarito".
# 2) Índice de dificuldade (proporção de acertos por item): arquivo "MedidasTCM_Item.xlsx"
#    + gráfico "dificdiscitems.pdf" (painel com dificuldade e discriminação).
# 3) Índice de discriminação (por item): mesmo arquivo/gráfico acima.
# 4) Proporção de escolhas por alternativa, incluindo Branco ("-") e Nulo ("*"):
#    • "mDificNRDF.xlsx" (percentuais por alternativa)
#    • "mDiscNRDF.xlsx"  (discriminação por alternativa)
#    • "mcpBisNRDF.xlsx" (ponto-bisserial por alternativa)
#    • "mcBisNRDF.xlsx"  (bisserial por alternativa)
# 5) Gráfico do percentual de acertos por faixas de escore (G1–G5):
#    • "graficos_proporcao_acertos.pdf" (um gráfico por item)
#    • "PropAcertosGrupo.xlsx" (tabela com G1–G5 por item)
# 6) Gráfico do percentual de escolhas (por alternativa × faixas de escore):
#    • "graficos_percentuais_escolha.pdf" (um gráfico por item, linhas por alternativa)
# 7) Histogramas/boxplot e resumo descritivo dos escores:
#    • "histbpescores.pdf" e "ResumoEscores.xlsx"
