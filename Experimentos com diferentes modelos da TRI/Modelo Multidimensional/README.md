# Modelo multidimensional 3PL — guia de execução

## O que o script faz

`SimMUGLP3.R` realiza uma simulação com **2.000 pessoas, 30 itens binários e duas dimensões latentes**, correlacionadas em **0,8**. Ajusta um modelo logístico multidimensional de três parâmetros e compara valores verdadeiros e estimativas. É uma única simulação, não um estudo com várias replicações.

A entrada é `modelo 3P Multidimensional.ods`, primeira aba:

| Coluna | Significado |
| --- | --- |
| `item` | Identificador do item. A ordem das linhas determina a posição no modelo. |
| `a1`, `a2` | Discriminações nas dimensões F1 e F2. |
| `b` | Negativo do intercepto: o código utiliza `d = -b`. |
| `c` | Assíntota inferior da probabilidade de acerto, usualmente chamada de acerto casual. |

**Atenção à parametrização:** a coluna `b` não é usada como a dificuldade unidimensional `-d/a`, nem como a dificuldade multidimensional `-d/sqrt(a1²+a2²)`. O script preserva a convenção original e identifica `d` como **intercepto** nas saídas.

A probabilidade simulada é:

`P(Y=1 | theta1, theta2) = c + (1-c) × plogis(a1×theta1 + a2×theta2 + d)`

`plogis(x) = 1/(1+exp(-x))`. O modelo é compensatório: uma contribuição maior de uma dimensão pode compensar uma contribuição menor da outra.

### Etapas

1. Localiza a entrada e valida colunas, identificadores e parâmetros. Descarta apenas linhas inteiramente vazias.
2. Gera duas colunas de valores normais, centraliza e transforma sua covariância para a matriz com variâncias 1 e correlação 0,8. Assim, a média e a covariância **amostrais** obedecem a esses valores, como pretendido no original. Essa transformação impõe restrições à amostra; os valores finais não são sorteios independentes sem ajuste. A semente 4142, antes comentada, foi ativada.
3. Simula respostas 0/1, sem ausências. O escore total varia de 0 a 30.
4. Ajusta por EM um modelo com F1 em todos os itens e F2 do segundo ao último. Mantém as prioris normais nas cargas, no intercepto e no logit de `c`, além do limite inferior -0,5 nas cargas livres. As prioris tornam o ajuste uma estimação penalizada, ou de máximo a posteriori.
5. Extrai `a1`, `a2`, `d`, `c`, seus erros-padrão e intervalos de 95%; estima também a correlação entre dimensões. Os nomes dos parâmetros substituem as posições fixas de colunas.
6. Estima os traços latentes por EAP (média posterior), preservando os erros-padrão das duas dimensões.
7. Compara valores verdadeiros e estimados por histogramas, boxplots, dispersões, QQ plots e intervalos.
8. Calcula S_X2, infit/outfit, distâncias entre proporções de pares de itens e diagnósticos dos respondentes.

### Estrutura de estimação preservada

Somente `a2` do **primeiro item** é fixado em zero. Os demais zeros em `a1`/`a2` na planilha são valores verdadeiros de simulação e continuam livres na estimação. As variâncias de F1 e F2 são fixadas em 1, suas médias em zero e a correlação é estimada.

Essa estrutura permite muitas cargas cruzadas e exige cuidado com a orientação e identificação dos fatores. As prioris regularizam o ajuste; convergência numérica, isoladamente, não demonstra identificação pela verossimilhança. Não foram acrescentadas novas âncoras ou restrições silenciosamente. Para uma análise substantiva, convém definir a estrutura confirmatória das dimensões antes de interpretar cargas e correlação como únicas.

Espera-se recuperação aproximada dos valores simulados, sujeita à amostragem, às prioris e à estrutura do ajuste. EAP tende a aproximar valores extremos do centro. Os erros-padrão dos escores condicionam nos parâmetros dos itens estimados; uma simulação não estabelece viés ou cobertura geral.

## Como executar

Instale as dependências, se necessário:

```r
install.packages(c("mirt", "readODS"))
```

Execute o arquivo inteiro no console do R:

```r
source("C:/Users/Usuário/OneDrive/Documentos/GitHub/CEnPE/Experimentos com diferentes modelos da TRI/Modelo Multidimensional/SimMUGLP3.R", encoding = "UTF-8")
```

Também aceita execução via `Rscript`. A entrada é procurada junto ao script, e a pasta de saída `resultados/Multidimensional` é criada automaticamente no mesmo diretório. Não é necessário usar `setwd()`. Há tratamento local de UTF-8 para caminhos com acentos no Windows.

No início do script, podem ser alterados `n`, `rho`, `semente`, `arquivo_entrada` e `pasta_saida`. O experimento é especificamente bidimensional; não basta alterar `D` para generalizá-lo. O número de itens é lido da entrada. Uma nova execução substitui arquivos com o mesmo nome; saídas antigas não são apagadas automaticamente.

## Saídas esperadas após execução completa

### 16 PDFs

| Arquivo | Conteúdo |
| --- | --- |
| `TracosLatentesV.pdf` | Histogramas por dimensão, boxplots e dispersão F1/F2 dos valores simulados; três páginas. |
| `TracosLatentesVEscoresO.pdf` | Escore total versus traço verdadeiro em cada dimensão. |
| `ParPopEIC.pdf` | Correlação estimada e IC de 95%, com referência verdadeira em 0,8. |
| `ParItensEDisp.pdf` | Verdadeiro versus estimado para `a1`, `a2`, intercepto `d` e `c`. |
| `ParItensEIC.pdf` | Estimativas e ICs de 95%; triângulos vermelhos indicam valores verdadeiros. O parâmetro fixo não recebe IC. |
| `TracosLatentesEHist.pdf` | Histogramas verdadeiros/EAP com classes e escala vertical comuns em cada painel. |
| `TracosLatentesEBP.pdf` | Boxplots verdadeiros/EAP por dimensão. |
| `TracosLatentesEDisp.pdf` | Dispersão verdadeiro/EAP por dimensão. |
| `TracosLatentesEQQplot.pdf` | QQ normal dos escores EAP padronizados. |
| `S_X2item.pdf` | RMSEA associado a S_X2 por item; não apresenta p-valores. |
| `infititem.pdf`, `outfititem.pdf` | Diagnósticos de ajuste dos itens. |
| `RQEQMIL.pdf` | Distâncias entre proporções observadas e esperadas para os 435 pares de itens. |
| `outfitindiv.pdf`, `infitindiv.pdf`, `zhindiv.pdf` | Diagnósticos dos 2.000 respondentes. |

Os intervalos das cargas e do intercepto são aproximações de Wald. Para `c`, os limites são calculados no logit e convertidos para probabilidades; para a correlação, usa-se a transformação de Fisher com erro-padrão pelo método delta, evitando limites fora de (-1,1). Não são intervalos de perfil nem de uma amostra posterior completa.

A fórmula de `RQEQMIL.pdf` é `sqrt(sum((p_observada - p_esperada)^2))`: uma distância euclidiana entre proporções, e não o RMSEA convencional. A expectativa já incorpora as duas dimensões. Linhas de referência dos diagnósticos são guias visuais, não critérios universais de aceitação.

### 8 CSVs

| Arquivo | Conteúdo |
| --- | --- |
| `respostas_simuladas.csv` | 2.000 linhas; identificador e respostas aos 30 itens. |
| `parametros_itens.csv` | 120 linhas: quatro parâmetros por item, indicação de parâmetro fixo, verdadeiro, estimativa, erro-padrão e IC. `a2` do primeiro item tem EP e IC ausentes porque é fixo. |
| `correlacao_dimensoes.csv` | Correlação verdadeira, amostral simulada, estimativa, erro-padrão e IC. |
| `tracos_latentes.csv` | 2.000 linhas; escore total, valores verdadeiros, EAP e erros-padrão das duas dimensões. |
| `ajuste_itens_S_X2.csv` | Estatística, graus de liberdade, RMSEA e p-valores. |
| `ajuste_itens_infit_outfit.csv` | Infit/outfit e versões padronizadas por item. |
| `independencia_local.csv` | 435 pares de itens identificados e suas distâncias. |
| `ajuste_individuos.csv` | Infit, outfit, versões padronizadas e Zh por respondente. |

### 2 RDSs e 2 TXTs

- `modelo_Multidimensional.rds`: objeto ajustado, salvo antes da checagem de convergência e extração dos resultados.
- `resultado_Multidimensional.rds`: lista com modelo, entrada, respostas, parâmetros, traços, correlação e configuração. Ao final, inclui os diagnósticos. Abrir com `readRDS()`.
- `especificacao_modelo.txt`: sintaxe efetivamente utilizada.
- `sessionInfo.txt`: versões do R e dos pacotes para rastreabilidade.

Respostas e resultados centrais são salvos antes dos diagnósticos. Uma falha pode deixar saídas parciais; a mensagem final `Analise concluida` confirma que todas as etapas terminaram.

## Alterações e validação

- Oito seções comentadas, caminhos portáveis e criação da pasta de saída.
- Removidos caminhos antigos, limpeza do ambiente e dependências sem uso. Substituídas funções auxiliares externas por extração nomeada do `mirt` e gráficos do R base.
- Simulação da covariância exata feita com R base, eliminando a dependência de `mvtnorm`.
- Preservadas a parametrização `d = -b` e a estrutura original de estimação, agora explícitas.
- Ativada a semente; extração por nomes; preservados erros-padrão dos escores; parâmetro fixo identificado separadamente.
- Acrescentados CSVs, RDSs e registros de configuração; fechamento protegido dos PDFs.

Sintaxe, localização dos arquivos, validação da entrada e geração de traços com média/covariância especificadas foram verificadas no R 4.5.0. A execução estatística completa não foi realizada: `mirt` e `readODS` não estão carregáveis nas bibliotecas examinadas neste terminal. Portanto, convergência, erros-padrão do ajuste e gráficos finais ainda precisam de verificação na sua sessão R com os pacotes disponíveis. Nenhuma saída numérica do ajuste foi gerada nesta revisão.

Referências de API: [especificação de modelos](https://philchalmers.github.io/mirt/docs/reference/mirt.model.html) e [extração de coeficientes](https://philchalmers.github.io/mirt/docs/reference/coef-method.html). A conversão automática `IRTpars=TRUE` não é apropriada para transformar uniformemente os itens com duas cargas; por isso o script reporta diretamente o intercepto.
