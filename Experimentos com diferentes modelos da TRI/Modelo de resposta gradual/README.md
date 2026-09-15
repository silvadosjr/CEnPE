# Modelo de resposta gradual — guia de execução

## O que o script faz

`SimUUMRG.R` executa uma simulação de um único grupo com uma dimensão latente. Não analisa respostas reais e não faz várias replicações de Monte Carlo.

1. Lê `modelo RG.ods`, primeira aba: 30 itens, com discriminação `a` e quatro limiares `b1` a `b4` por item. Linhas completamente vazias são descartadas.
2. Gera 1.000 traços latentes normais e os padroniza. A semente é 4142.
3. Simula respostas nas categorias 0 a 4 pelo MRG, usando os parâmetros da planilha. O escore total vai de 0 a 120. A máscara de observação está inteiramente preenchida: não há respostas ausentes neste experimento.
4. Ajusta um MRG unidimensional por EM, com prior lognormal na discriminação, preservada do original. Estima parâmetros, erros-padrão e intervalos de Wald de 95%.
5. Estima os traços latentes por EAP (média posterior), preservando seus erros-padrão, e compara estimativas com valores verdadeiros.
6. Calcula diagnósticos dos itens, dos respondentes e da independência local.

Espera-se que as estimativas acompanhem os valores verdadeiros, com erro amostral. O EAP tende a aproximar estimativas extremas do centro da distribuição. Uma única simulação não estabelece viés, cobertura ou desempenho geral do estimador.

## Breve descrição teórica

O **modelo de resposta gradual** (MRG), proposto por [Samejima (1969)](https://doi.org/10.1007/BF03372160), é um modelo da Teoria de Resposta ao Item para categorias ordenadas, como respostas em uma escala Likert. Nesta aplicação, cada resposta depende de um único traço latente e as respostas aos diferentes itens são consideradas independentes quando esse traço é conhecido (independência local).

Para a pessoa $i$, o item $j$ e as categorias $Y_{ij} \in \{0,1,2,3,4\}$, a versão logística utilizada define:

$$
P(Y_{ij} \geq k \mid \theta_i)
= \frac{1}{1 + \exp[-a_j(\theta_i-b_{jk})]},
\qquad k=1,\ldots,4.
$$

Aqui, $\theta_i$ é o traço latente, $a_j>0$ é a discriminação do item (valores maiores tornam a transição entre categorias mais acentuada) e $b_{j1}<\cdots<b_{j4}$ são os limiares. Quando $\theta_i=b_{jk}$, a probabilidade de responder na categoria $k$ ou em uma superior é 0,5. A probabilidade de uma categoria específica é obtida por diferença:

$$
P(Y_{ij}=k\mid\theta_i)
=P(Y_{ij}\geq k\mid\theta_i)-P(Y_{ij}\geq k+1\mid\theta_i),
$$

com $P(Y_{ij}\geq0\mid\theta_i)=1$ e $P(Y_{ij}\geq5\mid\theta_i)=0$.

No script, o pacote [`mirt` (Chalmers, 2012)](https://doi.org/10.18637/jss.v048.i06) usa interceptos $d_{jk}=-a_jb_{jk}$. O ajuste fixa a distribuição latente em $N(0,1)$ para definir a escala e utiliza EM com prior $\log(a_j)\sim N(-0{,}2058759;\,0{,}6^2)$. Portanto, os parâmetros dos itens são estimados por maximização da verossimilhança marginal acrescida da log-prior (MAP), e os traços individuais por EAP, a média posterior de $\theta_i$ condicionada às respostas e aos parâmetros estimados.

### Referências

- Samejima, F. (1969). *Estimation of latent ability using a response pattern of graded scores*. Psychometrika, 34(S1), 1–97. [https://doi.org/10.1007/BF03372160](https://doi.org/10.1007/BF03372160).
- Chalmers, R. P. (2012). *mirt: A multidimensional item response theory package for the R environment*. Journal of Statistical Software, 48(6), 1–29. [https://doi.org/10.18637/jss.v048.i06](https://doi.org/10.18637/jss.v048.i06).

## Caminhos e execução

A entrada é procurada junto ao script. As saídas são gravadas em `resultados/MRG`, dentro desta pasta, criada automaticamente. Os caminhos usam `file.path()` e não dependem do nome do usuário. Execute o arquivo inteiro para permitir a identificação de sua localização.

No console do R, instale uma vez os pacotes ausentes:

```r
install.packages(c("mirt", "readODS", "plotrix"))
```

Depois execute:

```r
source("C:/Users/Usuário/OneDrive/Documentos/GitHub/CEnPE/Experimentos com diferentes modelos da TRI/Modelo de resposta gradual/SimUUMRG.R", encoding = "UTF-8")
```

Também é possível usar `Rscript` com o caminho do arquivo. No início do script podem ser alterados `n`, `ncat`, `semente`, `arquivo_entrada` e `pasta_saida`. O número de categorias deve corresponder aos limiares disponíveis. Uma nova execução substitui arquivos de mesmo nome na pasta de saída; arquivos de execuções anteriores não são removidos automaticamente.

## Saídas esperadas após execução completa

### Gráficos: 16 arquivos PDF

| Arquivo | Conteúdo |
| --- | --- |
| `TracosLatentesV.pdf` | Histograma e boxplot dos traços verdadeiros. |
| `TracosLatentesVEscoreO.pdf` | Escore total observado versus traço verdadeiro. |
| `ParItensEDisp.pdf` | Discriminação e limiares: verdadeiro versus estimado; diagonal indica igualdade. |
| `ParItensEIC.pdf` | Estimativas e ICs de 95%; pontos vermelhos indicam valores verdadeiros. |
| `TracosLatentesE.pdf` | Histogramas, boxplots, dispersão verdadeiro/EAP e QQ normal. |
| `CCIPOE.pdf` | Curvas empíricas e previstas por categoria, com uma página por item (30 páginas). |
| `S_X2item.pdf`, `X2item.pdf`, `G2item.pdf`, `PV_Q1item.pdf` | RMSEA associado a cada estatística de ajuste dos itens. |
| `infititem.pdf`, `outfititem.pdf` | Infit e outfit por item. |
| `RQEQMIL.pdf` | Distância entre proporções observadas e previstas para os 435 pares de itens. |
| `outfitindiv.pdf`, `infitindiv.pdf`, `zhindiv.pdf` | Diagnósticos dos 1.000 respondentes. |

As linhas de referência dos diagnósticos são guias visuais, não critérios universais de aprovação. A medida em `RQEQMIL.pdf` é `sqrt(sum((p_observada - p_esperada)^2))`: o original a chamava de RMSEA, mas ela é uma distância euclidiana entre proporções, sem divisão pelo número de células.

### Tabelas e objetos: 10 CSVs, um RDS e um TXT

| Arquivo | Conteúdo esperado |
| --- | --- |
| `respostas_simuladas.csv` | 1.000 linhas; identificador e respostas aos 30 itens. |
| `parametros_itens.csv` | 150 linhas: item, parâmetro, valor verdadeiro, estimativa, erro-padrão e limites do IC. |
| `tracos_latentes.csv` | 1.000 linhas: traço verdadeiro, EAP, erro-padrão e escore total. |
| `ajuste_itens_S_X2.csv`, `ajuste_itens_X2.csv`, `ajuste_itens_G2.csv`, `ajuste_itens_PV_Q1.csv` | Estatísticas, graus de liberdade, RMSEA e p-valores por item. |
| `ajuste_itens_infit_outfit.csv` | Infit/outfit e suas versões padronizadas por item. |
| `independencia_local.csv` | 435 pares identificados e respectivas distâncias. |
| `ajuste_individuos.csv` | Infit, outfit, versões padronizadas e Zh por respondente. |
| `resultado_MRG.rds` | Lista com modelo ajustado, entrada, respostas, parâmetros, traços e semente; abrir com `readRDS()`. |
| `sessionInfo.txt` | Versões do R e dos pacotes para rastreabilidade. |

Os resultados centrais são salvos antes dos diagnósticos. Se uma etapa posterior falhar, a pasta poderá conter saídas parciais; a conclusão completa é indicada pela mensagem final no console.

## Alterações e validação

- Organizadas oito etapas e comentadas as decisões estatísticas.
- Removidos caminhos antigos, limpeza do ambiente, dependências sem uso e trechos comentados referentes a outros modelos.
- Corrigido o ajuste para usar a matriz com a máscara de ausências; o experimento padrão continua sem ausências. Para adaptar a dados incompletos, é necessário rever S_X2, que exige respostas completas.
- Número de itens calculado a partir da entrada; extrações feitas por nomes de parâmetros e índices.
- Semente fixada; verificação da entrada, categorias observadas e convergência; fechamento protegido dos PDFs.
- Acrescentadas saídas CSV/RDS e registro de versões.
- Sintaxe validada com R 4.5.0. A execução numérica completa não foi realizada: `mirt`, `readODS` e `plotrix` estão ausentes na biblioteca do R encontrado. Assim, convergência, gráficos e resultados numéricos ainda precisam ser verificados após instalar as dependências.

As chamadas e os nomes das saídas foram conferidos na documentação oficial: [coeficientes](https://philchalmers.github.io/mirt/docs/reference/coef-method.html), [ajuste de itens](https://philchalmers.github.io/mirt/docs/reference/itemfit.html), [resíduos](https://philchalmers.github.io/mirt/docs/reference/residuals-method.html) e [ajuste de pessoas](https://philchalmers.github.io/mirt/docs/reference/personfit.html).
