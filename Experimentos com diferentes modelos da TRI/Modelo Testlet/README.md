# Modelo Testlet — guia de execução

## O que está sendo feito

O script `Simulando Testlet.R` realiza uma simulação de um modelo logístico de três parâmetros (3PL) com um efeito adicional compartilhado por um grupo de itens, chamado **testlet**. Pense em quatro questões baseadas no mesmo texto: além da habilidade geral, uma facilidade ou dificuldade específica com aquele texto pode afetar as quatro respostas.

A entrada `Item_Parameters.ods`, primeira aba, contém **20 itens**:

| Coluna | Significado |
| --- | --- |
| `a` | Discriminação do item; deve ser positiva. |
| `b` | Dificuldade do item na escala da habilidade, quando o efeito testlet é zero. |
| `c` | Assíntota inferior da probabilidade de acerto, usualmente chamada de acerto casual. |
| `testlet` | 1 nos itens 4, 5, 6 e 7; 0 nos demais. |

As linhas da planilha identificam os itens, na ordem `Item_1` a `Item_20`. Linhas inteiramente vazias são descartadas. Esta versão trata **um único testlet**; a coluna não aceita códigos de múltiplos testlets.

### Etapas

1. Lê e valida a planilha, incluindo parâmetros numéricos e a estrutura do testlet.
2. Simula **3.000 pessoas**, usando a semente **4445**. A habilidade geral `theta` segue uma normal com média 0 e variância 1. O efeito `tau` segue uma normal com média 0 e variância **0,8**; as duas variáveis são independentes na população.
3. Simula respostas binárias, com a seguinte probabilidade de acerto:

   `P(Y = 1 | theta, tau) = c + (1-c) × plogis(a × (theta + testlet × tau - b))`

   `plogis(x) = 1/(1+exp(-x))`. Nos itens fora do testlet, `tau` não entra na probabilidade. O escore total vai de 0 a 20. Não há respostas ausentes neste experimento.
4. Ajusta o modelo com `mirt::bfactor()`. Os itens do testlet têm cargas iguais na habilidade geral e no efeito específico (`a1 = a2`); a variância do efeito específico é estimada. A variância da habilidade geral é fixada em 1, e a covariância entre os dois fatores é fixada em 0. Essa parametrização segue o exemplo de modelo testlet da [documentação de bfactor](https://philchalmers.github.io/mirt/docs/reference/bfactor.html).
5. Preserva as prioris do script original: lognormal nas cargas, normal no intercepto e normal no logit da assíntota inferior. Portanto, a estimação usa penalização pelas prioris. Os valores `-0.2058759` e `0.6` da lognormal são os parâmetros da distribuição na escala log.
6. Extrai discriminação, dificuldade, acerto casual e variância do testlet, com erros-padrão e intervalos de 95%. A dificuldade é calculada por `b = -d/a1`, incluindo a covariância entre `a1` e `d` em seu erro-padrão.
7. Estima `theta` e `tau` por EAP, a média posterior, e compara estimativas e valores simulados.
8. Produz curvas condicionais, infit/outfit, S_X2 e comparações entre frequências observadas e esperadas.

Espera-se recuperação aproximada dos parâmetros. O efeito individual do testlet pode ser estimado com menor precisão por depender de apenas quatro itens; os escores EAP tendem a aproximar valores extremos da média. Uma única simulação não determina viés ou cobertura geral do estimador.

## Caminhos e comando de execução

A entrada é localizada junto ao script. As saídas vão para `resultados/Testlet`, dentro desta pasta, criada automaticamente. Não é necessário executar `setwd()` nem alterar caminhos de outro computador. Para mudar o experimento, edite `n`, `sigma2tau`, `semente`, `arquivo_entrada` e `pasta_saida` no início do script. O número de itens e as posições do testlet são lidos da planilha.

Instale os pacotes uma vez, se necessário:

```r
install.packages(c("mirt", "readODS"))
```

Execute o script completo no console do R:

```r
source("C:/Users/Usuário/OneDrive/Documentos/GitHub/CEnPE/Experimentos com diferentes modelos da TRI/Modelo Testlet/Simulando Testlet.R", encoding = "UTF-8")
```

Também aceita `Rscript` com o caminho completo do arquivo. A semente permite reproduzir o experimento na mesma configuração de R e pacotes; resultados podem variar entre versões. Nova execução substitui arquivos de mesmo nome. Arquivos antigos não são apagados automaticamente.

## Saídas esperadas

### 10 PDFs

| Arquivo | Conteúdo |
| --- | --- |
| `ParItens.pdf` | Valores verdadeiros versus estimados de `a`, `b` e `c`; diagonal indica igualdade. |
| `ParItensEIC.pdf` | Estimativas e ICs de 95% por item; triângulos vermelhos mostram os valores verdadeiros. |
| `TLEAT.pdf` | Dispersões e boxplots de habilidade geral e efeito testlet, simulados e estimados por EAP. |
| `VarEAT.pdf` | Variância verdadeira de 0,8 versus estimativa e seu IC de 95%. |
| `CCI.pdf` | 20 páginas, uma por item: probabilidade de acerto em função de theta para tau igual a menos um desvio-padrão, zero e mais um desvio-padrão estimados. As curvas coincidem fora do testlet. |
| `Infit.pdf`, `Outfit.pdf` | Diagnósticos por item, calculados usando as duas dimensões estimadas. |
| `RQEQMIL.pdf` | Distância entre proporções observadas e esperadas para os 190 pares de itens. |
| `S_X2.pdf` | P-valores do teste S_X2 por item, sem correção por multiplicidade. |
| `ProObsEsp.pdf` | 20 páginas: proporções observadas versus esperadas nas células das tabelas de S_X2. |

Os ICs de `a` e `b` são de Wald, simétricos na escala do parâmetro. O IC de `c` é construído no logit e transformado para probabilidade; o da variância é aproximado na escala log e transformado de volta, evitando limites negativos. São aproximações locais, não intervalos de perfil ou de uma amostra posterior completa.

`CCI.pdf` mostra curvas **condicionais** em valores de tau; não integra sobre a distribuição do efeito testlet. Em `ProObsEsp.pdf`, cada frequência é dividida pelo total de respondentes, preservando a definição do original; não é uma proporção condicionada ao escore de cada faixa.

A medida em `RQEQMIL.pdf` é `sqrt(sum((p_observada - p_esperada)^2))`. É uma distância euclidiana entre proporções, não o RMSEA convencional. As frequências esperadas já incorporam o efeito testlet. As linhas de referência dos gráficos são guias visuais, não critérios universais de aceitação.

### 8 CSVs

| Arquivo | Conteúdo |
| --- | --- |
| `respostas_simuladas.csv` | 3.000 linhas; identificador e respostas aos 20 itens. |
| `parametros_itens.csv` | 60 linhas: item, participação no testlet, parâmetro, verdadeiro, estimativa, erro-padrão e IC de 95%. |
| `variancia_testlet.csv` | Variância verdadeira, estimada, erro-padrão e IC de 95%. |
| `tracos_latentes.csv` | 3.000 linhas; escore, theta e tau verdadeiros, estimativas EAP e erros-padrão. |
| `ajuste_itens_infit_outfit.csv` | Infit/outfit e suas versões padronizadas, por item. |
| `ajuste_itens_S_X2.csv` | Estatística, graus de liberdade, RMSEA e p-valor por item. |
| `independencia_local.csv` | 190 pares identificados e suas distâncias. |
| `proporcoes_observadas_esperadas.csv` | Frequências e proporções das células das tabelas S_X2, por item; número de linhas depende do agrupamento de células. |

### 2 RDSs e 2 TXTs

- `modelo_Testlet.rds`: objeto ajustado, salvo antes da extração de resultados, inclusive se a checagem posterior de convergência falhar.
- `resultado_Testlet.rds`: lista com modelo, entrada, respostas, parâmetros, traços e configuração. Ao terminar, inclui também os diagnósticos e as tabelas de S_X2. Abra com `readRDS()`.
- `sessionInfo.txt`: versões do R e dos pacotes.
- `especificacao_modelo.txt`: sintaxe efetivamente usada no ajuste.

O script salva resultados centrais antes dos diagnósticos. Se houver falha posterior, a pasta poderá conter saídas parciais. A mensagem `Analise concluida` no console indica que todas as etapas terminaram.

## O que foi corrigido e organizado

- Caminhos portáveis e criação automática da pasta de saída.
- Oito seções comentadas; retirada da limpeza do ambiente, de exemplos desativados e de dependências sem uso.
- Removida a dependência externa de `Aux Func IRT.R`; cálculo de erro-padrão de `b` implementado localmente pelo método delta.
- Número de itens e posições do testlet derivados da planilha, substituindo as posições fixas 1–20 e 4–7.
- Extração por nomes de parâmetros; associação da matriz de covariâncias aos números dos parâmetros, incluindo cargas ligadas por igualdade.
- Transformação explícita do logit de `g` em probabilidade `c`. A [documentação de coef](https://philchalmers.github.io/mirt/docs/reference/coef-method.html) descreve a saída dos coeficientes; a conversão automática para parâmetros tradicionais não se aplica diretamente aos itens com duas cargas.
- Variância do testlet extraída por `COV_22`; intervalos de `c` e da variância construídos em escalas que respeitam seus domínios.
- Fechamento protegido dos PDFs; curvas condicionais rotuladas; S_X2 identificado explicitamente como p-valor.
- Acrescentadas tabelas e objetos reutilizáveis, além dos PDFs do original. As tabelas de [itemfit](https://philchalmers.github.io/mirt/docs/reference/itemfit.html) preservam as estatísticas e os p-valores para inspeção.

## Validação nesta revisão

A sintaxe, a localização da entrada e funções de validação/transformação foram verificadas com R 4.5.0. A execução numérica completa está pendente: `mirt` e `readODS` não estão instalados na biblioteca do R encontrado. Portanto, ainda não foram verificados convergência, resultados numéricos e renderização dos gráficos. Nenhuma saída numérica foi gerada nesta revisão.
