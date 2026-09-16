# Scripts originais recuperados

Recuperação realizada em 16/09/2026, em pasta separada das versões reorganizadas.

## Conteúdo

| Modelo | Script original | Entrada |
| --- | --- | --- |
| Resposta gradual | `Modelo de resposta gradual/SimUUMRG.R` | `Modelo de resposta gradual/modelo RG.ods` |
| Testlet | `Modelo Testlet/Simulando Testlet.R` | `Modelo Testlet/Item_Parameters.ods` |
| Multidimensional | `Modelo Multidimensional/SimMUGLP3.R` | `Modelo Multidimensional/modelo 3P Multidimensional.ods` |

## Origem e verificação

Os scripts foram recuperados do conteúdo completo exibido nas leituras anteriores às edições, registrado na conversa original. Os primeiros commits disponíveis de resposta gradual e Testlet já contêm versões reorganizadas; o script multidimensional ainda não estava versionado.

A recuperação preserva o código, comentários, caminhos antigos e quebras de linha CRLF. Os arquivos foram gravados em UTF-8 sem BOM, e os tamanhos conferem com os registrados antes das alterações: 15.214 bytes (resposta gradual), 10.148 bytes (Testlet) e 16.747 bytes (multidimensional). Não há hash anterior à edição para comprovar identidade binária com os scripts originais; a origem desta recuperação é o registro textual das leituras.

As três planilhas são cópias dos arquivos de entrada disponíveis nas respectivas pastas. Elas não foram editadas durante a organização dos scripts. Os hashes SHA-256 das cópias foram comparados aos arquivos de origem e são idênticos.

`manifesto.csv` registra tamanho, SHA-256 e origem dos sete arquivos recuperados ou copiados, incluindo o auxiliar.

## Arquivo auxiliar

`Auxiliares/Aux Func IRT.R` é uma cópia da versão disponível em `Piaui/Aux Func IRT.R`. Foi incluído porque os scripts originais fazem referência a esse nome. Não foi possível comprovar que essa versão seja idêntica à que existia no computador do autor dos scripts.

## Execução

Esta pasta preserva as versões anteriores à organização. Os caminhos absolutos antigos, inclusive a referência ao arquivo auxiliar, foram mantidos. Para executar esses originais, é necessário adaptar os caminhos de entrada, saída e `source()` do auxiliar, além de instalar as dependências. Os scripts originais também contêm limpeza do ambiente com `rm(list = ls(...))`.

Nenhum dos scripts recuperados foi executado durante a recuperação. As versões reorganizadas e seus resultados permaneceram nas pastas existentes.
