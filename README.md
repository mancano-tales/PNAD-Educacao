# PNAD-Educacao

> **Visualizações interativas de desigualdade educacional no Brasil**  
> Microdados da PNAD e PNAD Contínua (IBGE) · Quarto + R + Plotly

[![Publish Quarto Site](https://github.com/mancano-tales/PNAD-Educacao/actions/workflows/publish.yml/badge.svg)](https://github.com/mancano-tales/PNAD-Educacao/actions/workflows/publish.yml)

**Site público:** https://mancano-tales.github.io/PNAD-Educacao/

---

## Sobre o projeto

Este repositório contém código R e documentos Quarto (`.qmd`) para análise
e visualização da desigualdade educacional no Brasil, usando os microdados
da **PNAD** e da **PNAD Contínua** do IBGE.

O objetivo é produzir análises reproduzíveis, com rigor metodológico
(pesos amostrais, desenho amostral complexo) e respaldo na literatura
acadêmica, apresentadas como gráficos interativos com texto explicativo.

---

## Estrutura do repositório

```
PNAD-Educacao/
├── _quarto.yml                          # Configuração do site Quarto
├── index.qmd                            # Página inicial
├── R/
│   └── 01_download_pnad.R               # Download e pré-processamento PNAD
├── graficos/
│   ├── desigualdade-escolaridade.qmd    # Nível de instrução por raça/renda
│   ├── raca-educacao.qmd                # Desigualdade racial (a criar)
│   └── renda-escolaridade.qmd           # Renda e escolaridade (a criar)
├── data/
│   └── processados/                     # Dados .rds (ignorados pelo git)
├── .github/workflows/
│   └── publish.yml                      # GitHub Actions: build + deploy
├── .gitignore                           # Ignora microdados brutos e cache
└── README.md
```

---

## Stack tecnológica

| Componente | Tecnologia |
|---|---|
| Linguagem | R 4.3+ |
| Dados | `{PNADcIBGE}`, `{survey}` |
| Visualização | `{ggplot2}`, `{plotly}` |
| Publicação | Quarto Website |
| CI/CD | GitHub Actions |
| Hospedagem | GitHub Pages |

---

## Como reproduzir localmente

```r
# 1. Clone o repositório
# git clone https://github.com/mancano-tales/PNAD-Educacao.git

# 2. Instale os pacotes
install.packages(c("PNADcIBGE", "survey", "tidyverse",
                   "plotly", "here", "scales"))

# 3. Baixe e processe os dados
source("R/01_download_pnad.R")

# 4. Renderize o site
# No terminal:
# quarto render
# quarto preview   # para ver ao vivo
```

---

## Notas sobre os dados

- Os **microdados brutos** da PNAD não são versionados (arquivo muito grande).
  O script `R/01_download_pnad.R` baixa os dados diretamente da API do IBGE.
- Os **outputs dos chunks R** são armazenados em `_freeze/` e versionados,
  permitindo que o CI/CD não precise re-executar o código a cada push.
- Todas as estimativas usam o **desenho amostral complexo** via `{survey}`,
  garantindo representatividade nacional e regional.

---

## Estrutura inicial gerada por IA

> A estrutura inicial deste repositório — incluindo os arquivos
> `_quarto.yml`, `index.qmd`, `R/01_download_pnad.R`,
> `graficos/desigualdade-escolaridade.qmd` e
> `.github/workflows/publish.yml` — foi gerada por
> **Claude (Anthropic)** em 23 de abril de 2026, a partir de
> instruções do autor do projeto.
>
> O conteúdo analítico, as interpretações e as referências
> bibliográficas serão desenvolvidos e revisados pelo autor.

---

## Licença

Código disponível sob [MIT License](LICENSE).
Dados: IBGE — uso conforme [política de dados abertos do IBGE](https://www.ibge.gov.br/acesso-informacao/institucional/politica-de-dados-abertos.html).
