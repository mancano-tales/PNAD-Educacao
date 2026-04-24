# =============================================================================
# 01_download_pnad.R
# Download e preparação dos microdados da PNAD Contínua (IBGE)
#
# Gerado por: Claude (Anthropic) em 2026-04-23
# Projeto   : PNAD-Educacao (https://github.com/mancano-tales/PNAD-Educacao)
# Autor do projeto: mancano-tales
# =============================================================================

# Pacotes necessários --------------------------------------------------------
# install.packages(c("PNADcIBGE", "survey", "tidyverse", "here"))

library(PNADcIBGE)
library(survey)
library(tidyverse)
library(here)

# Configurações --------------------------------------------------------------

# Variáveis de interesse para análise de desigualdade educacional
# Documentação completa: https://www.ibge.gov.br/estatisticas/sociais/trabalho/17270-pnad-continua.html
VARS_EDUCACAO <- c(
  "V2007",   # Sexo
  "V2009",   # Idade
  "V2010",   # Raça/cor
  "VD3004",  # Nível de instrução mais elevado
  "VD3005",  # Anos de estudo
  "VD4020",  # Rendimento mensal habitual do trabalho principal
  "VD4048",  # Rendimento mensal efetivo de todos os trabalhos
  "V1028",   # Peso do domicilio e das pessoas
  "UF",      # Unidade da Federação
  "V1022",   # Situação do domicílio (urbano/rural)
  "V2005"    # Condição no domicílio
)

# Ano e trimestre de referência (ajuste conforme necessidade)
ANO    <- 2023
TRIMESTRE <- 4

# Diretório de saída dos dados processados
DIR_DADOS <- here("data", "processados")
dir.create(DIR_DADOS, recursive = TRUE, showWarnings = FALSE)

# Download dos microdados ----------------------------------------------------
message("[1/3] Baixando microdados da PNAD Contínua ", ANO, " T", TRIMESTRE, "...")
message("      Isso pode levar alguns minutos dependendo da conexão.")

pnadc_raw <- get_pnadc(
  year      = ANO,
  quarter   = TRIMESTRE,
  vars      = VARS_EDUCACAO,
  design    = FALSE   # retorna data.frame, não svydesign (fazemos isso abaixo)
)

message("    Download concluído. Linhas: ", nrow(pnadc_raw))

# Criação do desenho amostral complexo ---------------------------------------
# IMPORTANTE: nunca ignore os pesos amostrais em análises da PNAD!
message("[2/3] Criando objeto de desenho amostral (svydesign)...")

pnadc_design <- pnadc_design(pnadc_raw)

# Pré-processamento ----------------------------------------------------------
message("[3/3] Pré-processando variáveis...")

pnadc_proc <- pnadc_raw |>
  # Seleciona apenas pessoas (não domicilios)
  filter(!is.na(VD3004)) |>
  mutate(
    # Simplifica nível de instrução em 5 categorias
    instrucao_cat = case_when(
      VD3004 %in% 1:2 ~ "Sem instrução / Fund. incompleto",
      VD3004 %in% 3:4 ~ "Fundamental completo / Médio incompleto",
      VD3004 %in% 5:6 ~ "Médio completo / Superior incompleto",
      VD3004 %in% 7   ~ "Superior completo",
      VD3004 %in% 8   ~ "Pós-graduação",
      TRUE            ~ NA_character_
    ) |> factor(levels = c(
      "Sem instrução / Fund. incompleto",
      "Fundamental completo / Médio incompleto",
      "Médio completo / Superior incompleto",
      "Superior completo",
      "Pós-graduação"
    )),
    # Raça/cor recodificada
    raca = case_when(
      V2010 == 1 ~ "Branca",
      V2010 == 2 ~ "Preta",
      V2010 == 4 ~ "Parda",
      V2010 == 3 ~ "Amarela",
      V2010 == 5 ~ "Indígena",
      TRUE       ~ NA_character_
    ),
    raca_agrupada = case_when(
      V2010 %in% c(2, 4) ~ "Negra (preta + parda)",
      V2010 == 1         ~ "Branca",
      TRUE               ~ "Outras"
    ),
    # Sexo
    sexo = if_else(V2007 == 1, "Homem", "Mulher"),
    # Faixa etaria
    faixa_etaria = cut(
      V2009,
      breaks = c(0, 14, 24, 34, 44, 54, 64, Inf),
      labels = c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+"),
      right  = TRUE
    ),
    # Quintis de renda (calculados com peso amostral - ver script 02)
    log_renda = log1p(VD4020)
  )

# Salva os dados processados -------------------------------------------------
saveRDS(pnadc_proc,   file.path(DIR_DADOS, paste0("pnadc_", ANO, "T", TRIMESTRE, "_proc.rds")))
saveRDS(pnadc_design, file.path(DIR_DADOS, paste0("pnadc_", ANO, "T", TRIMESTRE, "_design.rds")))

message("\nDados salvos em: ", DIR_DADOS)
message("Arquivo processado: pnadc_", ANO, "T", TRIMESTRE, "_proc.rds")
message("Objeto de design : pnadc_", ANO, "T", TRIMESTRE, "_design.rds")
