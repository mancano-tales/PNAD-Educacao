# R/gerar_grafico_renda_educacao.R
# ==============================================================================
# DESCRIÇÃO: Geração do gráfico de desigualdade de renda e acesso à educação
# OBJETIVO: Salvar o output (HTML interativo) na pasta graficos/
# ==============================================================================

library(dplyr)
library(survey)
library(Hmisc)
library(plotly)
library(tidyr)
library(htmlwidgets)
library(arrow)


# ATENÇÃO: Os dados devem ser lidos da pasta Data/ (que não é versionada no Git).
# Exemplo de como carregar (descomente e adapte conforme o formato dos seus dados):
# Salata_etal_2025_dataset <- readRDS("../Data/Salata_etal_2025_dataset.parquet")

# O código abaixo pressupõe que o dataframe `Salata_etal_2025_dataset` já está carregado.

COR_MEDIA_NACIONAL       <- "#1a9641"
LARGURA_MEDIA_NACIONAL   <- 4
TAMANHO_MARCADOR_MEDIA   <- 10
IDADE_MIN                <- 18
IDADE_MAX                <- 24

Salata_etal_2025_dataset <- read_parquet("../Data/Salata_etal_2025_dataset.parquet")

%>%
  haven::zap_labels()

# 1) PREPARAÇÃO DOS DECIS NACIONAIS
pop_decis <- Salata_etal_2025_dataset %>%
  group_by(ano) %>%
  group_modify(~ {
    breaks <- Hmisc::wtd.quantile(.x$renda_dom_pcta, weights = .x$peso, probs = seq(0, 1, 0.1))
    .x %>% mutate(
      decil_renda = cut(
        renda_dom_pcta,
        breaks = breaks,
        labels = paste0("D", 1:10),
        include_lowest = TRUE
      )
    )
  }) %>%
  ungroup()

# 2) FILTRAGEM DE JOVENS
jovens_base <- pop_decis %>%
#  mutate(across(where(is.labelled), as.numeric)) %>%  # converte todas as haven_labelled
#  mutate(ens_medio = case_when(
#    ens_medio == 1 ~ 1,
#    TRUE           ~ 0)) %>%
  filter(idade >= IDADE_MIN,
         idade <= IDADE_MAX,
         !is.na(decil_renda))

# 3A) CÁLCULO DA TAXA POR DECIL COM IC
lista_anos <- list()
anos_disponiveis <- sort(unique(jovens_base$ano))

for (a in anos_disponiveis) {
  df_ano <- jovens_base %>% filter(ano == a)
  design_jovens <- svydesign(ids = ~1, weights = ~peso, data = df_ano)
  
  rows_decil <- list()
  for (d in paste0("D", 1:10)) {
    design_d <- subset(design_jovens, decil_renda == d)
    prop_obj  <- svyciprop(~ens_medio, design_d, method = "logit")
    ci        <- confint(prop_obj)
    
    rows_decil[[d]] <- data.frame(
      decil = d,
      ano   = a,
      prop  = as.numeric(prop_obj) * 100,
      lower = ci[1, 1] * 100,
      upper = ci[1, 2] * 100
    )
  }
  lista_anos[[as.character(a)]] <- bind_rows(rows_decil)
}

dados_plot <- bind_rows(lista_anos) %>%
  mutate(decil_num = as.numeric(gsub("D", "", decil)))

# 3B) CÁLCULO DA MÉDIA NACIONAL COM IC
lista_nacional <- list()

for (a in anos_disponiveis) {
  df_ano        <- jovens_base %>% filter(ano == a)
  design_jovens <- svydesign(ids = ~1, weights = ~peso, data = df_ano)
  
  prop_obj <- svyciprop(~ens_medio, design_jovens, method = "logit")
  ci       <- confint(prop_obj)
  
  lista_nacional[[as.character(a)]] <- data.frame(
    ano   = a,
    prop  = as.numeric(prop_obj) * 100,
    lower = ci[1, 1] * 100,
    upper = ci[1, 2] * 100
  )
}

dados_nacional <- bind_rows(lista_nacional)

# 4) RÓTULOS PERSONALIZADOS PARA A LEGENDA
labels_decis <- c(
  "Decil 1: 10% mais pobres",
  "Decil 2: 10-20%",
  "Decil 3: 20-30%",
  "Decil 4: 30-40%",
  "Decil 5: 40-50%",
  "Decil 6: 50-60%",
  "Decil 7: 60-70%",
  "Decil 8: 70-80%",
  "Decil 9: 80-90%",
  "Decil 10: 10% mais ricos"
)

# 5) CONSTRUÇÃO DO GRÁFICO — DECIS
grafico_renda <- plot_ly()
cores <- colorRampPalette(c("#D7191C", "#FFFFBF", "#2B83BA"))(10)

for (i in 1:10) {
  df_d <- dados_plot %>% filter(decil_num == i)
  
  grafico_renda <- grafico_renda %>%
    add_trace(
      data = df_d,
      x = ~ano,
      y = ~prop,
      type = 'scatter',
      mode = 'lines+markers',
      name = labels_decis[i],
      line = list(color = cores[i], width = 3),
      marker = list(color = cores[i], size = 8),
      error_y = list(
        type      = "data",
        array     = ~upper - prop,
        arrayminus = ~prop - lower,
        visible   = TRUE,
        thickness = 1.2,
        width     = 2,
        color     = cores[i]
      ),
      hoverinfo = "text",
      text = ~paste0(
        "<b>Ano:</b> ", ano, "<br>",
        "<b>", labels_decis[i], "</b><br>",
        "<b>Taxa de conclusão:</b> ", sprintf('%.1f', prop), "%<br>",
        "<b>IC (95%):</b> [", sprintf('%.1f', lower), "% - ", sprintf('%.1f', upper), "%]"
      )
    )
}

# 6) ADICIONAR LINHA DA MÉDIA NACIONAL
grafico_renda <- grafico_renda %>%
  add_trace(
    data = dados_nacional,
    x = ~ano,
    y = ~prop,
    type = 'scatter',
    mode = 'lines+markers',
    name = paste0("<b>Média Nacional (", IDADE_MIN, "-", IDADE_MAX, " anos)</b>"),
    line = list(
      color = COR_MEDIA_NACIONAL,
      width = LARGURA_MEDIA_NACIONAL,
      dash  = "dash"
    ),
    marker = list(
      color  = COR_MEDIA_NACIONAL,
      size   = TAMANHO_MARCADOR_MEDIA,
      symbol = "diamond",
      line   = list(color = "white", width = 1.5)
    ),
    error_y = list(
      type       = "data",
      array      = ~upper - prop,
      arrayminus = ~prop - lower,
      visible    = TRUE,
      thickness  = 2,
      width      = 3,
      color      = COR_MEDIA_NACIONAL
    ),
    hoverinfo = "text",
    text = ~paste0(
      "<b>Ano:</b> ", ano, "<br>",
      "<b>★ MÉDIA NACIONAL (", IDADE_MIN, "-", IDADE_MAX, " anos) ★</b><br>",
      "<b>Taxa de conclusão:</b> ", sprintf('%.1f', prop), "%<br>",
      "<b>IC (95%):</b> [", sprintf('%.1f', lower), "% - ", sprintf('%.1f', upper), "%]"
    ),
    legendgroup = "nacional",
    showlegend  = TRUE
  )

# 7) LAYOUT FINAL
grafico_renda <- grafico_renda %>%
  layout(
    title = list(
      text = paste0(
        "<b>Conclusão do Ensino Médio por decil de renda no Brasil, 1992-2022</b><br>",
        "<sup>Pessoas de ", IDADE_MIN, " a ", IDADE_MAX, " anos</sup>"
      ),
      x = 0.5,
      y = 0.97,
      font = list(size = 20)
    ),
    xaxis = list(
      title     = "<b>Ano</b>",
      gridcolor = "#E5E5E5",
      tickmode  = "array",
      tickvals  = anos_disponiveis,
      tickangle = -45,
      font      = list(size = 10)
    ),
    yaxis = list(
      title      = "<b>Taxa de conclusão do ensino médio (%)</b>",
      ticksuffix = "%",
      range      = c(0, 100),
      gridcolor  = "#E5E5E5"
    ),
    legend = list(
      title    = list(text = "<b>Decil de renda (população)</b>"),
      x        = 1.02,
      y        = 1.0,
      xanchor  = "left",
      yanchor  = "top",
      bgcolor  = 'rgba(255, 255, 255, 0.9)',
      bordercolor = "#E5E5E5",
      borderwidth = 1,
      font     = list(size = 10)
    ),
    annotations = list(
      list(
        x       = 0.5,
        y       = -0.15,
        text    = "Fonte: Elaboração própria com base nos dados da PNAD/PNAD-C (IBGE).",
        showarrow = FALSE,
        xref    = 'paper',
        yref    = 'paper',
        xanchor = 'center',
        yanchor = 'top',
        font    = list(size = 15, color = "#555555")
      )
    ),
    margin       = list(t = 100, b = 120, l = 60, r = 200),
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    hovermode    = "closest"
  )

# 8) SALVAR O GRÁFICO NA PASTA GRAFICOS
if (!dir.exists("../graficos")) {
  dir.create("../graficos")
}

# Salva o output do gráfico interativo
htmlwidgets::saveWidget(
  widget = Teste191,
  file = normalizePath("../PNAD-Educacao/graficos/grafico_renda__ens_medio_decil_educacao.html", mustWork = FALSE),
  selfcontained = TRUE
)

cat("Gráfico salvo com sucesso em graficos/grafico_renda_educacao.html\n")

# 8) LIMPEZA DE MEMÓRIA (ENVIRONNMENT)
# Remover objetos temporários, loops e tabelas grandes para liberar a RAM
rm(
  list = c(
    "pop_decis", "jovens_base", "lista_anos", "anos_disponiveis", 
    "df_ano", "design_jovens", "rows_decil", "design_d", 
    "prop_obj", "ci", "dados_plot", "lista_nacional", 
    "dados_nacional", "labels_decis", "cores", "df_d",
    "a", "d", "i"
  ),
  # Impede erro caso algum dos objetos não exista
  envir = environment()
)

# Se quiser remover as constantes globais do gráfico também, descomente a linha abaixo:
# rm(COR_MEDIA_NACIONAL, LARGURA_MEDIA_NACIONAL, TAMANHO_MARCADOR_MEDIA, IDADE_MIN, IDADE_MAX)

# O Garbage Collector (gc) vai forçar o sistema a recuperar a memória alocada desses objetos
gc()

