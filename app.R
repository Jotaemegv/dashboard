library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(sf)
library(readr)
library(ggpattern)
library(leaflet)
library(rmarkdown)
library(shinycssloaders)

# === LEITURA DOS DADOS ===
shape_pr <- st_read("PR_Municipios_2023.shp") %>%
  st_transform(crs = 4326) %>%
  mutate(CD_MUN_6 = substr(as.character(CD_MUN), 1, 6))  # Truncar para 6 dígitos
bbox <- st_bbox(shape_pr)

ordered_patterns <- c(
  "New Hot Spot", "Consecutive Hot Spot", "Intensifying Hot Spot",
  "Persistent Hot Spot", "Diminishing Hot Spot", "Sporadic Hot Spot",
  "Oscillating Hot Spot", "Historical Hot Spot",
  "New Cold Spot", "Consecutive Cold Spot", "Intensifying Cold Spot",
  "Persistent Cold Spot", "Diminishing Cold Spot", "Sporadic Cold Spot",
  "Oscillating Cold Spot", "Historical Cold Spot",
  "No Pattern Detected"
)

todos_patterns <- ordered_patterns
cores_hotspot <- setNames(rep("gray90", length(todos_patterns)), todos_patterns)
cores_hotspot["New Hot Spot"] <- "#CB0800"
cores_hotspot["Consecutive Hot Spot"] <- "#BD6D5E"
cores_hotspot["Persistent Hot Spot"] <- "#851400"
cores_hotspot["Oscillating Hot Spot"] <- "#3468A8"
cores_hotspot["Oscillating Cold Spot"] <- "#EDA882"
cores_hotspot["New Cold Spot"] <- "#174C9C"
cores_hotspot["Consecutive Cold Spot"] <- "#6883BA"
cores_hotspot["Persistent Cold Spot"] <- "#1B3659"
cores_hotspot["Sporadic Hot Spot"] <- "#FFFFFF"
cores_hotspot["Sporadic Cold Spot"] <- "#FFFFFF"
cores_hotspot["Diminishing Cold Spot"] <- "#D4E4EA"
cores_hotspot["Diminishing Hot Spot"] <- "#FAC99D"
cores_hotspot["Intensifying Hot Spot"] <- "#E01709"
cores_hotspot["Intensifying Cold Spot"] <- "#4571AD"
cores_hotspot["Historical Hot Spot"] <- "#DEBCA4"
cores_hotspot["Historical Cold Spot"] <- "#BCC6CC"
cores_hotspot["No Pattern Detected"] <- "#F5F5F5"

pattern_list <- setNames(rep("none", length(todos_patterns)), todos_patterns)
pattern_list["Oscillating Hot Spot"] <- "circle"
pattern_list["Oscillating Cold Spot"] <- "circle"
pattern_list["Sporadic Hot Spot"] <- "circle"
pattern_list["Sporadic Cold Spot"] <- "circle"
pattern_list["Consecutive Hot Spot"] <- "none"
pattern_list["Consecutive Cold Spot"] <- "none"
pattern_list["Persistent Hot Spot"] <- "crosshatch"
pattern_list["Persistent Cold Spot"] <- "none"
pattern_list["Diminishing Cold Spot"] <- "none"

pattern_fill_map <- setNames(rep(NA, length(todos_patterns)), todos_patterns)
pattern_fill_map["Oscillating Hot Spot"] <- "#EDA882"
pattern_fill_map["Oscillating Cold Spot"] <- "#3468A8"
pattern_fill_map["Sporadic Hot Spot"] <- "#F5D2B5"
pattern_fill_map["Sporadic Cold Spot"] <- "#9EB6CF"
pattern_fill_map["Persistent Hot Spot"] <- "white"

dados_hotspot <- read_delim("EHA_TxSuavFraturas.csv", delim = ";") %>%
  mutate(PATTERN = factor(trimws(PATTERN), levels = ordered_patterns))

shape_pr$PATTERN <- dados_hotspot$PATTERN
shape_pr <- shape_pr %>%
  mutate(
    borda_color = ifelse(PATTERN == "Diminishing Cold Spot", "#AFC4CC", "white"),
    borda_size = ifelse(PATTERN == "Diminishing Cold Spot", 0.9, 0.3)
  )

# === UI
# === UI
ui <- dashboardPage(
  skin = "black",
  
  # Header
  dashboardHeader(
    title = tags$div("DASHBOARD", style = "font-weight:bold; text-align:center; width:100%;")
  ),
  
  # Sidebar
  dashboardSidebar(
    sidebarMenu(id = "tabs", selected = "nova",
                menuItem("Gráficos", tabName = "nova", icon = icon("chart-bar")),
                menuItem("Visualizar Mapa de hotspots", tabName = "mapa", icon = icon("map")),
                menuItem("Relatório", tabName = "relatorio", icon = icon("file-alt")),
                menuItem("Fontes", tabName = "fontes", icon = icon("info-circle"))
                
    )
  ),
  
  # Body
  dashboardBody(
    # Estilo geral
    tags$head(
      tags$style(HTML("
        html, body {
          height: 100%;
          background-color: #f5f5f5;
          overflow: hidden;
        }
        .content-wrapper {
          overflow-y: auto !important;
          height: calc(100vh - 50px);
          background-color: #f5f5f5;
        }
        .main-sidebar {
          background-color: #012340;
        }
        .main-header .navbar {
          background-color: #012340;
        }
        .main-header .logo {
          background-color: #012340;
          color: white;
          font-weight: bold;
          text-align: center;
        }
        .box {
          background: #FFFFFF;
          border: 1px solid #D9D9D9;
          box-shadow: 0 2px 3px rgba(0,0,0,0.1);
          border-radius: 8px;
        }
        .box-header {
          background-color: #012340;
          color: white;
          border-top-left-radius: 8px;
          border-top-right-radius: 8px;
          padding: 10px 15px;
          margin: 0;
        }
        h3.box-title {
          color: white;
          font-weight: bold;
          font-size: 18px;
          margin: 0;
          line-height: 1.2;
        }
        .btn {
          background-color: #025959;
          border: none;
          color: white;
          border-radius: 5px;
        }
        .btn:hover {
          background-color: #027373;
        }
      "))
    ),
    
    # Conteúdo das Abas
    tabItems(
      
      # --- Aba Nova Tela ---
      tabItem(tabName = "nova",
              fluidRow(
                column(width = 6,
                       box(
                         width = 12,
                         solidHeader = TRUE,
                         title = textOutput("map_title"),
                         style = "height: 300px;",
                         div(
                           style = "height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
                           div(
                             style = "margin-bottom: 5px;",
                             selectInput("filtro_municipio", "Escolha um município:",
                                         choices = c("Paraná", shape_pr$NM_MUN),
                                         selected = "Paraná")
                           ),
                           withSpinner(leafletOutput("mapa_dinamico", height = "210px"))
                         )
                       ),
                       box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Incidência por Faixa Etária e Sexo",
                         style = "height: 500px;",
                         div(
                           style = "height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
                           div(
                             style = "margin-bottom: 5px;",
                             selectInput("ano_grafico1", "Ano:", choices = c("Todos", 2010:2021), selected = "Todos")
                           ),
                           withSpinner(plotOutput("grafico_1", height = "380px"))
                         )
                       )
                ),
                
                column(width = 6,
                       box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Óbitos por Faixa Etária, Sexo e Tratamento",
                         style = "height: 300px;",
                         div(
                           style = "height: 100%; display: flex; flex-direction: column; justify-content: center;",
                           withSpinner(plotOutput("grafico_direita", height = "250px"))
                         )
                       ),
                       box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Fluxo por Município / Destino do Atendimento",
                         style = "height: 500px;",
                         fluidRow(
                           column(width = 6,
                                  style = "height: 473px;",
                                  withSpinner(uiOutput("mapa_setas"))
                           ),
                           column(width = 6,
                                  style = "height: 473px;",
                                  withSpinner(plotOutput("grafico_2", height = "300px"))
                           )
                         )
                       )
                )
              )
      ),
      
      # --- Aba Visualizar Mapa de Hotspots ---
      tabItem(tabName = "mapa",
              fluidRow(
                column(width = 12,
                       box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Hot Spots Emergentes (2010–2021)",
                         div(
                           style = "height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
                           withSpinner(imageOutput("hotspot_plot", height = "700px", width = "1000px")),
                           div(
                             style = "padding: 15px; font-size: 16px; color: #012340;",
                             HTML("
                    <h4>Como interpretar o Mapa de Hotspots:</h4>
                    <ul>
                      <li>O mapa utiliza a técnica <b>Space-Time Cube</b> combinada com o índice <b>Getis-Ord Gi*</b> para detectar áreas críticas.</li>
                      <li><b>Hotspots (áreas em vermelho)</b>: municípios com <b>altas taxas</b> de fratura de fêmur.</li>
                      <li><b>Coldspots (áreas em azul)</b>: municípios com <b>baixas taxas</b> de fratura de fêmur.</li>
                      <li><b>Áreas em branco</b>: não apresentam padrão espacial relevante.</li>
                    </ul>
                    <h5>Categorias dos padrões detectados:</h5>
                    <ul>
                      <li><b>Novo (New Hot Spot)</b>: Área recém-identificada com alta taxa.</li>
                      <li><b>Persistente (Persistent Hot Spot)</b>: Área que historicamente manteve alta taxa.</li>
                      <li><b>Intensificando (Intensifying Hot Spot)</b>: Área onde a taxa aumentou recentemente.</li>
                      <li><b>Diminuição (Diminishing Hot Spot)</b>: Área onde a taxa de fraturas vem caindo.</li>
                      <li><b>Oscilante, Esporádico ou Histórico</b>: Áreas com comportamento variável ao longo do tempo.</li>
                    </ul>
                    <p><i>Nota: A análise combina dados espaciais e temporais, identificando regiões prioritárias para intervenção em saúde pública.</i></p>
                  ")
                           )
                         )
                       )
                )
              )
      ),
      tabItem(tabName = "fontes",
              fluidRow(
                column(width = 12,
                       box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Fontes dos Dados",
                         status = "primary",
                         div(style = "font-size:16px; padding: 10px;",
                             HTML("
            <p><b>Fonte dos dados:</b></p>
            <ul>
              <li><b>DATASUS</b> – Departamento de Informática do SUS</li>
              <li><b>IBGE</b> – Instituto Brasileiro de Geografia e Estatística</li>
              
              <li><b>Ministério da Saúde</b> – Dados públicos de saúde</li>
            </ul>
            <p>----------------- 2025</p>
          ")
                         )
                       )
                )
              )
      ),
      
      
      # --- Aba Relatório ---
      # Na aba "Relatório" do UI
      tabItem(tabName = "relatorio",
              fluidRow(
                column(width = 6,
                       selectInput("relatorio_municipio", "Município:",
                                   choices = c("Paraná", unique(shape_pr$NM_MUN)),
                                   selected = "Paraná")
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Relatório de Variáveis Associadas às Fraturas de Quadril",
                         withSpinner(htmlOutput("relatorio_completo"))
                       )
                )
              ),
              fluidRow(
                column(width = 12,
                       box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Mapa do Município Selecionado",
                         leafletOutput("mapa_municipio")
                       )
                )
              )
      )
      
      
    )
  )
  
)



# === SERVER
server <- function(input, output, session) {
  municipio_selecionado <- reactiveVal(NULL)
  
  observeEvent(input$mapa_dinamico_shape_click, {
    cod_mun <- substr(input$mapa_dinamico_shape_click$id, 1, 6)
    municipio_selecionado(cod_mun)
    
    # Atualizar o selectInput para o nome do município clicado
    nome_mun <- shape_pr %>%
      filter(CD_MUN_6 == cod_mun) %>%
      pull(NM_MUN)
    
    updateSelectInput(session, "filtro_municipio", selected = nome_mun)
  })
  
  
  observeEvent(input$voltar_mapa, {
    municipio_selecionado(NULL)
    
    # Atualizar o selectInput de volta para "Paraná"
    updateSelectInput(session, "filtro_municipio", selected = "Paraná")
  })
  observeEvent(input$filtro_municipio, {
    if (input$filtro_municipio == "Paraná") {
      municipio_selecionado(NULL)
    } else {
      cod_mun <- shape_pr %>% 
        filter(NM_MUN == input$filtro_municipio) %>% 
        pull(CD_MUN_6)
      municipio_selecionado(cod_mun)
    }
  })
  output$municipioSelecionado <- reactive({
    !is.null(municipio_selecionado())
  })
  outputOptions(output, "municipioSelecionado", suspendWhenHidden = FALSE)
  
  output$map_title <- renderText({
    if (is.null(municipio_selecionado())) {
      "Mapa Interativo do Paraná"
    } else {
      nome <- shape_pr %>%
        filter(CD_MUN_6 == municipio_selecionado()) %>%
        pull(NM_MUN)
      paste("Município:", nome)
    }
  })
  
  output$mapa_dinamico <- renderLeaflet({
    library(geobr)
    
    estados_brasil <- read_state(year = 2020)
    parana_estado <- estados_brasil %>% filter(abbrev_state == "PR")
    vizinhos_estado <- estados_brasil %>% filter(abbrev_state != "PR")
    
    if (is.null(municipio_selecionado())) {
      leaflet(options = leafletOptions(minZoom = 6, maxZoom = 9)) %>%
        addProviderTiles("CartoDB.Positron") %>%
        
        # Estados vizinhos (bordas finas e cinza claro)
        addPolygons(data = vizinhos_estado,
                    fillColor = "#d9d9d9",
                    color = "#bdbdbd",
                    weight = 1,
                    fillOpacity = 0.3,
                    label = ~name_state) %>%
        
        # Paraná com borda preta sólida e espessa
        addPolygons(data = parana_estado,
                    fillColor = "#FFFFFF00",  # transparente
                    color = "black",         # borda preta
                    weight = 4,              # espessura
                    opacity = 1,
                    fillOpacity = 0,         # sem preenchimento
                    label = "Paraná") %>%
        
        # Municípios do PR
        addPolygons(data = shape_pr,
                    fillColor = "#025959", weight = 1, opacity = 1, color = "white",
                    dashArray = "3", fillOpacity = 0.7,
                    highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
                    label = ~NM_MUN, layerId = ~CD_MUN,
                    labelOptions = labelOptions(style = list("font-weight" = "normal"), textsize = "13px")) %>%
        
        setMaxBounds(lng1 = -55.5, lat1 = -27.7, lng2 = -48.0, lat2 = -22.5)
    } else {
      muni <- shape_pr %>% filter(CD_MUN_6 == municipio_selecionado())
      leaflet(data = muni) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(fillColor = "#009688", weight = 2, color = "black", fillOpacity = 0.8, label = ~NM_MUN) %>%
        setView(lng = st_coordinates(st_centroid(st_geometry(muni)))[1],
                lat = st_coordinates(st_centroid(st_geometry(muni)))[2], zoom = 10)
    }
  })
  
  
  output$hotspot_plot <- renderImage({
    list(
      src = "hotspot_plot.jpg",      # Caminho relativo ao diretório do app
      contentType = "image/jpg",
      width = 1000,         # controle de largura
      height = 700,        # controle de altura
      alt = "Hotspot Paraná"
    )
  }, deleteFile = FALSE)
  
  
  output$grafico_1 <- renderPlot({
    df <- read.csv("BANCO_FINAL_AIHs_CLASSIFICADAS(1).csv", encoding = "latin1")
    df$ANO_CMPT <- as.numeric(df$ANO_CMPT)
    df$SEXO_LABEL <- dplyr::recode(df$SEXO, `M` = "Masculino", `F` = "Feminino")
    
    if (input$ano_grafico1 != "Todos") {
      df <- df %>% filter(ANO_CMPT == as.numeric(input$ano_grafico1))
    }
    
    if (!is.null(municipio_selecionado())) {
      df <- df %>% filter(MUNIC_RES == as.numeric(municipio_selecionado()))
    }
    
    df <- df %>%
      mutate(
        FaixaEtaria = case_when(
          IDADE >= 60 & IDADE <= 69 ~ "60-69",
          IDADE >= 70 & IDADE <= 79 ~ "70-79",
          IDADE >= 80 & IDADE <= 89 ~ "80-89",
          IDADE >= 90 ~ ">90",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(FaixaEtaria)) %>%
      mutate(FaixaEtaria = factor(FaixaEtaria, levels = c("60-69", "70-79", "80-89", ">90")))
    
    dados <- df %>%
      group_by(FaixaEtaria, SEXO_LABEL) %>%
      summarise(total = n(), .groups = "drop")
    
    ggplot(dados, aes(x = FaixaEtaria, y = total, fill = SEXO_LABEL)) +
      geom_col(position = position_dodge(width = 0.9)) +
      geom_text(
        aes(label = total),
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 4
      ) +
      scale_fill_manual(values = c("Feminino" = "#027373", "Masculino" = "#025959")) +
      labs(
        title = ifelse(input$ano_grafico1 == "Todos",
                       "Fraturas de Fêmur por Faixa Etária e Sexo - Todos os anos",
                       paste("Fraturas de Fêmur por Faixa Etária e Sexo -", input$ano_grafico1)),
        x = "Faixa Etária", y = "Número de Fraturas",
        fill = "Sexo"
      ) +
      theme_minimal()
  })
  
  output$grafico_direita <- renderPlot({
    df <- read.csv("BANCO_FINAL_AIHs_CLASSIFICADAS(1).csv", encoding = "latin1")
    df$ANO_CMPT <- as.numeric(df$ANO_CMPT)
    df$SEXO_LABEL <- dplyr::recode(df$SEXO, `M` = "Masculino", `F` = "Feminino")
    
    if (input$ano_grafico1 != "Todos") {
      df <- df %>% filter(ANO_CMPT == as.numeric(input$ano_grafico1))
    }
    
    if (!is.null(municipio_selecionado())) {
      df <- df %>% filter(MUNIC_RES == as.numeric(municipio_selecionado()))
    }
    
    df <- df %>%
      mutate(
        FaixaEtaria = case_when(
          IDADE >= 60 & IDADE <= 69 ~ "60-69",
          IDADE >= 70 & IDADE <= 79 ~ "70-79",
          IDADE >= 80 & IDADE <= 89 ~ "80-89",
          IDADE >= 90 ~ ">90",
          TRUE ~ NA_character_
        ),
        Operado = PROC_REA == 408050632,
        Sexo = case_when(
          SEXO == "M" ~ "♂",
          SEXO == "F" ~ "♀",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(FaixaEtaria), !is.na(Sexo))
    
    df <- df %>%
      mutate(
        Grupo = case_when(
          Sexo == "♂" & Operado ~ "Óbito ♂ Operados",
          Sexo == "♂" & !Operado ~ "Óbito ♂ Não Operados",
          Sexo == "♀" & Operado ~ "Óbito ♀ Operadas",
          Sexo == "♀" & !Operado ~ "Óbito ♀ Não Operadas"
        )
      )
    
    # Total de pacientes internados por grupo
    total_por_grupo <- df %>%
      group_by(FaixaEtaria, Grupo) %>%
      summarise(total_casos = n(), .groups = "drop")
    
    # Total de óbitos por grupo
    obitos_por_grupo <- df %>%
      filter(MORTE == "OBITO") %>%
      group_by(FaixaEtaria, Grupo) %>%
      summarise(obitos = n(), .groups = "drop")
    
    # Juntar e calcular a % real de óbitos
    dados_plot <- left_join(total_por_grupo, obitos_por_grupo, by = c("FaixaEtaria", "Grupo")) %>%
      mutate(obitos = ifelse(is.na(obitos), 0, obitos)) %>%
      mutate(pct = 100 * obitos / total_casos)
    
    dados_plot$FaixaEtaria <- factor(dados_plot$FaixaEtaria, levels = c("60-69", "70-79", "80-89", ">90"))
    
    ggplot(dados_plot, aes(x = FaixaEtaria, y = pct, fill = Grupo)) +
      geom_col(position = position_dodge()) +
      geom_text(
        aes(label = obitos),  # <<<<< Mostrando o valor absoluto de óbitos
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 3.5
      ) +
      scale_fill_manual(values = c(
        "Óbito ♂ Operados" = "#027373",
        "Óbito ♂ Não Operados" = "#81A1C1",
        "Óbito ♀ Operadas" = "#014F86",
        "Óbito ♀ Não Operadas" = "#A9CCE3"
      )) +
      labs(
        title = "Desfecho de fraturas de fêmur por faixa etária e sexo",
        x = "Faixa Etária", y = "% de Óbitos",
        fill = "Grupo"
      ) +
      theme_minimal()
  })
  
  
  output$grafico_2 <- renderPlot({
    df <- read.csv("BANCO_FINAL_AIHs_CLASSIFICADAS(1).csv", encoding = "latin1")
    df$ANO_CMPT <- as.numeric(df$ANO_CMPT)
    
    if (input$ano_grafico1 != "Todos") {
      df <- df %>% filter(ANO_CMPT == as.numeric(input$ano_grafico1))
    }
    
    nomes_municipios <- shape_pr %>%
      st_set_geometry(NULL) %>%
      select(CD_MUN_6, NM_MUN) %>%
      distinct()
    
    if (!is.null(municipio_selecionado())) {
      df <- df %>% filter(MUNIC_RES == as.numeric(municipio_selecionado()))
      
      df <- df %>%
        mutate(
          status_transferencia = case_when(
            COD_ID == "E" ~ "Transferência Interna",
            COD_ID == "UE" ~ as.character(MUNIC_MOV),
            TRUE ~ "Não Transferido"
          )
        ) %>%
        left_join(nomes_municipios, by = c("status_transferencia" = "CD_MUN_6")) %>%
        mutate(
          status_transferencia_nome = case_when(
            status_transferencia == "Transferência Interna" ~ "Transferência Interna",
            status_transferencia == "Não Transferido" ~ "Não Transferido",
            TRUE ~ NM_MUN
          )
        )
    } else {
      df <- df %>%
        mutate(
          status_transferencia_nome = case_when(
            COD_ID == "UE" ~ "Transferência Externa",
            COD_ID == "E" ~ "Transferência Interna",
            TRUE ~ "Não Transferido"
          )
        )
    }
    
    if (nrow(df) == 0) {
      grid::grid.newpage()
      grid::grid.text(
        label = "Nenhuma transferência hospitalar encontrada",
        x = 0.5, y = 0.5,
        gp = grid::gpar(fontsize = 17, fontface = "bold", col = "#000000")
      )
      return()
    }
    
    resumo <- df %>%
      group_by(status_transferencia_nome) %>%
      summarise(total = n(), .groups = "drop") %>%
      mutate(prop = total / sum(total) * 100) %>%
      arrange(desc(prop)) %>%
      mutate(legenda = paste0(status_transferencia_nome, " (", round(prop, 1), "%)"))
    
    # <<<<<< ORDENAR A LEGENDA PELO MAIOR VALOR
    resumo <- resumo %>% arrange(desc(prop))
    
    cores_base <- c(
      "Não Transferido" = "#014F86",
      "Transferência Externa" = "#81A1C1",
      "Transferência Interna" = "#A9CCE3"
    )
    
    categorias <- resumo$status_transferencia_nome
    cores_legenda <- sapply(categorias, function(nome) {
      if (nome %in% names(cores_base)) {
        cores_base[[nome]]
      } else {
        NA
      }
    })
    
    n_municipios <- sum(is.na(cores_legenda))
    if (n_municipios > 0) {
      library(RColorBrewer)
      paleta_municipios <- brewer.pal(min(9, n_municipios), "Blues")
      cores_legenda[is.na(cores_legenda)] <- paleta_municipios[1:n_municipios]
    }
    
    names(cores_legenda) <- resumo$legenda
    
    ggplot(resumo, aes(x = "", y = prop, fill = legenda)) +
      geom_col(width = 1, color = "#666") +
      coord_polar("y", start = 0) +
      scale_fill_manual(
        values = cores_legenda,
        breaks = resumo$legenda   # ORDEM da legenda igual à ordem de 'resumo'
      ) +
      guides(fill = guide_legend(ncol = 2, byrow = TRUE)) +  
      theme_void() +
      theme(
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 9),
        legend.box.margin = margin(t = 5, b = 5),
        legend.spacing.x = unit(0.4, "cm"),
        plot.background = element_rect(fill = "white", color = NA),
        text = element_text(color = "#012340", size = 13)
      )
  })
  output$mapa_setas <- renderUI({
    df <- read.csv("BANCO_FINAL_AIHs_CLASSIFICADAS(1).csv", encoding = "latin1")
    df$ANO_CMPT <- as.numeric(df$ANO_CMPT)
    
    # Filtrar pelo ano selecionado
    if (input$ano_grafico1 != "Todos") {
      df <- df %>% filter(ANO_CMPT == as.numeric(input$ano_grafico1))
    }
    
    # Filtrar somente as transferências externas (UE)
    df <- df %>% filter(COD_ID == "UE")
    
    # Filtrar apenas as transferências originadas do município selecionado
    if (!is.null(municipio_selecionado())) {
      mun_sel <- as.numeric(municipio_selecionado())
      df <- df %>% filter(MUNIC_RES == mun_sel)  # Só manter transferências que começam no município selecionado
    }
    
    # Se não houver transferências, mostrar mensagem
    if (nrow(df) == 0) {
      return(div(
        style = "height:300px; display:flex; align-items:center; justify-content:right; font-size:16px; font-weight:bold;color:#000000",
        "Nenhuma transferência"
      ))
    }
    
    # Gerar o mapa interativo
    output$leaflet_mapa <- renderLeaflet({
      # Coordenadas dos municípios
      coord_muni <- shape_pr %>%
        st_centroid() %>%
        st_coordinates() %>%
        as.data.frame() %>%
        bind_cols(st_drop_geometry(shape_pr) %>% select(CD_MUN_6)) %>%
        rename(lon = X, lat = Y)
      
      # Se nenhum município estiver selecionado
      if (is.null(municipio_selecionado())) {
        return(
          leaflet() %>%
            addProviderTiles("CartoDB.Positron")
        )
      }
      
      # Converte códigos
      df$MUNIC_RES <- as.character(df$MUNIC_RES)
      df$MUNIC_MOV <- as.character(df$MUNIC_MOV)
      mun_sel <- as.character(municipio_selecionado())
      
      # Coordenada da origem (município selecionado)
      origem_coord <- coord_muni %>%
        filter(CD_MUN_6 == mun_sel) %>%
        select(lon_orig = lon, lat_orig = lat)
      
      # Se a coordenada do município não foi encontrada
      if (nrow(origem_coord) == 0) {
        return(
          leaflet() %>%
            addProviderTiles("CartoDB.Positron")
        )
      }
      
      # Filtra apenas transferências partindo DO município selecionado
      df_coords <- df %>%
        filter(COD_ID == "UE", MUNIC_RES == mun_sel) %>%
        distinct(MUNIC_MOV) %>%
        left_join(coord_muni, by = c("MUNIC_MOV" = "CD_MUN_6")) %>%
        rename(lon_dest = lon, lat_dest = lat) %>%
        mutate(
          lon_orig = origem_coord$lon_orig[1],
          lat_orig = origem_coord$lat_orig[1]
        ) %>%
        filter(!is.na(lat_dest), !is.na(lon_dest))
      
      # Desenha o mapa
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolylines(
          data = df_coords,
          lng = ~c(lon_orig, lon_dest),
          lat = ~c(lat_orig, lat_dest),
          color = "blue",
          weight = 2,
          opacity = 0.8
        )
    })
    
    leafletOutput("leaflet_mapa", height = "473px")
  })
  
  # No servidor
  output$mapa_municipio <- renderLeaflet({
    municipio <- input$relatorio_municipio  # Obter o município selecionado
    
    # Filtra os dados do município
    municipio_data <- shape_pr %>% filter(NM_MUN == municipio)
    
    # Verificar se o filtro encontrou dados
    print(municipio_data)  # Adicione esta linha para depuração
    
    # Se não encontrar dados
    if (nrow(municipio_data) == 0) {
      return(leaflet() %>% addTiles())  # Retorna um mapa em branco se nenhum município for encontrado
    }
    
    # Criar o mapa
    mapa <- leaflet(municipio_data) %>%
      addTiles() %>%
      addPolygons(
        fillColor = "blue", 
        weight = 1, 
        color = "white", 
        opacity = 0.5, 
        fillOpacity = 0.7,
        label = ~NM_MUN  # Rótulo com o nome do município
      ) %>%
      setView(
        lng = st_centroid(municipio_data)$coords[1], 
        lat = st_centroid(municipio_data)$coords[2], 
        zoom = 10  # Ajuste o zoom conforme necessário
      )
    
    return(mapa)
  })
  
  
  
  output$relatorio_completo <- renderUI({
    df_fraturas <- read.csv("BANCO_FINAL_AIHs_CLASSIFICADAS(1).csv", encoding = "latin1")
    dados_indicadores <- read.csv("dados_indicadores_corrigido.csv", encoding = "latin1")
    
    if (input$relatorio_municipio != "Paraná") {
      cd_mun <- shape_pr %>%
        filter(NM_MUN == input$relatorio_municipio) %>%
        pull(CD_MUN_6)
      
      df_fraturas <- df_fraturas %>% filter(MUNIC_RES == as.numeric(cd_mun))
      indicadores_filtrados <- dados_indicadores %>% filter(MUNIC_RES == as.numeric(cd_mun))
    } else {
      indicadores_filtrados <- dados_indicadores
    }
    
    total_quedas <- nrow(df_fraturas)
    
    densito <- if (input$relatorio_municipio == "Paraná") {
      mean(dados_indicadores$tx_apac_densito10k, na.rm = TRUE)
    } else {
      indicadores_filtrados$tx_apac_densito10k[1]
    }
    
    esf <- if (input$relatorio_municipio == "Paraná") {
      mean(dados_indicadores$cob_esf, na.rm = TRUE) * 100
    } else {
      indicadores_filtrados$cob_esf[1] * 100
    }
    
    plano <- if (input$relatorio_municipio == "Paraná") {
      mean(dados_indicadores$tx_cob_plano, na.rm = TRUE) 
    } else {
      indicadores_filtrados$tx_cob_plano[1]
    }
    
    alf_5564 <- if (input$relatorio_municipio == "Paraná") {
      mean(dados_indicadores$`tx_alf_5564`, na.rm = TRUE) * 100
    } else {
      indicadores_filtrados$`tx_alf_5564`[1] * 100
    }
    
    alf_65 <- if (input$relatorio_municipio == "Paraná") {
      mean(dados_indicadores$`tx_alf_ac65`, na.rm = TRUE) * 100
    } else {
      indicadores_filtrados$`tx_alf_ac65`[1] * 100
    }
    
    trata_na <- function(x) {
      if (is.na(x) || length(x) == 0) {
        "Não disponível"
      } else {
        format(round(x, 2), nsmall = 2)
      }
    }
    
    HTML(paste0(
      "<p style='font-size:18px;'><b>Total de  registradas:</b> ", total_quedas, "</p>",
      "<hr>",
      "<p style='font-size:18px;'><b>Indicadores Gerais:</b></p>",
      "<ul>",
      "<li><b>Densitometria (APAC/10.000 hab):</b> ", trata_na(densito), "</li>",
      "<li><b>Cobertura da ESF (%):</b> ", trata_na(esf), "</li>",
      "<li><b>Plano de Saúde (%):</b> ", trata_na(plano), "</li>",
      "<li><b>Alfabetização (55–64 anos) (%):</b> ", trata_na(alf_5564), "</li>",
      "<li><b>Alfabetização (65+ anos) (%):</b> ", trata_na(alf_65), "</li>",
      "</ul>"
    ))
  })
  
  
}

shinyApp(ui, server)