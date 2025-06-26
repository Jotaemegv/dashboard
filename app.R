library(shiny)
library(shinydashboard)
library(ggplot2)
library(grid) 
library(dplyr)
library(sf)
library(readr)
library(leaflet)
library(rmarkdown)
library(shinycssloaders)
library(geobr)
library(tidyr)
library(stringr)
library(pagedown)
library(webshot2)


# === LEITURA DOS DADOS ===
shape_pr <- st_read("PR_Municipios_2023.shp") %>%
  st_transform(crs = 4326) %>%
  mutate(CD_MUN_6 = substr(as.character(CD_MUN), 1, 6))  # Truncar para 6 dígitos
bbox <- st_bbox(shape_pr)

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
                menuItem("Relatório", tabName = "relatorio", icon = icon("file-alt"))
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
                         style = "height: 400px;",
                         div(
                           style = "height: 100%; display: flex; flex-direction: column; justify-content: space-between;",
                           
                           # Seletor de MUNICÍPIO
                           div(
                             style = "margin-bottom: 5px;",
                             selectInput(
                               "filtro_municipio", "Escolha um município:",
                               choices   = c("Paraná", shape_pr$NM_MUN),
                               selected  = "Paraná"
                             )
                           ),
                           
                           # Seletor de ANO  (movido para cá)
                           div(
                             style = "margin-bottom: 5px;",
                             selectInput(
                               "ano_grafico1", "Ano:",
                               choices   = c("Todos", 2010:2021),
                               selected  = "Todos"
                             )
                           ),
                           
                           # Mapa
                           withSpinner(
                             leafletOutput("mapa_dinamico", height = "210px")
                           )
                         )
                       ),
                       box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Incidência por Faixa Etária e Sexo",
                         style = "height: 420px; padding-bottom: 0;",
                         div(
                           style = "height: 100%; display: flex; flex-direction: column; justify-content: flex-start;",
                           withSpinner(
                             plotOutput("grafico_1", height = "400px")
                           )
                         )
                       )
                       
                ),
                
                column(width = 6,
                       
                       ##################################################################
                       ## NOVO LAYOUT: 3 BOXES QUADRADAS AZUIS ACIMA DO GRÁFICO ÓBITOS ##
                       ##################################################################
                       fluidRow(
                         box(
                           width = 4,
                           title = "Número total de fraturas",
                           status = NULL,
                           solidHeader = FALSE,
                           background = NULL,
                           collapsible = FALSE,
                           style = "border:none; box-shadow:none; padding-top: 5px; padding-bottom: 5px;",  # padding vertical
                           
                           div(
                             style = "height: 100px; display: flex; justify-content: center; align-items: center; color: #F6A31B; font-size: 40px; font-weight: bold; margin: 5px 0;",
                             textOutput("box1_val")
                           )
                         )
                         
                         
                         
                         ,
                         
                         # ── dentro do fluidRow das KPIs (segunda posição) ──────────────────────────
                         box(
                           width = 4,
                           title = "Percentual de Óbitos",
                           solidHeader = FALSE,
                           status = NULL,
                           background = NULL,
                           collapsible = FALSE,
                           style = "border:none; box-shadow:none; padding-top: 8px; height: 120px;",
                           
                           div(
                             style = "height: 100%; display: flex; justify-content: center; align-items: center; margin: 0; padding: 0;",
                             plotOutput("box2_donut", height = "120px", width = "120px")
                           )
                         )
                         ,
                         box(
                           width = 4,
                           title = tags$div(
                             "Faixa etária mais acometida",
                             style = "
      width: 180px;       /* aumenta largura para caber 2 linhas */
      margin: 0 auto;     /* centraliza */
      line-height: 1.1;   /* compacta linhas */
      font-weight: bold;
      font-size: 16px;
      white-space: normal; /* força quebra */
      overflow-wrap: break-word;
    "
                           ),
                           solidHeader = FALSE,
                           status = NULL,
                           background = NULL,
                           collapsible = FALSE,
                           style = "border:none; box-shadow:none; padding-top: 8px; height: 120px;",
                           
                           div(
                             style = "height: 100%; display: flex; flex-direction: column; justify-content: center; align-items: center; margin: 0; padding: 0;",
                             
                             div(
                               style = "color: #F6A31B; font-weight: bold; font-size: 30px; text-align: center;",
                               textOutput("box3_val")
                             )
                           )
                         )
                         
                         
                       ),
                       ##################################################################
                       
                       box(
                         width = 12,
                         solidHeader = TRUE,
                         title = "Óbitos por Faixa Etária, Sexo e Tratamento",
                         style = "height: 350px;",
                         div(
                           style = "height: 100%; display: flex; flex-direction: column; justify-content: center;",
                           withSpinner(plotOutput("grafico_direita", height = "340px"))
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
      
      # --- Aba Relatório ---
      tabItem(tabName = "relatorio",
              fluidRow(
                column(width = 6,
                       selectInput("relatorio_municipio", "Município:",
                                   choices = c("Paraná", unique(shape_pr$NM_MUN)),
                                   selected = "Paraná")
                ),
    
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
                column(
                  width = 12,
                  
                  # População idosa × Fraturas
                  box(
                    width = 12, solidHeader = TRUE,
                    title = "População ≥60 anos e Fraturas de Fêmur (2010–2021)",
                    withSpinner(plotOutput("grafico_pop_fraturas", height = "350px"))
                  ),
                  
                  # Densitometrias
                  box(
                    width = 12, solidHeader = TRUE,
                    title = "Densitometrias (2010–2021)",
                    withSpinner(plotOutput("grafico_densito", height = "350px"))
                  )
                )
              ),
              fluidRow(
                box(
                  width  = 3, solidHeader = TRUE, status = "primary",
                  title  = "Densitometria (APAC/10 mil hab)",
                  style  = "height:120px; display:flex; align-items:center; justify-content:center;",
                  h3(textOutput("box_densito"))
                ),
                box(
                  width  = 3, solidHeader = TRUE, status = "primary",
                  title  = "Cobertura ESF (%)",
                  style  = "height:120px; display:flex; align-items:center; justify-content:center;",
                  h3(textOutput("box_esf"))
                ),
                box(
                  width  = 3, solidHeader = TRUE, status = "primary",
                  title  = "Plano de Saúde (%)",
                  style  = "height:120px; display:flex; align-items:center; justify-content:center;",
                  h3(textOutput("box_plano"))
                ),
                box(
                  width  = 3, solidHeader = TRUE, status = "primary",
                  title  = "Alfabetização (%)",
                  style  = "height:120px; display:flex; align-items:center; justify-content:center;",
                  h3(textOutput("box_alf"))
                )
              ),
      )
    )
  )
)


# === SERVER
server <- function(input, output, session) {
  municipio_selecionado <- reactiveVal(NULL)
  
  # ——— 1. Pré-processamento de fluxos incluindo ANO_CMPT ———
  # ——— Pré-processamento de fluxos incluindo todas as transferências ———
  dados <- readr::read_csv("BANCO_FINAL_AIHs_CLASSIFICADAS(1).csv", col_types = readr::cols())
  
  # 1) filtra apenas casos classificados como transferência interna ("E") ou externa ("UE")
  dados_transf <- dados %>%
    dplyr::filter(COD_ID %in% c("E", "UE"))
  
  # 2) separa múltiplos destinos, mantendo também os simples
  dados_long <- dados_transf %>%
    tidyr::separate_rows(MUNIC_MOV, sep = ",") %>%
    dplyr::mutate(
      MUNIC_MOV = stringr::str_trim(as.character(MUNIC_MOV)),
      MUNIC_RES = as.character(MUNIC_RES)
    )
  
  # 3) agrega número de transferências por ano, origem e destino
  fluxos_ag <- dados_long %>%
    dplyr::group_by(ANO_CMPT, MUNIC_RES, MUNIC_MOV) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")
  
  # 4) calcula centróides dos municípios
  centroids <- shape_pr %>%
    sf::st_centroid() %>%
    dplyr::mutate(code = CD_MUN_6) %>%
    sf::st_coordinates() %>%
    as.data.frame() %>%
    dplyr::bind_cols(code = shape_pr$CD_MUN_6) %>%
    dplyr::rename(lon = X, lat = Y)
  
  # 5) junta coordenadas de origem e destino
  fluxos_coords <- fluxos_ag %>%
    dplyr::left_join(centroids, by = c("MUNIC_RES" = "code")) %>%
    dplyr::rename(orig_lon = lon, orig_lat = lat) %>%
    dplyr::left_join(centroids, by = c("MUNIC_MOV" = "code")) %>%
    dplyr::rename(dest_lon = lon, dest_lat = lat) %>%
    dplyr::filter(!is.na(orig_lon) & !is.na(dest_lon))
  
  # 6) normaliza espessura (1 a 5)
  min_c <- min(fluxos_coords$count); max_c <- max(fluxos_coords$count)
  fluxos_coords <- fluxos_coords %>%
    dplyr::mutate(weight = if (min_c == max_c) 3 else 1 + (count - min_c)/(max_c - min_c)*4)
  
  # 7) constrói geometria LINESTRING e gera o sf final
  linhas <- lapply(seq_len(nrow(fluxos_coords)), function(i) {
    sf::st_linestring(matrix(
      c(fluxos_coords$orig_lon[i], fluxos_coords$orig_lat[i],
        fluxos_coords$dest_lon[i], fluxos_coords$dest_lat[i]),
      ncol = 2, byrow = TRUE
    ))
  })
  fluxos_sf <- sf::st_sf(fluxos_coords, geometry = sf::st_sfc(linhas, crs = 4326))
  
  
  
  # ——— 2. Reactive para filtrar por ano e município ———
  fluxos_filtrados <- reactive({
    df <- fluxos_sf
    
    # filtrar por ano se não for "Todos"
    if (input$ano_grafico1 != "Todos") {
      ano_sel <- as.numeric(input$ano_grafico1)
      df <- df %>% dplyr::filter(ANO_CMPT == ano_sel)
    }
    # filtrar saídas do município selecionado
    if (!is.null(municipio_selecionado())) {
      df <- df %>% dplyr::filter(MUNIC_RES == municipio_selecionado())
    }
    df
  })
  
  
  # ——— 3. Renderizar mapa com fluxos filtrados ———
  output$leaflet_mapa <- renderLeaflet({
    mapa <- leaflet(options = leafletOptions(minZoom = 6, maxZoom = 12)) %>%
      addProviderTiles("CartoDB.Positron") %>%
      # todos em cinza, não clicáveis
      addPolygons(
        data        = shape_pr,
        fillColor   = "#DDDDDD",
        color       = "#FFFFFF",
        weight      = 0.5,
        fillOpacity = 0.8,
        options     = pathOptions(clickable = FALSE)
      )
    
    # destacar e dar zoom no município selecionado
    if (!is.null(municipio_selecionado())) {
      muni_sel <- shape_pr %>% filter(CD_MUN_6 == municipio_selecionado())
      centro   <- st_coordinates(st_centroid(muni_sel))
      mapa <- mapa %>%
        addPolygons(
          data        = muni_sel,
          fillColor   = "#025959",
          color       = "#000000",
          weight      = 2,
          fillOpacity = 0.6,
          highlight   = highlightOptions(
            weight      = 3,
            color       = "#333",
            fillOpacity = 0.7,
            bringToFront= TRUE
          )
        ) %>%
        setView(lng = centro[1], lat = centro[2], zoom = 10)
    }
    
    # adicionar as linhas de fluxo filtradas
    mapa %>%
      addPolylines(
        data      = fluxos_filtrados(),
        weight    = ~weight,
        color     = "blue",
        opacity   = 0.6,
        dashArray = "5,5",
        group     = "fluxos"
      )
  })

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
    if (is.null(municipio_selecionado())) {
      leaflet(data = shape_pr, options = leafletOptions(minZoom = 6, maxZoom = 9)) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setMaxBounds(
          lng1 = -55.5, lat1 = -27.7,
          lng2 = -48.0, lat2 = -22.5
        ) %>%
        addPolygons(
          fillColor = "#025959", weight = 1, opacity = 1, color = "white",
          dashArray = "3", fillOpacity = 0.7,
          highlight = highlightOptions(weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE),
          label = ~NM_MUN, layerId = ~CD_MUN,
          labelOptions = labelOptions(style = list("font-weight" = "normal"), textsize = "13px")
        )
      
      
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
        size = 5,
        color = "#F6A31B",
        fontface = "bold"
      ) +
      scale_fill_manual(values = c("Feminino" = "#027373", "Masculino" = "#025959")) +
      labs(
        title = ifelse(input$ano_grafico1 == "Todos",
                       "",
                       paste("Fraturas de Fêmur por Faixa Etária e Sexo -", input$ano_grafico1)),
        x = "Faixa Etária", y = "Número de Fraturas",
        fill = "Sexo"
      ) +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 14, color = "black"),
        axis.title = element_text(size = 16, color = "black"),
        legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 16, color = "black"),
        plot.title = element_text(size = 18, color = "black")
      )
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
    
    output$box1_val <- renderText({
  
      df <- read.csv("BANCO_FINAL_AIHs_CLASSIFICADAS(1).csv", encoding = "latin1")
      
      # filtro de ano
      if (input$ano_grafico1 != "Todos") {
        df <- dplyr::filter(df, ANO_CMPT == as.numeric(input$ano_grafico1))
      }
      
      # filtro de município
      if (!is.null(municipio_selecionado())) {
        df <- dplyr::filter(df, MUNIC_RES == as.numeric(municipio_selecionado()))
      }
      
      # retorna o total formatado
      format(nrow(df), big.mark = ".")
    })
    
    # ---------------------------------------------------------------------------
    #  SEGUNDA “BOX” – Donut com a porcentagem de óbitos no centro
    # ---------------------------------------------------------------------------
    output$box2_donut <- renderPlot({
      
      ## --- filtros (mesmo código) ---
      df <- read.csv("BANCO_FINAL_AIHs_CLASSIFICADAS(1).csv", encoding = "latin1")
      if (input$ano_grafico1 != "Todos") {
        df <- subset(df, ANO_CMPT == as.numeric(input$ano_grafico1))
      }
      if (!is.null(municipio_selecionado())) {
        df <- subset(df, MUNIC_RES == as.numeric(municipio_selecionado()))
      }
      
      total_fraturas <- nrow(df)
      total_obitos   <- sum(df$MORTE == "OBITO", na.rm = TRUE)
      
      if (total_fraturas == 0) {
        grid::grid.newpage()
        grid::grid.text("0%", gp = grid::gpar(fontsize = 16, fontface = "bold"))
        return()
      }
      
      perc_obitos <- total_obitos / total_fraturas * 100
      
      ## --- donut (mesmo código) ---
      donut_df <- data.frame(
        grupo = factor(c("Óbitos", "Demais"), levels = c("Óbitos", "Demais")),
        valor = c(total_obitos, total_fraturas - total_obitos)
      )
      
      g <- ggplot2::ggplot(donut_df, aes(x = 2, y = valor, fill = grupo)) +
        ggplot2::geom_col(width = 1, colour = NA) +
        ggplot2::coord_polar(theta = "y") +
        ggplot2::xlim(0.5, 2.5) +
        ggplot2::scale_fill_manual(values = c("Óbitos" = "#F6A31B",
                                              "Demais" = "#012340")) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "none")
      
      print(g)   # desenha o donut
      
      ## --- texto central: SOMENTE a porcentagem ---
      grid::grid.text(
        sprintf("%.1f%%", round(perc_obitos, 1)),
        x = 0.5, y = 0.5,
        gp = grid::gpar(fontsize = 20, fontface = "bold", col = "#F6A31B")
      )
      
      
    }, bg = "transparent")
    
    
    
    output$box3_val <- renderText({
      df <- read.csv("BANCO_FINAL_AIHs_CLASSIFICADAS(1).csv", encoding = "latin1")
      
      # filtro pelo ano selecionado
      if (input$ano_grafico1 != "Todos") {
        df <- subset(df, ANO_CMPT == as.numeric(input$ano_grafico1))
      }
      
      # filtro pelo município selecionado (se houver)
      if (!is.null(municipio_selecionado())) {
        df <- subset(df, MUNIC_RES == as.numeric(municipio_selecionado()))
      }
      
      # cria a variável FaixaEtaria
      df$FaixaEtaria <- with(df, ifelse(IDADE >= 60 & IDADE <= 69, "60-69",
                                        ifelse(IDADE >= 70 & IDADE <= 79, "70-79",
                                               ifelse(IDADE >= 80 & IDADE <= 89, "80-89",
                                                      ifelse(IDADE >= 90,              "90+",  NA)))))
      
      # remove linhas fora das faixas
      df <- subset(df, !is.na(FaixaEtaria))
      
      if (nrow(df) == 0) return("Sem dados")
      
      # conta e pega a faixa com maior frequência
      tab <- sort(table(df$FaixaEtaria), decreasing = TRUE)
      faixa_max <- names(tab)[1]
      total_max <- as.integer(tab[1])
      
      paste0(faixa_max, " (", format(total_max, big.mark = "."), ")")
    })
    
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
        aes(label = obitos), 
        position = position_dodge(width = 0.9),
        vjust = -0.5,
        size = 4,
        fontface = "bold",
        color = "#F6A31B"  # Laranja
      ) +
      scale_fill_manual(values = c(
        "Óbito ♂ Operados" = "#027373",
        "Óbito ♂ Não Operados" = "#81A1C1",
        "Óbito ♀ Operadas" = "#014F86",
        "Óbito ♀ Não Operadas" = "#A9CCE3"
      )) +
      theme_minimal() +
      theme(
        legend.position = "bottom",         # legenda embaixo
        legend.direction = "horizontal",    # legenda horizontal
        legend.title = element_text(size = 14, face = "bold"),
        legend.text = element_text(size = 13, face = "bold", color = "black"),
        legend.background = element_blank(),
        legend.key = element_blank(),
        legend.box = "wrap"                  # permite que a legenda quebre em mais de uma linha
      ) +
      guides(fill = guide_legend(nrow = 2, byrow = TRUE))  # força 2 linhas na legenda
    
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
    
    if (input$ano_grafico1 != "Todos") {
      df <- df %>% filter(ANO_CMPT == as.numeric(input$ano_grafico1))
    }
    
    df <- df %>% filter(COD_ID == "UE")
    
    if (!is.null(municipio_selecionado())) {
      df <- df %>% filter(MUNIC_RES == as.numeric(municipio_selecionado()))
    }
    
    if (nrow(df) == 0) {
      return(div(
        style = "height:300px; display:flex; align-items:center; justify-content:right; font-size:16px; font-weight:bold;color:#000000",
        "Nenhuma transferência"
      ))
    }
    
    leafletOutput("leaflet_mapa", height = "473px")
  })
  
  output$relatorio_completo <- renderUI({
    df_fraturas <- read.csv("BANCO_FINAL_AIHs_CLASSIFICADAS(1).csv", encoding = "latin1")
    dados_indicadores <- read.csv("gwrt.csv", encoding = "latin1")
    
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
    
    HTML(paste0(
      "<p style='font-size:18px;'><b>Total de  registradas:</b> ", total_quedas, "</p>",
      "<hr>",
      "</ul>"
    ))
  })
  
  
  # ───────────────────────────────────────────────────────────────────────
  #  REACTIVE único com todos os dados combinados (mesma lógica de antes)
  # ───────────────────────────────────────────────────────────────────────
  dados_completos_rel <- reactive({
    # ---- leitura das bases ----
    fraturas     <- read.csv("BANCO_FINAL_AIHs_CLASSIFICADAS(1).csv", encoding = "latin1") %>%
      mutate(MUNIC_RES = as.character(MUNIC_RES))
    indicadores  <- read.csv("tabela_STC_txDensito10k_PR_2010_21_ac60anos.csv", encoding = "latin1") %>%
      mutate(MUNIC_RES = as.character(MUNIC_RES))
    
    # ---- município selecionado ----
    if (input$relatorio_municipio != "Paraná") {
      cod_mun <- shape_pr %>%
        filter(NM_MUN == input$relatorio_municipio) %>%
        pull(CD_MUN_6) %>% as.character()
      fraturas    <- fraturas    %>% filter(MUNIC_RES == cod_mun)
      indicadores <- indicadores %>% filter(MUNIC_RES == cod_mun)
    }
    
    # ---- contagem de fraturas ----
    dados_fraturas <- fraturas %>%
      filter(!is.na(ANO_CMPT)) %>%
      group_by(ANO_CMPT) %>%
      summarise(fraturas = n(), .groups = "drop")
    
    # ---- indicadores: população idosa (÷1000) e densitometrias ----
    dados_ind <- indicadores %>%
      group_by(ANO_CMPT) %>%
      summarise(
        pop_idosa     = sum(pop_ac60 , na.rm = TRUE) / 1000,
        densitometria = sum(n_densito, na.rm = TRUE),
        .groups = "drop"
      )
    
    # ---- junção final ----
    full_join(dados_fraturas, dados_ind, by = "ANO_CMPT") %>%
      replace_na(list(fraturas = 0, pop_idosa = 0, densitometria = 0)) %>%
      arrange(ANO_CMPT)
  })
  
  # ───────────────────────────────────────────────────────────────────────
  #  GRÁFICO 1 — População idosa  ×  Fraturas
  # ───────────────────────────────────────────────────────────────────────
  output$grafico_pop_fraturas <- renderPlot({
    dados <- dados_completos_rel()
    if (nrow(dados) == 0) {
      ggplot() + annotate("text", x = 1, y = 1, label = "Sem dados") + theme_void()
    } else {
      dados %>%
        select(ANO_CMPT, pop_idosa, fraturas) %>%
        pivot_longer(-ANO_CMPT, names_to = "variavel", values_to = "valor") %>%
        mutate(
          variavel = factor(variavel,
                            levels = c("pop_idosa", "fraturas"),
                            labels  = c("População ≥60 anos (milhares)", "Fraturas de Fêmur"))
        ) %>%
        ggplot(aes(x = ANO_CMPT, y = valor, color = variavel)) +
        geom_line(size = 1.2) + geom_point(size = 3) +
        scale_color_manual(values = c("#012340", "#014F86")) +
        scale_x_continuous(breaks = 2010:2021) +
        scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
        labs(x = "Ano", color = NULL) +
        theme_minimal(base_size = 14) +
        theme(legend.position = "bottom",
              panel.grid.minor = element_blank())
    }
  })
  
  # ───────────────────────────────────────────────────────────────────────
  #  GRÁFICO 2 — Densitometrias
  # ───────────────────────────────────────────────────────────────────────
  output$grafico_densito <- renderPlot({
    dados <- dados_completos_rel()
    if (nrow(dados) == 0) {
      ggplot() + annotate("text", x = 1, y = 1, label = "Sem dados") + theme_void()
    } else {
      ggplot(dados, aes(x = ANO_CMPT, y = densitometria)) +
        geom_line(size = 1.2, color = "#027373") +
        geom_point(size = 3,  color = "#027373") +
        scale_x_continuous(breaks = 2010:2021) +
        scale_y_continuous(labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
        labs(x = "Ano", y = "Densitometrias") +
        theme_minimal(base_size = 14) +
        theme(panel.grid.minor = element_blank())
    }
  })
  
  
  # Função utilitária de formatação
  trata_na <- function(x, casas = 2) {
    if (is.na(x) || length(x) == 0) "N/D"
    else format(round(x, casas), nsmall = casas, big.mark = ".",
                decimal.mark = ",")
  }
  
  # ───────────────────────────────────────────────────────────────────
  # Reactive: calcula os quatro indicadores (reusa a mesma regra do relatório)
  indicadores_box <- reactive({
    dados_indicadores <- read.csv("gwrt.csv", encoding = "latin1")
    
    if (input$relatorio_municipio != "Paraná") {
      cd_mun <- shape_pr %>%
        filter(NM_MUN == input$relatorio_municipio) %>%
        pull(CD_MUN_6)
      ind <- dados_indicadores %>% filter(MUNIC_RES == as.numeric(cd_mun))
    } else {
      ind <- dados_indicadores
    }
    
    list(
      densito = if (input$relatorio_municipio == "Paraná")
        mean(ind$tx_apac_densito10k, na.rm = TRUE)
      else ind$tx_apac_densito10k[1],
      esf     = if (input$relatorio_municipio == "Paraná")
        mean(ind$cob_esf, na.rm = TRUE) * 100
      else ind$cob_esf[1] * 100,
      plano   = if (input$relatorio_municipio == "Paraná")
        mean(ind$tpi, na.rm = TRUE) * 100
      else ind$tpi[1] * 100,
      alf     = if (input$relatorio_municipio == "Paraná")
        mean(ind$txalfabet_2022, na.rm = TRUE) * 100
      else ind$txalfabet_2022[1] * 100
    )
  })
  
  # ───────────────────────────────────────────────────────────────────
  # Saídas das quatro boxes
  output$box_densito <- renderText(trata_na(indicadores_box()$densito))
  output$box_esf     <- renderText(trata_na(indicadores_box()$esf))
  output$box_plano   <- renderText(trata_na(indicadores_box()$plano))
  output$box_alf     <- renderText(trata_na(indicadores_box()$alf))
  
}

shinyApp(ui, server)