library(shiny)
library(bslib)
library(shinycssloaders)
library(leaflet)
library(dplyr)
library(stringi)
library(htmlwidgets)
library(webshot2)
library(htmltools)
library(DT)

options(scipen = 999)

# Configuración
LAT_MIN <- 47
LAT_MAX <- 56
LON_MIN <- 5
LON_MAX <- 16

TSV_PATH <- "~/Desktop/toponimos_de_final.tsv"

# -------------------------------
# ANÁLISIS DINÁMICO DE SUFIJOS
# -------------------------------

# Diccionario de sufijos conocidos (para interpretación)
sufijos_conocidos <- list(
  # Asentamientos
  "dorf" = list(tipo = "asentamiento", significado = "pueblo/aldea"),
  "burg" = list(tipo = "asentamiento", significado = "fortaleza/castillo"),
  "heim" = list(tipo = "asentamiento", significado = "hogar"),
  "hausen" = list(tipo = "asentamiento", significado = "casas (alto alemán)"),
  "husen" = list(tipo = "asentamiento", significado = "casas (bajo alemán)"),
  "stadt" = list(tipo = "asentamiento", significado = "ciudad"),
  "stedt" = list(tipo = "asentamiento", significado = "lugar (bajo alemán)"),

  # Geografía
  "berg" = list(tipo = "geografía", significado = "montaña/colina"),
  "tal" = list(tipo = "geografía", significado = "valle"),
  "bach" = list(tipo = "geografía", significado = "arroyo"),
  "furt" = list(tipo = "geografía", significado = "vado"),
  "brunn" = list(tipo = "geografía", significado = "fuente"),
  "born" = list(tipo = "geografía", significado = "fuente (norte)"),
  "brück" = list(tipo = "geografía", significado = "puente"),

  # Vegetación
  "wald" = list(tipo = "vegetación", significado = "bosque"),
  "feld" = list(tipo = "vegetación", significado = "campo"),
  "holz" = list(tipo = "vegetación", significado = "madera/bosque"),
  "au" = list(tipo = "vegetación", significado = "pradera ribereña"),
  "rode" = list(tipo = "vegetación", significado = "claro/rozado"),

  # Eslavos
  "itz" = list(tipo = "eslavo", significado = "sufijo eslavo"),
  "ow" = list(tipo = "eslavo", significado = "sufijo eslavo"),
  "in" = list(tipo = "eslavo", significado = "sufijo eslavo"),
  "witz" = list(tipo = "eslavo", significado = "sufijo eslavo -ovice"),

  # Patronímicos
  "ingen" = list(tipo = "patronímico", significado = "gente de (suabo)"),
  "ing" = list(tipo = "patronímico", significado = "gente de (bávaro)"),
  "ungen" = list(tipo = "patronímico", significado = "gente de")
)

analizar_sufijo <- function(toponimo_busqueda, datos) {
  # Analiza un sufijo específico en el conjunto de datos
  # Devuelve estadísticas solo si se está buscando un sufijo

  # Limpiar búsqueda
  busqueda <- tolower(trimws(toponimo_busqueda))

  if (!nzchar(busqueda) || nchar(busqueda) < 2) {
    return(NULL)
  }

  # Verificar si la búsqueda podría ser un sufijo
  es_sufijo <- FALSE
  sufijo_detectado <- NULL

  for (suf in names(sufijos_conocidos)) {
    if (busqueda == suf || grepl(paste0(suf, "$"), busqueda)) {
      es_sufijo <- TRUE
      sufijo_detectado <- suf
      break
    }
  }

  if (!es_sufijo) {
    return(NULL)
  }

  # Realizar análisis del sufijo
  info <- sufijos_conocidos[[sufijo_detectado]]

  # Extraer Bundesland
  datos$bundesland <- gsub(".*\\((..)\\)$", "\\1", datos$provincia)

  # Análisis geográfico
  dist_bundesland <- sort(table(datos$bundesland), decreasing = TRUE)

  # Densidad por región
  densidad <- data.frame(
    bundesland = names(dist_bundesland),
    n = as.numeric(dist_bundesland),
    stringsAsFactors = FALSE
  )

  # Determinar región predominante
  region_principal <- if (nrow(densidad) > 0) {
    bl_top <- densidad$bundesland[1]

    if (bl_top %in% c("SH", "HH", "NI", "HB", "MV")) {
      "Norte (Bajo alemán)"
    } else if (bl_top %in% c("BY")) {
      "Sur (Bávaro)"
    } else if (bl_top %in% c("BW", "SL")) {
      "Suroeste (Alemánico)"
    } else if (bl_top %in% c("SN", "ST", "BB", "MV")) {
      "Este (posible influencia eslava)"
    } else if (bl_top %in% c("NW", "RP", "HE")) {
      "Oeste (Alto alemán central)"
    } else if (bl_top %in% c("TH")) {
      "Centro (Franconio)"
    } else {
      "Distribuido"
    }
  } else {
    "Desconocido"
  }

  list(
    es_analisis_sufijo = TRUE,
    sufijo = sufijo_detectado,
    tipo = info$tipo,
    significado = info$significado,
    n_total = nrow(datos),
    dist_bundesland = densidad,
    region_principal = region_principal
  )
}

# -------------------------------
# Helpers
# -------------------------------
norm_txt <- function(x, strip_accents = TRUE){
  x <- tolower(trimws(as.character(x)))
  if(strip_accents) x <- stringi::stri_trans_general(x, "Latin-ASCII")
  x
}

parse_num <- function(x){
  x <- gsub(",", ".", trimws(as.character(x)), fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

load_data <- function(){
  df <- read.delim(
    TSV_PATH,
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    encoding = "UTF-8"
  )

  df %>%
    mutate(
      lon = parse_num(lon),
      lat = parse_num(lat)
    ) %>%
    filter(
      !is.na(lon), !is.na(lat),
      lat >= LAT_MIN, lat <= LAT_MAX,
      lon >= LON_MIN, lon <= LON_MAX
    )
}

# -------------------------------
# UI
# -------------------------------
ui <- page_fluid(

  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#003366",
    base_font = font_google("Source Sans Pro")
  ),

  tags$style(HTML("
    .uva-header {
      display:flex;
      align-items:center;
      gap:20px;
      padding:12px 0;
      border-bottom:2px solid #003366;
      margin-bottom:18px;
    }
    .uva-logo {
      height:55px;
    }
    .uva-title {
      font-size:26px;
      font-weight:600;
      color:#003366;
    }
    .analisis-panel {
      background: #f8f9fa;
      border-left: 4px solid #003366;
      padding: 15px;
      margin: 15px 0;
      border-radius: 4px;
    }
    .analisis-title {
      font-size: 18px;
      font-weight: 600;
      color: #003366;
      margin-bottom: 10px;
    }
    .stat-box {
      display: inline-block;
      background: white;
      padding: 10px 15px;
      margin: 5px;
      border-radius: 4px;
      border: 1px solid #dee2e6;
    }
    .stat-label {
      font-size: 12px;
      color: #666;
      text-transform: uppercase;
    }
    .stat-value {
      font-size: 24px;
      font-weight: 600;
      color: #003366;
    }
    .footer-uv {
      text-align:center;
      color:#666;
      font-size:12px;
      padding:18px 0;
      border-top:1px solid #ddd;
      margin-top:40px;
    }
  ")),

  tags$div(
    class = "uva-header",
    tags$img(
      src = "https://imagencorporativa.uva.es/.marca_principal_horizontal/AZUL-P654C/logo-pantone-654.png",
      class = "uva-logo"
    ),
    tags$div(
      class = "uva-title",
      "Análisis toponímico de Alemania"
    )
  ),

  layout_sidebar(

    sidebar = sidebar(
      width = 350,

      h4("🔍 Búsqueda"),

      textInput(
        "q",
        NULL,
        placeholder = "Topónimo o sufijo (ej: dorf, burg, bach...)"
      ),

      p(class = "text-muted small",
        "Busca un sufijo específico para ver análisis detallado"
      ),

      radioButtons(
        "mode",
        "Modo",
        choices = c("Contiene" = "contains", "Exacta" = "exact"),
        inline = TRUE
      ),

      checkboxInput("ignore_accents", "Ignorar acentos", TRUE),

      tags$hr(),

      h4("Mapa"),

      checkboxInput("cluster", "Agrupar puntos", TRUE),

      sliderInput(
        "max_points",
        "Máx. puntos a mostrar",
        min = 100,
        max = 11000,
        value = 11000,
        step = 100
      ),

      tags$hr(),

      downloadButton("download_map", "Descargar mapa", class = "btn-primary"),
      br(), br(),
      downloadButton("download_table", "Descargar datos (TSV)", class = "btn-outline-primary")
    ),

    navset_card_tab(

      nav_panel(
        "Mapa",

        uiOutput("analisis_panel"),

        withSpinner(
          leafletOutput("map", height = "65vh"),
          type = 4
        )
      ),

      nav_panel(
        "Estadísticas",

        conditionalPanel(
          condition = "output.hay_analisis",
          h4("📊 Análisis del sufijo"),
          uiOutput("analisis_detallado")
        ),

        hr(),

        h4("📍 Distribución geográfica"),
        plotOutput("plot_bundesland", height = "400px"),

        hr(),

        h4("📋 Tabla de datos"),
        DTOutput("tabla_datos")
      )
    )
  ),

  tags$div(
    class = "footer-uv",
    tags$b("GIR Filología Digital"),
    tags$br(),
    "Universidad de Valladolid • 2026"
  )
)

# -------------------------------
# SERVER
# -------------------------------
server <- function(input, output, session){

  data_full <- reactiveVal()

  observe({
    tryCatch({
      df <- load_data()
      data_full(df)
      cat(sprintf("✓ Datos cargados: %d topónimos\n", nrow(df)))
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = NULL)
    })
  })

  filtered <- reactive({
    req(data_full())
    data <- data_full()

    q_raw <- trimws(input$q)
    if (!nzchar(q_raw)) return(data)

    top <- norm_txt(data$toponimo, input$ignore_accents)
    q   <- norm_txt(q_raw, input$ignore_accents)

    keep <- if (input$mode == "exact") {
      top == q
    } else {
      grepl(q, top, fixed = TRUE)
    }

    data[keep, , drop = FALSE]
  })

  analisis_sufijo <- reactive({
    req(filtered())
    q_raw <- trimws(input$q)

    if (!nzchar(q_raw)) return(NULL)

    analizar_sufijo(q_raw, filtered())
  })

  output$hay_analisis <- reactive({
    !is.null(analisis_sufijo())
  })
  outputOptions(output, "hay_analisis", suspendWhenHidden = FALSE)

  map_data <- reactive({
    d <- filtered()
    if (nrow(d) > input$max_points) {
      d <- d[seq_len(input$max_points), ]
    }
    d
  })

  output$analisis_panel <- renderUI({
    analisis <- analisis_sufijo()

    if (is.null(analisis)) return(NULL)

    tags$div(
      class = "analisis-panel",
      tags$div(
        class = "analisis-title",
        sprintf("📚 Análisis del sufijo: -%s", analisis$sufijo)
      ),

      p(
        strong("Tipo:"), analisis$tipo, "•",
        strong("Significado:"), analisis$significado
      ),

      tags$div(
        tags$div(
          class = "stat-box",
          tags$div(class = "stat-label", "Total encontrado"),
          tags$div(class = "stat-value", format(analisis$n_total, big.mark = ","))
        ),
        tags$div(
          class = "stat-box",
          tags$div(class = "stat-label", "Región principal"),
          tags$div(class = "stat-value", style = "font-size: 16px;", analisis$region_principal)
        )
      )
    )
  })

  output$analisis_detallado <- renderUI({
    analisis <- analisis_sufijo()

    if (is.null(analisis)) return(NULL)

    top5 <- head(analisis$dist_bundesland, 5)

    tagList(
      p(
        strong(sprintf("El sufijo '-%s' se encuentra en %d topónimos", 
                      analisis$sufijo, analisis$n_total))
      ),
      p("Distribución por Bundesland (top 5):"),
      tags$ul(
        lapply(1:nrow(top5), function(i) {
          tags$li(sprintf("%s: %d topónimos (%.1f%%)", 
                         top5$bundesland[i], 
                         top5$n[i],
                         top5$n[i]/analisis$n_total*100))
        })
      ),
      p(strong("Región predominante:"), analisis$region_principal)
    )
  })

  output$map <- renderLeaflet({
    req(data_full())
    d <- map_data()

    m <- leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron)

    if (nrow(d) == 0) {
      return(m %>% setView(10.5, 51.2, 6))
    }

    popup <- paste0(
      "<b>", htmlEscape(d$toponimo), "</b><br>",
      htmlEscape(d$provincia)
    )

    if (input$cluster) {
      m <- m %>%
        addCircleMarkers(
          lng = d$lon, lat = d$lat,
          radius = 6,
          color = "#2171B5",
          fillColor = "#2171B5",
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 1,
          popup = popup,
          clusterOptions = markerClusterOptions()
        )
    } else {
      m <- m %>%
        addCircleMarkers(
          lng = d$lon, lat = d$lat,
          radius = 6,
          color = "#2171B5",
          fillColor = "#2171B5",
          fillOpacity = 0.7,
          stroke = TRUE,
          weight = 1,
          popup = popup
        )
    }

    m %>% fitBounds(
      min(d$lon, na.rm = TRUE),
      min(d$lat, na.rm = TRUE),
      max(d$lon, na.rm = TRUE),
      max(d$lat, na.rm = TRUE)
    )
  })

  output$plot_bundesland <- renderPlot({
    d <- filtered()

    if (nrow(d) == 0) return(NULL)

    d$bundesland <- gsub(".*\\((..)\\)$", "\\1", d$provincia)

    dist <- sort(table(d$bundesland), decreasing = TRUE)

    par(mar = c(4, 4, 3, 2))
    barplot(
      rev(dist),
      horiz = TRUE,
      las = 1,
      col = "#2171B5",
      border = NA,
      main = sprintf("Distribución por Bundesland (n=%s)", 
                    format(nrow(d), big.mark = ",")),
      xlab = "Número de topónimos"
    )
  })

  output$tabla_datos <- renderDT({
    d <- filtered() %>%
      mutate(bundesland = gsub(".*\\((..)\\)$", "\\1", provincia)) %>%
      select(toponimo, bundesland, provincia, lat, lon) %>%
      head(500)

    datatable(
      d,
      options = list(
        pageLength = 25,
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros"
        )
      ),
      filter = "top",
      rownames = FALSE
    )
  })

  output$download_map <- downloadHandler(
    filename = function() {
      q <- trimws(input$q)
      sufijo <- if (nzchar(q)) paste0("_", gsub("\\s+", "_", q)) else ""
      paste0("mapa_toponimos", sufijo, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      d <- map_data()
      m <- leaflet(d) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addCircleMarkers(~lon, ~lat, radius = 5, fillOpacity = 0.7)
      tmp <- tempfile(fileext = ".html")
      saveWidget(m, tmp, selfcontained = TRUE)
      webshot2::webshot(tmp, file, vwidth = 1400, vheight = 900, zoom = 2)
    }
  )

  output$download_table <- downloadHandler(
    filename = function() {
      q <- trimws(input$q)
      sufijo <- if (nzchar(q)) paste0("_", gsub("\\s+", "_", q)) else "_todos"
      paste0("toponimos", sufijo, "_", Sys.Date(), ".tsv")
    },
    content = function(file) {
      d <- filtered()
      write.table(d, file, sep = "\t", row.names = FALSE, 
                  quote = FALSE, fileEncoding = "UTF-8")
    }
  )
}

shinyApp(ui, server)
