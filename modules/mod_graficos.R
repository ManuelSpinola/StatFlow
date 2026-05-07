# ============================================================
# mod_graficos.R — EDA Gráfico: visualización exploratoria
# ============================================================

# ── Función auxiliar para selector de color ─────────────────
colourInput_simple <- function(inputId, label, value = "#1D9E75") {
  tags$div(
    class = "mb-3",
    tags$label(label, class = "form-label"),
    tags$input(
      type  = "color",
      id    = inputId,
      value = value,
      class = "form-control form-control-color"
    )
  )
}

# ── Notas didácticas ────────────────────────────────────────
notas_graficos <- list(
  histograma = list(
    ventaja    = "Familiar y fácil de leer. Muestra la frecuencia de valores en intervalos.",
    desventaja = "El resultado cambia según el número de intervalos (bins) elegido."
  ),
  densidad = list(
    ventaja    = "Suaviza la distribución y no depende de intervalos artificiales.",
    desventaja = "Puede ser difícil de interpretar: el eje Y muestra densidad, no frecuencia."
  ),
  boxplot = list(
    ventaja    = "Resume la distribución en 5 números y detecta valores atípicos fácilmente.",
    desventaja = "Oculta la forma real de la distribución — dos distribuciones muy distintas pueden tener el mismo boxplot."
  ),
  violin = list(
    ventaja    = "Combina la forma de la distribución con los datos individuales. Es el más informativo.",
    desventaja = "Menos familiar para personas sin experiencia en estadística."
  ),
  barras = list(
    ventaja    = "Fácil de leer. Ideal para comparar frecuencias entre categorías.",
    desventaja = "No muestra la distribución interna de los datos dentro de cada categoría."
  ),
  dispersion = list(
    ventaja    = "Muestra la relación entre dos variables numéricas y permite detectar patrones.",
    desventaja = "Con muchos datos los puntos se superponen y el patrón puede perderse."
  )
)

# ── UI ──────────────────────────────────────────────────────
mod_graficos_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),

      # ── Controles ──
      card(
        card_header("📊 Opciones del gráfico"),
        card_body(
          uiOutput(ns("sel_tipo")),
          hr(),
          uiOutput(ns("sel_variable_graf")),
          uiOutput(ns("sel_variable_y")),
          uiOutput(ns("sel_grupo_graf")),
          uiOutput(ns("slider_bins")),
          hr(),
          textInput(ns("titulo"), "Título (opcional)", placeholder = "Mi gráfico"),
          colourInput_simple(ns("color_principal"), "Color principal", value = "#1D9E75"),
          hr(),
          accordion(
            open = FALSE,
            accordion_panel(
              "📖 ¿Qué es el EDA gráfico?",
              p(class = "small",
                "El ", tags$strong("EDA gráfico"), " usa visualizaciones para explorar
                la distribución de variables y las relaciones entre ellas.
                Cada tipo de gráfico revela aspectos distintos de los datos.")
            )
          )
        )
      ),

      # ── Gráfico ──
      card(
        card_header("Resultado"),
        card_body(
          plotOutput(ns("grafico_principal"), height = "420px"),
          hr(),
          uiOutput(ns("nota_grafico")),
          hr(),
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tagList(bs_icon("code-slash"), " Código R reproducible"),
              downloadButton(
                ns("descargar_script"),
                label = "Descargar .R",
                icon  = bs_icon("download"),
                class = "btn-sm btn-outline-primary"
              )
            ),
            p(
              "Script que reproduce este gráfico con tus datos.",
              class = "text-muted small px-3 pt-2 mb-1"
            ),
            verbatimTextOutput(ns("codigo_r"))
          )
        )
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_graficos_server <- function(id, datos) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Columnas por tipo ──
    cols_num <- reactive({
      req(datos())
      names(datos())[map_lgl(datos(), is.numeric)]
    })
    cols_cat <- reactive({
      req(datos())
      names(datos())[!map_lgl(datos(), is.numeric)]
    })

    # ── Selector de tipo de gráfico ──
    output$sel_tipo <- renderUI({
      selectInput(
        ns("tipo_grafico"),
        "Tipo de gráfico:",
        choices = c(
          "── Una variable ──"   = "",
          "Histograma"           = "histograma",
          "Densidad"             = "densidad",
          "Boxplot"              = "boxplot",
          "Violin + puntos"      = "violin",
          "Barras (categórica)"  = "barras",
          "── Dos variables ──"  = "",
          "Dispersión (scatter)" = "dispersion"
        ),
        selected = "violin"
      )
    })

    # ── Variable X (principal) ──
    output$sel_variable_graf <- renderUI({
      req(input$tipo_grafico)
      if (input$tipo_grafico %in% c("histograma", "densidad", "boxplot", "violin", "dispersion")) {
        req(length(cols_num()) > 0)
        selectInput(ns("var_graf"), "Variable numérica:", choices = cols_num())
      } else if (input$tipo_grafico == "barras") {
        req(length(cols_cat()) > 0)
        selectInput(ns("var_graf"), "Variable categórica:", choices = cols_cat())
      }
    })

    # ── Variable Y (solo dispersión) ──
    output$sel_variable_y <- renderUI({
      req(input$tipo_grafico)
      if (input$tipo_grafico == "dispersion") {
        req(length(cols_num()) > 1)
        selectInput(
          ns("var_y"),
          "Variable Y:",
          choices  = cols_num(),
          selected = cols_num()[2]
        )
      }
    })

    # ── Variable de grupo ──
    output$sel_grupo_graf <- renderUI({
      req(input$tipo_grafico)
      if (input$tipo_grafico %in% c("boxplot", "violin", "dispersion") &&
          length(cols_cat()) > 0) {
        selectInput(
          ns("grupo_graf"),
          "Colorear por grupo (opcional):",
          choices = c("Ninguno" = "ninguno", cols_cat())
        )
      }
    })

    # ── Slider de bins (solo histograma) ──
    output$slider_bins <- renderUI({
      req(input$tipo_grafico)
      if (input$tipo_grafico == "histograma") {
        req(datos(), input$var_graf)
        x <- datos()[[input$var_graf]]
        sliderInput(
          ns("n_bins"),
          "Número de intervalos (bins):",
          min = 2, max = 30,
          value = nclass.Sturges(x),
          step = 1
        )
      }
    })

    # ── Color y título ──
    color_activo <- reactive({
      if (!is.null(input$color_principal) && nchar(input$color_principal) == 7)
        input$color_principal
      else
        colores$acento
    })

    titulo_activo <- reactive({
      if (!is.null(input$titulo) && nchar(trimws(input$titulo)) > 0)
        input$titulo
      else
        input$var_graf
    })

    # ── Tema base ──
    base_theme <- theme_minimal(base_size = 14) +
      theme(
        plot.title       = element_text(face = "bold", color = colores$primario),
        plot.background  = element_rect(fill = "transparent", color = NA),
        panel.background = element_rect(fill = "transparent", color = NA)
      )

    # ── Gráfico principal ──
    output$grafico_principal <- renderPlot({
      req(datos(), input$tipo_grafico, input$var_graf)
      df    <- datos()
      color <- color_activo()
      titulo <- titulo_activo()
      tipo  <- input$tipo_grafico

      if (tipo == "histograma") {
        bins <- if (!is.null(input$n_bins)) input$n_bins else nclass.Sturges(df[[input$var_graf]])
        ggplot(df, aes(x = .data[[input$var_graf]])) +
          geom_histogram(fill = color, color = "white", bins = bins, alpha = 0.85) +
          labs(title = titulo, x = input$var_graf, y = "Frecuencia") +
          base_theme

      } else if (tipo == "densidad") {
        ggplot(df, aes(x = .data[[input$var_graf]])) +
          geom_density(fill = color, color = colores$primario, alpha = 0.6) +
          labs(title = titulo, x = input$var_graf, y = "Densidad") +
          base_theme

      } else if (tipo == "boxplot") {
        grupo <- input$grupo_graf
        if (!is.null(grupo) && grupo != "ninguno") {
          ggplot(df, aes(x = .data[[grupo]], y = .data[[input$var_graf]],
                         fill = .data[[grupo]])) +
            geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.fill = "white") +
            geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
            scale_fill_brewer(palette = "Set2") +
            labs(title = titulo, x = grupo, y = input$var_graf) +
            base_theme + theme(legend.position = "none")
        } else {
          ggplot(df, aes(x = "", y = .data[[input$var_graf]])) +
            geom_boxplot(fill = color, alpha = 0.7, width = 0.4,
                         outlier.shape = 21, outlier.fill = "white") +
            labs(title = titulo, x = NULL, y = input$var_graf) +
            base_theme
        }

      } else if (tipo == "violin") {
        grupo <- input$grupo_graf
        if (!is.null(grupo) && grupo != "ninguno") {
          ggplot(df, aes(x = .data[[grupo]], y = .data[[input$var_graf]],
                         fill = .data[[grupo]])) +
            geom_violin(alpha = 0.6) +
            geom_jitter(width = 0.1, alpha = 0.5, size = 1.8) +
            scale_fill_brewer(palette = "Set2") +
            labs(title = titulo, x = grupo, y = input$var_graf) +
            base_theme + theme(legend.position = "none")
        } else {
          ggplot(df, aes(x = "", y = .data[[input$var_graf]])) +
            geom_violin(fill = color, color = colores$primario, alpha = 0.6) +
            geom_jitter(color = colores$primario, width = 0.08, alpha = 0.6, size = 1.8) +
            labs(title = titulo, x = NULL, y = input$var_graf) +
            base_theme
        }

      } else if (tipo == "barras") {
        df %>%
          count(.data[[input$var_graf]]) %>%
          ggplot(aes(x = reorder(.data[[input$var_graf]], n), y = n)) +
          geom_col(fill = color, alpha = 0.85) +
          coord_flip() +
          labs(title = titulo, x = NULL, y = "Frecuencia") +
          base_theme

      } else if (tipo == "dispersion") {
        req(input$var_y)
        grupo <- input$grupo_graf
        if (!is.null(grupo) && grupo != "ninguno") {
          ggplot(df, aes(x = .data[[input$var_graf]], y = .data[[input$var_y]],
                         color = .data[[grupo]])) +
            geom_point(alpha = 0.7, size = 2.5) +
            geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
            scale_color_brewer(palette = "Set2") +
            labs(title = titulo, x = input$var_graf, y = input$var_y) +
            base_theme
        } else {
          ggplot(df, aes(x = .data[[input$var_graf]], y = .data[[input$var_y]])) +
            geom_point(color = color, alpha = 0.7, size = 2.5) +
            geom_smooth(method = "lm", se = TRUE, fill = paste0(color, "40"),
                        color = colores$primario, linewidth = 0.8) +
            labs(title = titulo, x = input$var_graf, y = input$var_y) +
            base_theme
        }
      }
    }, bg = "transparent")

    # ── Nota didáctica ──
    output$nota_grafico <- renderUI({
      req(input$tipo_grafico)
      if (input$tipo_grafico == "") return(NULL)
      nota <- notas_graficos[[input$tipo_grafico]]
      if (is.null(nota)) return(NULL)
      tags$div(
        class = "small",
        style = paste0("border-left: 3px solid ", colores$acento, "; padding-left: 10px;"),
        tags$p(tags$strong("✅ Ventaja: "), nota$ventaja, class = "mb-1"),
        tags$p(tags$strong("⚠️ Limitación: "), nota$desventaja, class = "mb-0")
      )
    })

    # ── Código R reproducible ─────────────────────────────
    codigo_generado <- reactive({
      req(datos(), input$tipo_grafico, input$var_graf)
      tipo  <- input$tipo_grafico
      var_x <- input$var_graf
      color <- color_activo()
      tit   <- titulo_activo()
      grupo <- input$grupo_graf

      encabezado <- encabezado_script("StatFlow", "Gráficos")

      carga <- paste0(
        "library(tidyverse)\n\n",
        "# Cargá tus datos\n",
        "datos <- read.csv(\"tu_archivo.csv\")\n\n"
      )

      geom <- if (tipo == "histograma") {
        bins <- if (!is.null(input$n_bins)) input$n_bins else "nclass.Sturges(datos$`var`)"
        paste0(
          "ggplot(datos, aes(x = `", var_x, "`)) +\n",
          "  geom_histogram(fill = \"", color, "\", color = \"white\",\n",
          "                 bins = ", bins, ", alpha = 0.85) +\n",
          "  labs(title = \"", tit, "\", x = \"", var_x, "\", y = \"Frecuencia\") +\n",
          "  theme_minimal()\n"
        )
      } else if (tipo == "densidad") {
        paste0(
          "ggplot(datos, aes(x = `", var_x, "`)) +\n",
          "  geom_density(fill = \"", color, "\", alpha = 0.6) +\n",
          "  labs(title = \"", tit, "\", x = \"", var_x, "\", y = \"Densidad\") +\n",
          "  theme_minimal()\n"
        )
      } else if (tipo == "boxplot") {
        if (!is.null(grupo) && grupo != "ninguno") {
          paste0(
            "ggplot(datos, aes(x = `", grupo, "`, y = `", var_x, "`,\n",
            "                  fill = `", grupo, "`)) +\n",
            "  geom_boxplot(alpha = 0.7, outlier.shape = 21) +\n",
            "  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +\n",
            "  scale_fill_brewer(palette = \"Set2\") +\n",
            "  labs(title = \"", tit, "\", x = \"", grupo, "\", y = \"", var_x, "\") +\n",
            "  theme_minimal() + theme(legend.position = \"none\")\n"
          )
        } else {
          paste0(
            "ggplot(datos, aes(x = \"\", y = `", var_x, "`)) +\n",
            "  geom_boxplot(fill = \"", color, "\", alpha = 0.7, width = 0.4) +\n",
            "  labs(title = \"", tit, "\", x = NULL, y = \"", var_x, "\") +\n",
            "  theme_minimal()\n"
          )
        }
      } else if (tipo == "violin") {
        if (!is.null(grupo) && grupo != "ninguno") {
          paste0(
            "ggplot(datos, aes(x = `", grupo, "`, y = `", var_x, "`,\n",
            "                  fill = `", grupo, "`)) +\n",
            "  geom_violin(alpha = 0.6) +\n",
            "  geom_jitter(width = 0.1, alpha = 0.5, size = 1.8) +\n",
            "  scale_fill_brewer(palette = \"Set2\") +\n",
            "  labs(title = \"", tit, "\", x = \"", grupo, "\", y = \"", var_x, "\") +\n",
            "  theme_minimal() + theme(legend.position = \"none\")\n"
          )
        } else {
          paste0(
            "ggplot(datos, aes(x = \"\", y = `", var_x, "`)) +\n",
            "  geom_violin(fill = \"", color, "\", alpha = 0.6) +\n",
            "  geom_jitter(width = 0.08, alpha = 0.6, size = 1.8) +\n",
            "  labs(title = \"", tit, "\", x = NULL, y = \"", var_x, "\") +\n",
            "  theme_minimal()\n"
          )
        }
      } else if (tipo == "barras") {
        paste0(
          "datos |>\n",
          "  count(`", var_x, "`) |>\n",
          "  ggplot(aes(x = reorder(`", var_x, "`, n), y = n)) +\n",
          "  geom_col(fill = \"", color, "\", alpha = 0.85) +\n",
          "  coord_flip() +\n",
          "  labs(title = \"", tit, "\", x = NULL, y = \"Frecuencia\") +\n",
          "  theme_minimal()\n"
        )
      } else if (tipo == "dispersion") {
        req(input$var_y)
        var_y <- input$var_y
        if (!is.null(grupo) && grupo != "ninguno") {
          paste0(
            "ggplot(datos, aes(x = `", var_x, "`, y = `", var_y, "`,\n",
            "                  color = `", grupo, "`)) +\n",
            "  geom_point(alpha = 0.7, size = 2.5) +\n",
            "  geom_smooth(method = \"lm\", se = FALSE, linewidth = 0.8) +\n",
            "  scale_color_brewer(palette = \"Set2\") +\n",
            "  labs(title = \"", tit, "\", x = \"", var_x, "\", y = \"", var_y, "\") +\n",
            "  theme_minimal()\n"
          )
        } else {
          paste0(
            "ggplot(datos, aes(x = `", var_x, "`, y = `", var_y, "`)) +\n",
            "  geom_point(color = \"", color, "\", alpha = 0.7, size = 2.5) +\n",
            "  geom_smooth(method = \"lm\", se = TRUE, linewidth = 0.8) +\n",
            "  labs(title = \"", tit, "\", x = \"", var_x, "\", y = \"", var_y, "\") +\n",
            "  theme_minimal()\n"
          )
        }
      } else {
        "# Seleccioná un tipo de gráfico para ver el código\n"
      }

      paste0(encabezado, carga, geom)
    })

    output$codigo_r <- renderText({ codigo_generado() })

    output$descargar_script <- downloadHandler(
      filename = function() paste0("grafico_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(codigo_generado(), file)
    )

  })
}
