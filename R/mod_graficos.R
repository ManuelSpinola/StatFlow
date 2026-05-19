# ============================================================
# mod_graficos.R — EDA Gráfico: visualización exploratoria
# ============================================================

# ── Función auxiliar para selector de color ─────────────────
colourInput_simple <- function(inputId, label, value = "#1D9E75") {
  tags$div(
    class = "mb-3",
    tags$label(label, `for` = inputId, class = "form-label"),
    tags$input(
      type  = "color",
      id    = inputId,
      name  = inputId,
      value = value,
      class = "form-control form-control-color shiny-bound-input",
      onchange = sprintf("Shiny.setInputValue('%s', this.value)", inputId)
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
    bslib::layout_columns(
      col_widths = c(3, 9),

      # ── Controles ──
      bslib::card(
        bslib::card_header(tagList(bsicons::bs_icon("graph-up"), " Opciones del gráfico")),
        bslib::card_body(
          uiOutput(ns("sel_tipo")),
          hr(),
          uiOutput(ns("sel_variable_graf")),
          uiOutput(ns("sel_variable_y")),
          uiOutput(ns("sel_grupo_graf")),
          uiOutput(ns("slider_bins")),
          hr(),
          textInput(ns("titulo"), "Título (opcional)", placeholder = "Mi gráfico"),
          uiOutput(ns("input_eje_x")),
          uiOutput(ns("input_eje_y")),
          colourInput_simple(ns("color_principal"), "Color principal", value = "#1D9E75"),
          p("Se aplica cuando no hay grupo seleccionado.", class = "text-muted small mt-1"),
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

      # ── Gráfico + Nota ──
      bslib::layout_columns(
        col_widths = c(7, 5),
        gap = "1rem",

        # Gráfico y código
        bslib::card(
          bslib::card_header("Resultado"),
          bslib::card_body(
            div(
              style = "height: 420px;",
              plotOutput(ns("grafico_principal"), height = "100%")
            ),
            hr(),
            accordion(
              open = FALSE,
              accordion_panel(
                title = tagList(bsicons::bs_icon("code-slash"), " Código R reproducible"),
                value = "codigo_r",
                p("Script que reproduce este gráfico con tus datos.",
                  class = "text-muted small mb-2"),
                verbatimTextOutput(ns("codigo_r")),
                downloadButton(
                  ns("descargar_script"),
                  label = "Descargar .R",
                  icon  = bsicons::bs_icon("download"),
                  class = "btn-sm btn-outline-primary mt-2"
                )
              )
            )
          )
        ),

        # Nota didáctica
        bslib::card(
          bslib::card_header(tagList(bsicons::bs_icon("info-circle"), " Sobre este gráfico")),
          bslib::card_body(
            uiOutput(ns("nota_grafico"))
          ),
          style = "height: 100%;"
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

    # ── Etiquetas de ejes ──
    output$input_eje_x <- renderUI({
      req(input$tipo_grafico, input$var_graf)
      tipo  <- input$tipo_grafico
      grupo <- input$grupo_graf

      tiene_eje_x <- tipo %in% c("histograma", "densidad", "dispersion", "barras") ||
        (tipo %in% c("boxplot", "violin") && !is.null(grupo) && grupo != "ninguno")

      if (tiene_eje_x) {
        placeholder <- if (tipo %in% c("boxplot", "violin") && !is.null(grupo) && grupo != "ninguno")
          grupo
        else
          input$var_graf
        textInput(ns("eje_x"), "Etiqueta eje X (opcional)", placeholder = placeholder)
      }
    })

    output$input_eje_y <- renderUI({
      req(input$tipo_grafico)
      tipo <- input$tipo_grafico
      label_default <- switch(tipo,
                              "histograma" = "Frecuencia",
                              "densidad"   = "Densidad",
                              "barras"     = "Frecuencia",
                              "boxplot"    = input$var_graf,
                              "violin"     = input$var_graf,
                              "dispersion" = input$var_y,
                              NULL
      )
      if (!is.null(label_default)) {
        textInput(ns("eje_y"), "Etiqueta eje Y (opcional)", placeholder = label_default)
      }
    })

    eje_x_activo <- reactive({
      if (!is.null(input$eje_x) && nchar(trimws(input$eje_x)) > 0) return(input$eje_x)
      tipo  <- input$tipo_grafico
      grupo <- input$grupo_graf
      if (tipo %in% c("boxplot", "violin") && !is.null(grupo) && grupo != "ninguno")
        grupo
      else
        input$var_graf
    })

    eje_y_activo <- reactive({
      if (!is.null(input$eje_y) && nchar(trimws(input$eje_y)) > 0) return(input$eje_y)
      tipo <- input$tipo_grafico
      switch(tipo,
             "histograma" = "Frecuencia",
             "densidad"   = "Densidad",
             "barras"     = "Frecuencia",
             "boxplot"    = input$var_graf,
             "violin"     = input$var_graf,
             "dispersion" = input$var_y,
             ""
      )
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
    base_theme <- ggplot2::theme_minimal(base_size = 14) +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(face = "bold", color = colores$primario),
        plot.background  = ggplot2::element_rect(fill = "transparent", color = NA),
        panel.background = ggplot2::element_rect(fill = "transparent", color = NA)
      )

    # ── Gráfico principal ──
    output$grafico_principal <- renderPlot({
      req(datos(), input$tipo_grafico, input$var_graf)
      df    <- datos()
      color  <- color_activo()
      titulo <- titulo_activo()
      tipo   <- input$tipo_grafico
      eje_x  <- eje_x_activo()
      eje_y  <- eje_y_activo()

      if (tipo == "histograma") {
        bins <- if (!is.null(input$n_bins)) input$n_bins else nclass.Sturges(df[[input$var_graf]])
        ggplot2::ggplot(df, ggplot2::aes(x = .data[[input$var_graf]])) +
          ggplot2::geom_histogram(fill = color, color = "white", bins = bins, alpha = 0.85) +
          ggplot2::labs(title = titulo, x = eje_x, y = eje_y) +
          base_theme

      } else if (tipo == "densidad") {
        ggplot2::ggplot(df, ggplot2::aes(x = .data[[input$var_graf]])) +
          ggplot2::geom_density(fill = color, color = colores$primario, alpha = 0.6) +
          ggplot2::labs(title = titulo, x = eje_x, y = eje_y) +
          base_theme

      } else if (tipo == "boxplot") {
        grupo <- input$grupo_graf
        if (!is.null(grupo) && grupo != "ninguno") {
          ggplot2::ggplot(df, ggplot2::aes(x = .data[[grupo]], y = .data[[input$var_graf]],
                         fill = .data[[grupo]])) +
            ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = 21, outlier.fill = "white") +
            geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
            scale_fill_brewer(palette = "Set2") +
            ggplot2::labs(title = titulo, x = eje_x, y = eje_y) +
            base_theme + ggplot2::theme(legend.position = "none")
        } else {
          ggplot2::ggplot(df, ggplot2::aes(x = "", y = .data[[input$var_graf]])) +
            ggplot2::geom_boxplot(fill = color, alpha = 0.7, width = 0.4,
                         outlier.shape = 21, outlier.fill = "white") +
            ggplot2::labs(title = titulo, x = NULL, y = eje_y) +
            base_theme
        }

      } else if (tipo == "violin") {
        grupo <- input$grupo_graf
        if (!is.null(grupo) && grupo != "ninguno") {
          ggplot2::ggplot(df, ggplot2::aes(x = .data[[grupo]], y = .data[[input$var_graf]],
                         fill = .data[[grupo]])) +
            geom_violin(alpha = 0.6) +
            geom_jitter(width = 0.1, alpha = 0.5, size = 1.8) +
            scale_fill_brewer(palette = "Set2") +
            ggplot2::labs(title = titulo, x = eje_x, y = eje_y) +
            base_theme + ggplot2::theme(legend.position = "none")
        } else {
          ggplot2::ggplot(df, ggplot2::aes(x = "", y = .data[[input$var_graf]])) +
            geom_violin(fill = color, color = colores$primario, alpha = 0.6) +
            geom_jitter(color = colores$primario, width = 0.08, alpha = 0.6, size = 1.8) +
            ggplot2::labs(title = titulo, x = NULL, y = eje_y) +
            base_theme
        }

      } else if (tipo == "barras") {
        df %>%
          dplyr::count(.data[[input$var_graf]]) %>%
          ggplot2::ggplot(ggplot2::aes(x = reorder(.data[[input$var_graf]], n), y = n)) +
          ggplot2::geom_col(fill = color, alpha = 0.85) +
          ggplot2::coord_flip() +
          ggplot2::labs(title = titulo, x = NULL, y = eje_y) +
          base_theme

      } else if (tipo == "dispersion") {
        req(input$var_y)
        grupo <- input$grupo_graf
        if (!is.null(grupo) && grupo != "ninguno") {
          ggplot2::ggplot(df, ggplot2::aes(x = .data[[input$var_graf]], y = .data[[input$var_y]],
                         color = .data[[grupo]])) +
            ggplot2::geom_point(alpha = 0.7, size = 2.5) +
            ggplot2::geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
            scale_color_brewer(palette = "Set2") +
            ggplot2::labs(title = titulo, x = eje_x, y = eje_y) +
            base_theme
        } else {
          ggplot2::ggplot(df, ggplot2::aes(x = .data[[input$var_graf]], y = .data[[input$var_y]])) +
            ggplot2::geom_point(color = color, alpha = 0.7, size = 2.5) +
            ggplot2::geom_smooth(method = "lm", se = TRUE, fill = paste0(color, "40"),
                        color = colores$primario, linewidth = 0.8) +
            ggplot2::labs(title = titulo, x = eje_x, y = eje_y) +
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

      nombre_grafico <- switch(input$tipo_grafico,
                               "histograma" = "Histograma",
                               "densidad"   = "Gráfico de densidad",
                               "boxplot"    = "Diagrama de caja (Boxplot)",
                               "violin"     = "Gráfico de violín",
                               "barras"     = "Gráfico de barras",
                               "dispersion" = "Gráfico de dispersión"
      )

      cuando_usar <- switch(input$tipo_grafico,
                            "histograma" = "Usalo cuando querés ver cómo se distribuyen los valores de una variable numérica y cuántas observaciones caen en cada intervalo.",
                            "densidad"   = "Usalo cuando querés una vista suavizada de la distribución, especialmente útil para comparar grupos superpuestos.",
                            "boxplot"    = "Usalo cuando querés comparar la distribución entre grupos o identificar valores atípicos rápidamente.",
                            "violin"     = "Usalo cuando además de comparar grupos, querés ver la forma completa de la distribución en cada uno.",
                            "barras"     = "Usalo cuando querés comparar la frecuencia o conteo de las categorías de una variable.",
                            "dispersion" = "Usalo cuando querés explorar si existe una relación o tendencia entre dos variables numéricas."
      )

      tags$div(
        tags$p(tags$strong(nombre_grafico), style = "font-size: 1.05rem; color: #1170AA;"),
        tags$hr(),
        tags$div(
          style = paste0("border-left: 4px solid ", colores$exito, "; padding-left: 12px; margin-bottom: 1rem;"),
          tags$p(tags$strong("✅ Ventaja"), class = "mb-1", style = "font-size: 0.95rem;"),
          tags$p(nota$ventaja, style = "font-size: 0.9rem; color: #57606C;")
        ),
        tags$div(
          style = paste0("border-left: 4px solid ", colores$advertencia, "; padding-left: 12px; margin-bottom: 1rem;"),
          tags$p(tags$strong("⚠️ Limitación"), class = "mb-1", style = "font-size: 0.95rem;"),
          tags$p(nota$desventaja, style = "font-size: 0.9rem; color: #57606C;")
        ),
        tags$div(
          style = paste0("border-left: 4px solid ", colores$secundario, "; padding-left: 12px;"),
          tags$p(tags$strong("💡 ¿Cuándo usarlo?"), class = "mb-1", style = "font-size: 0.95rem;"),
          tags$p(cuando_usar, style = "font-size: 0.9rem; color: #57606C;")
        )
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
          "ggplot2::ggplot(datos, ggplot2::aes(x = `", var_x, "`)) +\n",
          "  ggplot2::geom_histogram(fill = \"", color, "\", color = \"white\",\n",
          "                 bins = ", bins, ", alpha = 0.85) +\n",
          "  ggplot2::labs(title = \"", tit, "\", x = \"", var_x, "\", y = \"Frecuencia\") +\n",
          "  ggplot2::theme_minimal()\n"
        )
      } else if (tipo == "densidad") {
        paste0(
          "ggplot2::ggplot(datos, ggplot2::aes(x = `", var_x, "`)) +\n",
          "  ggplot2::geom_density(fill = \"", color, "\", alpha = 0.6) +\n",
          "  ggplot2::labs(title = \"", tit, "\", x = \"", var_x, "\", y = \"Densidad\") +\n",
          "  ggplot2::theme_minimal()\n"
        )
      } else if (tipo == "boxplot") {
        if (!is.null(grupo) && grupo != "ninguno") {
          paste0(
            "ggplot2::ggplot(datos, ggplot2::aes(x = `", grupo, "`, y = `", var_x, "`,\n",
            "                  fill = `", grupo, "`)) +\n",
            "  ggplot2::geom_boxplot(alpha = 0.7, outlier.shape = 21) +\n",
            "  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +\n",
            "  scale_fill_brewer(palette = \"Set2\") +\n",
            "  ggplot2::labs(title = \"", tit, "\", x = \"", grupo, "\", y = \"", var_x, "\") +\n",
            "  ggplot2::theme_minimal() + ggplot2::theme(legend.position = \"none\")\n"
          )
        } else {
          paste0(
            "ggplot2::ggplot(datos, ggplot2::aes(x = \"\", y = `", var_x, "`)) +\n",
            "  ggplot2::geom_boxplot(fill = \"", color, "\", alpha = 0.7, width = 0.4) +\n",
            "  ggplot2::labs(title = \"", tit, "\", x = NULL, y = \"", var_x, "\") +\n",
            "  ggplot2::theme_minimal()\n"
          )
        }
      } else if (tipo == "violin") {
        if (!is.null(grupo) && grupo != "ninguno") {
          paste0(
            "ggplot2::ggplot(datos, ggplot2::aes(x = `", grupo, "`, y = `", var_x, "`,\n",
            "                  fill = `", grupo, "`)) +\n",
            "  geom_violin(alpha = 0.6) +\n",
            "  geom_jitter(width = 0.1, alpha = 0.5, size = 1.8) +\n",
            "  scale_fill_brewer(palette = \"Set2\") +\n",
            "  ggplot2::labs(title = \"", tit, "\", x = \"", grupo, "\", y = \"", var_x, "\") +\n",
            "  ggplot2::theme_minimal() + ggplot2::theme(legend.position = \"none\")\n"
          )
        } else {
          paste0(
            "ggplot2::ggplot(datos, ggplot2::aes(x = \"\", y = `", var_x, "`)) +\n",
            "  geom_violin(fill = \"", color, "\", alpha = 0.6) +\n",
            "  geom_jitter(width = 0.08, alpha = 0.6, size = 1.8) +\n",
            "  ggplot2::labs(title = \"", tit, "\", x = NULL, y = \"", var_x, "\") +\n",
            "  ggplot2::theme_minimal()\n"
          )
        }
      } else if (tipo == "barras") {
        paste0(
          "datos |>\n",
          "  dplyr::count(`", var_x, "`) |>\n",
          "  ggplot2::ggplot(ggplot2::aes(x = reorder(`", var_x, "`, n), y = n)) +\n",
          "  ggplot2::geom_col(fill = \"", color, "\", alpha = 0.85) +\n",
          "  ggplot2::coord_flip() +\n",
          "  ggplot2::labs(title = \"", tit, "\", x = NULL, y = \"Frecuencia\") +\n",
          "  ggplot2::theme_minimal()\n"
        )
      } else if (tipo == "dispersion") {
        req(input$var_y)
        var_y <- input$var_y
        if (!is.null(grupo) && grupo != "ninguno") {
          paste0(
            "ggplot2::ggplot(datos, ggplot2::aes(x = `", var_x, "`, y = `", var_y, "`,\n",
            "                  color = `", grupo, "`)) +\n",
            "  ggplot2::geom_point(alpha = 0.7, size = 2.5) +\n",
            "  ggplot2::geom_smooth(method = \"lm\", se = FALSE, linewidth = 0.8) +\n",
            "  scale_color_brewer(palette = \"Set2\") +\n",
            "  ggplot2::labs(title = \"", tit, "\", x = \"", var_x, "\", y = \"", var_y, "\") +\n",
            "  ggplot2::theme_minimal()\n"
          )
        } else {
          paste0(
            "ggplot2::ggplot(datos, ggplot2::aes(x = `", var_x, "`, y = `", var_y, "`)) +\n",
            "  ggplot2::geom_point(color = \"", color, "\", alpha = 0.7, size = 2.5) +\n",
            "  ggplot2::geom_smooth(method = \"lm\", se = TRUE, linewidth = 0.8) +\n",
            "  ggplot2::labs(title = \"", tit, "\", x = \"", var_x, "\", y = \"", var_y, "\") +\n",
            "  ggplot2::theme_minimal()\n"
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
