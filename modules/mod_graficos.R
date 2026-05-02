# ============================================================
# mod_graficos.R — Gráficos descriptivos personalizables
# ============================================================

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
          uiOutput(ns("sel_grupo_graf")),
          hr(),
          textInput(ns("titulo"), "Título del gráfico (opcional)", placeholder = "Mi gráfico"),
          colourInput_simple(ns("color_principal"), "Color principal", value = "#2D6A2D")
        )
      ),

      # ── Gráfico ──
      card(
        card_header("Resultado"),
        card_body(
          plotOutput(ns("grafico_principal"), height = "420px"),
          hr(),
          uiOutput(ns("nota_grafico"))
        )
      )
    )
  )
}

# Función auxiliar liviana para elegir color (evita dependencia de colourpicker)
colourInput_simple <- function(inputId, label, value = "#2D6A2D") {
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

mod_graficos_server <- function(id, datos) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Columnas numéricas y categóricas ──
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
          "Histograma"    = "histograma",
          "Boxplot"       = "boxplot",
          "Barras"        = "barras"
        )
      )
    })

    # ── Variable principal ──
    output$sel_variable_graf <- renderUI({
      req(input$tipo_grafico)
      if (input$tipo_grafico %in% c("histograma", "boxplot")) {
        req(length(cols_num()) > 0)
        selectInput(ns("var_graf"), "Variable numérica:", choices = cols_num())
      } else {
        req(length(cols_cat()) > 0)
        selectInput(ns("var_graf"), "Variable de categorías:", choices = cols_cat())
      }
    })

    # ── Variable de grupo (opcional) ──
    output$sel_grupo_graf <- renderUI({
      req(input$tipo_grafico)
      if (input$tipo_grafico == "boxplot" && length(cols_cat()) > 0) {
        selectInput(
          ns("grupo_graf"), "Colorear por grupo (opcional):",
          choices = c("Ninguno" = "ninguno", cols_cat())
        )
      }
    })

    # ── Gráfico principal ──
    output$grafico_principal <- renderPlot({
      req(datos(), input$tipo_grafico, input$var_graf)

      df    <- datos()
      color <- if (!is.null(input$color_principal) && nchar(input$color_principal) == 7)
        input$color_principal else "#2D6A2D"
      titulo <- if (!is.null(input$titulo) && nchar(trimws(input$titulo)) > 0)
        input$titulo else input$var_graf

      base_theme <- theme_minimal(base_size = 14) +
        theme(
          plot.title       = element_text(face = "bold", color = "#1A2E1A"),
          plot.background  = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA)
        )

      if (input$tipo_grafico == "histograma") {
        ggplot(df, aes(x = .data[[input$var_graf]])) +
          geom_histogram(fill = color, color = "white", bins = 25, alpha = 0.85) +
          labs(title = titulo, x = input$var_graf, y = "Frecuencia") +
          base_theme

      } else if (input$tipo_grafico == "boxplot") {
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
          ggplot(df, aes(y = .data[[input$var_graf]])) +
            geom_boxplot(fill = color, alpha = 0.7, width = 0.4,
                         outlier.shape = 21, outlier.fill = "white") +
            labs(title = titulo, y = input$var_graf, x = NULL) +
            base_theme
        }

      } else if (input$tipo_grafico == "barras") {
        df %>%
          count(.data[[input$var_graf]]) %>%
          ggplot(aes(x = reorder(.data[[input$var_graf]], n), y = n)) +
          geom_col(fill = color, alpha = 0.85) +
          coord_flip() +
          labs(title = titulo, x = NULL, y = "Frecuencia") +
          base_theme
      }
    }, bg = "transparent")

    # ── Nota contextual ──
    output$nota_grafico <- renderUI({
      req(input$tipo_grafico)
      texto <- switch(input$tipo_grafico,
        histograma = "El histograma muestra cuántas veces aparece cada rango de valores. Barras más altas = más frecuentes.",
        boxplot    = "El boxplot muestra la distribución: la línea del medio es el promedio, la caja contiene el 50% de los datos.",
        barras     = "Las barras muestran cuántos registros hay en cada categoría."
      )
      tags$p(class = "text-muted small", tags$em(texto))
    })
  })
}
