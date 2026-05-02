# ============================================================
# mod_explorar.R — Resumen descriptivo y frecuencias
# ============================================================

mod_explorar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),

      # ── Controles ──
      card(
        card_header("🔍 Opciones"),
        card_body(
          uiOutput(ns("sel_variable")),
          hr(),
          p("Selecciona cualquier columna de tus datos para ver su resumen.",
            class = "text-muted small")
        )
      ),

      # ── Resultados ──
      card(
        card_header("Resumen de la variable"),
        card_body(
          uiOutput(ns("tipo_badge")),
          br(),
          # Numérica
          conditionalPanel(
            condition = sprintf("output['%s'] == 'Numérica'", ns("tipo_var")),
            h6("Estadísticas descriptivas"),
            tableOutput(ns("tabla_resumen")),
            hr(),
            h6("Distribución (histograma)"),
            plotOutput(ns("histograma_exploracion"), height = "220px")
          ),
          # Categórica
          conditionalPanel(
            condition = sprintf("output['%s'] == 'Categórica'", ns("tipo_var")),
            h6("Tabla de frecuencias"),
            tableOutput(ns("tabla_frecuencias")),
            hr(),
            h6("Gráfico de frecuencias"),
            plotOutput(ns("grafico_frecuencias"), height = "220px")
          )
        )
      )
    )
  )
}

mod_explorar_server <- function(id, datos) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Selector de variable ──
    output$sel_variable <- renderUI({
      req(datos())
      selectInput(
        ns("variable"),
        "Variable a explorar:",
        choices  = names(datos()),
        selected = names(datos())[1]
      )
    })

    # ── Tipo de variable seleccionada ──
    tipo_actual <- reactive({
      req(datos(), input$variable)
      tipo_variable(datos()[[input$variable]])
    })

    output$tipo_var <- reactive({ tipo_actual() })
    outputOptions(output, "tipo_var", suspendWhenHidden = FALSE)

    # ── Badge de tipo ──
    output$tipo_badge <- renderUI({
      color <- if (tipo_actual() == "Numérica") "success" else "info"
      tags$span(
        class = paste0("badge bg-", color, " fs-6"),
        paste("Tipo:", tipo_actual())
      )
    })

    # ── Tabla resumen (numérica) ──
    output$tabla_resumen <- renderTable({
      req(tipo_actual() == "Numérica")
      resumen_numerico(datos(), input$variable)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ── Histograma rápido ──
    output$histograma_exploracion <- renderPlot({
      req(tipo_actual() == "Numérica")
      x <- datos()[[input$variable]]
      ggplot(data.frame(x = x), aes(x = x)) +
        geom_histogram(fill = "#2D6A2D", color = "white", bins = 20, alpha = 0.85) +
        labs(x = input$variable, y = "Frecuencia") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = "transparent", color = NA))
    }, bg = "transparent")

    # ── Tabla de frecuencias (categórica) ──
    output$tabla_frecuencias <- renderTable({
      req(tipo_actual() == "Categórica")
      df <- datos()
      df %>%
        count(.data[[input$variable]], name = "Frecuencia") %>%
        mutate(Porcentaje = paste0(round(Frecuencia / sum(Frecuencia) * 100, 1), " %")) %>%
        rename(Categoría = 1)
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ── Gráfico de frecuencias (categórica) ──
    output$grafico_frecuencias <- renderPlot({
      req(tipo_actual() == "Categórica")
      df <- datos()
      df %>%
        count(.data[[input$variable]]) %>%
        ggplot(aes(x = reorder(.data[[input$variable]], n), y = n)) +
        geom_col(fill = "#2D6A2D", alpha = 0.85) +
        coord_flip() +
        labs(x = NULL, y = "Frecuencia") +
        theme_minimal(base_size = 13) +
        theme(plot.background = element_rect(fill = "transparent", color = NA))
    }, bg = "transparent")
  })
}
