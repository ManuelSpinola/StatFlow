# ============================================================
# mod_explorar.R — EDA Numérico: resumen descriptivo completo
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
          p("Selecciona una variable para ver su resumen descriptivo completo.",
            class = "text-muted small"),
          hr(),
          accordion(
            open = FALSE,
            accordion_panel(
              "📖 ¿Qué es el EDA numérico?",
              p(class = "small",
                "El ", tags$strong("Análisis Exploratorio de Datos (EDA)"), " numérico resume
                las características principales de una variable usando estadísticas descriptivas.
                Permite entender su centro, dispersión y forma antes de aplicar cualquier análisis.")
            )
          )
        )
      ),

      # ── Resultados ──
      card(
        card_header("Resumen descriptivo"),
        card_body(
          uiOutput(ns("tipo_badge")),
          br(),

          # ── Numérica ──
          conditionalPanel(
            condition = sprintf("output['%s'] == 'Numérica'", ns("tipo_var")),

            h6("📍 Tendencia central"),
            p("Indican dónde se concentran los datos.", class = "text-muted small"),
            tableOutput(ns("tabla_central")),
            hr(),

            h6("📏 Dispersión"),
            p("Indican qué tan dispersos o concentrados están los datos.", class = "text-muted small"),
            tableOutput(ns("tabla_dispersion")),
            hr(),

            h6("📐 Forma de la distribución"),
            p("Describen la simetría y el peso de las colas.", class = "text-muted small"),
            tableOutput(ns("tabla_forma")),
            hr(),

            h6("🔎 Valores extremos"),
            p("Mínimo, máximo y posibles valores atípicos (outliers).", class = "text-muted small"),
            tableOutput(ns("tabla_extremos")),
            hr(),

            h6("❓ Valores perdidos"),
            tableOutput(ns("tabla_na"))
          ),

          # ── Categórica ──
          conditionalPanel(
            condition = sprintf("output['%s'] == 'Categórica'", ns("tipo_var")),

            h6("📊 Tabla de frecuencias"),
            p("Muestra cuántas veces aparece cada categoría y su proporción.", class = "text-muted small"),
            tableOutput(ns("tabla_frecuencias")),
            hr(),

            h6("🔎 Resumen"),
            tableOutput(ns("tabla_cat_resumen"))
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

    # ── Tipo de variable ──
    tipo_actual <- reactive({
      req(datos(), input$variable)
      tipo_variable(datos()[[input$variable]])
    })

    output$tipo_var <- reactive({ tipo_actual() })
    outputOptions(output, "tipo_var", suspendWhenHidden = FALSE)

    # ── Badge ──
    output$tipo_badge <- renderUI({
      estilo <- if (tipo_actual() == "Numérica")
        paste0("background-color:", colores$primario, "; color:#ffffff;")
      else
        paste0("background-color:", colores$acento, "; color:#ffffff;")
      tags$span(
        class = "badge fs-6",
        style = estilo,
        paste("Tipo:", tipo_actual())
      )
    })

    # ── Variable activa ──
    var_activa <- reactive({
      req(datos(), input$variable)
      datos()[[input$variable]]
    })

    # ── Tendencia central ──
    output$tabla_central <- renderTable({
      req(tipo_actual() == "Numérica")
      x <- var_activa()
      data.frame(
        Estadístico = c("Media", "Mediana", "Moda"),
        Valor = c(
          round(mean(x, na.rm = TRUE), 3),
          round(median(x, na.rm = TRUE), 3),
          round(as.numeric(names(sort(table(x[!is.na(x)]), decreasing = TRUE))[1]), 3)
        )
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ── Dispersión ──
    output$tabla_dispersion <- renderTable({
      req(tipo_actual() == "Numérica")
      x <- var_activa()
      data.frame(
        Estadístico = c(
          "Desviación estándar",
          "Varianza",
          "Rango intercuartílico (IQR)",
          "Coeficiente de variación (%)"
        ),
        Valor = c(
          round(sd(x, na.rm = TRUE), 3),
          round(var(x, na.rm = TRUE), 3),
          round(IQR(x, na.rm = TRUE), 3),
          round(sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE) * 100, 1)
        )
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ── Forma ──
    output$tabla_forma <- renderTable({
      req(tipo_actual() == "Numérica")
      x    <- var_activa()
      asim <- round(datawizard::skewness(x)$Skewness, 3)
      kurt <- round(datawizard::kurtosis(x)$Kurtosis, 3)
      interp_asim <- dplyr::case_when(
        asim >  1    ~ "Asimetría positiva fuerte (cola derecha larga)",
        asim >  0.5  ~ "Asimetría positiva moderada",
        asim < -1    ~ "Asimetría negativa fuerte (cola izquierda larga)",
        asim < -0.5  ~ "Asimetría negativa moderada",
        TRUE         ~ "Aproximadamente simétrica"
      )
      interp_kurt <- dplyr::case_when(
        kurt >  1 ~ "Leptocúrtica (colas pesadas, picos altos)",
        kurt < -1 ~ "Platicúrtica (colas ligeras, distribución plana)",
        TRUE      ~ "Mesocúrtica (similar a la normal)"
      )
      data.frame(
        Estadístico    = c("Asimetría (Skewness)", "Curtosis (Kurtosis)"),
        Valor          = c(asim, kurt),
        Interpretación = c(interp_asim, interp_kurt)
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ── Valores extremos ──
    output$tabla_extremos <- renderTable({
      req(tipo_actual() == "Numérica")
      x          <- var_activa()
      q          <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
      fence_low  <- q[1] - 1.5 * IQR(x, na.rm = TRUE)
      fence_high <- q[2] + 1.5 * IQR(x, na.rm = TRUE)
      n_outliers <- sum(x < fence_low | x > fence_high, na.rm = TRUE)
      data.frame(
        Estadístico = c(
          "Mínimo", "Máximo",
          "Percentil 25 (Q1)", "Percentil 75 (Q3)",
          "Posibles outliers (regla IQR)"
        ),
        Valor = c(
          round(min(x, na.rm = TRUE), 3),
          round(max(x, na.rm = TRUE), 3),
          round(q[1], 3),
          round(q[2], 3),
          n_outliers
        )
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ── Valores perdidos ──
    output$tabla_na <- renderTable({
      req(tipo_actual() == "Numérica")
      x     <- var_activa()
      n_na  <- sum(is.na(x))
      n_tot <- length(x)
      data.frame(
        Estadístico = c("Total de observaciones", "Valores perdidos (NA)", "Valores válidos"),
        Valor       = c(n_tot, n_na, n_tot - n_na)
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ── Tabla de frecuencias (categórica) ──
    output$tabla_frecuencias <- renderTable({
      req(tipo_actual() == "Categórica")
      x  <- var_activa()
      as.data.frame(table(x)) %>%
        rename(Categoría = x, Frecuencia = Freq) %>%
        mutate(
          Porcentaje        = paste0(round(Frecuencia / sum(Frecuencia) * 100, 1), " %"),
          `Frec. acumulada` = cumsum(Frecuencia),
          `% acumulado`     = paste0(round(cumsum(Frecuencia) / sum(Frecuencia) * 100, 1), " %")
        ) %>%
        arrange(desc(Frecuencia))
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

    # ── Resumen categórica ──
    output$tabla_cat_resumen <- renderTable({
      req(tipo_actual() == "Categórica")
      x    <- var_activa()
      moda <- names(sort(table(x), decreasing = TRUE))[1]
      data.frame(
        Estadístico = c(
          "Total de observaciones",
          "Categorías únicas",
          "Valores perdidos (NA)",
          "Categoría más frecuente (moda)"
        ),
        Valor = c(
          length(x),
          length(unique(x[!is.na(x)])),
          sum(is.na(x)),
          moda
        )
      )
    }, striped = TRUE, hover = TRUE, bordered = TRUE)

  })
}
