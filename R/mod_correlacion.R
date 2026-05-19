# ============================================================
# mod_correlacion.R — Análisis de correlación
#   · Modo bivariado: 2 variables, scatterplot + IC
#   · Modo matriz: múltiples variables, heatmap + tabla
#   · Métodos: Pearson, Spearman, Kendall
#   · Paquete: correlation (easystats)
# ============================================================

mod_correlacion_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(3, 9),

      # ── Controles ──
      bslib::card(
        bslib::card_header(tagList(bsicons::bs_icon("diagram-3"), " Opciones")),
        bslib::card_body(
          radioButtons(
            ns("modo"),
            "Tipo de análisis:",
            choices  = c("Dos variables" = "bivariado",
                         "Matriz de correlaciones" = "matriz"),
            selected = "bivariado"
          ),
          hr(),
          selectInput(
            ns("metodo"),
            "Método:",
            choices  = c("Pearson"  = "pearson",
                         "Spearman" = "spearman",
                         "Kendall"  = "kendall"),
            selected = "pearson"
          ),
          hr(),

          # Modo bivariado
          conditionalPanel(
            condition = sprintf("input['%s'] == 'bivariado'", ns("modo")),
            uiOutput(ns("sel_var_x")),
            uiOutput(ns("sel_var_y")),
            checkboxInput(ns("mostrar_ic"), "Mostrar IC 95% en el gráfico", value = TRUE)
          ),

          # Modo matriz
          conditionalPanel(
            condition = sprintf("input['%s'] == 'matriz'", ns("modo")),
            uiOutput(ns("sel_vars_matriz")),
            checkboxInput(ns("mostrar_valores"), "Mostrar valores en el heatmap", value = TRUE)
          ),

          hr(),
          actionButton(
            ns("calcular"),
            "Calcular correlación",
            class = "btn btn-primary w-100",
            icon  = icon("calculator")
          ),
          hr(),
          accordion(
            open = FALSE,
            accordion_panel(
              "📖 ¿Qué es la correlación?",
              p(class = "small",
                "La ", tags$strong("correlación"), " mide la fuerza y dirección de la
                relación lineal entre dos variables numéricas. Va de ",
                tags$strong("-1"), " (relación negativa perfecta) a ",
                tags$strong("+1"), " (relación positiva perfecta). Un valor cercano a ",
                tags$strong("0"), " indica poca o ninguna relación lineal."),
              hr(),
              p(class = "small",
                tags$strong("Pearson:"), " asume distribución normal, sensible a outliers. ",
                tags$strong("Spearman/Kendall:"), " no asumen normalidad, más robustos.")
            )
          )
        )
      ),

      # ── Resultados ──
      bslib::card(
        bslib::card_header("Resultados"),
        bslib::card_body(
          uiOutput(ns("resultado_texto")),

          # Modo bivariado
          conditionalPanel(
            condition = sprintf("input['%s'] == 'bivariado'", ns("modo")),
            div(style = "height: 440px;",
                plotOutput(ns("grafico_scatter"), height = "100%"))
          ),

          # Modo matriz
          conditionalPanel(
            condition = sprintf("input['%s'] == 'matriz'", ns("modo")),
            div(style = "height: 440px;",
                plotOutput(ns("grafico_heatmap"), height = "100%")),
            br(),
            DT::DTOutput(ns("tabla_matriz"))
          ),

          accordion(
            open = FALSE,
            accordion_panel(
              title = tagList(bsicons::bs_icon("code-slash"), " Código R reproducible"),
              value = "panel_codigo",
              p("Script que reproduce este análisis con tus datos.",
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
      )
    )
  )
}

# ── Server ──────────────────────────────────────────────────
mod_correlacion_server <- function(id, datos) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Columnas numéricas ──
    cols_num <- reactive({
      req(datos())
      names(datos())[sapply(datos(), is.numeric)]
    })

    # ── Selectores modo bivariado ──
    output$sel_var_x <- renderUI({
      selectInput(ns("var_x"), "Variable X:", choices = cols_num(), selected = cols_num()[1])
    })

    output$sel_var_y <- renderUI({
      req(length(cols_num()) >= 2)
      selectInput(ns("var_y"), "Variable Y:", choices = cols_num(), selected = cols_num()[2])
    })

    # ── Selector modo matriz ──
    output$sel_vars_matriz <- renderUI({
      req(cols_num())
      checkboxGroupInput(
        ns("vars_matriz"),
        "Variables a incluir:",
        choices  = cols_num(),
        selected = cols_num()[seq_len(min(5, length(cols_num())))]
      )
    })

    # ── Cálculo bivariado ──
    corr_bivariada <- eventReactive(input$calcular, {
      req(datos(), input$var_x, input$var_y, input$modo == "bivariado")
      df <- datos() %>% select(all_of(c(input$var_x, input$var_y))) %>% drop_na()
      correlation(df, method = input$metodo)
    })

    # ── Cálculo matriz ──
    corr_matriz <- eventReactive(input$calcular, {
      req(datos(), input$vars_matriz, length(input$vars_matriz) >= 2,
          input$modo == "matriz")
      df <- datos() %>% select(all_of(input$vars_matriz)) %>% drop_na()
      correlation(df, method = input$metodo)
    })

    # ── Texto de resultados (bivariado) ──
    output$resultado_texto <- renderUI({
      req(input$calcular)

      if (input$modo == "bivariado") {
        req(corr_bivariada())
        res <- as.data.frame(corr_bivariada())
        r   <- round(res$r, 3)
        lwr <- round(res$CI_low, 3)
        upr <- round(res$CI_high, 3)
        p   <- res$p

        fuerza <- dplyr::case_when(
          abs(r) >= 0.9 ~ "muy fuerte",
          abs(r) >= 0.7 ~ "fuerte",
          abs(r) >= 0.5 ~ "moderada",
          abs(r) >= 0.3 ~ "débil",
          TRUE          ~ "muy débil o nula"
        )
        direccion <- if (r > 0) "positiva" else "negativa"
        sig_texto <- if (p < 0.05)
          tags$span(class = "text-success", "✅ estadísticamente significativa (p < 0.05)")
        else
          tags$span(class = "text-warning", "⚠️ no significativa (p ≥ 0.05)")

        bslib::card(
          bslib::card_body(
            class = "mb-3",
            bslib::layout_columns(
              col_widths = c(4, 4, 4),
              bslib::value_box(
                title    = paste0("r de ", tools::toTitleCase(input$metodo)),
                value    = r,
                showcase = bsicons::bs_icon("graph-up"),
                theme    = "primary"
              ),
              bslib::value_box(
                title    = "IC 95%",
                value    = paste0("[", lwr, " – ", upr, "]"),
                showcase = bsicons::bs_icon("dash-lg"),
                theme    = "secondary"
              ),
              bslib::value_box(
                title    = "Valor p",
                value    = ifelse(p < 0.001, "< 0.001", round(p, 3)),
                showcase = bsicons::bs_icon("calculator"),
                theme    = if (p < 0.05) "success" else "warning"
              )
            ),
            tags$p(
              "La correlación entre ", tags$strong(res$Parameter1),
              " y ", tags$strong(res$Parameter2),
              " es ", tags$strong(fuerza), " y ", tags$strong(direccion),
              sprintf(" (r = %s).", r)
            ),
            tags$p(
              "El IC 95% de la correlación es ",
              tags$strong(paste0("[", lwr, " – ", upr, "]")),
              " — rango de valores plausibles para la correlación real en la población."
            ),
            tags$p(sig_texto)
          )
        )

      } else {
        req(corr_matriz())
        res <- as.data.frame(corr_matriz())
        n_vars <- length(input$vars_matriz)
        n_pares <- nrow(res)
        n_sig   <- sum(res$p < 0.05, na.rm = TRUE)

        bslib::card(
          bslib::card_body(
            class = "mb-3",
            tags$p(
              "Matriz de correlaciones entre ", tags$strong(n_vars), " variables. ",
              tags$strong(n_sig), " de ", tags$strong(n_pares),
              " pares muestran correlación estadísticamente significativa (p < 0.05)."
            )
          )
        )
      }
    })

    # ── Gráfico scatter (bivariado) ──
    output$grafico_scatter <- renderPlot({
      req(input$calcular, corr_bivariada(), input$modo == "bivariado")
      req(datos(), input$var_x, input$var_y)

      df  <- datos() %>% select(all_of(c(input$var_x, input$var_y))) %>% drop_na()
      res <- as.data.frame(corr_bivariada())
      r   <- round(res$r, 3)
      p   <- res$p
      p_label <- ifelse(p < 0.001, "p < 0.001", paste0("p = ", round(p, 3)))
      subtitulo <- paste0("r = ", r, "  |  ", p_label)

      p <- ggplot(df, aes(x = .data[[input$var_x]], y = .data[[input$var_y]])) +
        geom_point(color = colores$primario, alpha = 0.65, size = 2.5) +
        labs(
          title    = paste("Correlación:", input$var_x, "vs", input$var_y),
          subtitle = subtitulo,
          x = input$var_x,
          y = input$var_y
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title       = element_text(face = "bold", color = colores$primario),
          plot.background  = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA)
        )

      if (isTRUE(input$mostrar_ic)) {
        p <- p + geom_smooth(
          method = "lm", se = TRUE,
          color  = colores$acento,
          fill   = paste0(colores$acento, "30"),
          linewidth = 0.9
        )
      } else {
        p <- p + geom_smooth(
          method = "lm", se = FALSE,
          color  = colores$acento,
          linewidth = 0.9
        )
      }
      p
    })

    # ── Heatmap (matriz) ──
    output$grafico_heatmap <- renderPlot({
      req(input$calcular, corr_matriz(), input$modo == "matriz")

      res  <- as.data.frame(corr_matriz())
      vars <- input$vars_matriz

      # Construir matriz simétrica
      mat <- matrix(NA, nrow = length(vars), ncol = length(vars),
                    dimnames = list(vars, vars))
      diag(mat) <- 1
      for (i in seq_len(nrow(res))) {
        mat[res$Parameter1[i], res$Parameter2[i]] <- res$r[i]
        mat[res$Parameter2[i], res$Parameter1[i]] <- res$r[i]
      }

      mat_df <- as.data.frame(mat) %>%
        tibble::rownames_to_column("var1") %>%
        tidyr::pivot_longer(-var1, names_to = "var2", values_to = "r") %>%
        mutate(
          var1 = factor(var1, levels = vars),
          var2 = factor(var2, levels = rev(vars))
        )

      p <- ggplot(mat_df, aes(x = var1, y = var2, fill = r)) +
        geom_tile(color = "white", linewidth = 0.5) +
        scale_fill_gradient2(
          low      = colores$peligro,
          mid      = "#ffffff",
          high     = colores$primario,
          midpoint = 0,
          limits   = c(-1, 1),
          name     = "r"
        ) +
        labs(
          title = paste0("Matriz de correlaciones (", tools::toTitleCase(input$metodo), ")"),
          x = NULL, y = NULL
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title       = element_text(face = "bold", color = colores$primario),
          plot.background  = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          axis.text.x      = element_text(angle = 35, hjust = 1)
        )

      if (isTRUE(input$mostrar_valores)) {
        p <- p + geom_text(
          aes(label = ifelse(is.na(r), "", sprintf("%.2f", r))),
          size = 3.5, color = "#333333"
        )
      }
      p
    })

    # ── Tabla matriz ──
    output$tabla_matriz <- DT::renderDT({
      req(input$calcular, corr_matriz(), input$modo == "matriz")
      res <- as.data.frame(corr_matriz()) %>%
        select(Variable1 = Parameter1, Variable2 = Parameter2,
               r, IC_bajo = CI_low, IC_alto = CI_high,
               t, gl = df_error, p) %>%
        mutate(
          across(c(r, IC_bajo, IC_alto, t), ~ round(.x, 3)),
          p = ifelse(p < 0.001, "< 0.001", as.character(round(p, 3)))
        )
      DT::datatable(
        res,
        rownames  = FALSE,
        options   = list(pageLength = 10, dom = "tip", scrollX = TRUE),
        class     = "stripe hover compact"
      )
    })

    # ── Código R reproducible ──
    codigo_generado <- reactive({
      req(input$calcular)
      encabezado <- encabezado_script("StatFlow", "Correlación")

      if (input$modo == "bivariado") {
        req(input$var_x, input$var_y)
        paste0(
          encabezado,
          "library(correlation)\n",
          "library(tidyverse)\n\n",
          "datos <- read.csv(\"tu_archivo.csv\")\n\n",
          "# Correlación bivariada\n",
          "df <- datos |> select(`", input$var_x, "`, `", input$var_y, "`) |> drop_na()\n\n",
          "res <- correlation(df, method = \"", input$metodo, "\")\n",
          "print(res)\n",
          "summary(res)\n\n",
          "# Gráfico\n",
          "ggplot(df, aes(x = `", input$var_x, "`, y = `", input$var_y, "`)) +\n",
          "  geom_point(alpha = 0.65) +\n",
          "  geom_smooth(method = \"lm\", se = TRUE) +\n",
          "  labs(title = \"Correlación: ", input$var_x, " vs ", input$var_y, "\")\n"
        )
      } else {
        req(input$vars_matriz)
        vars_str <- paste0('"`', input$vars_matriz, '`"', collapse = ", ")
        paste0(
          encabezado,
          "library(correlation)\n",
          "library(tidyverse)\n\n",
          "datos <- read.csv(\"tu_archivo.csv\")\n\n",
          "# Matriz de correlaciones\n",
          "df <- datos |> select(", vars_str, ") |> drop_na()\n\n",
          "res <- correlation(df, method = \"", input$metodo, "\")\n",
          "print(res)\n",
          "summary(res)  # Vista en formato matriz\n"
        )
      }
    })

    output$codigo_r <- renderText({ codigo_generado() })

    output$descargar_script <- downloadHandler(
      filename = function() paste0("correlacion_", format(Sys.Date(), "%Y%m%d"), ".R"),
      content  = function(file) writeLines(codigo_generado(), file)
    )

  })
}
