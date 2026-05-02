# ============================================================
# mod_comparar.R — Comparación de grupos
#   · Diferencia de medias (absoluta y porcentual)
#   · Tamaño del efecto no estandarizado (Cohen's d como referencia)
# ============================================================

mod_comparar_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),

      # ── Controles ──
      card(
        card_header("⚖️ Opciones"),
        card_body(
          uiOutput(ns("sel_variable_comp")),
          uiOutput(ns("sel_grupo_comp")),
          hr(),
          uiOutput(ns("sel_grupos_especificos")),
          hr(),
          actionButton(ns("calcular"), "Calcular comparación",
                       class = "btn btn-primary w-100",
                       icon  = icon("calculator"))
        )
      ),

      # ── Resultados ──
      card(
        card_header("Resultados de la comparación"),
        card_body(
          uiOutput(ns("resultado_texto")),
          hr(),
          plotOutput(ns("grafico_comparacion"), height = "300px")
        )
      )
    )
  )
}

mod_comparar_server <- function(id, datos) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cols_num <- reactive({ names(datos())[map_lgl(datos(), is.numeric)] })
    cols_cat <- reactive({ names(datos())[!map_lgl(datos(), is.numeric)] })

    output$sel_variable_comp <- renderUI({
      req(length(cols_num()) > 0)
      selectInput(ns("var_comp"), "Variable a comparar (numérica):", choices = cols_num())
    })

    output$sel_grupo_comp <- renderUI({
      req(length(cols_cat()) > 0)
      selectInput(ns("grupo_comp"), "Variable de grupos:", choices = cols_cat())
    })

    # ── Selector de los dos grupos a comparar ──
    output$sel_grupos_especificos <- renderUI({
      req(input$grupo_comp, datos())
      niveles <- unique(as.character(datos()[[input$grupo_comp]]))
      niveles <- sort(niveles)
      tagList(
        selectInput(ns("grupo_a"), "Grupo A:", choices = niveles, selected = niveles[1]),
        selectInput(ns("grupo_b"), "Grupo B:", choices = niveles,
                    selected = if (length(niveles) > 1) niveles[2] else niveles[1])
      )
    })

    # ── Cálculo reactivo ──
    comparacion <- eventReactive(input$calcular, {
      req(datos(), input$var_comp, input$grupo_comp, input$grupo_a, input$grupo_b)

      validate(need(input$grupo_a != input$grupo_b, "Elige dos grupos diferentes."))

      df <- datos() %>%
        filter(.data[[input$grupo_comp]] %in% c(input$grupo_a, input$grupo_b)) %>%
        mutate(grupo = as.character(.data[[input$grupo_comp]]))

      x_a <- df %>% filter(grupo == input$grupo_a) %>% pull(.data[[input$var_comp]])
      x_b <- df %>% filter(grupo == input$grupo_b) %>% pull(.data[[input$var_comp]])

      validate(
        need(length(x_a) >= 2, paste("El grupo", input$grupo_a, "tiene muy pocos datos.")),
        need(length(x_b) >= 2, paste("El grupo", input$grupo_b, "tiene muy pocos datos."))
      )

      media_a   <- mean(x_a, na.rm = TRUE)
      media_b   <- mean(x_b, na.rm = TRUE)
      dif_abs   <- media_a - media_b
      dif_pct   <- if (media_b != 0) (dif_abs / abs(media_b)) * 100 else NA_real_

      # Tamaño del efecto (Cohen's d no estandarizado = diferencia de medias / SD pooled)
      ef <- cohens_d(x_a, x_b)
      d_val  <- round(ef$Cohens_d, 2)
      interp <- interpret_cohens_d(d_val, rules = "cohen1988")

      list(
        grupo_a   = input$grupo_a,
        grupo_b   = input$grupo_b,
        variable  = input$var_comp,
        media_a   = round(media_a, 2),
        media_b   = round(media_b, 2),
        dif_abs   = round(dif_abs, 2),
        dif_pct   = round(dif_pct, 1),
        d_val     = d_val,
        interp    = as.character(interp),
        df_filt   = df
      )
    })

    # ── Texto de resultados ──
    output$resultado_texto <- renderUI({
      res <- comparacion()

      grupo_mayor  <- if (res$media_a >= res$media_b) res$grupo_a else res$grupo_b
      grupo_menor  <- if (res$grupo_mayor == res$grupo_a) res$grupo_b else res$grupo_a
      dif_abs_pos  <- abs(res$dif_abs)
      dif_pct_pos  <- abs(res$dif_pct)

      interp_es <- switch(res$interp,
        "small"        = "pequeño",
        "medium"       = "moderado",
        "large"        = "grande",
        "very large"   = "muy grande",
        "negligible"   = "muy pequeño (casi sin diferencia)",
        res$interp
      )

      tagList(
        layout_columns(
          col_widths = c(6, 6),
          value_box(
            title    = paste("Promedio —", res$grupo_a),
            value    = res$media_a,
            showcase = bsicons::bs_icon("bar-chart-fill"),
            theme    = "success"
          ),
          value_box(
            title    = paste("Promedio —", res$grupo_b),
            value    = res$media_b,
            showcase = bsicons::bs_icon("bar-chart"),
            theme    = "secondary"
          )
        ),
        br(),
        card(
          card_body(
            tags$p(
              tags$strong(grupo_mayor), " tiene un promedio de ",
              tags$strong(res$variable), " mayor que ",
              tags$strong(grupo_menor), " en ",
              tags$strong(dif_abs_pos, " unidades"),
              sprintf(" (%.1f%% más).", dif_pct_pos)
            ),
            tags$p(
              "El tamaño de esta diferencia es ",
              tags$strong(interp_es),
              sprintf(" (d = %s).", res$d_val)
            ),
            tags$p(
              class = "text-muted small",
              "d de Cohen: compara la diferencia con la variabilidad natural de los datos.",
              " Un efecto 'grande' significa que la diferencia es notable en el contexto de los datos."
            )
          )
        )
      )
    })

    # ── Gráfico de comparación ──
    output$grafico_comparacion <- renderPlot({
      res <- comparacion()
      df_plot <- res$df_filt

      ggplot(df_plot, aes(x = grupo, y = .data[[res$variable]], fill = grupo)) +
        geom_boxplot(alpha = 0.6, outlier.shape = 21, outlier.fill = "white") +
        geom_jitter(width = 0.15, alpha = 0.5, size = 2, color = "#333") +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 4,
                     fill = "white", color = "#1A2E1A") +
        scale_fill_manual(values = c("#2D6A2D", "#E65100")) +
        labs(
          title   = paste("Comparación de", res$variable),
          x       = res$grupo_a,
          y       = res$variable,
          caption = "◇ = promedio del grupo"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          legend.position  = "none",
          plot.title       = element_text(face = "bold", color = "#1A2E1A"),
          plot.background  = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA)
        )
    }, bg = "transparent")
  })
}
