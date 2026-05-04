# ============================================================
# mod_medias.R — Comparación de grupos
#   · Medias e IC 95% con t.test() (R base)
#   · Diferencia de medias absoluta y porcentual
#   · Tamaño del efecto (Cohen's d) con effectsize
#   · Gráfico geom_pointrange con puntos individuales
# ============================================================

mod_medias_ui <- function(id) {
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
                       icon  = icon("calculator")),
          hr(),
          checkboxInput(ns("mostrar_puntos"), "Mostrar datos individuales", value = TRUE),
          accordion(
            open = FALSE,
            accordion_panel(
              "📖 ¿Qué estamos comparando?",
              p(class = "small",
                "Comparamos la ", tags$strong("media"), " de una variable numérica
                entre dos grupos. El gráfico muestra la media con su ",
                tags$strong("intervalo de confianza al 95%"), " — el rango de valores plausibles para la media real, dado lo que observamos en los datos.")
            )
          )
        )
      ),

      # ── Resultados ──
      card(
        card_header("Resultados de la comparación"),
        card_body(
          uiOutput(ns("resultado_texto")),
          hr(),
          layout_columns(
            col_widths = c(6, 6),
            plotOutput(ns("grafico_comparacion"), height = "360px"),
            plotOutput(ns("grafico_efecto"),      height = "360px")
          ),
          br(),
          uiOutput(ns("nota_grafico"))
        )
      )
    )
  )
}

mod_medias_server <- function(id, datos) {
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

    # ── Selector de los dos grupos ──
    output$sel_grupos_especificos <- renderUI({
      req(input$grupo_comp, datos())
      niveles <- sort(unique(as.character(datos()[[input$grupo_comp]])))
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

      # ── Medias e IC 95% con t.test() (R base) ──
      tt_a <- t.test(x_a, conf.level = 0.95)
      tt_b <- t.test(x_b, conf.level = 0.95)

      media_a <- tt_a$estimate
      media_b <- tt_b$estimate
      dif_abs <- media_a - media_b
      dif_pct <- if (media_b != 0) (dif_abs / abs(media_b)) * 100 else NA_real_

      # IC 95% de cada grupo
      ic_a <- list(mean   = media_a,
                   lwr.ci = tt_a$conf.int[1],
                   upr.ci = tt_a$conf.int[2])
      ic_b <- list(mean   = media_b,
                   lwr.ci = tt_b$conf.int[1],
                   upr.ci = tt_b$conf.int[2])

      # ── IC 95% de la diferencia de medias con t.test() ──
      tt_dif <- t.test(x_a, x_b, conf.level = 0.95)
      ic_dif <- list(meandiff = tt_dif$estimate[1] - tt_dif$estimate[2],
                     lwr.ci   = tt_dif$conf.int[1],
                     upr.ci   = tt_dif$conf.int[2])

      # ── Cohen's d con effectsize ──
      ef     <- cohens_d(x_a, x_b)
      d_val  <- round(ef$Cohens_d, 2)
      interp <- as.character(interpret_cohens_d(d_val, rules = "cohen1988"))

      # ── Tabla de resumen por grupo para gráfico ──
      resumen <- tibble(
        grupo = c(input$grupo_a, input$grupo_b),
        media = c(media_a, media_b),
        lwr   = c(ic_a$lwr.ci, ic_b$lwr.ci),
        upr   = c(ic_a$upr.ci, ic_b$upr.ci)
      )

      list(
        grupo_a  = input$grupo_a,
        grupo_b  = input$grupo_b,
        variable = input$var_comp,
        media_a  = round(media_a, 2),
        media_b  = round(media_b, 2),
        ic_a     = ic_a,
        ic_b     = ic_b,
        ic_dif   = ic_dif,
        dif_abs  = round(dif_abs, 2),
        dif_pct  = round(dif_pct, 1),
        d_val    = d_val,
        interp   = interp,
        df_filt  = df,
        resumen  = resumen
      )
    })

    # ── Texto de resultados ──
    output$resultado_texto <- renderUI({
      res <- comparacion()

      grupo_mayor <- if (res$media_a >= res$media_b) res$grupo_a else res$grupo_b
      grupo_menor <- if (res$media_a >= res$media_b) res$grupo_b else res$grupo_a
      dif_abs_pos <- abs(res$dif_abs)
      dif_pct_pos <- abs(res$dif_pct)

      interp_es <- switch(res$interp,
                          "small"      = "pequeño",
                          "medium"     = "moderado",
                          "large"      = "grande",
                          "very large" = "muy grande",
                          "negligible" = "muy pequeño (casi sin diferencia)",
                          res$interp
      )

      tagList(
        layout_columns(
          col_widths = c(6, 6),
          value_box(
            title    = paste("Media —", res$grupo_a),
            value    = paste0(res$media_a,
                              " [", round(res$ic_a$lwr.ci, 2),
                              " – ", round(res$ic_a$upr.ci, 2), "]"),
            showcase = bsicons::bs_icon("bar-chart-fill"),
            theme    = "primary"
          ),
          value_box(
            title    = paste("Media —", res$grupo_b),
            value    = paste0(res$media_b,
                              " [", round(res$ic_b$lwr.ci, 2),
                              " – ", round(res$ic_b$upr.ci, 2), "]"),
            showcase = bsicons::bs_icon("bar-chart"),
            theme    = "secondary"
          )
        ),
        br(),
        card(
          card_body(
            tags$p(
              tags$strong(grupo_mayor), " tiene una media de ",
              tags$strong(res$variable), " mayor que ",
              tags$strong(grupo_menor), " en ",
              tags$strong(dif_abs_pos, " unidades"),
              sprintf(" (%.1f%% más).", dif_pct_pos)
            ),
            tags$p(
              "El tamaño de esta diferencia es ",
              tags$strong(interp_es),
              sprintf(" (d de Cohen = %s).", res$d_val)
            ),
            tags$p(
              class = "text-muted small",
              "Los valores entre corchetes son el intervalo de confianza al 95%:
              el rango de valores plausibles para la media real, dado lo que observamos en los datos."
            )
          )
        )
      )
    })

    # ── Gráfico geom_pointrange ──
    output$grafico_comparacion <- renderPlot({
      res <- comparacion()

      p <- ggplot()

      if (isTRUE(input$mostrar_puntos)) {
        p <- p + geom_jitter(
          data  = res$df_filt,
          aes(x = grupo, y = .data[[res$variable]], color = grupo),
          width = 0.12, alpha = 0.5, size = 2
        )
      }

      p +
        geom_pointrange(
          data = res$resumen,
          aes(x = grupo, y = media, ymin = lwr, ymax = upr, color = grupo),
          size = 0.9, linewidth = 1.2, fatten = 4
        ) +
        scale_color_manual(values = c(colores$primario, colores$acento)) +
        labs(
          title   = paste("Comparación de", res$variable),
          x       = input$grupo_comp,
          y       = res$variable,
          caption = if (isTRUE(input$mostrar_puntos))
            "● = media  |  barra = IC 95%  |  puntos = observaciones individuales"
          else
            "● = media  |  barra = IC 95%"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          legend.position  = "none",
          plot.title       = element_text(face = "bold", color = colores$primario),
          plot.caption     = element_text(color = colores$texto, size = 10),
          plot.background  = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA)
        )
    }, bg = "transparent")

    # ── Gráfico de efecto crudo ──
    output$grafico_efecto <- renderPlot({
      res <- comparacion()

      df_ef <- tibble(
        etiqueta = paste0(res$grupo_a, " vs ", res$grupo_b),
        dif      = res$ic_dif$meandiff,
        lwr      = res$ic_dif$lwr.ci,
        upr      = res$ic_dif$upr.ci
      )

      ggplot(df_ef, aes(y = etiqueta, x = dif, xmin = lwr, xmax = upr)) +
        geom_vline(xintercept = 0, linetype = "dashed",
                   color = colores$texto, linewidth = 0.7) +
        geom_pointrange(
          color     = colores$primario,
          size      = 0.9,
          linewidth = 1.2,
          fatten    = 4
        ) +
        labs(
          title   = "Tamaño del efecto (diferencia cruda)",
          x       = paste0("Diferencia de medias (", res$variable, ")"),
          y       = NULL,
          caption = "Si la barra no cruza el 0, la diferencia es robusta"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          plot.title       = element_text(face = "bold", color = colores$primario),
          plot.caption     = element_text(color = colores$texto, size = 10),
          plot.background  = element_rect(fill = "transparent", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA)
        )
    }, bg = "transparent")

    # ── Nota didáctica ──
    output$nota_grafico <- renderUI({
      tags$div(
        class = "small",
        style = paste0("border-left: 3px solid ", colores$acento, "; padding-left: 10px;"),
        tags$p(tags$strong("📌 Cómo leer estos gráficos:"), class = "mb-1"),
        tags$ul(
          class = "mb-0",
          tags$li(tags$strong("Izquierda:"), " el punto grande es la media de cada grupo y la barra vertical es el IC 95%.
                  Si los intervalos no se solapan, la diferencia probablemente es real."),
          tags$li(tags$strong("Derecha:"), " muestra la diferencia cruda de medias con su IC 95%.
                  Si la barra horizontal ", tags$strong("no cruza el 0"), ", la diferencia es robusta.
                  El valor está en las unidades originales de la variable."),
          tags$li("Los ", tags$strong("puntos pequeños"), " son las observaciones individuales.
                  Podés ocultarlos con el control de la izquierda si un valor extremo
                  dificulta ver las medias.")
        )
      )
    })

  })
}
