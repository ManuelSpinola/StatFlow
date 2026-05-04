# ============================================================
# mod_frecuencias.R — Comparación de frecuencias / proporciones
#   · IC 95% por grupo con prop.test() (R base)
#   · Diferencia de proporciones con prop.test()
#   · Gráfico geom_pointrange: proporciones + diferencia cruda
# ============================================================

mod_frecuencias_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 9),

      # ── Controles ──
      card(
        card_header("📊 Opciones"),
        card_body(
          uiOutput(ns("sel_var_resultado")),
          uiOutput(ns("sel_categoria")),
          hr(),
          uiOutput(ns("sel_var_grupo")),
          uiOutput(ns("sel_grupos_especificos")),
          hr(),
          actionButton(ns("calcular"), "Calcular comparación",
                       class = "btn btn-primary w-100",
                       icon  = icon("calculator")),
          hr(),
          accordion(
            open = FALSE,
            accordion_panel(
              "📖 ¿Qué estamos comparando?",
              p(class = "small",
                "Comparamos la ", tags$strong("proporción"), " de una categoría de interés
                entre dos grupos. Por ejemplo: ¿en qué zona se avista más la Danta?
                El gráfico muestra la proporción con su ",
                tags$strong("intervalo de confianza al 95%"), ".")
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
            plotOutput(ns("grafico_proporciones"), height = "360px"),
            plotOutput(ns("grafico_diferencia"),   height = "360px")
          ),
          br(),
          uiOutput(ns("nota_grafico"))
        )
      )
    )
  )
}

mod_frecuencias_server <- function(id, datos) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    cols_cat <- reactive({
      req(datos())
      names(datos())[!map_lgl(datos(), is.numeric)]
    })

    # ── Variable de resultado (categórica) ──
    output$sel_var_resultado <- renderUI({
      req(length(cols_cat()) > 0)
      selectInput(ns("var_resultado"), "Variable de resultado:", choices = cols_cat())
    })

    # ── Categoría de interés ──
    output$sel_categoria <- renderUI({
      req(input$var_resultado, datos())
      cats <- sort(unique(as.character(datos()[[input$var_resultado]])))
      selectInput(ns("categoria"), "Categoría de interés:", choices = cats)
    })

    # ── Variable de grupo ──
    output$sel_var_grupo <- renderUI({
      req(length(cols_cat()) > 0)
      otras <- setdiff(cols_cat(), input$var_resultado)
      req(length(otras) > 0)
      selectInput(ns("var_grupo"), "Variable de grupos:", choices = otras)
    })

    # ── Selector de dos grupos específicos ──
    output$sel_grupos_especificos <- renderUI({
      req(input$var_grupo, datos())
      niveles <- sort(unique(as.character(datos()[[input$var_grupo]])))
      tagList(
        selectInput(ns("grupo_a"), "Grupo A:", choices = niveles, selected = niveles[1]),
        selectInput(ns("grupo_b"), "Grupo B:", choices = niveles,
                    selected = if (length(niveles) > 1) niveles[2] else niveles[1])
      )
    })

    # ── Cálculo reactivo ──
    comparacion <- eventReactive(input$calcular, {
      req(datos(), input$var_resultado, input$categoria,
          input$var_grupo, input$grupo_a, input$grupo_b)

      validate(need(input$grupo_a != input$grupo_b, "Elige dos grupos diferentes."))

      df <- datos() %>%
        filter(.data[[input$var_grupo]] %in% c(input$grupo_a, input$grupo_b)) %>%
        mutate(
          grupo     = as.character(.data[[input$var_grupo]]),
          resultado = as.character(.data[[input$var_resultado]]) == input$categoria
        )

      df_a <- df %>% filter(grupo == input$grupo_a)
      df_b <- df %>% filter(grupo == input$grupo_b)

      validate(
        need(nrow(df_a) >= 2, paste("El grupo", input$grupo_a, "tiene muy pocos datos.")),
        need(nrow(df_b) >= 2, paste("El grupo", input$grupo_b, "tiene muy pocos datos."))
      )

      x_a <- sum(df_a$resultado)
      n_a <- nrow(df_a)
      x_b <- sum(df_b$resultado)
      n_b <- nrow(df_b)

      validate(
        need(x_a > 0 || x_b > 0,
             paste0("La categoría '", input$categoria, "' no aparece en ninguno de los grupos."))
      )

      # ── IC 95% por grupo con prop.test() (R base) ──
      warn_aprox <- FALSE

      capturar <- function(expr) {
        withCallingHandlers(expr, warning = function(w) {
          if (grepl("Chi-squared approximation may be incorrect", conditionMessage(w))) {
            warn_aprox <<- TRUE
            invokeRestart("muffleWarning")
          }
        })
      }

      pt_a   <- capturar(prop.test(x_a, n_a, conf.level = 0.95, correct = FALSE))
      pt_b   <- capturar(prop.test(x_b, n_b, conf.level = 0.95, correct = FALSE))

      prop_a <- pt_a$estimate
      prop_b <- pt_b$estimate

      ic_a <- list(est    = prop_a,
                   lwr.ci = pt_a$conf.int[1],
                   upr.ci = pt_a$conf.int[2])
      ic_b <- list(est    = prop_b,
                   lwr.ci = pt_b$conf.int[1],
                   upr.ci = pt_b$conf.int[2])

      # ── IC 95% diferencia de proporciones con prop.test() ──
      pt_dif <- capturar(prop.test(c(x_a, x_b), c(n_a, n_b), conf.level = 0.95, correct = FALSE))
      ic_dif <- list(est    = prop_a - prop_b,
                     lwr.ci = pt_dif$conf.int[1],
                     upr.ci = pt_dif$conf.int[2])

      # ── Tabla resumen para gráfico ──
      resumen <- tibble(
        grupo = c(input$grupo_a, input$grupo_b),
        prop  = c(prop_a, prop_b),
        lwr   = c(ic_a$lwr.ci, ic_b$lwr.ci),
        upr   = c(ic_a$upr.ci, ic_b$upr.ci)
      )

      list(
        grupo_a     = input$grupo_a,
        grupo_b     = input$grupo_b,
        categoria   = input$categoria,
        variable    = input$var_resultado,
        var_grupo   = input$var_grupo,
        x_a = x_a, n_a = n_a,
        x_b = x_b, n_b = n_b,
        prop_a      = round(prop_a, 3),
        prop_b      = round(prop_b, 3),
        ic_a        = ic_a,
        ic_b        = ic_b,
        ic_dif      = ic_dif,
        resumen     = resumen,
        warn_aprox  = warn_aprox
      )
    })

    # ── Texto de resultados ──
    output$resultado_texto <- renderUI({
      res <- comparacion()

      grupo_mayor <- if (res$prop_a >= res$prop_b) res$grupo_a else res$grupo_b
      grupo_menor <- if (res$prop_a >= res$prop_b) res$grupo_b else res$grupo_a
      dif_pos     <- abs(round(res$prop_a - res$prop_b, 3))

      tagList(
        if (res$warn_aprox)
          div(
            class = "alert alert-warning small mb-3",
            icon("triangle-exclamation"), " ",
            tags$strong("Muestra pequeña:"),
            " alguno de los grupos tiene pocas observaciones, por lo que la aproximación
              chi-cuadrado puede ser imprecisa. Interpretá los intervalos con cautela."
          ),
        layout_columns(
          col_widths = c(6, 6),
          value_box(
            title    = paste0("Proporción — ", res$grupo_a,
                              " (", res$x_a, "/", res$n_a, ")"),
            value    = paste0(round(res$prop_a * 100, 1), "%",
                              "  [", round(res$ic_a$lwr.ci * 100, 1),
                              "–", round(res$ic_a$upr.ci * 100, 1), "%]"),
            showcase = bsicons::bs_icon("pie-chart-fill"),
            theme    = "primary"
          ),
          value_box(
            title    = paste0("Proporción — ", res$grupo_b,
                              " (", res$x_b, "/", res$n_b, ")"),
            value    = paste0(round(res$prop_b * 100, 1), "%",
                              "  [", round(res$ic_b$lwr.ci * 100, 1),
                              "–", round(res$ic_b$upr.ci * 100, 1), "%]"),
            showcase = bsicons::bs_icon("pie-chart"),
            theme    = "secondary"
          )
        ),
        br(),
        card(
          card_body(
            tags$p(
              "La categoría ", tags$strong(paste0("'", res$categoria, "'")),
              " aparece con mayor frecuencia en ",
              tags$strong(grupo_mayor), " que en ",
              tags$strong(grupo_menor), ".",
              sprintf(" La diferencia de proporciones es %.1f puntos porcentuales.",
                      dif_pos * 100)
            ),
            tags$p(
              class = "text-muted small",
              "Los valores entre corchetes son el intervalo de confianza al 95%."
            )
          )
        )
      )
    })

    # ── Gráfico de proporciones ──
    output$grafico_proporciones <- renderPlot({
      res <- comparacion()

      ggplot(res$resumen, aes(x = grupo, y = prop, ymin = lwr, ymax = upr,
                              color = grupo)) +
        geom_pointrange(size = 0.9, linewidth = 1.2, fatten = 4) +
        scale_color_manual(values = c(colores$primario, colores$acento)) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                           limits = c(0, 1)) +
        labs(
          title   = paste0("Proporción de '", res$categoria, "' por grupo"),
          x       = res$var_grupo,
          y       = "Proporción",
          caption = "● = proporción  |  barra = IC 95%"
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

    # ── Gráfico de diferencia cruda ──
    output$grafico_diferencia <- renderPlot({
      res <- comparacion()

      df_dif <- tibble(
        etiqueta = paste0(res$grupo_a, " vs ", res$grupo_b),
        dif      = res$ic_dif$est,
        lwr      = res$ic_dif$lwr.ci,
        upr      = res$ic_dif$upr.ci
      )

      ggplot(df_dif, aes(y = etiqueta, x = dif, xmin = lwr, xmax = upr)) +
        geom_vline(xintercept = 0, linetype = "dashed",
                   color = colores$texto, linewidth = 0.7) +
        geom_pointrange(
          color     = colores$primario,
          size      = 0.9,
          linewidth = 1.2,
          fatten    = 4
        ) +
        scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
        labs(
          title   = "Diferencia de proporciones",
          x       = paste0("Diferencia (", res$grupo_a, " − ", res$grupo_b, ")"),
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
          tags$li(tags$strong("Izquierda:"), " muestra la proporción de la categoría de interés
                  en cada grupo con su IC 95%. Si los intervalos no se solapan,
                  la diferencia probablemente es real."),
          tags$li(tags$strong("Derecha:"), " muestra la diferencia cruda de proporciones con su IC 95%.
                  Si la barra ", tags$strong("no cruza el 0"), ", la diferencia es robusta.
                  Un valor positivo indica que el Grupo A tiene mayor proporción que el Grupo B.")
        )
      )
    })

  })
}
