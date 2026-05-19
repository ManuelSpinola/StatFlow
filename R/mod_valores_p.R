# ============================================================
# mod_valores_p.R — ¿Qué es un valor de p?
#   Módulo educativo basado en el blog de Manuel Spínola
#   Secciones: intro, historia, mitos, problemas, alternativas
#   + Intérprete interactivo de valores de p
# ============================================================

mod_valores_p_ui <- function(id) {
  ns <- NS(id)
  div(
    style = "overflow-y: auto; height: calc(100vh - 80px); padding: 1rem;",

    # ── Encabezado ──
    div(
      class = "py-3 px-1 mb-3",
      style = paste0("border-bottom: 2px solid ", "#C8D9EC", ";"),
      tags$h4(
        style = paste0("color: #1170AA; font-weight: 700;"),
        bsicons::bs_icon("bar-chart-steps"), " ¿Qué es un valor de p?"
      ),
      tags$p(
        class = "text-muted mb-0",
        "Una guía para entender, interpretar y no abusar del valor de p en ciencia."
      )
    ),

    # ── Fila 1: Cita + Intérprete interactivo ──
    bslib::layout_columns(
      col_widths = c(5, 7),
      gap = "1rem",

      # Cita
      bslib::card(
        bslib::card_header(tagList(bsicons::bs_icon("quote"), " Para empezar")),
        bslib::card_body(
          tags$blockquote(
            class = "blockquote",
            style = paste0("border-left: 4px solid #1170AA; padding-left: 1rem;
                            font-style: italic; color: #57606C;"),
            tags$p(
              '"It is foolish to ask \'Are the effects of A and B different?\'.
               They are always different — for some decimal place."'
            ),
            tags$footer(
              class = "blockquote-footer mt-2",
              "John Tukey"
            )
          ),
          hr(),
          tags$p(
            "El ", tags$strong("valor de p"), " ha sido durante décadas la estrella de
            los análisis estadísticos. Nos dice la probabilidad de obtener resultados
            como los observados si la hipótesis nula fuera cierta. Sin embargo, su
            interpretación errónea ha llevado a malentendidos y a una lectura equívoca de los resultados."
          ),
          tags$p(
            "Este módulo explora la realidad detrás del valor de p, sus límites,
            cómo interpretarlo correctamente y qué alternativas existen."
          )
        )
      ),

      # Intérprete interactivo
      bslib::card(
        bslib::card_header(tagList(bsicons::bs_icon("search"), " Interpretá tu valor de p")),
        bslib::card_body(
          tags$p(
            class = "text-muted small",
            "Ingresá el valor de p de tu análisis y el tamaño del efecto para
            obtener una interpretación contextualizada."
          ),
          bslib::layout_columns(
            col_widths = c(6, 6),
            numericInput(
              ns("p_valor"),
              "Valor de p:",
              value = 0.03, min = 0, max = 1, step = 0.001
            ),
            selectInput(
              ns("efecto"),
              "Tamaño del efecto:",
              choices = c(
                "Muy pequeño / Nulo" = "nulo",
                "Pequeño"            = "pequeño",
                "Moderado"           = "moderado",
                "Grande"             = "grande"
              )
            )
          ),
          numericInput(
            ns("alpha"),
            "Nivel de significancia (α):",
            value = 0.05, min = 0.001, max = 0.2, step = 0.001
          ),
          actionButton(
            ns("interpretar"),
            "Interpretar",
            class = "btn btn-primary w-100",
            icon  = icon("magnifying-glass")
          ),
          br(), br(),
          uiOutput(ns("interpretacion"))
        )
      )
    ),

    br(),

    # ── Fila 2: Definición y Historia ──
    bslib::layout_columns(
      col_widths = c(6, 6),
      gap = "1rem",

      bslib::card(
        bslib::card_header(tagList(bsicons::bs_icon("info-circle"), " ¿Qué es realmente el valor de p?")),
        bslib::card_body(
          tags$p(tags$strong("Definición correcta:")),
          div(
            style = paste0("background-color: #EEF3FA; border-left: 4px solid #1170AA;
                            padding: 12px 16px; border-radius: 4px; margin-bottom: 1rem;"),
            tags$p(
              class = "mb-0",
              "Probabilidad de obtener un resultado ", tags$strong("tan o más extremo"),
              " que el observado, ", tags$strong("si la hipótesis nula fuera verdadera"), "."
            )
          ),
          tags$p(tags$strong("Lo que el valor de p NO es:"), class = "mt-3"),
          tags$ul(
            tags$li("No es la probabilidad de que la hipótesis nula sea verdadera."),
            tags$li("No expresa la magnitud del efecto."),
            tags$li("No expresa la importancia práctica de un resultado."),
            tags$li("No garantiza que el resultado sea reproducible.")
          ),
          div(
            style = paste0("background-color: #FFF3CD; border-left: 4px solid #F1CE63;
                            padding: 12px 16px; border-radius: 4px; margin-top: 1rem;"),
            tags$p(
              class = "mb-0 small",
              tags$strong("Ejemplo frecuente de error: "),
              '"p = 0.03 → ', tags$em("H₀"), ' tiene 3% de probabilidad de ser verdadera."',
              tags$br(),
              tags$strong("Correcto: "),
              '"p = 0.03 → si ', tags$em("H₀"), ' fuera verdadera, hay 3% de probabilidad
              de obtener este resultado o uno más extremo."'
            )
          )
        )
      ),

      bslib::card(
        bslib::card_header(tagList(bsicons::bs_icon("clock-history"), " Breve historia")),
        bslib::card_body(
          div(
            style = "border-left: 3px solid #C8D9EC; padding-left: 1rem; margin-bottom: 1rem;",
            tags$p(tags$strong("R. A. Fisher (1935)"), class = "mb-1"),
            tags$p(
              class = "small text-muted",
              'En su libro "Design of Experiments" sugirió el 5% como guía práctica,
              no como umbral rígido. Fisher lo veía como una medida de evidencia,
              no como una regla de decisión binaria.'
            )
          ),
          div(
            style = "border-left: 3px solid #C8D9EC; padding-left: 1rem; margin-bottom: 1rem;",
            tags$p(tags$strong("Neyman y Pearson"), class = "mb-1"),
            tags$p(
              class = "small text-muted",
              "Desarrollaron un marco más formal para pruebas de hipótesis con
              nivel de significancia α fijo. No establecieron que α debía ser 0.05 —
              podía ser 0.01, 0.10, etc., según el contexto."
            )
          ),
          div(
            style = paste0("border-left: 3px solid #FC7D0B; padding-left: 1rem;"),
            tags$p(tags$strong("La fusión problemática"), class = "mb-1"),
            tags$p(
              class = "small text-muted",
              "La práctica científica fusionó ambas visiones: el p-value fisheriano
              se interpretó con la lógica binaria de Neyman–Pearson. El 0.05 de Fisher
              se convirtió en umbral estándar, generando décadas de confusión."
            )
          )
        )
      )
    ),

    br(),

    # ── Fila 3: Mitos ──
    bslib::card(
      bslib::card_header(tagList(bsicons::bs_icon("exclamation-triangle"), " Mitos más comunes")),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = c(6, 6),
          gap = "1rem",

          mito_card("Mito 1", "p < 0.05 significa que el efecto es real.",
                    "Solo indica evidencia contra la hipótesis nula, no garantiza que el efecto exista."),
          mito_card("Mito 2", "p < 0.05 significa que la hipótesis nula es falsa.",
                    "No prueba falsedad, solo mide compatibilidad de los datos con la hipótesis nula."),
          mito_card("Mito 3", "p < 0.05 significa que el resultado es importante.",
                    "La significancia estadística no implica relevancia práctica ni tamaño del efecto."),
          mito_card("Mito 4", "p < 0.05 significa que el resultado es reproducible.",
                    "La reproducibilidad depende del diseño, la potencia y la variabilidad, no solo del p."),
          mito_card("Mito 5", "p > 0.05 significa que no hay efecto.",
                    "Puede haber efecto, pero el estudio no tuvo suficiente poder estadístico para detectarlo."),
          mito_card("Mito 6", "p es la probabilidad de que H₀ sea cierta.",
                    "p no da probabilidades sobre hipótesis, solo sobre los datos bajo un supuesto modelo.")
        )
      )
    ),

    br(),

    # ── Fila 4: Problemas y Alternativas ──
    bslib::layout_columns(
      col_widths = c(5, 7),
      gap = "1rem",

      bslib::card(
        bslib::card_header(tagList(bsicons::bs_icon("bug"), " Problemas del uso del p")),
        bslib::card_body(
          problema_item("Dicotomía significativo / no significativo",
                        "Se interpreta el p < 0.05 como un sí/no absoluto, cuando refleja un grado de evidencia."),
          problema_item("P-hacking",
                        "Repetir análisis hasta obtener un resultado significativo, inflando la tasa de falsos positivos."),
          problema_item("Publicación selectiva",
                        "Solo se publican estudios con p < 0.05, ocultando resultados nulos y sesgando la literatura."),
          problema_item("Crisis de reproducibilidad",
                        "Muchos hallazgos no pueden replicarse porque dependen de umbrales arbitrarios."),
          problema_item("Sensibilidad al tamaño de muestra",
                        "Con muestras grandes, diferencias triviales se vuelven 'significativas'; con muestras pequeñas,
                         efectos reales pasan desapercibidos.")
        )
      ),

      bslib::card(
        bslib::card_header(tagList(bsicons::bs_icon("lightbulb"), " Alternativas y buenas prácticas")),
        bslib::card_body(
          tags$h6(tags$strong("1. Reportar tamaños del efecto con IC 95%"), class = "text-primary"),
          tags$p(
            class = "small mb-3",
            "Ejemplo: ", tags$em('"el salario promedio anual de los hombres fue 2500
            (95% IC: 2200–2700) USD más alto que el de las mujeres"'), ".",
            " El intervalo de confianza comunica tanto la magnitud como la incertidumbre del efecto."
          ),
          hr(),
          tags$h6(tags$strong("2. Factor de Bayes (BF₁₀)"), class = "text-primary"),
          tags$p(class = "small mb-1",
                 "Mide cuántas veces los datos son más probables bajo H₁ que bajo H₀."
          ),
          div(
            style = "background-color: #EEF3FA; padding: 8px 12px; border-radius: 4px;
                     font-size: 0.85rem; margin-bottom: 1rem;",
            tags$table(
              class = "table table-sm mb-0",
              tags$thead(tags$tr(tags$th("BF₁₀"), tags$th("Evidencia a favor de H₁"))),
              tags$tbody(
                tags$tr(tags$td("1 – 3"),   tags$td("Débil")),
                tags$tr(tags$td("3 – 10"),  tags$td("Moderada")),
                tags$tr(tags$td("10 – 30"), tags$td("Fuerte")),
                tags$tr(tags$td("> 30"),    tags$td("Muy fuerte"))
              )
            )
          ),
          hr(),
          tags$h6(tags$strong("3. Probabilidades bayesianas"), class = "text-primary"),
          tags$p(
            class = "small mb-0",
            "A diferencia del valor de p, el enfoque bayesiano permite obtener directamente
            la probabilidad de que H₁ sea cierta dados los datos observados — una interpretación
            más intuitiva y menos propensa a malentendidos."
          )
        )
      )
    ),

    br(),

    # ── Referencia ──
    bslib::card(
      bslib::card_body(
        class = "py-2",
        tags$p(
          class = "text-muted small mb-0",
          bsicons::bs_icon("book"), " ",
          tags$strong("Referencia: "),
          "Spínola, M. (2025). Prácticamente insignificante. Blog sobre Ciencia de Datos. ICOMVIS, Universidad Nacional, Costa Rica."
        )
      )
    )
  )
}

# ── Helpers de UI ────────────────────────────────────────────
mito_card <- function(titulo, mito, realidad) {
  bslib::card(
    bslib::card_body(
      tags$p(
        tags$span(class = "badge",
                  style = "background-color: #C85200; color: white;",
                  titulo),
        " ", tags$strong(mito), class = "mb-1 small"
      ),
      tags$p(
        tags$span("✅ Realidad: ", style = "color: #1170AA; font-weight: 600;"),
        realidad, class = "mb-0 small text-muted"
      )
    )
  )
}

problema_item <- function(titulo, descripcion) {
  div(
    class = "mb-3",
    style = "border-left: 3px solid #C85200; padding-left: 12px;",
    tags$p(tags$strong(titulo), class = "mb-1 small"),
    tags$p(descripcion, class = "mb-0 small text-muted")
  )
}

# ── Server ──────────────────────────────────────────────────
mod_valores_p_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    output$interpretacion <- renderUI({
      req(input$interpretar)
      isolate({
        p      <- input$p_valor
        alpha  <- input$alpha
        efecto <- input$efecto

        if (is.na(p) || p < 0 || p > 1)
          return(div(class = "alert alert-danger small",
                     "Por favor ingresá un valor de p entre 0 y 1."))

        sig     <- p < alpha
        p_label <- if (p < 0.001) "< 0.001" else as.character(round(p, 3))

        # Veredicto principal
        veredicto <- if (sig) {
          div(class = "alert alert-success small mb-2",
              bsicons::bs_icon("check-circle"), " ",
              tags$strong(paste0("p = ", p_label, " es menor que α = ", alpha, ".")),
              " Hay evidencia estadística suficiente para rechazar la hipótesis nula.")
        } else {
          div(class = "alert alert-warning small mb-2",
              bsicons::bs_icon("exclamation-circle"), " ",
              tags$strong(paste0("p = ", p_label, " es mayor o igual que α = ", alpha, ".")),
              " No hay evidencia estadística suficiente para rechazar la hipótesis nula.")
        }

        # Interpretación según tamaño del efecto
        interpretacion_efecto <- switch(efecto,
                                        "nulo" = if (sig)
                                          "⚠️ Aunque el resultado es estadísticamente significativo, el tamaño del efecto es
             muy pequeño o nulo. Esto puede deberse a un tamaño de muestra muy grande.
             La significancia estadística no implica relevancia práctica."
                                        else
                                          "El resultado no es significativo y el efecto reportado es muy pequeño.
             Probablemente no hay una diferencia real de interés.",
                                        "pequeño" = if (sig)
                                          "El resultado es significativo con un efecto pequeño. Considerá si la magnitud
             de la diferencia tiene relevancia práctica en tu contexto."
                                        else
                                          "El resultado no es significativo. Con un efecto pequeño, es posible que el
             estudio no tenga suficiente potencia estadística para detectarlo.",
                                        "moderado" = if (sig)
                                          "✅ El resultado es estadísticamente significativo y el tamaño del efecto es
             moderado. Esta combinación ofrece buena evidencia de un efecto real y relevante."
                                        else
                                          "El resultado no es significativo, pero el efecto es moderado.
             Considerá aumentar el tamaño de muestra — puede haber un efecto real
             que el estudio no tiene poder suficiente para detectar.",
                                        "grande" = if (sig)
                                          "✅ Resultado significativo con un efecto grande. Evidencia sólida de un efecto
             real y con relevancia práctica."
                                        else
                                          "El resultado no es significativo a pesar de un efecto grande.
             Revisá el diseño del estudio — este resultado inusual puede indicar
             un problema metodológico o un tamaño de muestra muy pequeño."
        )

        # Recomendaciones
        tagList(
          veredicto,
          div(
            style = "border-left: 4px solid #5FA2CE; padding-left: 12px;",
            tags$p(tags$strong("Contexto del efecto:"), class = "mb-1 small"),
            tags$p(interpretacion_efecto, class = "small text-muted mb-0")
          ),
          br(),
          div(
            class = "small text-muted",
            style = "background-color: #EEF3FA; padding: 10px 12px; border-radius: 4px;",
            tags$strong("💡 Recordá siempre: "),
            "reportar el valor exacto de p, el tamaño del efecto con su IC 95%,
             y no limitar la conclusión a si p superó o no el umbral de 0.05."
          )
        )
      })
    })

  })
}
