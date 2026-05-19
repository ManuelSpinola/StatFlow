# ============================================================
# mod_acerca_de.R — Módulo "Acerca de" para StatFlow
#
# Muestra información sobre R y los paquetes utilizados,
# así como las citas para referencia académica.
#
# StatSuite:
#   StatDesign  — Diseño de estudios y muestreo
#   StatFlow    — Primeros análisis y visualización  ← esta app
#   StatGeo     — Para trabajar con mapas (SIG)
#   StatModels  — Modelos avanzados (próximamente)
# ============================================================

library(bsicons)

mod_acerca_de_ui <- function(id) {
  ns <- NS(id)
  bslib::nav_panel(
    title = "Acerca de",
    icon  = bsicons::bs_icon("info-circle"),

    bslib::layout_columns(
      col_widths = c(12),
      gap        = "1rem",

      # ── Desarrollado con R ────────────────────────────────
      bslib::card(
        bslib::card_header(
          bsicons::bs_icon("code-slash"), " Desarrollado con R"
        ),
        bslib::card_body(
          p("StatFlow fue desarrollada con R y los siguientes paquetes de código abierto."),

          # Entorno R
          p(class = "text-muted small fw-bold mt-3 mb-1", "ENTORNO DE DESARROLLO"),
          div(
            style = "display: flex; align-items: center; gap: 12px; background: var(--bs-secondary-bg); border-radius: 8px; padding: 10px 14px;",
            bsicons::bs_icon("r-circle", size = "1.2em"),
            tags$strong("R Project for Statistical Computing"),
            tags$span(
              style = "color: gray; font-size: 0.85em;",
              paste0("v", R.version$major, ".", R.version$minor)
            ),
            tags$a(
              "r-project.org",
              href   = "https://www.r-project.org",
              target = "_blank",
              style  = "margin-left: auto; font-size: 0.85em;"
            )
          ),

          # Paquetes
          p(class = "text-muted small fw-bold mt-3 mb-2", "PAQUETES"),
          tags$table(
            class = "table table-sm table-hover",
            style = "font-size: 0.85em;",
            tags$thead(
              tags$tr(
                tags$th("Paquete"),
                tags$th("Versión"),
                tags$th("Referencia")
              )
            ),
            tags$tbody(
              tags$tr(
                tags$td(tags$code("shiny")),
                tags$td(paste0("v", packageVersion("shiny"))),
                tags$td(tags$a("shiny.posit.co", href = "https://shiny.posit.co", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("bslib")),
                tags$td(paste0("v", packageVersion("bslib"))),
                tags$td(tags$a("rstudio.github.io/bslib", href = "https://rstudio.github.io/bslib", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("bsicons")),
                tags$td(paste0("v", packageVersion("bsicons"))),
                tags$td(tags$a("github.com/rstudio/bsicons", href = "https://github.com/rstudio/bsicons", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("tidyverse")),
                tags$td(paste0("v", packageVersion("tidyverse"))),
                tags$td(tags$a("tidyverse.org", href = "https://www.tidyverse.org", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("parameters")),
                tags$td(paste0("v", packageVersion("parameters"))),
                tags$td(tags$a("easystats.github.io/parameters", href = "https://easystats.github.io/parameters", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("performance")),
                tags$td(paste0("v", packageVersion("performance"))),
                tags$td(tags$a("easystats.github.io/performance", href = "https://easystats.github.io/performance", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("effectsize")),
                tags$td(paste0("v", packageVersion("effectsize"))),
                tags$td(tags$a("easystats.github.io/effectsize", href = "https://easystats.github.io/effectsize", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("bayestestR")),
                tags$td(paste0("v", packageVersion("bayestestR"))),
                tags$td(tags$a("easystats.github.io/bayestestR", href = "https://easystats.github.io/bayestestR", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("datawizard")),
                tags$td(paste0("v", packageVersion("datawizard"))),
                tags$td(tags$a("easystats.github.io/datawizard", href = "https://easystats.github.io/datawizard", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("insight")),
                tags$td(paste0("v", packageVersion("insight"))),
                tags$td(tags$a("easystats.github.io/insight", href = "https://easystats.github.io/insight", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("readxl")),
                tags$td(paste0("v", packageVersion("readxl"))),
                tags$td(tags$a("readxl.tidyverse.org", href = "https://readxl.tidyverse.org", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("DT")),
                tags$td(paste0("v", packageVersion("DT"))),
                tags$td(tags$a("rstudio.github.io/DT", href = "https://rstudio.github.io/DT", target = "_blank"))
              ),
              tags$tr(
                tags$td(tags$code("scales")),
                tags$td(paste0("v", packageVersion("scales"))),
                tags$td(tags$a("scales.r-lib.org", href = "https://scales.r-lib.org", target = "_blank"))
              )
            )
          )
        )
      ),

      # ── Citas ─────────────────────────────────────────────
      bslib::card(
        bslib::card_header(
          bsicons::bs_icon("journal-text"), " Citas"
        ),
        bslib::card_body(
          p("Si utilizás StatFlow en tu investigación, por favor citá R y esta aplicación."),

          p(class = "text-muted small fw-bold mt-3 mb-1", "CÓMO CITAR R"),
          tags$pre(
            style = "background: var(--bs-secondary-bg); border-left: 3px solid #6c757d; border-radius: 0 8px 8px 0; padding: 10px 14px; font-size: 0.8em; white-space: pre-wrap;",
            "R Core Team (2026). R: A Language and Environment for Statistical
Computing. R Foundation for Statistical Computing, Vienna, Austria.
https://www.R-project.org/"
          ),

          p(class = "text-muted small fw-bold mt-3 mb-1", "CÓMO CITAR ESTA APLICACIÓN"),
          tags$pre(
            style = "background: var(--bs-secondary-bg); border-left: 3px solid #6c757d; border-radius: 0 8px 8px 0; padding: 10px 14px; font-size: 0.8em; white-space: pre-wrap;",
            "Spínola, M. (2026). StatFlow: Primeros análisis y visualización
[Aplicación web]. StatSuite. https://statsuite.netlify.app"
          ),

          p(class = "text-muted small fw-bold mt-3 mb-1", "ASISTENCIA EN DESARROLLO"),
          tags$pre(
            style = "background: var(--bs-secondary-bg); border-left: 3px solid #6c757d; border-radius: 0 8px 8px 0; padding: 10px 14px; font-size: 0.8em; white-space: pre-wrap;",
            "Anthropic. (2026). Claude (claude-sonnet-4-6) [Modelo de lenguaje].
El desarrollo de esta aplicación contó con asistencia de Claude
como herramienta de programación. https://www.anthropic.com"
          )
        )
      )
    )
  )
}

mod_acerca_de_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Sin lógica de servidor por ahora
  })
}
