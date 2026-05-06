# ============================================================
# app.R — Punto de entrada de StatFlow
#
# Este archivo SOLO:
#   1. Carga librerías y helpers compartidos
#   2. Carga los módulos
#   3. Define ui y server ensamblando los módulos
#
# La lógica de cada pestaña vive en modules/mod_*.R
# Las funciones compartidas viven en R/helpers.R
# ============================================================

# ── 1. Librerías y helpers ─────────────────────────────────
source("R/helpers.R")

# ── 2. Módulos ─────────────────────────────────────────────
source("modules/mod_datos.R")
source("modules/mod_explorar.R")
source("modules/mod_graficos.R")
source("modules/mod_medias.R")
source("modules/mod_frecuencias.R")
source("modules/mod_ayuda.R")
source("modules/mod_acerca_de.R")

# ── 3. UI ──────────────────────────────────────────────────
ui <- page_navbar(
  title = div(
    style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
    img(
      src    = "hexsticker_StatFlow.png",
      height = "38px"
    ),
    span("StatFlow", style = "font-weight: 600;")
  ),
  theme  = tema_app,
  lang   = "es",
  footer = div(
    class = "text-center text-muted small py-2",
    style = paste0("border-top: 1px solid ", colores$borde, ";"),
    "Manuel Spínola · ICOMVIS · Universidad Nacional · Costa Rica"
  ),

  nav_panel(title = "Mis datos",           icon = bs_icon("folder2-open"),  mod_datos_ui("datos")),
  nav_panel(title = "Explorar",            icon = bs_icon("search"),         mod_explorar_ui("explorar")),
  nav_panel(title = "Gráficos",            icon = bs_icon("graph-up"),       mod_graficos_ui("graficos")),
  nav_panel(title = "Comparar medias",     icon = bs_icon("bar-chart"),      mod_medias_ui("medias")),
  nav_panel(title = "Comparar frecuencias",icon = bs_icon("pie-chart"),      mod_frecuencias_ui("frecuencias")),
  nav_panel(title = "Ayuda",               icon = bs_icon("question-circle"),mod_ayuda_ui("ayuda")),
  mod_acerca_de_ui("acerca_de"),

  nav_spacer(),
  nav_item(tags$span(class = "text-muted small", "StatFlow v1.0"))
)

# ── 4. Server ──────────────────────────────────────────────
server <- function(input, output, session) {

  # El módulo de datos devuelve un reactive() con el dataframe activo.
  # Ese reactive se pasa como argumento a los demás módulos.
  datos_activos <- mod_datos_server("datos")

  mod_explorar_server("explorar", datos = datos_activos)
  mod_graficos_server("graficos", datos = datos_activos)
  mod_medias_server("medias",     datos = datos_activos)
  mod_frecuencias_server("frecuencias", datos = datos_activos)
  mod_ayuda_server("ayuda")
  mod_acerca_de_server("acerca_de")
}

# ── 5. Lanzar ──────────────────────────────────────────────
shinyApp(ui, server)
