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

  nav_panel("📂 Mis datos",           mod_datos_ui("datos")),
  nav_panel("🔍 Explorar",            mod_explorar_ui("explorar")),
  nav_panel("📈 Gráficos",            mod_graficos_ui("graficos")),
  nav_panel("⚖️ Comparar medias",     mod_medias_ui("medias")),
  nav_panel("📊 Comparar frecuencias", mod_frecuencias_ui("frecuencias")),
  nav_panel("❓ Ayuda",                mod_ayuda_ui("ayuda")),

  nav_spacer(),
  nav_item(tags$span(class = "text-muted small", "StatFlow v2.0"))
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
}

# ── 5. Lanzar ──────────────────────────────────────────────
shinyApp(ui, server)
