# ============================================================
# app.R — Punto de entrada de AnalizApp
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
source("modules/mod_comparar.R")
source("modules/mod_ayuda.R")

# ── 3. UI ──────────────────────────────────────────────────
ui <- page_navbar(
  title  = "🌿 AnalizApp",
  theme  = tema_app,
  lang   = "es",

  nav_panel("📂 Mis datos",      mod_datos_ui("datos")),
  nav_panel("🔍 Explorar",       mod_explorar_ui("explorar")),
  nav_panel("📊 Gráficos",       mod_graficos_ui("graficos")),
  nav_panel("⚖️ Comparar grupos", mod_comparar_ui("comparar")),
  nav_panel("❓ Ayuda",           mod_ayuda_ui("ayuda")),

  nav_spacer(),
  nav_item(tags$span(class = "text-muted small", "AnalizApp v2.0"))
)

# ── 4. Server ──────────────────────────────────────────────
server <- function(input, output, session) {

  # El módulo de datos devuelve un reactive() con el dataframe activo.
  # Ese reactive se pasa como argumento a los demás módulos.
  datos_activos <- mod_datos_server("datos")

  mod_explorar_server("explorar", datos = datos_activos)
  mod_graficos_server("graficos", datos = datos_activos)
  mod_comparar_server("comparar", datos = datos_activos)
  mod_ayuda_server("ayuda")
}

# ── 5. Lanzar ──────────────────────────────────────────────
shinyApp(ui, server)
