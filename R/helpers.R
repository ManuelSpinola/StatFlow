# ============================================================
# helpers.R — Funciones y objetos compartidos entre módulos
# Paleta: Tableau Color Blind (ggthemes)
# ============================================================

library(shiny)
library(bslib)
library(tidyverse)
library(parameters)
library(performance)
library(effectsize)
library(bayestestR)
library(datawizard)
library(insight)
library(correlation)
library(readxl)
library(DT)
library(scales)

# ── Paleta de colores StatFlow ────────────────────────────
# Tableau Color Blind — accesible y profesional
colores <- list(
  fondo       = "#F4F7FB",  # azul muy pálido — fondo general
  primario    = "#1170AA",  # azul oscuro — navbar, encabezados
  acento      = "#FC7D0B",  # naranja — botones, íconos activos
  secundario  = "#5FA2CE",  # azul claro — elementos secundarios
  texto       = "#57606C",  # gris oscuro — texto principal
  exito       = "#5FA2CE",  # azul claro — valores positivos
  advertencia = "#F1CE63",  # amarillo — badge categórica, valores medios
  peligro     = "#C85200",  # naranja oscuro — errores, outliers
  borde       = "#C8D9EC",  # azul muy pálido — bordes y separadores

  # Paleta completa Tableau Color Blind para gráficos
  tableau = c(
    "#1170AA", # azul oscuro
    "#FC7D0B", # naranja
    "#A3ACB9", # gris medio
    "#57606C", # gris oscuro
    "#C85200", # naranja oscuro
    "#7BC8ED", # azul cielo
    "#5FA2CE", # azul claro
    "#F1CE63", # amarillo
    "#9F8B75", # marrón
    "#B85A0D"  # marrón naranja
  )
)

# ── Tema visual ────────────────────────────────────────────
tema_app <- bs_theme(
  version      = 5,
  bg           = colores$fondo,
  fg           = colores$texto,
  primary      = colores$primario,
  secondary    = colores$secundario,
  success      = colores$exito,
  danger       = colores$peligro,
  warning      = colores$advertencia,
  base_font    = font_google("Nunito"),
  heading_font = font_google("Nunito", wght = 700),
  bootswatch   = NULL
) |>
  bs_add_rules("
  .navbar { background-color: #1170AA !important; }
  .navbar-brand { color: #ffffff !important; display: flex !important;
                  align-items: center !important;
                  padding-top: 0 !important; padding-bottom: 0 !important; }
  .navbar .nav-link { color: #ffffff !important; }
  .navbar .nav-link.active { border-bottom: 2px solid #FC7D0B; }
  .btn-primary { background-color: #FC7D0B; border-color: #FC7D0B; color: #ffffff; }
  .btn-primary:hover { background-color: #d4680a; border-color: #d4680a; }
  .card > .card-header { background-color: #5FA2CE; color: #ffffff; font-weight: 700;
                         border-bottom: none; }
")

# ── Escala de color para gráficos (ggplot2) ───────────────
# Uso: + scale_fill_tableau_cb() o + scale_color_tableau_cb()
scale_fill_tableau_cb <- function(...) {
  scale_fill_manual(values = colores$tableau, ...)
}
scale_color_tableau_cb <- function(...) {
  scale_color_manual(values = colores$tableau, ...)
}

# ── Leer archivo CSV o Excel ───────────────────────────────
leer_archivo <- function(path, ext) {
  tryCatch({
    if (ext == "csv") {
      read_csv(path, show_col_types = FALSE)
    } else if (ext %in% c("xlsx", "xls")) {
      read_excel(path)
    }
  }, error = function(e) NULL)
}

# ── Clasificar variable ────────────────────────────────────
tipo_variable <- function(x) {
  if (is.numeric(x)) "Numérica" else "Categórica"
}

# ── Resumen numérico de una columna ───────────────────────
resumen_numerico <- function(df, col) {
  x <- df[[col]]
  x <- x[!is.na(x)]
  tibble(
    Estadístico = c("N (datos válidos)", "Promedio", "Mediana",
                    "Mínimo", "Máximo", "Desviación estándar"),
    Valor = c(
      length(x),
      round(mean(x), 2),
      round(median(x), 2),
      round(min(x), 2),
      round(max(x), 2),
      round(sd(x), 2)
    )
  )
}

# ── Datasets de ejemplo ────────────────────────────────────
# Los datos se generan en data-raw/crear_datos.R y se cargan
# desde data/ como archivos .rds (más rápido, sin dependencias).

datos_ejemplo <- list(
  fauna     = readRDS("inst/app/data/fauna.rds"),
  arboles   = readRDS("inst/app/data/arboles.rds"),
  cobertura = readRDS("inst/app/data/cobertura.rds"),
  penguins  = readRDS("inst/app/data/penguins.rds"),
  birthwt   = readRDS("inst/app/data/birthwt.rds")
)

# ── Código R reproducible: encabezado estándar ────────────
# Usada por todos los módulos de StatSuite que generan código R.
# Parámetros:
#   app    — "StatDesign", "StatFlow", "StatGeo", "StatMonitor"
#   modulo — nombre del módulo, p.ej. "Explorar", "Comparar medias"
encabezado_script <- function(app, modulo) {
  paste0(
    "# ============================================\n",
    "# ", app, " · StatSuite\n",
    "# Módulo: ", modulo, "\n",
    "# Generado: ", format(Sys.Date(), "%Y-%m-%d"), "\n",
    "# Manuel Spínola · ICOMVIS · UNA · Costa Rica\n",
    "# ============================================\n\n"
  )
}
