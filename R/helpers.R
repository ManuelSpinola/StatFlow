# ============================================================
# helpers.R — Funciones y objetos compartidos entre módulos
# ============================================================

library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(DT)
library(scales)
library(effectsize)

# ── Paleta de colores AnalizApp ────────────────────────────
# Opción E — Teal académico + Rojo institucional UNA
colores <- list(
  fondo       = "#F0FAF7",  # verde agua pálido — fondo general
  primario    = "#0D6E56",  # teal oscuro — navbar, encabezados
  acento      = "#1D9E75",  # teal vibrante — botones, íconos activos
  texto       = "#4A4A4A",  # gris oscuro — texto principal
  una         = "#A93226",  # rojo UNA — acento institucional
  exito       = "#27AE60",  # verde — valores positivos en gráficos
  advertencia = "#E9C46A",  # ámbar — valores medios
  peligro     = "#C0392B",  # rojo — valores negativos o errores
  borde       = "#C8EBE0"   # verde pálido — bordes y separadores
)

# ── Tema visual ────────────────────────────────────────────
tema_app <- bs_theme(
  version      = 5,
  bg           = colores$fondo,
  fg           = colores$texto,
  primary      = colores$primario,
  secondary    = colores$acento,
  success      = colores$exito,
  danger       = colores$peligro,
  warning      = colores$advertencia,
  base_font    = font_google("Nunito"),
  heading_font = font_google("Nunito", wght = 700),
  bootswatch   = NULL
) |>
  bs_add_rules("
    .navbar { background-color: #0D6E56 !important; }
    .navbar-brand, .nav-link { color: #ffffff !important; }
    .nav-link.active { border-bottom: 2px solid #A93226; }
    .btn-primary { background-color: #A93226; border-color: #A93226; }
    .btn-primary:hover { background-color: #8a2720; border-color: #8a2720; }
  ")

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
datos_ejemplo <- list(
  fauna = tibble(
    especie       = sample(c("Danta", "Pizote", "Tepezcuintle", "Puma"), 60, replace = TRUE),
    zona          = sample(c("Norte", "Sur", "Este"), 60, replace = TRUE),
    peso_kg       = round(c(
      rnorm(15, 200, 20), rnorm(15, 4.5, 0.8),
      rnorm(15, 8, 1.5),  rnorm(15, 55, 10)
    ), 1),
    avistamientos = sample(1:15, 60, replace = TRUE),
    mes           = sample(month.name[1:6], 60, replace = TRUE)
  ),
  arboles = tibble(
    especie   = sample(c("Ceibo", "Guanacaste", "Pochote", "Cristóbal"), 50, replace = TRUE),
    parcela   = sample(paste("Parcela", 1:5), 50, replace = TRUE),
    dap_cm    = round(rnorm(50, 45, 15), 1),
    altura_m  = round(rnorm(50, 18, 5), 1),
    cobertura = round(runif(50, 10, 95), 1)
  ),
  cobertura = tibble(
    tipo_cobertura = sample(c("Bosque primario", "Bosque secundario",
                              "Pastizal", "Matorral"), 48, replace = TRUE),
    sector         = sample(c("Sector A", "Sector B", "Sector C"), 48, replace = TRUE),
    area_ha        = round(runif(48, 0.5, 25), 2),
    porcentaje     = round(runif(48, 5, 90), 1),
    anio           = sample(2020:2024, 48, replace = TRUE)
  )
)
