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
library(readxl)
library(DT)
library(scales)

# ── Paleta de colores AnalizApp ────────────────────────────
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
  .navbar-brand, .nav-link { color: #ffffff !important; }
  .nav-link.active { border-bottom: 2px solid #FC7D0B; }
  .btn-primary { background-color: #FC7D0B; border-color: #FC7D0B; color: #ffffff; }
  .btn-primary:hover { background-color: #d4680a; border-color: #d4680a; }
  .navbar-brand { display: flex !important; align-items: center !important; padding-top: 0 !important; padding-bottom: 0 !important; }
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
set.seed(42)

datos_ejemplo <- list(
  fauna = {
    # Pesos realistas por especie (Costa Rica)
    # Danta (Tapirus bairdii): 150-300 kg
    # Puma (Puma concolor): 40-80 kg
    # Tepezcuintle (Cuniculus paca): 6-12 kg
    # Pizote (Nasua narica): 3-6 kg
    n_por_especie <- 15
    tibble(
      especie = rep(c("Danta", "Puma", "Tepezcuintle", "Pizote"), each = n_por_especie),
      zona    = sample(c("Norte", "Sur", "Este"), n_por_especie * 4, replace = TRUE),
      peso_kg = round(c(
        rnorm(n_por_especie, mean = 220, sd = 25),   # Danta
        rnorm(n_por_especie, mean = 58,  sd = 8),    # Puma
        rnorm(n_por_especie, mean = 8.5, sd = 1.2),  # Tepezcuintle
        rnorm(n_por_especie, mean = 4.2, sd = 0.6)   # Pizote
      ), 1),
      avistamientos = sample(1:15, n_por_especie * 4, replace = TRUE),
      mes           = sample(month.name[1:6], n_por_especie * 4, replace = TRUE)
    )
  },
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
