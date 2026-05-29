# ============================================================
# data-raw/crear_datos.R
# Genera los datasets de ejemplo de StatFlow y los guarda
# como archivos .rds en la carpeta data/
#
# Ejecutar una sola vez (o cuando se quieran regenerar):
#   source("inst/app/data-raw/crear_datos.R")
#
# Manuel Spínola · ICOMVIS · UNA · Costa Rica
# ============================================================

library(tidyverse)

set.seed(42)

# ── 1. Fauna (mamíferos de Costa Rica) ─────────────────────
# Pesos realistas por especie:
#   Danta (Tapirus bairdii):       150–300 kg
#   Puma (Puma concolor):           40–80 kg
#   Tepezcuintle (Cuniculus paca):  6–12 kg
#   Pizote (Nasua narica):           3–6 kg

n_por_especie <- 15

fauna <- tibble(
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

saveRDS(fauna, "inst/app/data/fauna.rds")
cat("✔ data/fauna.rds guardado\n")

# ── 2. Árboles ─────────────────────────────────────────────

arboles <- tibble(
  especie   = sample(c("Ceibo", "Guanacaste", "Pochote", "Cristóbal"), 50, replace = TRUE),
  parcela   = sample(paste("Parcela", 1:5), 50, replace = TRUE),
  dap_cm    = round(rnorm(50, 45, 15), 1),
  altura_m  = round(rnorm(50, 18, 5), 1),
  cobertura = round(runif(50, 10, 95), 1)
)

saveRDS(arboles, "inst/app/data/arboles.rds")
cat("✔ data/arboles.rds guardado\n")

# ── 3. Cobertura ────────────────────────────────────────────

cobertura <- tibble(
  tipo_cobertura = sample(c("Bosque primario", "Bosque secundario",
                            "Pastizal", "Matorral"), 48, replace = TRUE),
  sector         = sample(c("Sector A", "Sector B", "Sector C"), 48, replace = TRUE),
  area_ha        = round(runif(48, 0.5, 25), 2),
  porcentaje     = round(runif(48, 5, 90), 1),
  anio           = sample(2020:2024, 48, replace = TRUE)
)

saveRDS(cobertura, "inst/app/data/cobertura.rds")
cat("✔ data/cobertura.rds guardado\n")

# ── 4. Penguins ─────────────────────────────────────────────
# Fuente: palmerpenguins (Horst, Hill & Gorman 2020)
# Se incluye directamente para evitar dependencia del paquete.

penguins <- read_csv("inst/app/data-raw/penguins.csv", show_col_types = FALSE) |>
  mutate(across(where(is.character), as.factor))

saveRDS(penguins, "inst/app/data/penguins.rds")
cat("✔ data/penguins.rds guardado\n")

cat("\nTodos los datasets generados correctamente en data/\n")
