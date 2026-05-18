# ============================================================
# mod_ayuda.R — Instrucciones de uso y glosario básico
# ============================================================

mod_ayuda_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(6, 6),

      card(
        card_header(tagList(bs_icon("question-circle"), " ¿Cómo usar esta herramienta?")),
        card_body(
          tags$ol(
            class = "ps-3",
            tags$li(tags$strong("Mis datos:"), " Sube tu archivo Excel o CSV,",
                    " o elige un dataset de ejemplo para practicar."),
            tags$li(tags$strong("Explorar:"), " Selecciona una variable para ver",
                    " sus estadísticas o tabla de frecuencias."),
            tags$li(tags$strong("Gráficos:"), " Elige el tipo de gráfico,",
                    " la variable y personaliza el color o título."),
            tags$li(tags$strong("Comparar grupos:"), " Selecciona una variable numérica",
                    " y una columna de grupos para ver la diferencia entre dos grupos."),
          ),
          hr(),
          tags$h6("Formatos de archivo aceptados"),
          tags$ul(
            tags$li(tags$strong(".xlsx / .xls"), " — archivos de Microsoft Excel"),
            tags$li(tags$strong(".csv"), " — archivo de texto separado por comas")
          ),
          hr(),
          tags$p(class = "text-muted small",
                 "Si tu archivo no carga, verifica que la primera fila tenga los nombres",
                 " de las columnas (encabezados) y que los datos empiecen en la segunda fila.")
        )
      ),

      card(
        card_header(tagList(bs_icon("book"), " Glosario básico")),
        card_body(
          tags$dl(
            tags$dt("Promedio (media)"),
            tags$dd("Suma de todos los valores dividida entre la cantidad de datos.",
                    " Ejemplo: si hay 3 animales con pesos 10, 12 y 14 kg,",
                    " el promedio es 12 kg."),
            tags$dt("Mediana"),
            tags$dd("El valor que queda en el centro cuando ordenamos los datos.",
                    " Es más resistente a valores extremos que el promedio."),
            tags$dt("Desviación estándar"),
            tags$dd("Indica qué tan dispersos están los datos alrededor del promedio.",
                    " Valor alto = datos muy variables; valor bajo = datos parecidos entre sí."),
            tags$dt("Frecuencia"),
            tags$dd("Cuántas veces aparece un valor o categoría en los datos."),
            tags$dt("Tamaño del efecto (d de Cohen)"),
            tags$dd("Mide qué tan grande es la diferencia entre dos grupos",
                    " considerando la variabilidad natural de los datos.",
                    " Valores: pequeño (0.2), moderado (0.5), grande (0.8).")
          )
        )
      )
    )
  )
}

mod_ayuda_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Este módulo es solo de presentación — no necesita lógica de servidor
  })
}
