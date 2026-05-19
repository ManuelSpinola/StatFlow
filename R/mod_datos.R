# ============================================================
# mod_datos.R — Carga de archivos CSV / Excel
# ============================================================

mod_datos_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(4, 8),

      # ── Panel izquierdo: controles ──
      bslib::card(
        bslib::card_header(tagList(bsicons::bs_icon("folder2-open"), " Cargar datos")),
        bslib::card_body(
          p("Sube tu archivo de Excel o CSV, o elige uno de los ejemplos para practicar.",
            class = "text-muted small"),
          hr(),
          radioButtons(
            ns("fuente"), "¿De dónde vienen los datos?",
            choices = c(
              "Subir mi archivo"     = "subir",
              "Datos de ejemplo: Fauna"     = "fauna",
              "Datos de ejemplo: Árboles"   = "arboles",
              "Datos de ejemplo: Cobertura" = "cobertura"
            ),
            selected = "fauna"
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'subir'", ns("fuente")),
            fileInput(
              ns("archivo"),
              label    = NULL,
              accept   = c(".csv", ".xlsx", ".xls"),
              placeholder = "Selecciona un archivo...",
              buttonLabel = "Buscar archivo"
            )
          )
        )
      ),

      # ── Panel derecho: vista previa ──
      bslib::card(
        bslib::card_header("Vista previa de los datos"),
        bslib::card_body(
          uiOutput(ns("info_columnas")),
          hr(),
          accordion(
            open = FALSE,
            accordion_panel(
              "📖 ¿Qué tipos de variables existen?",
              bslib::layout_columns(
                col_widths = c(6, 6),
                # Numérica
                bslib::card(
                  class = "border-0 bg-light",
                  bslib::card_body(
                    tags$span(
                      class = "badge mb-2",
                      style = paste0("background-color:", colores$primario, "; color:#ffffff;"),
                      "Numérica"
                    ),
                    p("Representa cantidades medibles.", class = "small mb-2"),
                    tags$ul(
                      class = "small mb-0",
                      tags$li(tags$strong("Discreta:"), " valores enteros contables. ",
                              tags$em("Ej: número de individuos, cantidad de huevos")),
                      tags$li(tags$strong("Continua:"), " cualquier valor en un rango. ",
                              tags$em("Ej: peso, temperatura, altura"))
                    )
                  )
                ),
                # Categórica
                bslib::card(
                  class = "border-0 bg-light",
                  bslib::card_body(
                    tags$span(
                      class = "badge mb-2",
                      style = paste0("background-color:", colores$acento, "; color:#ffffff;"),
                      "Categórica"
                    ),
                    p("Representa grupos o etiquetas.", class = "small mb-2"),
                    tags$ul(
                      class = "small mb-0",
                      tags$li(tags$strong("Nominal:"), " sin orden entre categorías. ",
                              tags$em("Ej: especie, color, sexo")),
                      tags$li(tags$strong("Ordinal:"), " con orden definido. ",
                              tags$em("Ej: nivel educativo, intensidad del dolor"))
                    )
                  )
                )
              )
            )
          ),
          br(),
          DT::DTOutput(ns("tabla_preview"))
        )
      )
    )
  )
}

mod_datos_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # ── Reactive principal: devuelve el dataframe activo ──
    datos <- reactive({
      df <- if (input$fuente == "subir") {
        req(input$archivo)
        ext <- tools::file_ext(input$archivo$name)
        df  <- leer_archivo(input$archivo$datapath, tolower(ext))
        validate(need(!is.null(df), "No se pudo leer el archivo. Verifica que sea CSV o Excel."))
        df
      } else {
        datos_ejemplo[[input$fuente]]
      }

      # Convertir character a factor
      df |> mutate(across(where(is.character), as.factor))
    })

    # ── Info de columnas ──
    output$info_columnas <- renderUI({
      df <- datos()
      tipos <- map_chr(df, tipo_variable)
      tags$div(
        class = "d-flex flex-wrap gap-2 mb-2",
        imap(tipos, function(tipo, nombre) {
          estilo <- if (tipo == "Numérica")
            paste0("background-color:", colores$primario, "; color:#ffffff;")
          else
            paste0("background-color:", colores$acento, "; color:#ffffff;")
          tags$span(
            class = "badge",
            style = estilo,
            paste0(nombre, " (", tipo, ")")
          )
        })
      )
    })

    # ── Tabla preview ──
    output$tabla_preview <- DT::renderDT({
      DT::datatable(
        datos(),
        options = list(
          pageLength = 8,
          scrollX    = TRUE,
          language   = list(
            search      = "Buscar:",
            lengthMenu  = "Mostrar _MENU_ filas",
            info        = "Mostrando _START_ a _END_ de _TOTAL_ registros",
            paginate    = list(previous = "Anterior", `next` = "Siguiente")
          )
        ),
        rownames = FALSE,
        class    = "table table-sm table-hover"
      )
    })

    # ── Devolver reactive para otros módulos ──
    return(datos)
  })
}
