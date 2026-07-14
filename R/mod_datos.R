# ============================================================
# mod_datos.R — Carga y exploración de datos para StatFlow
# StatFlow · StatSuite · Manuel Spínola · ICOMVIS · UNA
#
# Exporta:
#   mod_datos_ui(id)
#   mod_datos_server(id) → reactive data.frame
# ============================================================

mod_datos_ui <- function(id) {
  ns <- NS(id)

  tagList(
    bslib::navset_card_tab(

      # ══════════════════════════════════════════════════════
      # PESTAÑA 1: Cargar datos
      # ══════════════════════════════════════════════════════
      bslib::nav_panel(
        title    = tagList(bsicons::bs_icon("folder2-open", class = "me-1"), "Cargar datos"),
        fillable = FALSE,
        bslib::card_body(

          bslib::layout_columns(
            col_widths = c(4, 8),
            fill       = FALSE,

            # ── Panel izquierdo: controles ──
            bslib::card(
              fill = FALSE,
              bslib::card_header(tagList(bsicons::bs_icon("folder2-open"), " Cargar datos")),
              bslib::card_body(
                p("Sube tu archivo de Excel o CSV, o elige uno de los ejemplos para practicar.",
                  class = "text-muted small"),
                hr(),
                radioButtons(
                  ns("fuente"), "¿De dónde vienen los datos?",
                  choices = c(
                    "Subir mi archivo"                         = "subir",
                    "Datos de ejemplo: Fauna"                  = "fauna",
                    "Datos de ejemplo: Árboles"                = "arboles",
                    "Datos de ejemplo: Cobertura"              = "cobertura",
                    "Datos de ejemplo: Pingüinos"              = "penguins",
                    "Datos de ejemplo: Salud materno-infantil" = "birthwt"
                  ),
                  selected = "fauna"
                ),
                conditionalPanel(
                  condition = sprintf("input['%s'] == 'subir'", ns("fuente")),
                  fileInput(
                    ns("archivo"),
                    label       = NULL,
                    accept      = c(".csv", ".xlsx", ".xls"),
                    placeholder = "Selecciona un archivo...",
                    buttonLabel = "Buscar archivo"
                  )
                ),
                uiOutput(ns("info_dataset"))
              )
            ),

            # ── Panel derecho: vista previa ──
            bslib::card(
              fill = FALSE,
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
                      fill       = FALSE,
                      bslib::card(
                        fill  = FALSE,
                        class = "border-0 bg-light",
                        bslib::card_body(
                          tags$span(class = "badge mb-2",
                                    style = paste0("background-color:", colores$primario, "; color:#ffffff;"),
                                    "Numérica"),
                          p("Representa cantidades medibles.", class = "small mb-2"),
                          tags$ul(class = "small mb-0",
                            tags$li(tags$strong("Discreta:"), " valores enteros contables. ",
                                    tags$em("Ej: número de individuos, cantidad de huevos")),
                            tags$li(tags$strong("Continua:"), " cualquier valor en un rango. ",
                                    tags$em("Ej: peso, temperatura, altura"))
                          )
                        )
                      ),
                      bslib::card(
                        fill  = FALSE,
                        class = "border-0 bg-light",
                        bslib::card_body(
                          tags$span(class = "badge mb-2",
                                    style = paste0("background-color:", colores$acento, "; color:#ffffff;"),
                                    "Categórica"),
                          p("Representa grupos o etiquetas.", class = "small mb-2"),
                          tags$ul(class = "small mb-0",
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
      ), # /PESTAÑA 1

      # ══════════════════════════════════════════════════════
      # PESTAÑA 2: Variables
      # ══════════════════════════════════════════════════════
      bslib::nav_panel(
        title    = tagList(bsicons::bs_icon("sliders", class = "me-1"), "Variables"),
        fillable = FALSE,
        bslib::card_body(

          p(class = "text-muted small mb-3",
            bsicons::bs_icon("info-circle", class = "me-1"),
            "Revisá el tipo detectado para cada variable y corregilo si es necesario. ",
            "Variables mal tipificadas pueden causar errores al analizar. ",
            "Podés también ", strong("excluir"), " variables que no necesitás."),

          uiOutput(ns("tabla_tipos")),
          uiOutput(ns("tipos_aplicados_msg")),

          tags$hr(),

          bslib::layout_columns(
            col_widths = c(3, 9),
            fill       = FALSE,

            bslib::card(
              fill = FALSE,
              bslib::card_header(bsicons::bs_icon("book", class = "me-1"), "Tipos de variables"),
              bslib::card_body(
                tags$ul(class = "small mb-0",
                  tags$li(
                    tags$span(class = "badge me-1",
                              style = paste0("background:", colores$primario),
                              "Numérica"),
                    " — valores continuos o discretos. Ej: peso, temperatura, conteos"
                  ),
                  tags$li(
                    tags$span(class = "badge me-1",
                              style = paste0("background:", colores$acento),
                              "Factor"),
                    " — grupos o etiquetas. Ej: especie, sexo, país, año categórico"
                  ),
                  tags$li(
                    tags$span(class = "badge me-1",
                              style = paste0("background:", colores$texto),
                              "Excluir"),
                    " — variable no se usará en los análisis"
                  )
                )
              )
            ),

            div(uiOutput(ns("resumen_tipos")))
          )
        )
      ) # /PESTAÑA 2

    ) # /navset_card_tab
  )
}


# ── Server ────────────────────────────────────────────────────────────────────
mod_datos_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ── Datos crudos ─────────────────────────────────────────────────────────
    datos <- reactive({
      if (input$fuente == "subir") {
        req(input$archivo)
        ext <- tolower(tools::file_ext(input$archivo$name))
        df  <- leer_archivo(input$archivo$datapath, ext)
        validate(need(!is.null(df),
                      "No se pudo leer el archivo. Verificá que sea CSV o Excel."))
        df |> dplyr::mutate(dplyr::across(where(is.character), as.factor))
      } else {
        datos_ejemplo[[input$fuente]] |>
          dplyr::mutate(dplyr::across(where(is.character), as.factor))
      }
    })

    # ── Tipos definidos por el usuario ───────────────────────────────────────
    tipos_usuario <- reactiveVal(NULL)

    observeEvent(input$fuente, {
      tipos_usuario(NULL)
    })

    observe({
      df <- datos()
      req(df)
      tu <- lapply(names(df), function(nm) {
        val <- input[[paste0("tipo_", nm)]]
        if (!is.null(val)) val else NULL
      })
      names(tu) <- names(df)
      tu <- tu[!sapply(tu, is.null)]
      if (length(tu) > 0) tipos_usuario(tu)
    })

    # ── Datos con tipos aplicados ────────────────────────────────────────────
    datos_conv <- reactive({
      df <- datos()
      tu <- tipos_usuario()
      req(df)
      for (nm in names(df)) {
        tipo_dest <- if (!is.null(tu) && !is.null(tu[[nm]])) tu[[nm]] else NULL
        if (is.null(tipo_dest) || tipo_dest == "excluir") next
        df[[nm]] <- switch(tipo_dest,
          "factor"  = as.factor(df[[nm]]),
          "numeric" = suppressWarnings(as.numeric(as.character(df[[nm]]))),
          df[[nm]]
        )
      }
      if (!is.null(tu)) {
        excluir <- names(tu)[sapply(tu, function(t) !is.null(t) && t == "excluir")]
        df <- df[, !names(df) %in% excluir, drop = FALSE]
      }
      df
    })

    # ── Info del dataset ─────────────────────────────────────────────────────
    output$info_dataset <- renderUI({
      if (input$fuente == "subir") return(NULL)
      info <- list(
        fauna     = list(titulo = "Fauna silvestre — Costa Rica",
                         desc   = "Registros simulados de 4 especies de mamíferos en tres zonas del país.",
                         vars   = "especie, zona, peso_kg, avistamientos, mes"),
        arboles   = list(titulo = "Árboles tropicales",
                         desc   = "Registros simulados de 50 árboles de 4 especies en 5 parcelas.",
                         vars   = "especie, parcela, dap_cm, altura_m, cobertura (%)"),
        cobertura = list(titulo = "Cobertura del suelo",
                         desc   = "Registros simulados de 48 parcelas clasificadas por tipo de cobertura y sector.",
                         vars   = "tipo_cobertura, sector, area_ha, porcentaje, anio"),
        penguins  = list(titulo = "Pingüinos de Palmer",
                         desc   = "Mediciones de 344 pingüinos de 3 especies. Fuente: Horst, Hill & Gorman (2020).",
                         vars   = "species, island, bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g, sex"),
        birthwt   = list(titulo = "Salud materno-infantil",
                         desc   = "Datos de 189 neonatos del Baystate Medical Center (1986). Fuente: MASS::birthwt.",
                         vars   = "bwt, age, lwt, race, smoke, ht, ui")
      )
      m <- info[[input$fuente]]
      div(
        class = "mt-3 p-3 rounded",
        style = paste0("background:", colores$fondo,
                       "; border-left: 4px solid ", colores$primario, ";"),
        tags$p(class = "fw-bold mb-1",
               style = paste0("color:", colores$primario), m$titulo),
        tags$p(class = "small text-muted mb-1", m$desc),
        tags$p(class = "small mb-0", tags$b("Variables: "), m$vars)
      )
    })

    # ── Badges de variables ──────────────────────────────────────────────────
    output$info_columnas <- renderUI({
      df <- datos_conv()
      req(df)
      div(
        class = "d-flex flex-wrap gap-2 mb-2",
        lapply(names(df), function(nm) {
          tipo   <- tipo_variable(df[[nm]])
          estilo <- if (tipo == "Numérica")
            paste0("background-color:", colores$primario, "; color:#ffffff;")
          else
            paste0("background-color:", colores$acento, "; color:#ffffff;")
          tags$span(class = "badge", style = estilo,
                    paste0(nm, " (", tipo, ")"))
        })
      )
    })

    # ── Tabla preview ────────────────────────────────────────────────────────
    output$tabla_preview <- DT::renderDT({
      DT::datatable(
        datos_conv(),
        options = list(
          pageLength = 8,
          scrollX    = TRUE,
          language   = list(
            search     = "Buscar:",
            lengthMenu = "Mostrar _MENU_ filas",
            info       = "Mostrando _START_ a _END_ de _TOTAL_ registros",
            paginate   = list(previous = "Anterior", `next` = "Siguiente")
          )
        ),
        rownames = FALSE,
        class    = "table table-sm table-hover"
      )
    })

    # ── Tabla de tipos con selectores ────────────────────────────────────────
    output$tabla_tipos <- renderUI({
      df <- datos()
      req(df)
      tu <- tipos_usuario()

      filas <- lapply(names(df), function(nm) {
        col    <- df[[nm]]
        actual <- if (is.factor(col) || is.character(col)) "factor" else "numeric"
        icono  <- if (actual == "factor")
          bsicons::bs_icon("tag-fill", style = paste0("color:", colores$acento))
        else
          bsicons::bs_icon("123", style = paste0("color:", colores$primario))
        sel <- if (!is.null(tu) && !is.null(tu[[nm]])) tu[[nm]] else actual

        tags$tr(
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  div(class = "d-flex align-items-center gap-2", icono, strong(nm))),
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  tags$span(class = "badge",
                            style = paste0("background:",
                              if (actual == "factor") colores$acento else colores$primario,
                              "; font-size:0.75rem;"),
                            if (actual == "factor") "Factor" else "Numérico")),
          tags$td(style = "padding:5px 8px;",
                  selectInput(
                    inputId  = ns(paste0("tipo_", nm)),
                    label    = NULL,
                    choices  = c("Numérico"             = "numeric",
                                 "Factor (categórico)"  = "factor",
                                 "Excluir"              = "excluir"),
                    selected = sel, width = "190px")),
          tags$td(style = "vertical-align:middle; padding:5px 8px;",
                  if (!is.null(tu) && !is.null(tu[[nm]]) && tu[[nm]] != actual)
                    tags$span(class = "badge",
                              style = paste0("background:", colores$exito),
                              "Modificado")
                  else
                    tags$span(class = "text-muted small", "Sin cambios"))
        )
      })

      tagList(
        tags$table(
          class = "table table-sm table-hover small mb-0",
          tags$thead(
            style = paste0("background:", colores$primario,
                           " !important; color:#fff !important;"),
            tags$tr(
              tags$th(style = "padding:7px 8px;", "Variable"),
              tags$th(style = "padding:7px 8px;", "Tipo detectado"),
              tags$th(style = "padding:7px 8px;", "Tipo a usar"),
              tags$th(style = "padding:7px 8px;", "Estado")
            )
          ),
          tags$tbody(filas)
        )
      )
    })

    output$tipos_aplicados_msg <- renderUI({
      tu <- tipos_usuario()
      if (is.null(tu)) return(NULL)
      df <- datos()
      req(df)
      n_cambios <- sum(sapply(names(tu), function(nm) {
        if (!nm %in% names(df)) return(FALSE)
        col    <- df[[nm]]
        actual <- if (is.factor(col) || is.character(col)) "factor" else "numeric"
        !is.null(tu[[nm]]) && tu[[nm]] != actual && tu[[nm]] != "excluir"
      }))
      n_excl <- sum(sapply(tu, function(t) !is.null(t) && t == "excluir"))
      if (n_cambios == 0 && n_excl == 0) return(NULL)
      div(class = "alert alert-info small py-2 px-3 mt-2 mb-0",
          bsicons::bs_icon("check-circle", class = "me-1",
                           style = paste0("color:", colores$exito)),
          if (n_cambios > 0) paste0(n_cambios, " variable(s) convertida(s). "),
          if (n_excl   > 0) paste0(n_excl,    " variable(s) excluida(s). "),
          "Los análisis usarán estos tipos.")
    })

    output$resumen_tipos <- renderUI({
      df <- datos_conv()
      req(df)
      n_num <- sum(sapply(df, is.numeric))
      n_fac <- sum(sapply(df, is.factor))
      div(
        class = "p-3 rounded",
        style = paste0("background:", colores$fondo,
                       "; border-left: 4px solid ", colores$secundario, ";"),
        tags$p(class = "fw-bold mb-2",
               style = paste0("color:", colores$primario),
               bsicons::bs_icon("bar-chart", class = "me-1"), "Resumen"),
        tags$p(class = "small mb-1", tags$b("Total de variables: "), ncol(df)),
        tags$p(class = "small mb-1",
               tags$span(class = "badge me-1",
                         style = paste0("background:", colores$primario), "Numéricas"), n_num),
        tags$p(class = "small mb-0",
               tags$span(class = "badge me-1",
                         style = paste0("background:", colores$acento), "Factores"), n_fac)
      )
    })

    # ── Devolver datos con tipos aplicados ───────────────────────────────────
    return(datos_conv)

  })
}
