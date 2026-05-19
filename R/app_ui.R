#' Application UI
#'
#' @return A Shiny UI object.
#' @noRd
app_ui <- function() {

  golem::add_resource_path(
    "www",
    system.file("app/www", package = "StatFlow")
  )

  bslib::page_navbar(
    title = div(
      style = "display: flex; align-items: center; gap: 10px; margin-top: 4px;",
      img(src = "www/hexsticker_StatFlow.png", height = "38px"),
      span("StatFlow", style = "font-weight: 600;")
    ),
    theme  = tema_app,
    lang   = "es",
    footer = div(
      class = "text-center text-muted small py-2",
      style = paste0("border-top: 1px solid ", colores$borde, ";"),
      "Manuel Sp\u00ednola \u00b7 ICOMVIS \u00b7 Universidad Nacional \u00b7 Costa Rica"
    ),

    bslib::nav_panel(title = "Mis datos",            icon = bsicons::bs_icon("folder2-open"),   mod_datos_ui("datos")),
    bslib::nav_panel(title = "Explorar",             icon = bsicons::bs_icon("search"),          mod_explorar_ui("explorar")),
    bslib::nav_panel(title = "Gr\u00e1ficos",        icon = bsicons::bs_icon("graph-up"),        mod_graficos_ui("graficos")),
    bslib::nav_panel(title = "Comparar medias",      icon = bsicons::bs_icon("bar-chart"),       mod_medias_ui("medias")),
    bslib::nav_panel(title = "Comparar frecuencias", icon = bsicons::bs_icon("pie-chart"),       mod_frecuencias_ui("frecuencias")),
    bslib::nav_panel(title = "Correlaci\u00f3n",     icon = bsicons::bs_icon("diagram-3"),       mod_correlacion_ui("correlacion")),
    bslib::nav_panel(title = "Valores de p",         icon = bsicons::bs_icon("bar-chart-steps"), mod_valores_p_ui("valores_p")),
    bslib::nav_panel(title = "Ayuda",                icon = bsicons::bs_icon("question-circle"), mod_ayuda_ui("ayuda")),
    mod_acerca_de_ui("acerca_de"),

    bslib::nav_spacer(),
    bslib::nav_item(tags$span(class = "text-muted small", "StatFlow v1.0"))
  )
}
