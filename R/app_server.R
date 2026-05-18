#' Application Server
#'
#' @param input,output,session Internal parameters for Shiny.
#' @noRd
app_server <- function(input, output, session) {

  # mod_datos_server devuelve un reactive() con el dataframe activo.
  # Ese reactive se pasa como argumento a los demás módulos.
  datos_activos <- mod_datos_server("datos")

  mod_explorar_server("explorar",       datos = datos_activos)
  mod_graficos_server("graficos",       datos = datos_activos)
  mod_medias_server("medias",           datos = datos_activos)
  mod_frecuencias_server("frecuencias", datos = datos_activos)
  mod_correlacion_server("correlacion", datos = datos_activos)
  mod_valores_p_server("valores_p")
  mod_ayuda_server("ayuda")
  mod_acerca_de_server("acerca_de")
}
