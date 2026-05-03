# AnalizApp

Herramienta de análisis y visualización de datos para usuarios sin experiencia en programación ni estadística.
    Parte de la suite Stat de herramientas educativas para ciencia de datos.
Versión 2.0 — arquitectura modular.

## Estructura del proyecto

```
statlab/
├── app.R                  ← entrada: solo carga módulos y ensambla ui/server
├── DESCRIPTION            ← dependencias para shinyapps.io
├── R/
│   └── helpers.R          ← tema visual, funciones compartidas, datos de ejemplo
└── modules/
    ├── mod_datos.R        ← carga CSV/Excel, vista previa, reactive compartido
    ├── mod_explorar.R     ← resumen descriptivo y frecuencias
    ├── mod_graficos.R     ← histograma, boxplot, barras
    ├── mod_comparar.R     ← diferencia de medias y tamaño de efecto
    └── mod_ayuda.R        ← instrucciones y glosario
```

## Instalar dependencias

```r
install.packages(c(
  "shiny", "bslib", "bsicons",
  "tidyverse", "readxl",
  "DT", "scales", "effectsize"
))
```

## Correr localmente

```r
shiny::runApp("ruta/a/statlab/")
```

## Desplegar en shinyapps.io

```r
install.packages("rsconnect")

rsconnect::setAccountInfo(
  name   = "TU_USUARIO",
  token  = "TU_TOKEN",
  secret = "TU_SECRET"
)
# Tokens: https://www.shinyapps.io/admin/#/tokens

rsconnect::deployApp(
  appDir  = "ruta/a/statlab/",
  appName = "statlab"
)
```

Después del despliegue, los usuarios acceden con un enlace — sin instalar R.
