library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(data.table)

source("scripts/plot-boxplot.R")
source("scripts/plot-serie.R")

DT <- `[`
file <- "datos/PROSAT_DeltaSuperior_2022-05-08.csv"

datos <- data.table::fread(file)
datos[, fecha := lubridate::dmy(fecha)]
datos <- datos[!is.na(fecha)]

humedales <- unique(datos$tipo_humed)
names(humedales) <- gsub("_", " ", humedales)

escenas <- datos[tipo_sensor == "SAR", unique(tipo_escena)]

polarizaciones <- c("HH", "VH", "VV")
bandas <- letters[1:5]  # Cambiar por las bandas reales

textos_paisajes <- yaml::read_yaml("datos/textos_paisajes.yaml")


descripcionUPModal <- function(datos) {
  modalDialog(
    title = h2(datos[["titulo"]]),
    h3("Descripcion:"),
    p(datos[["descripcion"]]),
    h3("Contexto en el Inventario Nacional de Humedales:"),
    p(datos[["contexto"]])
  )
}

detallesUI <- function(id) {
  id <- gsub("_", "-", id)
  ns <- NS(id)
  actionButton(inputId = ns("detalles"),
               shiny::icon("question-sign", lib = "glyphicon"))
}

detallesServer <- function(id, datos) {
  id <- gsub("_", "-", id)
  force(datos)
  moduleServer(
    id,
    function(input, output, session) {
      # browser()
      original <- input$detalles

      observeEvent(input$detalles, {
        if (is.null(original) || input$detalles > original) {
        showModal(
          modalDialog(
            title = h2(datos[["titulo"]]),
            p(HTML(datos[["descripcion"]]))
          )
        )
        }
      })
    }
  )
}



textos_humedales <- yaml::read_yaml("datos/textos_humedales.yaml")

humedales_names <- function(humedales) {
  unname(lapply(seq_along(humedales), function(h)
    span(humedales[[h]]$titulo,
         detallesUI(names(humedales)[[h]]),
         title = humedales[[h]]$descripcion)
  ))
}


alerta <- div(h2("El filtro no devolvió ningún dato"),
              p(style = "display:block;", "Pruebe con otra combinación de filtros"))

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    shinyjs::useShinyjs(),
    fluidRow(
      column(width = 4,
             selectInput("UP", "Unidad de paisaje",
                         choices = c("I4", "I2b")),
             actionButton("UP_info", shiny::icon("question-sign", lib = "glyphicon"),
                          style = "display:inline-block")
      ),
      column(width = 8,
             uiOutput("tipo_humed_checkbox")
      )
    ),
    fluidRow(
      column(width = 4,
             selectInput("tipo_sensor", "Tipo de sensor",
                         choices = c("SAR", "Óptico"))
      ),
      column(width = 4,
             selectInput("sensor", "Sistema satelital",
                         choices = "")
      ),
      column(width = 4,
             shinyjs::disabled(
               selectInput("tipo_escena", "Tipo de escena",
                           choices = escenas, multiple = TRUE))
      )
    ),
    fluidRow(
      column(width = 4,
             shinyjs::disabled(
               sliderInput("angulo_incidencia", "Ángulo de incidencia",
                           min = 20, max = 50, value = c(20, 50)))
      ),
      column(width = 4,
             selectInput("banda_nombre", "Polarización",
                         choices = polarizaciones, multiple = TRUE,
                         selected = polarizaciones)
      ),
      column(width = 4,
             dateRangeInput("rango_fechas", "Rango de fechas",
                            language = "es",
                            start = min(datos$fecha),
                            min = min(datos$fecha))
      )
    ),
    fluidRow(
      tabBox(width = 12,
             tabPanel("Boxplot", uiOutput("boxplot_ph")),
             tabPanel("Serie temporal", uiOutput("serie_ph"))
      )


    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observeEvent(input$UP_info, {
    showModal(descripcionUPModal(textos_paisajes[[input$UP]]))

  })


  observe({
    choices <- datos[tipo_sensor == input$tipo_sensor, unique(sensor)]
    updateSelectInput(inputId = "sensor",
                      choices = choices,
                      selected = choices[1])
  })

  observe({
    updateSelectInput(inputId = "tipo_escena",
                      choices = datos[tipo_sensor == input$tipo_sensor, unique(tipo_escena)])
  })


  output$tipo_humed_checkbox <- renderUI({
    humedales <- textos_humedales[[input$UP]]
    # browser()
    for (h in seq_along(humedales)) {
      detallesServer(names(humedales)[[h]], humedales[[h]])
    }

    checkboxGroupButtons(
      inputId = "tipo_humed",
      label = "Tipo de humedal:",
      individual = TRUE,
      choiceNames = humedales_names(humedales),
      choiceValues = names(humedales),
      selected = names(humedales)
    )


  })



  observe({
    if (input$tipo_sensor == "SAR") {
      shinyjs::enable("tipo_escena")
      shinyjs::enable("angulo_incidencia")

      updateSelectInput(inputId = "banda_nombre",
                        label = "Polarización",
                        choices = polarizaciones,
                        selected = polarizaciones)
      updateDateRangeInput(inputId = "rango_fechas",
                           min = as.Date("2006-01-01"))


    } else if (input$tipo_sensor == "Óptico") {
      shinyjs::disable("tipo_escena")
      shinyjs::disable("angulo_incidencia")

      updateSelectInput(inputId = "banda_nombre",
                        label = "Bandas",
                        choices = bandas,
                        selected = bandas)

      updateDateRangeInput(inputId = "rango_fechas",
                           min = as.Date("1984-01-01"))
    }
  })

  datos_select <- reactive({
    datos |>
      DT(UP == input$UP) |>
      DT(tipo_humed %in% input$tipo_humed) |>   # TODO: cuando es NULL
      DT(tipo_sensor == input$tipo_sensor) |>
      DT(fecha %between% input$rango_fechas) |>
      DT(banda_nombre %in% input$banda_nombre) |>
      # DT(angulo_incidencia %between% input$angulo_incidencia) |>   # los datos todavía no están
      identity()
  })


  output$boxplot_ph <- renderUI({
    if (nrow(datos_select()) == 0) {
      alerta
    } else {
      plotOutput("boxplot_plot")
    }
  })
  output$boxplot_plot <- renderPlot({
    req(nrow(datos_select()) > 0)
    plot_boxplot(datos_select())
  })

  output$serie_ph <- renderUI({
    if (nrow(datos_select()) == 0) {
      alerta
    } else {
      plotOutput("serie_plot")
    }
  })
  output$serie_plot <- renderPlot({
    req(nrow(datos_select()) > 0)
    plot_serie(datos_select())
  })


}

# Run the application
shinyApp(ui = ui, server = server)

