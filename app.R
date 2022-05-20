#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(data.table)

source("scripts/plot-boxplot.R")

DT <- `[`
file <- "datos/PROSAT_DeltaSuperior_2022-05-08.csv"

datos <- data.table::fread(file)
datos[, fecha := lubridate::dmy(fecha)]
datos <- datos[!is.na(fecha)]

humedales <- unique(datos$tipo_humed)
names(humedales) <- gsub("_", " ", humedales)

escenas <- datos[tipo_sensor == "SAR", unique(tipo_escena)]


# Define UI for application that draws a histogram
ui <- fluidPage(
  shinyjs::useShinyjs(),
  # Application title
  titlePanel("App borrador no publicar"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("UP", "Unidad de paisaje",
                  choices = c("I4", "I2b")),
      selectInput("tipo_humed", "Tipo de humedal",
                  choices = humedales, multiple = TRUE, selected = humedales),
      selectInput("tipo_sensor", "Tipo de sensor",
                  choices = c("SAR", "Óptico")),

      selectInput("sensor", "Sistema satelital",
                  choices = ""),

      shinyjs::disabled(
        selectInput("tipo_escena", "Tipo de escena",
                    choices = escenas, multiple = TRUE)),

      shinyjs::disabled(
        sliderInput("angulo_incidencia", "Ángulo de incidencia",
                    min = 20, max = 50, value = c(20, 50))),

      shinyjs::disabled(
        selectInput("banda_nombre", "Polarización",
                    choices = c("HH", "VH", "VV"), multiple = TRUE,
                    selected = c("HH", "VH", "VV"))
      ),

      shiny::dateRangeInput("rango_fechas", "Rango de fechas",
                            language = "es",
                            start = min(datos$fecha),
                            min = min(datos$fecha)),

    ),

    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Boxplot", plotOutput("boxplot")),
                  tabPanel("Serie temporal", plotOutput("serie"))
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # shinyjs::disable("test")

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

  observe({
    choices <- datos[UP == input$UP, unique(tipo_humed)]
    updateSelectInput(inputId = "tipo_humed",
                      choices = choices,
                      selected = choices)
  })

  observe({
    if (input$tipo_sensor == "SAR") {
      shinyjs::enable("tipo_escena")
      shinyjs::enable("angulo_incidencia")
      shinyjs::enable("banda_nombre")

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


  output$boxplot <- renderPlot({
    plot_boxplot(datos_select())
  })

  output$serie <- renderPlot({

    datos_select() |>
      DT(, .(valor_promedio = mean(valor_promedio)), by = .(fecha, tipo_humed, banda_nombre)) |>
      ggplot(aes(fecha, decibel(valor_promedio))) +
      geom_line(aes(group = interaction(tipo_humed, banda_nombre))) +
      geom_point()


  })
}

# Run the application
shinyApp(ui = ui, server = server)
