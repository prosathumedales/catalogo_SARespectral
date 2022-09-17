library(shinydashboard)
library(shiny)
library(shinyWidgets)
library(ggplot2)
library(data.table)

source("scripts/plots.R")
source("scripts/globals.R")

descripcionUPModal <- function(datos) {
  modalDialog(
    easyClose = TRUE,
    title = h2(datos[["titulo"]]),
    footer = modalButton("Cerrar"),
    h3("Descripción"),
    p(datos[["descripcion"]]),
    h3("Contexto en el Inventario Nacional de Humedales"),
    p(datos[["contexto"]]),


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
      original <- input$detalles

      observeEvent(input$detalles, {
        if (is.null(original) || input$detalles > original) {
          showModal(
            modalDialog(
              easyClose = TRUE,
              title = h2(datos[["titulo"]]),
              footer = modalButton("Cerrar"),
              p(HTML(datos[["descripcion"]]))
            )
          )
        }
      })
    }
  )
}

humedales_names <- function(humedales) {
  unname(lapply(seq_along(humedales), function(h)
    span(humedales[[h]]$titulo,
         detallesUI(names(humedales)[[h]]))
  ))
}

alerta <- div(h2("El filtro no devolvió ningún dato"),
              p(style = "display:block;", "Pruebe con otra combinación de filtros"))

zonas_dirs <- list.files("datos", pattern = "encabezado.yaml", recursive = TRUE, full.names = TRUE) |>
  dirname()
zonas_id <- basename(zonas_dirs)
zonas <- lapply(file.path(zonas_dirs, "encabezado.yaml"), yaml::read_yaml)
names(zonas) <- zonas_id
zonas_choices <- vapply(zonas, \(x) x[["etiqueta"]], character(1))
zonas_choices <- setNames(names(zonas_choices), zonas_choices)

ui <- dashboardPage(

  dashboardHeader(titleWidth = "100%",
                  title = "Catálogo de respuestas de sistemas satelitales SAR y ópticos multiespectrales "),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    shinyjs::useShinyjs(),
    # dashboardthemes::shinyDashboardThemes(
    #   theme = "blue_gradient"
    # ),
    # From https://community.rstudio.com/t/shiny-how-to-center-and-fix-width-of-dashboard/3575/5
    tags$style("html,body{background-color: white;}
                .container{
                    width: 100%;
                    margin: 0 auto;
                    padding: 0;
                }
                #myimg{
                    width:30%;
                }
               @media screen and (min-width: 800px){
                .container{
                    width: 800px;
                }
               }"),
    tags$style(".btn-default {
                  background-color: #fafafa;
               }"
    ),
    tags$style("body {
                font-size: 160%;
               }"),
    div(class="container",
        fluidRow(style = "margin:1.5em;",
                 column(style = "background-color:white; padding:1em;",
                        width = 12,
                        p(readLines("datos/encabezado.txt"))
                 )
        ),
        fluidRow(
          column(width = 12,
                 shinyWidgets::radioGroupButtons(inputId =  "zona",
                                                 label = "Seleccionar zona",
                                                 choices = zonas_choices,
                                                 selected = zonas_choices[1]
                 )
          )
        ),
        fluidRow(
          column(width = 4,
                 column(width = 10,
                        selectInput("UP", "Unidad de paisaje de humedales",
                                    choices = c("I4", "I2b"))
                 ),
                 column(width = 1,
                        actionButton("UP_info", shiny::icon("question-sign", lib = "glyphicon"),
                                     style = "margin-top: 50px")
                 )
          ),
          column(width = 8,
                 uiOutput("tipo_humed_checkbox")
          )
        ),
        fluidRow(
          column(width = 4,
                 selectInput("tipo_sensor", "Tipo de sensor satelital",
                             choices = c("SAR (microondas activas)" = "SAR",
                                         "Óptico" = "Optico"))
          ),
          column(width = 4,

                 selectInput("sensor", "Sistema satelital",
                             choices = "")
          ),
          column(width = 4,
                 shinyjs::disabled(
                   sliderInput("angulo_incidencia", "Ángulo de incidencia (señal SAR)",
                               min = 20, max = 50, value = c(20, 50),
                               post = "º"))
          )
        ),
        fluidRow(
          column(width = 6,
                 checkboxGroupButtons(
                   inputId = "banda_nombre",
                   label = "Polarización",
                   individual = TRUE,
                   choices = gl$polarizaciones,
                   selected = gl$polarizaciones
                 ),
          ),
          column(width = 6,
                 dateRangeInput("rango_fechas",
                                "Rango de fechas",
                                # HTML(paste0("Rango de fechas <small>(",
                                #             format(min( <- $fecha), "%d/%m/%Y"),
                                #             " &ndash; ",
                                #             format(max(datos$fecha), "%d/%m/%Y"),
                                #             ")</small>")
                                # ),
                                language = "es",
                                separator = "a",
                                format = "dd/mm/yyyy"
                                # start = min(datos$fecha),
                                # min = min(datos$fecha),
                                # end = max(datos$fecha),
                                # max = max(datos$fecha),
                 ),
                 tags$style(HTML(".datepicker {z-index:99999 !important;}"))
          )
        ),
        fluidRow(
          tabBox(width = 12,
                 tabPanel(textOutput("boxplot_title"), uiOutput("boxplot_ph")),
                 tabPanel("Serie temporal", uiOutput("serie_ph")),
                 tabPanel("Descomposición de Cloude-Pottier", uiOutput("entropia_ph")),
                 tabPanel("Descomposición de Freeman-Durden", uiOutput("freeman_ph"))
          )
        )
    ),
    fluidRow(
      column(width = 12,
             style = "background-color:white; padding:1em;",
             align = 'center',
             div(style = "margin:0.4em;", lapply(list.files("www/logos"), function(x) div(style = "margin:0.5em;display:inline;", img(src = file.path("logos", x), height = 40)))),
             p('El presente tablero es un producto del proyecto "Desarrollo de un sistema de monitoreo y manejo integral de humedales a partir de información satelital", financiado por la Comisión Nacional de Actividades Espaciales (CONAE) en el marco del llamado PROSAT-II.'),
             p(strong(a("Contacto - Natalia Morandeira", href = "mailto:nmorandeira@unsam.edu.ar")),
               " - ",
               strong(a("Código fuente", href = "https://github.com/prosathumedales/catalogo_SARespectral")),
               " - ",
               strong("Licencia CC BY SA 4.0"),
               br(),
               "La información de ", em("SAOCOM"), " fue generada a partir de productos SAOCOM® Originales – ©CONAE – (2022), adquiridos en el marco del proyecto.",
               br(),
               strong("Autoras/es: "), "Natalia Morandeira, Francisco Grings, Mercedes Salvia, Matías Barber, Adriana Rojas Barrios, Mariela Rajngewerc, Maira Gayol, Esteban Roitberg, Elio Campitelli, Priscilla Minotti, Patricia Kandus."
             ),
             h4("Referencias:"),

             a("Kandus et al. 2019 - Inventario de Humedales en la Región del Complejo Fluvio-Litoral del Bajo Paraná",
               href = "https://lac.wetlands.org/download/6536/"),
             br(),
             a("Benzaquén et al. 2013 - Sistemas de paisajes de humedales del Corredor fluvial Paraná-Paraguay",
               href = "http://www.unsam.edu.ar/tau/sitio/wp-content/uploads/inventario_humedales_parana_paraguay.pdf"),
             br(),
             a("Benzaquén et al. 2017 - Regiones de humedales de la Argentina",
               href = "https://www.argentina.gob.ar/sites/default/files/regioneshumedbaja2.pdf")
      )
    )
  )
)


server <- function(input, output, session) {


  data_path <- reactive(file.path("datos", input$zona))

  datos <- reactive({
    datos <- readRDS(file.path(data_path(), "datos.Rds"))

    # esto tendría que estar en el preprocesamiento
    datos$sensor <- ifelse(datos$sensor == "SENTINEL", "Sentinel-1", datos$sensor)
    datos$sensor <- ifelse(datos$sensor == "ALOS", "ALOS/PALSAR-1", datos$sensor)
    datos
  })


  textos_paisajes <- reactive(yaml::read_yaml(file.path(data_path(), "textos_paisajes.yaml")))
  textos_humedales <- reactive({
    textos_humedales <- yaml::read_yaml(file.path(data_path(), "textos_humedales.yaml"))

    # Ordeno alfabéticamente
    textos_humedales <- lapply(textos_humedales, function(x) {
      x[order(names(x))]
    })
    textos_humedales
  }
  )



  observe({
    choices <- names(textos_humedales())
    updateSelectInput(inputId = "UP", choices = choices, selected = choices[1])
  })


  observeEvent(input$UP_info, {
    showModal(descripcionUPModal(textos_paisajes()[[input$UP]]))

  })

  observe({
    choices <- datos()[tipo_sensor == input$tipo_sensor & UP == input$UP, unique(sensor)]
    updateSelectInput(inputId = "sensor",
                      choices = choices,
                      selected = choices[1])
  })
  output$tipo_humed_checkbox <- renderUI({

    humedales <- textos_humedales()[[input$UP]]

    for (h in seq_along(humedales)) {
      detallesServer(names(humedales)[[h]], humedales[[h]])
    }

    checkboxGroupButtons(
      inputId = "tipo_humed",
      label = "Seleccionar uno o más tipos de humedales a graficar",
      individual = TRUE,
      choiceNames = humedales_names(humedales),
      choiceValues = names(humedales),
      selected = names(humedales)
    )
  })


  output$boxplot_title <- renderText({
    if (input$tipo_sensor == "SAR") {
      "Boxplot"
    } else {
      "Firma espectral"
    }
  })
  observe({
    if (input$tipo_sensor == "SAR") {
      shinyjs::enable("angulo_incidencia")
      shinyjs::enable("banda_nombre")
      # Algunos sistemas satelitales tienen VH y otros HV
      polarizaciones <- datos()[sensor == input$sensor, unique(banda_nombre)]
      polarizaciones <- polarizaciones[polarizaciones %in% gl$polarizaciones]
      updateCheckboxGroupButtons(
        inputId = "banda_nombre",
        label = "Polarización",
        choices = polarizaciones,
        selected = polarizaciones
      )

      # No parece funcionar el update del label
      shinyjs::html(id = "banda_nombre-label",
                    html = "Polarización")
    } else if (input$tipo_sensor == "Optico") {
      shinyjs::disable("angulo_incidencia")
      updateCheckboxGroupButtons(
        inputId = "banda_nombre",
        label = "Índices Sintéticos",
        choices = gl$indices_sinteticos,
        selected = unname(gl$indices_sinteticos)
      )

      shinyjs::html(id = "banda_nombre-label",
                    html = "Índices Sintéticos")
    }


    fecha_range <- range(datos()[tipo_sensor == input$tipo_sensor]$fecha)

    updateDateRangeInput(inputId = "rango_fechas",
                         start = fecha_range[1],
                         min = fecha_range[1],
                         end = fecha_range[2],
                         max = fecha_range[2]
    )
    # updateDateRangeInput no funciona con html en el label
    # https://github.com/rstudio/shiny/issues/3079
    shinyjs::html(id = "rango_fechas-label",
                  html = paste0("Rango de fechas </br><small>Datos entre ",
                                format(fecha_range[1], "%d/%m/%Y"),
                                " y ",
                                format(fecha_range[2], "%d/%m/%Y"),
                                "</small>"))


  })

  datos_select <- reactive({
    datos <- datos() |>
      DT(UP == input$UP) |>
      DT(tipo_sensor == input$tipo_sensor) |>
      DT(rep(is.null(input$tipo_humed), .N) | tipo_humed %in% input$tipo_humed) |>
      DT(sensor == input$sensor) |>
      DT(fecha %between% input$rango_fechas)

    if (input$tipo_sensor == "SAR") {
      datos <- datos[angulo_incidencia %between% input$angulo_incidencia]
    }

    datos
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
    if (isolate(input$tipo_sensor) == "SAR") {
      plot_boxplot(datos_select(), textos_humedales()[[input$UP]])
    } else {
      plot_respuesta_polarimetrica(datos_select(), textos_humedales()[[input$UP]])
    }
  })

  output$serie_ph <- renderUI({
    if (nrow(datos_select()) == 0) {
      alerta
    } else {
      plotOutput("serie_plot_sar")
    }
  })

  output$serie_plot_sar <- renderPlot({
    req(nrow(datos_select()) > 0)
    plot_serie_sar(datos_select())
  })



  output$entropia_ph <- renderUI({
    if (nrow(datos_select()[banda_nombre == "Entropy"]) == 0) {
      alerta
    } else {
      plotOutput("entropia")
    }
  })

  output$entropia <- renderPlot({
    req(nrow(datos_select()[banda_nombre == "Entropy"])  > 0)
    plot_entropy(datos_select(), textos_humedales()[[input$UP]])
  })


  output$freeman_ph <- renderUI({
    if (nrow(datos_select()[banda_nombre %in% gl$bandas_freeman]) == 0) {
      alerta
    } else {
      plotOutput("freeman")
    }
  })

  output$freeman <- renderPlot({
    req(nrow(datos_select()[banda_nombre %in% gl$bandas_freeman])  > 0)
    plot_freeman(datos_select(), textos_humedales()[[input$UP]])
  })

}

# Run the application
shinyApp(ui = ui, server = server)

