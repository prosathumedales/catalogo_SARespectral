source("scripts/globals.R")

plot_serie_sar <- function(datos) {
  fechas <- range(datos$fecha)
  fechas <- paste0(fechas, collapse = " a ")
  sensor <- unique(datos$tipo_sensor)
  title <- paste0("Dinámica temporal del tipo de humedal en el período ",
                  fechas,
                  " con datos satelitales ", sensor)

  datos <- datos[banda_nombre %in% c(gl$indices_sinteticos,
                                     gl$polarizaciones)] |>
    DT(, .(valor_promedio = mean(valor_promedio)), by = .(fecha, tipo_humed, banda_nombre))

  if (sensor == "SAR") {
    color_lab <- "Polarización"
    ylab <- "Retrodispersión (decibeles)"

    datos[, valor_promedio := decibel(valor_promedio)]

  } else {
    color_lab <- "Índice Sintético"
    ylab <- "Índice sintético"

  }
  paleta_bandas <- gl$paleta_bandas[names(gl$paleta_bandas) %in% unique(datos$banda_nombre)]

  datos |>
    ggplot(aes(fecha, valor_promedio)) +
    geom_line(aes(group = interaction(tipo_humed, banda_nombre),
                  color = banda_nombre)) +
    geom_point(aes(color = banda_nombre)) +
    labs(title = title) +
    theme_minimal() +
    scale_x_date("Fecha", date_labels = "%m/%Y") +
    scale_y_continuous(ylab) +

    scale_color_manual(color_lab, values = paleta_bandas,
                       guide = guide_legend(title.position = "top",
                                            keywidth = grid::unit(1, "lines"))) +
    theme(legend.direction = "horizontal", legend.position = "bottom")  +
    facet_wrap(~gsub("_", " ", tipo_humed), ncol = 2)

}



plot_boxplot <- function(datos, textos_humedales) {
  datos <- datos[!(banda_nombre %in% gl$bandas_especiales)]
  fechas <- range(datos$fecha)
  fechas <- paste0(fechas, collapse = " a ")
  sensor <- unique(datos$tipo_sensor)
  title <- paste0("Respuesta media del tipo de humedal en el período ",
                  fechas,
                  " con datos satelitales ", sensor)

  paleta <- gl$paleta_humedales
  names(paleta) <-  gsub("_", " ", names(textos_humedales[[1]]))

  ggplot(datos, aes(banda_nombre, decibel(valor_promedio))) +
    geom_boxplot(aes(color = gsub("_", " ", tipo_humed))) +
    scale_x_discrete("Polarizacion") +
    scale_y_continuous("Retrodispersión (decibeles)") +
    theme_minimal() +
    theme(legend.direction = "horizontal", legend.position = "bottom") +
    labs(title = title) +
    scale_color_manual("Tipo de Humedal", values = paleta, limits = force,
                       guide = guide_legend(title.position = "top",
                                            keywidth = grid::unit(1, "lines")))
}


plot_respuesta_polarimetrica <- function(datos, textos_humedales) {
  datos <- datos[banda_nombre %in% gl$bandas_interes]
  fechas <- range(datos$fecha)
  fechas <- paste0(fechas, collapse = " a ")
  sensor <- unique(datos$tipo_sensor)
  title <- paste0("Respuesta media del tipo de humedal en el período ",
                  fechas,
                  " con datos satelitales ", sensor)

  paleta <- gl$paleta_humedales
  names(paleta) <-  gsub("_", " ", names(textos_humedales[[1]]))

  datos <- datos[, .(valor_promedio = mean(valor_promedio)), by = .(tipo_humed, banda_nombre)]

  gl$lambdas[datos,  on  = "banda_nombre"] |>
    ggplot(aes(lambda, valor_promedio)) +
    geom_line(aes(color = gsub("_", " ", tipo_humed))) +
    geom_point(aes(color = gsub("_", " ", tipo_humed))) +
    scale_x_continuous("Longitud de onda (nm)") +
    scale_y_continuous("Reflectancia (%)") +
    theme_minimal() +
    theme(legend.direction = "horizontal", legend.position = "bottom") +
    labs(title = title) +
    scale_color_manual("Tipo de Humedal", values = paleta, limits = force,
                       guide = guide_legend(title.position = "top",
                                            keywidth = grid::unit(1, "lines")))


}



plot_freeman <- function(datos, textos_humedales) {
  datos <- datos[banda_nombre %in% gl$bandas_freeman]

  fechas <- range(datos$fecha)
  fechas <- paste0(fechas, collapse = " a ")
  sensor <- unique(datos$tipo_sensor)
  title <- paste0("Descomposición de Freeman-Durden en el período ",
                  fechas,
                  " con datos satelitales ", sensor)

  paleta <- gl$paleta_humedales
  names(paleta) <-  gsub("_", " ", names(textos_humedales[[1]]))

  freeman_breaks <- setNames(gl$bandas_freeman_descripcion, gl$bandas_freeman)

  ggplot(datos, aes(banda_nombre, valor_promedio)) +
    geom_boxplot(aes(color =  gsub("_", " ", tipo_humed))) +
    scale_x_discrete("Mecanismo de interacción según la descomposición de Freeman-Durden",
                     labels = freeman_breaks) +
    scale_y_continuous("Contribución a la potencia total (dB)") +
    theme_minimal() +
    theme(legend.direction = "horizontal", legend.position = "bottom") +
    labs(title = title) +
    scale_color_manual("Tipo de Humedal", values = paleta, limits = force,
                       guide = guide_legend(title.position = "top",
                                            keywidth = grid::unit(1, "lines")))

}


plot_entropy <- function(datos, textos_humedales) {
  fechas <- range(datos$fecha)
  fechas <- paste0(fechas, collapse = " a ")
  sensor <- unique(datos$tipo_sensor)
  title <- paste0("Descomposición de Cloude-Pottier en el período período ",
                  fechas,
                  " con datos satelitales ", sensor)

  paleta <- gl$paleta_humedales
  names(paleta) <-  gsub("_", " ", names(textos_humedales[[1]]))

  datos <- datos[banda_nombre == "Entropy"]

  datos |>
    ggplot(aes(valor_promedio, angulo_incidencia)) +
    geom_point(aes(color =  gsub("_", " ", tipo_humed))) +
    scale_x_continuous("Entropía (H)", limits = c(0, 1)) +
    scale_y_continuous("Ángulo alfa", limits = c(0, 60)) +
    scale_color_manual("Tipo de Humedal", values = paleta, limits = force,
                       guide = guide_legend(title.position = "top",
                                            keywidth = grid::unit(1, "lines"))) +
    theme_minimal() +

    theme(legend.direction = "horizontal", legend.position = "bottom") +
    labs(title = title)
}


