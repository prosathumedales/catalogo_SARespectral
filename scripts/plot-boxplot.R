source("scripts/globals.R")



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
    scale_color_manual("Tipo de Humedal", values = paleta_humedales, limits = force,
                       guide = guide_legend(title.position = "top",
                                            keywidth = grid::unit(1, "lines")))


}
