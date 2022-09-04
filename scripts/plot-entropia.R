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
    scale_x_continuous("Entropía (H)") +
    scale_y_continuous("Ángulo alfa") +
    scale_color_manual("Tipo de Humedal", values = paleta, limits = force,
                       guide = guide_legend(title.position = "top",
                                            keywidth = grid::unit(1, "lines"))) +
    theme(legend.direction = "horizontal", legend.position = "bottom") +
    labs(title = title)
}


