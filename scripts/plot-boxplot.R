decibel <- function(x) {
  10*log10(x)
}

plot_boxplot <- function(datos) {

  fechas <- range(datos$fecha)
  fechas <- paste0(fechas, collapse = " a ")
  sensor <- unique(datos$sensor)
  title <- paste0("Respuesta media del tipo de humedal en el período ",
                  fechas,
                  " con datos satelitales ", sensor)

  xlab <- ifelse(sensor == "SENTINEL", "Polarización", "Banda")

  ggplot(datos, aes(banda_nombre, decibel(valor_promedio))) +
    geom_boxplot(aes(color = gsub("_", " ", tipo_humed))) +
    scale_x_discrete(xlab) +
    scale_y_continuous("Retrodispersión (decibeles)") +
    theme_minimal() +
    theme(legend.direction = "horizontal", legend.position = "bottom") +
    labs(title = title) +
    scale_color_brewer("Tipo de Humedal", palette = "Set3")
}
