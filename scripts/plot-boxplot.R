decibel <- function(x) {
  10*log10(x)
}

# RColorBrewer::brewer.pal(10, "Paired")
paleta_humedales <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
                      "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A")

textos_humedales <- yaml::read_yaml("datos/textos_humedales.yaml")
names(paleta_humedales) <-  gsub("_", " ", names(textos_humedales[[1]]))


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
    scale_color_manual("Tipo de Humedal", values = paleta_humedales, limits = force,
                       guide = guide_legend(title.position = "top",
                                            keywidth = grid::unit(1, "lines")))
}
