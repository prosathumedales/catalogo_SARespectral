decibel <- function(x) {
  10*log10(x)
}

# RColorBrewer::brewer.pal(10, "Paired")
paleta_humedales <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
                      "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A")

textos_humedales <- yaml::read_yaml("datos/textos_humedales.yaml")
names(paleta_humedales) <-  gsub("_", " ", names(textos_humedales[[1]]))

indices_sinteticos <- c("NDVI", "EVI", "NDWI")
polarizaciones <- c("HH", "HV", "VH", "VV")


paleta_bandas <- c(NDVI = "#1b9e77",
                   EVI = "#d95f02",
                   NDWI = "#7570b3",
                   HH = "#1b9e77",
                   HV = "#d95f02",
                   VH = "#d95f02",
                   VV = "#7570b3"
                   )

plot_serie_sar <- function(datos) {
  fechas <- range(datos$fecha)
  fechas <- paste0(fechas, collapse = " a ")
  sensor <- unique(datos$tipo_sensor)
  title <- paste0("Dinámica temporal del tipo de humedal en el período ",
                  fechas,
                  " con datos satelitales ", sensor)

  datos <- datos[banda_nombre %in% c(indices_sinteticos,
                                     polarizaciones)]

  if (sensor == "SAR") {
    color_lab <- "Polarización"
  } else {
    color_lab <- "Índice Sintético"
  }

  datos[, .(valor_promedio = mean(valor_promedio)), by = .(fecha, tipo_humed, banda_nombre)] |>
    ggplot(aes(fecha, decibel(valor_promedio))) +
    geom_line(aes(group = interaction(tipo_humed, banda_nombre),
                  color = banda_nombre)) +
    geom_point(aes(color = banda_nombre)) +
    labs(title = title) +
    theme_minimal() +
    scale_x_date("Fecha") +
    scale_y_continuous("Retrodispersión (decibeles)") +

    scale_color_manual(color_lab, values = paleta_bandas,
                       guide = guide_legend(title.position = "top",
                                            keywidth = grid::unit(1, "lines"))) +
    theme(legend.direction = "horizontal", legend.position = "bottom")  +
    facet_wrap(~gsub("_", " ", tipo_humed), ncol = 2)

}

