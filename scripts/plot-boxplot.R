decibel <- function(x) {
  10*log10(x)
}

# RColorBrewer::brewer.pal(10, "Paired")
paleta_humedales <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
                      "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A")

textos_humedales <- yaml::read_yaml("datos/textos_humedales.yaml")
names(paleta_humedales) <-  gsub("_", " ", names(textos_humedales[[1]]))

bandas_interes <- c("B2",  "B3", "B4", "B5", "B6", "B7", "B8", "B11", "B12")
bandas_interes_lambda <-  c(492.5,  559.35, 664.75, 703.95, 739.7, 781.25, 832.85, 1612.05, 2194.05)
names(bandas_interes) <- c("Azul",  "Verde", "Rojo", "Borde rojo 1",
                           "Borde rojo 2", "Borde rojo 3", "IR cercano",
                           "IR medio 1", "IR medio 2")

lambdas <- data.table(banda_nombre = names(bandas_interes),
                      lambda = bandas_interes_lambda)

plot_boxplot <- function(datos) {
  fechas <- range(datos$fecha)
  fechas <- paste0(fechas, collapse = " a ")
  sensor <- unique(datos$tipo_sensor)
  title <- paste0("Respuesta media del tipo de humedal en el período ",
                  fechas,
                  " con datos satelitales ", sensor)


  ggplot(datos, aes(banda_nombre, decibel(valor_promedio))) +
    geom_boxplot(aes(color = gsub("_", " ", tipo_humed))) +
    scale_x_discrete("Polarizacion") +
    scale_y_continuous("Retrodispersión (decibeles)") +
    theme_minimal() +
    theme(legend.direction = "horizontal", legend.position = "bottom") +
    labs(title = title) +
    scale_color_manual("Tipo de Humedal", values = paleta_humedales, limits = force,
                       guide = guide_legend(title.position = "top",
                                            keywidth = grid::unit(1, "lines")))
}


plot_respuesta_polarimetrica <- function(datos) {

  fechas <- range(datos$fecha)
  fechas <- paste0(fechas, collapse = " a ")
  sensor <- unique(datos$tipo_sensor)
  title <- paste0("Respuesta media del tipo de humedal en el período ",
                  fechas,
                  " con datos satelitales ", sensor)

  datos <- datos |>
    DT(, .(valor_promedio = mean(valor_promedio)), by = .(tipo_humed, banda_nombre))

  lambdas[datos,  on  = "banda_nombre"] |>
    ggplot(aes(lambda, valor_promedio)) +
    geom_line(aes(color = gsub("_", " ", tipo_humed))) +
    geom_point(aes(color = gsub("_", " ", tipo_humed))) +
    scale_x_continuous("Longitud de onda (nm)") +
    scale_y_continuous("Retrodispersión (decibeles)") +
    theme_minimal() +
    theme(legend.direction = "horizontal", legend.position = "bottom") +
    labs(title = title) +
    scale_color_manual("Tipo de Humedal", values = paleta_humedales, limits = force,
                       guide = guide_legend(title.position = "top",
                                            keywidth = grid::unit(1, "lines")))


}
