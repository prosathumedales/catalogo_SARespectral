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

