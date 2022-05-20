plot_serie <- function(datos) {
  datos |>
    DT(, .(valor_promedio = mean(valor_promedio)), by = .(fecha, tipo_humed, banda_nombre)) |>
    ggplot(aes(fecha, decibel(valor_promedio))) +
    geom_line(aes(group = interaction(tipo_humed, banda_nombre))) +
    geom_point() +
    theme_minimal() +
    scale_x_date("Fecha") +
    scale_y_continuous("Retrodispersión (decibeles)") +
    labs(title = "Dinámica temporal del tipo de humedal en el período con datos satelitales")
}
