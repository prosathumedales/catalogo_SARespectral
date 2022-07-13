columnas_importantes <- c("tipo_humed",
                          "tipo_sensor",
                          "UP",
                          "sensor",
                          "tipo_escena",
                          "angulo_incidencia",
                          "banda_nombre",
                          "fecha",
                          "valor_promedio"
)



archivos <- list.files("datos_raw", full.names = TRUE)


a <- archivos[1]

datos <- lapply(archivos, \(x) {
  datos <- data.table::fread(x)
  datos <- datos[, colnames(datos) %in% columnas_importantes, with = FALSE]
  datos
}) |>
  data.table::rbindlist(use.names = TRUE)

bandas_interes <- c("B2",  "B3", "B4", "B5", "B6", "B7", "B8", "B11", "B12")
indices_sinteticos <- c("NDVI", "EVI", "NDWI")
polarizaciones <- c("HH", "HV", "VH", "VV")

datos <- datos[banda_nombre %in% c(bandas_interes, indices_sinteticos, polarizaciones)]

datos[tipo_sensor == "Optico" & banda_nombre %in% bandas_interes, valor_promedio := valor_promedio * 0.01]

datos[, fecha := lubridate::ymd(fecha[1]), by = fecha]
datos <- datos[!is.na(fecha)]

saveRDS(datos, "datos/datos.Rds", compress = TRUE)
