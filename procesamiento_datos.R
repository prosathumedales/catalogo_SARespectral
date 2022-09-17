source("scripts/globals.R")

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

zonas <- list.files("datos_raw")

for (zona in zonas) {
  dir_raw <- file.path("datos_raw", zona)
  archivos <- list.files(dir_raw, full.names = TRUE)

  datos <- lapply(archivos, \(x) {
    datos <- data.table::fread(x)
    datos <- datos[, colnames(datos) %in% columnas_importantes, with = FALSE]
    datos
  }) |>
    data.table::rbindlist(use.names = TRUE)

  if (zona == "02-concepcion") {
    datos[tipo_humed == "Bajos_con_praderas_herbáceas_y_leñosas",
          tipo_humed := "Bajos_con_praderas_de_herbáceas_y_leñosas"]

    datos[tipo_humed == "Riberas_con_bosque",
          tipo_humed := "Riberas_con_bosques"]

    datos <- datos[sensor != "0"]

  }
  datos <- datos[banda_nombre %in% c(gl$bandas_interes, gl$indices_sinteticos, gl$polarizaciones, gl$bandas_especiales)]

  datos[tipo_sensor == "Optico" & banda_nombre %in% gl$bandas_interes,
        valor_promedio := valor_promedio * 0.01]

  datos[, fecha := lubridate::ymd(fecha[1]), by = fecha]
  datos <- datos[!is.na(fecha)]


  saveRDS(datos, file.path("datos", zona, "datos.Rds"), compress = TRUE)
}



