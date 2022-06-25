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

bandas_interes <- c("B1", "B2",  "B3", "B4", "B5", "B6", "B7", "B8", "B11", "B12")
bandas_interes_labels <- c("Aerosoles costeros", "Azul",  "Verde", "Rojo", "Borde rojo 1", "Borde rojo 2", "Borde rojo 3", "IR cercano", "IR medio 1", "IR medio 2")


optico <- data.table::fread("datos/PROSAT_DeltaSuperior_Optico_S2Harmonized_UPI4.csv")

colnames(optico)[20:21] <- c("tipo_sensor", "sensor")
optico <- optico[, colnames(optico) %in% columnas_importantes, with = FALSE]

optico <- optico[banda_nombre %in% bandas_interes]
optico[, valor_promedio := valor_promedio * 0.01]


optico[, tipo_humed := gsub(" ", "_", tipo_humed[1]), by = tipo_humed]

sar <- data.table::fread("datos/PROSAT_DeltaSuperior_SAR_SENTINEL1_20220614.csv")
sar <- sar[, colnames(sar) %in% columnas_importantes, with = FALSE]


datos <- rbind(sar, optico, use.names = TRUE)
datos[, fecha := lubridate::ymd(fecha[1]), by = fecha]
datos <- datos[!is.na(fecha)]

saveRDS(datos, "datos/datos.Rds", compress = TRUE)
