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


optico <- data.table::fread("datos/PROSAT_DeltaSuperior_Optico_S2Harmonized_UPI4.csv")
optico <- optico[, colnames(optico) %in% columnas_importantes, with = FALSE]

optico[, tipo_humed := gsub(" ", "_", tipo_humed[1]), by = tipo_humed]

sar <- data.table::fread("datos/PROSAT_DeltaSuperior_SAR_SENTINEL1_20220614.csv")
sar <- sar[, colnames(sar) %in% columnas_importantes, with = FALSE]


datos <- rbind(sar, optico, use.names = TRUE)
datos[, fecha := lubridate::ymd(fecha[1]), by = fecha]
datos <- datos[!is.na(fecha)]

saveRDS(datos, "datos/datos.Rds", compress = TRUE)
