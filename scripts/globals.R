decibel <- function(x) {
  10*log10(x)
}

DT <- `[`


gl <- list()

gl$bandas_freeman <- c("Freeman_dbl", "Freeman_vol", "Freeman_surf")
gl$bandas_freeman_descripcion <- c("Doble rebote", "Dispersión de volumen", "Dispersión de superficie rugosa")

gl$bandas_especiales <-  c("Entropy", gl$bandas_freeman)

gl$bandas_interes <- c("B2",  "B3", "B4", "B5", "B6", "B7", "B8", "B11", "B12")
gl$bandas_interes_lambda <-  c(492.5,  559.35, 664.75, 703.95, 739.7, 781.25, 832.85, 1612.05, 2194.05)

# names(bandas_interes) <- c("Azul",  "Verde", "Rojo", "Borde rojo 1",
#                            "Borde rojo 2", "Borde rojo 3", "IR cercano",
#                            "IR medio 1", "IR medio 2")

gl$lambdas <- data.table(banda_nombre = unname(gl$bandas_interes),
                         lambda = gl$bandas_interes_lambda)

# RColorBrewer::brewer.pal(10, "Paired")
gl$paleta_humedales <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99",
                         "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A")


gl$indices_sinteticos <- c("NDVI", "EVI", "NDWI")
gl$polarizaciones <- c("HH", "HV", "VH", "VV")

gl$paleta_bandas <- c(NDVI = "#1b9e77",
                      EVI = "#d95f02",
                      NDWI = "#7570b3",
                      HH = "#1b9e77",
                      HV = "#d95f02",
                      VH = "#d95f02",
                      VV = "#7570b3"
)

