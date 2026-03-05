library(dplyr)
library(ggplot2)
library(patchwork)
library(sf)
library(dplyr)
library(stringr)

# Helper per matching nomi (Acciano vs ACCIANO vs Accianò ecc.)
norm_name <- function(x) {
        x |>
                str_to_upper() |>
                str_replace_all("[’`´]", "'") |>
                str_replace_all("\\s+", " ") |>
                str_trim()
}

# 1) Confini amministrativi ISTAT (Comuni/Regioni) – scarico zip e leggo
# NB: l’URL ISTAT cambia talvolta: se dà 404 ti dico cosa fare sotto.
istat_zip <- "istat_boundaries.zip"
istat_url <- "https://www.istat.it/storage/cartografia/confini_amministrativi/non_generalizzati/2026/Limiti01012026.zip"

if (!file.exists(istat_zip)) download.file(istat_url, istat_zip, mode = "wb")

unzip_dir <- "istat_boundaries"
dir.create(unzip_dir, showWarnings = FALSE)
unzip(istat_zip, exdir = unzip_dir)

# Cerca shapefile dei Comuni e delle Regioni (i nomi file possono variare leggermente)
shps <- list.files(unzip_dir, pattern = "\\.shp$", recursive = TRUE, full.names = TRUE)

# Prova a beccare "Comuni" e "Regioni"
shp_comuni   <- shps[str_detect(tolower(shps), "com")][1]
shp_regioni  <- shps[str_detect(tolower(shps), "reg")][1]

comuni   <- st_read(shp_comuni, quiet = TRUE) |> st_make_valid()
regioni  <- st_read(shp_regioni, quiet = TRUE) |> st_make_valid()

# 2) Tieni solo Abruzzo + Comuni abruzzesi
# Colonne ISTAT tipiche: DEN_REG / DEN_PROV / DEN_CM ... ma variano.
# Provo diverse possibilità:
reg_col <- intersect(names(regioni), c("DEN_REG", "DEN_REGI", "REGIONE", "den_reg"))
abr <- regioni |> filter(norm_name(.data[[reg_col[1]]]) == "ABRUZZO")

prov_col <- intersect(names(comuni), c("DEN_PROV", "DEN_UTS", "PROVINCIA", "den_prov"))
com_col  <- intersect(names(comuni), c("DEN_CM", "COMUNE", "den_cm", "DEN_COM"))

comuni_abruzzo <- comuni |>
        filter(COD_REG == 13) |>
        mutate(
                .prov = if (length(prov_col)) norm_name(.data[[prov_col[1]]]) else NA_character_,
                .com  = if (length(com_col))  norm_name(.data[[com_col[1]]])  else NA_character_
        )

# Salvo un cache “pulito” (sf objects)
saveRDS(list(
        abruzzo = abr,
        comuni_abruzzo = comuni_abruzzo
), "abruzzo_boundaries.rds")



