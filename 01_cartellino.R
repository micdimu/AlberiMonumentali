library(docxtractr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)

doc <- read_docx("arf/01.A345.AQ.13.docx")
tbls <- docx_extract_all_tbls(doc, guess_header = TRUE)

# prima tabella:
tbls_z <- tbls[[1]] |> 
        select(n1 = 1, n2 = 2)  



# tb = il tuo tibble 20x2
# nomi colonne come nel print: n..progr..Per.provincia e X1

clean_key <- function(x) {
        x |>
                stringr::str_to_lower() |>
                stringr::str_replace_all("[’`´]", "'") |>
                stringr::str_replace_all("[^[:alnum:]àèéìòù ]+", " ") |>
                stringr::str_squish() |>
                stringr::str_replace_all(" ", "_")
}

# DMS "N 42°12'04\"" -> decimal degrees
dms_to_dec <- function(x) {
        if (is.na(x) || !nzchar(x)) return(NA_real_)
        x <- str_squish(x)
        hemi <- str_extract(x, "^[NSEW]")
        nums <- str_match(x, "([0-9]+)°\\s*([0-9]+)'\\s*([0-9]+)")
        if (any(is.na(nums))) return(NA_real_)
        deg <- as.numeric(nums[,2]); min <- as.numeric(nums[,3]); sec <- as.numeric(nums[,4])
        dec <- deg + min/60 + sec/3600
        if (hemi %in% c("S","W")) dec <- -dec
        dec
}

albero <- tbls_z |>
        transmute(
                key = clean_key(n1),
                val = str_squish(n2)
        ) |>
        # se ci sono chiavi duplicate, tieni la prima non-NA (o concatena se preferisci)
        group_by(key) |>
        summarise(val = val[which.max(nzchar(val))][1], .groups = "drop") |>
        pivot_wider(names_from = key, values_from = val)

# helper per recuperare in modo safe (se una colonna non esiste)
get1 <- function(df, nm) if (nm %in% names(df)) df[[nm]] else NA_character_


cartellino_df <- albero |>
        mutate(
                # prendo tutte le colonne della riga corrente (1 riga, n colonne)
                .row = pick(everything()),
                
                nome = str_squish(paste(
                        get1(.row, "nome_volgare"),
                        get1(.row, "localita"),
                        get1(.row, "n_scheda"),
                        sep = " — "
                )),
                
                lat_dec = vapply(get1(.row, "latitudine"),  dms_to_dec, numeric(1)),
                lon_dec = vapply(get1(.row, "longitudine"), dms_to_dec, numeric(1)),
                
                localizzazione = str_squish(paste0(
                        "Provincia: ", get1(.row, "provincia"), "\n",
                        "Comune: ",    get1(.row, "comune"),    "\n",
                        "Localita: ",  get1(.row, "localita"),  "\n",
                        "Coord.: ",    get1(.row, "latitudine"), "  ", get1(.row, "longitudine"),
                        ifelse(!is.na(lat_dec) & !is.na(lon_dec),
                               paste0(" (", sprintf("%.6f", lat_dec), ", ", sprintf("%.6f", lon_dec), ")"),
                               ""),
                        "\nQuota: ", get1(.row, "altitudine_m_s_l_m"), " m"
                )),
                
                caratteristiche = str_squish(paste0(
                        "Circonferenza: ", get1(.row, "circonferenza_cm"), " cm\n",
                        "Altezza: ",       get1(.row, "altezza_cm"),       " cm\n",
                        "Criterio monumentalita: ", get1(.row, "criterio_monumentalita")
                )),
                
                note = coalesce(
                        get1(.row, "correzioni"),
                        get1(.row, "proposta_dichiarazione_notevole_interesse_pubblico_vigente_proposta")
                )
        ) |>
        select(nome, localizzazione, caratteristiche, note, everything()) |>
        select(-.row)

saveRDS(cartellino_df, "cartellino_df.rds")

