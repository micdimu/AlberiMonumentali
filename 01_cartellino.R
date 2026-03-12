library(tidyverse)
library(docxtractr)
library(patchwork)
library(maptiles)
library(terra)
library(tidyterra)
library(sf)
source("source.R")

tbls <- list.files("input", pattern = ".docx$", full.names = TRUE, recursive = TRUE)|> 
  map(read_docx) |> 
  map(docx_extract_all_tbls, guess_header = TRUE) 


### carica i confini
if(!file.exists("boundaries/abruzzo_boundaries.rds")){
  errorCondition("see code in getmap.R to download boundaries from ISTAT and save as RDS")
}
bnd <- readRDS("boundaries/abruzzo_boundaries.rds")
abruzzo <- bnd$abruzzo
comuni_ab <- bnd$comuni_abruzzo



# prima tabella:
tbls_z <- tbls |>
  map(\(x){
    if (length(x) >= 1) 
    select(x[[1]], n1 = 1, n2 = 2)
   }) |> 
  compact()

albero <- tbls_z |>
  map(\(x){
    x |> 
        transmute(
                key = clean_key(n1),
                val = str_squish(n2)
        ) |>
      group_by(key) |>
      summarise(val = val[which.max(nzchar(val))][1], .groups = "drop") |>
      filter(val != "") |>
      pivot_wider(names_from = key, values_from = val)
  }) |> 
  (\(.) do.call(bind_rows, .))() |> 
  mutate(altezza_cm = case_when(
    is.na(altezza_cm) ~ altezza_m,
    TRUE ~ altezza_cm
  ))

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


# Assumo già lat_dec/lon_dec nel cartellino_df
cartellino_df2 <- cartellino_df %>%
  mutate(
    comune_poly = Map(
      \(c, p) find_comune(c, p),
      if ("comune" %in% names(.)) comune else NA_character_,
      if ("provincia" %in% names(.)) provincia else NA_character_
    ),
    map_plot = Map(make_map_abruzzo, lon_dec, lat_dec, comune_poly)
  )

cartellino_df2 |> 
  filter(is.null(map_plot)) |> 
  pull(n_scheda)

for(i in seq_len(nrow(cartellino_df2))){
  
  p <- cartellino_df2$map_plot[[i]]
  
  if(!is.null(p)){
    
    file <- sprintf("maps/map_%03d.png", i)
    
    ggsave(
      file,
      plot = p,
      width = 18,
      height = 9,
      units = "cm",
      dpi = 300
    )
    
    cartellino_df2$map_file[i] <- file
    
  } else {
    
    cartellino_df2$map_file[i] <- NA
    
  }
  
}

cartellino_df3 <- cartellino_df2 |> 
  select(-map_plot) |> 
  filter(!is.na(map_file))

cartellino_df2 |> 
  filter(is.na(map_file)) |> 
  pull(n_scheda)


saveRDS(cartellino_df3, "output/cartellino_df_maps.rds")





