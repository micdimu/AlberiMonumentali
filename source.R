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
  if (length(x) == 0 || is.na(x) || !nzchar(trimws(x))) return(NA_real_)
  
  x <- trimws(x)
  
  # normalizza apostrofi e virgolette strane
  x <- gsub("[’`´]", "'", x)
  x <- gsub("[“”″]", '"', x)
  x <- gsub("''", '"', x)
  x <- gsub("’’", '"', x)
  
  # normalizza virgola decimale
  x <- gsub(",", ".", x)
  
  # comprime spazi
  x <- gsub("\\s+", " ", x)
  
  # estrae emisfero sia all'inizio che alla fine
  hemi <- stringr::str_extract(x, "^[NSEW]|[NSEW]$")
  hemi <- gsub("\\s+", "", hemi)
  
  # tieni solo numeri + separatori utili
  x_num <- gsub("[NSEW]", "", x)
  x_num <- trimws(x_num)
  
  # prova a leggere gradi minuti secondi con secondi anche decimali
  m <- regexec("^([0-9]+)°\\s*([0-9]+)['′]?\\s*([0-9]+(?:\\.[0-9]+)?)?[\"']?$", x_num)
  z <- regmatches(x_num, m)[[1]]
  
  if (length(z) == 0) {
    return(NA_real_)
  }
  
  deg <- as.numeric(z[2])
  min <- as.numeric(z[3])
  sec <- if (length(z) >= 4 && nzchar(z[4])) as.numeric(z[4]) else 0
  
  dec <- deg + min / 60 + sec / 3600
  
  if (hemi %in% c("S", "W")) dec <- -dec
  
  dec
}

get1 <- function(df, nm) if (nm %in% names(df)) df[[nm]] else NA_character_

norm_name <- function(x) {
  x |>
    str_to_upper() |>
    str_replace_all("[’`´]", "'") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

# Trova il poligono del comune (match su nome comune + (opzionale) provincia)
find_comune <- function(comune, provincia = NA_character_) {
  cc <- norm_name(comune)
  pp <- norm_name(provincia)
  
  x <- comuni_ab |> filter(.com == cc)
  if (nrow(x) == 0) return(NULL)
  
  # se esiste provincia e ci sono duplicati, prova a filtrare
  if (!is.na(pp) && any(!is.na(comuni_ab$.prov)) && nrow(x) > 1) {
    x2 <- x |> filter(.prov == pp)
    if (nrow(x2) > 0) x <- x2
  }
  x[1, ]
}

# Prendo tiles OSM su bbox (cached su disco se vuoi)
get_osm_tiles <- function(bbox, zoom = 13) {
  # bbox in sf: st_bbox
  bb <- sf::st_as_sfc(bbox)
  bb <- sf::st_transform(bb, 3857) # web mercator per tiles
  # maptiles lavora bene con sf in 3857
  r <- maptiles::get_tiles(bb, provider = "OpenStreetMap", zoom = zoom, crop = TRUE)
  r
}


make_map_abruzzo <- function(lon, lat, comune_poly = NULL, n_scheda = NULL) {
  if (is.na(lat) || is.na(lon)) return(NULL)
  
  nsch  <- gsub("/", "_", n_scheda) 
 
  file <- sprintf("maps/map_%s.png", nsch)
  
  if(file.exists(file)) return(file)
  
  pt <- st_as_sf(data.frame(lon = lon, lat = lat), coords = c("lon","lat"), crs = 4326)
  
  # pannello contesto Abruzzo
  p_ctx <- ggplot() +
    geom_sf(data = abruzzo, fill = "grey95", color = "grey40", linewidth = 0.4) +
    { if (!is.null(comune_poly)) geom_sf(data = comune_poly, fill = "grey85", color = "orange", linewidth = 0.5) } +
    geom_sf(data = pt, size = 1., col = "red") +
    coord_sf() +
    theme_void(base_size = 10) +
    labs(title = "Cartografia semplificata") +
    theme(plot.title = element_text(face = "bold", hjust = 0))
  
  # pannello zoom Comune con OSM + confini
  if (!is.null(comune_poly)) {
    comune_ll <- st_transform(comune_poly, 4326)
    bb <- st_bbox(comune_ll)
    
    # tiles OSM (serve internet qui, UNA volta)
    osm <- get_osm_tiles(bb, zoom = 13)
    
    p_zoom <- ggplot() +
      tidyterra::geom_spatraster_rgb(data = osm, alpha = 1) +
      geom_sf(data = comune_ll, fill = NA, color = "orange", linewidth = 0.5) +
      geom_sf(data = pt, size = 1., col = "red") +
      theme_void(base_size = 10) +
      labs(title = "") +
      theme(plot.title = element_text(face = "bold", hjust = 0))
  } else {
    p_zoom <- ggplot() +
      geom_sf(data = pt, size = 1., col = "red") +
      coord_sf(xlim = c(lon-0.05, lon+0.05), ylim = c(lat-0.05, lat+0.05), expand = FALSE) +
      theme_void(base_size = 10) +
      labs(title = "Zoom") +
      theme(plot.title = element_text(face = "bold", hjust = 0))
  }
  
  plotmap <- p_ctx + p_zoom + plot_layout(ncol = 2, widths = c(1, 1.5))
  
  
  ggsave(
    file,
    plot = plotmap,
    width = 18,
    height = 9,
    units = "cm",
    dpi = 300
  )
  
  return(file)
}
