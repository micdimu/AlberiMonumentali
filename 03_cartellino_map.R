library(dplyr)
library(sf)
library(ggplot2)
library(patchwork)
library(stringr)
library(maptiles)
library(terra)
library(tidyterra)

cartellino_df <- readRDS("cartellino_df.rds")
bnd <- readRDS("abruzzo_boundaries.rds")
abruzzo <- bnd$abruzzo
comuni_ab <- bnd$comuni_abruzzo

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


make_map_abruzzo <- function(lon, lat, comune_poly = NULL) {
        if (is.na(lat) || is.na(lon)) return(NULL)
        
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
        
        p_ctx + p_zoom + plot_layout(ncol = 2, widths = c(1, 1.5))
}


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

saveRDS(cartellino_df2, "cartellino_df_maps.rds")


usethis::use_git_config(user.name = "micdimu", user.email = "michele.dimusciano@gmail.com")
