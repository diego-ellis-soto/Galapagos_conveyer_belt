# --- Cerro Mesa diagnostic: outline vs sampling positions vs tortoise fixes
library(sf); library(mapview); library(dplyr); library(readr); library(move)
setwd("/Users/diegoellis/Desktop/Projects/Postdoc/Galapagos_conveyer_belt")

cm <- st_read("Outdir/P_CM_buf.shp", quiet = TRUE) |> st_transform(4326)

# the 9 places you stood to sample Cerro Mesa
pts <- read_csv("/Users/diegoellis/Desktop/Projects/Postdoc/Pond_2025/Pond_2025_fieldwork_done/pond_2025_post_fieldwork_clean_deep_clean.csv",
                show_col_types = FALSE, locale = locale(encoding = "Latin1")) |>
  filter(GPS_Pond_name == "P_CM", !is.na(Latitude)) |>
  mutate(Longitude = as.numeric(Longitude), Latitude = as.numeric(Latitude),
         Longitude = if_else(Longitude > 0, -Longitude, Longitude),
         Latitude  = if_else(Latitude  > 0, -Latitude,  Latitude)) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# tortoise fixes within 150 m, from the snapshot the last run saved
snap <- tail(sort(Sys.glob("Data/tortoises_movebank_*.Rdata")), 1)
load(snap)
fx <- data.frame(tortugas) |>
  filter(!is.na(location_long), !is.na(location_lat)) |>
  st_as_sf(coords = c("location_long", "location_lat"), crs = 4326)
fx <- fx[lengths(st_intersects(fx, st_buffer(st_transform(cm, 32715), 150) |>
                                 st_transform(4326))) > 0, ]

cat("sampling positions:", nrow(pts), " inside outline:",
    sum(lengths(st_intersects(pts, cm)) > 0), "\n")
cat("tortoise fixes within 150 m:", nrow(fx), "\n")

mapviewOptions(basemaps = c("Esri.WorldImagery", "Esri.NatGeoWorldMap"))
mapview(cm,  col.regions = "cyan",  alpha.regions = 0.35, layer.name = "CM outline") +
  mapview(fx,  col.regions = "grey30", cex = 2, layer.name = "Tortoise fixes") +
  mapview(pts, col.regions = "red",   cex = 7, layer.name = "Sampling positions")
