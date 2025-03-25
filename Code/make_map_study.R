library(leaflet)
library(RColorBrewer)

# A color palette for your Tourist_lagoon field
tourist_pal <- colorFactor(
  palette = c( "steelblue", "goldenrod1"),
  domain  = df_sf$Tourist_lagoon
)

# A color palette for your tortoises
animal_colors <- colorFactor(
  palette = "Dark2",
  domain  = df_sp_sf$name
)

# -- Subset Montemar and 'other' ponds --
montemar_sf    <- df_sf[df_sf$GPS_Pond_name == "P_Montem_1", ]
other_ponds_sf <- df_sf[df_sf$GPS_Pond_name != "P_Montem_1", ]

leaflet() %>%
  # -----------------------------------------------------------------
# 1) Base Layers
# -----------------------------------------------------------------
addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo Map") %>%
  
  # -----------------------------------------------------------------
# 2) Topographic Contour Lines from the DEM
# -----------------------------------------------------------------
addPolylines(
  data        = contours_sf,            # your contour lines as sf
  color       = "white",
  weight      = 2,
  label       = ~paste0(level, " m"),   # <-- numeric field from contours_sf
  labelOptions= labelOptions(
    direction = "auto",
    textOnly  = TRUE,
    style     = list("color" = "white", "font-size" = "12px")
  ),
  group       = "Topo Lines"
) %>%
  
  # -----------------------------------------------------------------
# 3) Tortoise Tracking Data (small points)
# -----------------------------------------------------------------
addCircleMarkers(
  data        = df_sp_sf,
  radius      = 1,
  color       = ~animal_colors(name),
  stroke      = FALSE,
  fillOpacity = 0.8,
  popup       = ~paste0("Animal: ", name, "<br>Timestamp: ", timestamp),
  group       = "Tracking Data"
) %>%
  
  # -----------------------------------------------------------------
# 4) Pond Buffers (Polygons)
# -----------------------------------------------------------------
addPolygons(
  data        = pond_buffers,
  color       = "blue",
  weight      = 10, 
  fillOpacity = 0.7,
  popup       = ~GPS_Pn_,
  group       = "Pond Buffers"
) %>%
  
  # -----------------------------------------------------------------
# 5) Regular Ponds: Circle Markers w/ Black Outlines, Colored by Tourist_lagoon
# -----------------------------------------------------------------
addCircleMarkers(
  data        = other_ponds_sf,
  lng         = ~Longitude,
  lat         = ~Latitude,
  radius      = 7,
  fillColor   = ~tourist_pal(Tourist_lagoon),
  color       = "black",  # black outline
  weight      = 1,        # outline thickness
  fillOpacity = 0.9,
  popup       = ~paste0("Pond: ", GPS_Pond_name, "<br>Type: ", Tourist_lagoon),
  group       = "Pond Points"
) %>%
  
  addCircleMarkers(
    data        = montemar_sf,
    lng         = ~Longitude,
    lat         = ~Latitude,
    radius      = 7,
    fillColor   = ~tourist_pal(Tourist_lagoon),
    color       = "black",  # black outline
    weight      = 1,        # outline thickness
    fillOpacity = 0.9,
    popup       = ~paste0("Pond: ", GPS_Pond_name, "<br>Type: ", Tourist_lagoon),
    group       = "Montemar"
  ) %>%
  
  # -----------------------------------------------------------------
# 6) Montemar as a Label-Only Marker with its Name
# -----------------------------------------------------------------
# addCircleMarkers(
#   data       = montemar_sf,
#   lng        = ~Longitude,
#   lat        = ~Latitude,
#   label      = ~GPS_Pond_name,  # shows “Montemar”
#   labelOptions = labelOptions(
#     noHide    = TRUE,
#     direction = "auto",
#     style     = list(
#       "color"       = "yellow",
#       "font-weight" = "bold",
#       "font-size"   = "14px",
#       "text-shadow" = "1px 1px 2px black"
#     )
#   ),
#   group      = "Pond Points"
# ) %>%
  
  # -----------------------------------------------------------------
# 7) Layer Control
# -----------------------------------------------------------------
addLayersControl(
  baseGroups    = c("Satellite", "Topo Map"),
  overlayGroups = c("Topo Lines", "Pond Buffers", "Pond Points", "Tracking Data"),
  options       = layersControlOptions(collapsed = FALSE)
)




library(mapview)
library(sf)
library(tidyverse)
library(RColorBrewer)

## 1) Prepare reversed color palette
reversed_tourist_pal <- c("steelblue", "goldenrod1")

## 2) Create separate mapview layers

# -- Contour lines --
m_contours <- mapview(
  contours_sf,
  color       = "white",
  lwd         = 1,
  layer.name  = "Topo Lines",
  label       = contours_sf$level      # If 'level' is the elevation field
)

# -- Tortoise tracking data --
m_tracks <- mapview(
  df_sp_sf,
  zcol       = "name",
  cex        = 2,                      # make points small
  legend     = TRUE,
  layer.name = "Tracking Data"
)

# -- Pond buffers --
m_pond_buffers <- mapview(
  pond_buffers,
  color       = "blue",
  lwd         = 10,                    # thick outlines
  alpha.regions = 0.7,
  layer.name  = "Pond Buffers"
)

# -- Split ponds: Montemar vs others --
montemar_sf    <- df_sf[df_sf$GPS_Pond_name == "P_Montem_1", ]
other_ponds_sf <- df_sf[df_sf$GPS_Pond_name != "P_Montem_1", ]

# -- Other ponds (circle markers), color-coded by Tourist_lagoon with reversed palette
m_other_ponds <- mapview(
  other_ponds_sf,
  zcol         = "Tourist_lagoon",
  col.regions  = reversed_tourist_pal,
  cex          = 7,                    # bigger circles
  legend       = TRUE,
  layer.name   = "Pond Points"
)

# -- Montemar pond
m_montemar <- mapview(
  montemar_sf,
  zcol        = "Tourist_lagoon",
  col.regions = reversed_tourist_pal,
  cex         = 7,
  color       = "black",              # black outline
  layer.name  = "Montemar"
)

## 3) Combine layers
my_map <- m_contours + m_tracks + m_pond_buffers + m_other_ponds + m_montemar

## 4) Display the combined map in RStudio's viewer or browser
my_map

## 5) Save as an interactive HTML file
mapshot(my_map, file = "my_mapview.html")



library(ggplot2)
library(sf)
# 1) Base ggplot
p <- ggplot() +
  
  # ------------------------------------------------------
# A) Contour lines (colored white); add labels for elevation
# ------------------------------------------------------
geom_sf(
  data = contours_sf,
  color = "black",
  size = 1    # thickness of contour lines
) +
  # Optional: label each contour with its 'level'
  geom_sf_text(
    data = contours_sf,
    aes(label = level),
    color = "white",
    size = 2
  ) +
  
  # ------------------------------------------------------
# B) Pond buffers (blue outline, no fill)
# ------------------------------------------------------
geom_sf(
  data = pond_buffers,
  color = "blue",
  fill = NA,
  size = 1
) +
  
  # ------------------------------------------------------
# C) Tortoise tracking points (colored by animal name)
# ------------------------------------------------------
geom_sf(
  data = df_sp_sf,
  aes(color = name),
  size = 1    # small points
) +
  
  # ------------------------------------------------------
# D) Other ponds: black‐outlined circles, fill depends on Tourist_lagoon
# ------------------------------------------------------
geom_sf(
  data = other_ponds_sf,
  aes(fill = Tourist_lagoon),
  shape = 21,        # circle with a fill
  color = "black",   # black outline
  size = 4           # circle size
) +
  
  # ------------------------------------------------------
# E) Montemar pond: use a star shape (shape=8)
# ------------------------------------------------------
geom_sf(
  data = montemar_sf,
  aes(fill = Tourist_lagoon),
  shape = 8,         # star
  color = "black",
  size = 5
) +
  
  # ------------------------------------------------------
# F) Scales & Theme
# ------------------------------------------------------
scale_fill_manual(
  values = c("steelblue", "goldenrod1"),  # reversed palette from your Leaflet code
  name   = "Pond Type"
) +
  scale_color_brewer(
    palette = "Dark2",
    name    = "Animal Name"                 # legend title for tortoise colors
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  
  # You can name axes or add a title if you like:
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Santa Cruz Ponds & Tortoise Tracks (ggplot)"
  ) +
  
  coord_sf()  # ensure correct map projection

# 2) Display your ggplot
print(p)


library(raster)   # or terra, if you prefer
library(sf)
library(dplyr)
library(ggplot2)

# Suppose srtm_santa_cruz is a RasterLayer object
# e.g., srtm_santa_cruz <- raster(\"path/to/srtm_santa_cruz.tif\")

# Convert raster to a data frame for ggplot
srtm_df <- as.data.frame(rasterToPoints(srtm_santa_cruz), xy = TRUE)

# Typically this yields columns: x, y, and the raster's value (e.g. 'layer')
# Rename the value column to something more descriptive, e.g. Elevation
colnames(srtm_df)[3] <- "Elevation"

p <- ggplot() +
  # (A) DEM Raster as a background
  geom_raster(
    data = srtm_df,
    aes(x = x, y = y, fill = Elevation),
    alpha = 0.6    # partial transparency so other layers are visible
  ) +
  scale_fill_viridis_c(name = "Elevation (m)")    # or another gradient scale
  