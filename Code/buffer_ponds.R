library(mapedit)
library(sf)
library(leaflet)
require(tidyverse)

df_raw <- read_csv(
  '/Users/diegoellis/Desktop/Projects/Postdoc/Pond_2025/Pond_2025_fieldwork_done/pond_2025_post_fieldwork.csv'
)

# Convert empty GPS_Pond_name to NA
df <- df_raw %>%
  mutate(GPS_Pond_name = na_if(GPS_Pond_name, "")) %>%
  drop_na(GPS_Pond_name, Latitude, Longitude) 


# Convert Latitude & Longitude to numeric (ensure correct data type)
df <- df %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude)
  ) %>%
  drop_na(Latitude, Longitude)  # Remove rows with missing coordinates


df <- df %>%
  mutate(
    Longitude = if_else(Longitude > 0, -Longitude, Longitude),  # Flip positive values
    Latitude = if_else(Latitude > 0, -Latitude, Latitude)  # Flip positive values
  )

df = df[!df$GPS_Pond_name %in% c('Poza_vigen', 'P_Laguna_Ch? REMOVE SEE NOTE'),]

df_sf <- st_as_sf(df, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  mutate(Longitude = st_coordinates(.)[,1], Latitude = st_coordinates(.)[,2]) |> 
  filter(GPS_Pond_name != "Poza virgen")


# Create a Leaflet map with your points
m <- leaflet(df_sf) %>%
  addTiles() %>%
  addCircleMarkers(radius = 4, color = "blue", stroke = FALSE, fillOpacity = 0.7)

# Laguna Chato
# Launch map editor with your points as reference
drawn_laguna_Ch <- editMap(m, editor = "leafpm")

# Load points
# drawn <- editMap(leaflet() %>% addTiles(), editor = "leafpm")
drawn_laguna_Ch_sf <- st_as_sf(drawn_laguna_Ch$finished)
# st_write(drawn_sf, "my_selected_area.shp")
# Transform to same projected CRS
drawn_laguna_Ch_sf_proj <- st_transform(drawn_laguna_Ch_sf, 32715) |>
st_cast("POLYGON")

# Optional: buffer the drawn polygon too (e.g., to grow it slightly)
drawn_laguna_buffer <- st_buffer(drawn_laguna_Ch_sf_proj, dist = 5) |>
  mutate(GPS_Pond_name = 'P_Laguna_Ch') |> 
  select(GPS_Pond_name)

st_write(drawn_laguna_buffer, "Outdir/P_Laguna_Ch_buf.shp")

# Now draw buffer for Cerro Mesa
# drawn_CM <- editMap(m, editor = "leafpm")
drawn_CM <- editMap(m, editor = "leafpm")
drawn_CM_sf <- st_as_sf(drawn_CM$finished)
drawn_CM_sf_proj <- st_transform(drawn_CM_sf, 32715) |>
  st_cast("POLYGON")
drawn_CM_sf_proj_buffer <- st_buffer(drawn_CM_sf_proj, dist = 5) |>
  mutate(GPS_Pond_name = 'P_CM') |> 
  select(GPS_Pond_name)

st_write(drawn_CM_sf_proj_buffer, "Outdir/P_CM_buf.shp")


df_sf_proj <- st_transform(df_sf, 32715)  # UTM Zone 15S
df_buffer <- st_buffer(df_sf_proj, dist = 5) |> 
  filter(! GPS_Pond_name %in% c('P_Laguna_Ch',
                                'P_CM_1')) |>
  select(GPS_Pond_name)


# Add buffer to 10m to SBD 

# Combine the buffered geometries
all_buffers <- rbind(
  st_cast(df_buffer, "MULTIPOLYGON"),
  st_cast(drawn_CM_sf_proj_buffer, "MULTIPOLYGON"),
  st_cast(drawn_laguna_buffer, "MULTIPOLYGON")
)

all_buffers_wgs84 <- st_transform(all_buffers, 4326)
st_write(all_buffers_wgs84, "Outdir/buffered_ponds.shp")

# Optional: union to dissolve boundaries if you want a single feature
# combined_multipolygon <- st_union(all_buffers)
# combined_wgs84 <- st_transform(combined_multipolygon, 4326)
# mapview(combined_wgs84)



# Based on size column draw 5, 10 or 20m buffer
