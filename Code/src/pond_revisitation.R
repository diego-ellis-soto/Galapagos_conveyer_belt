# ============================================================
# Galapagos giant tortoises: pond delineation, pond use and
# sediment export
#
# Analysis code for Ellis-Soto et al., "Zoogeochemical engineers:
# Galapagos giant tortoises link terrestrial and freshwater
# ecosystems through sediment transport and nutrient dynamics".
#
# One script, run top to bottom.
#
#   PART A  build the pond zones from the field sampling positions
#   PART B  which tortoises and which ponds intersect
#   PART C  revisitation with the recurse package
#   PART D  figures and interactive maps
#   PART E  overnight stays
#   PART F  seasonality, network, transitions and sediment export
#   PART G  the numbers reported in the paper
#
# Figure 5 and Supplementary Figure 3 (sediment load by pond type,
# and against carapace length) and the Montemar camera figures are
# built in Code/src/pond_mud_camera.R, which needs no telemetry.
# The sediment export estimate stays here because it depends on the
# recurse departure rate.
#
# ------------------------------------------------------------
# HOW A POND IS DEFINED
#
# Ponds are not fixed shapes. They shrink and grow between years
# with rainfall, so there is no single correct outline. What we do
# have is where we stood at each pond on every sampling trip from
# 2018 onwards, recording a GPS position at the margin while taking
# water samples and YSI measurements. Pooled across years, those
# positions bracket the range of extents the pond has occupied.
#
# A pond is therefore defined as everything within 5 m of any of its
# sampling positions. We take the union of those buffers and do not
# interpolate between them, so the delineation claims nothing about
# pond extent where nothing was measured. Using the convex hull of
# the buffers instead changes the total visit count by under 1%, and
# no pond zone contains an interior gap.
#
# Three ponds - Cerro Mesa, Laguna Chato and Manzanillo 1 - are too
# large to represent this way and their outlines were digitised by
# hand from satellite imagery (PART A2), then buffered by the same
# 5 m. Their sampling positions are kept alongside the outline
# rather than replaced, because a traced outline captures the pond
# on one imagery date while the sampling positions record where the
# margin was on each visit.
#
# ------------------------------------------------------------
# COUNTING A VISIT
#
# Because a pond is the union of several overlapping buffers, one
# arrival is detected once per buffer entered, and a tortoise moving
# between sampling positions within a pond leaves one buffer and
# enters the next. Detections are therefore merged, per tortoise and
# pond, whenever the intervals inside the zone overlap or touch, and
# each merged interval is treated as one visit. This counts arrivals
# at a pond rather than crossings of individual buffers. Visits of
# 0.1 h or less are excluded as brief pass-by events.
#
# All distances are computed in a Lambert azimuthal equal-area
# projection centred on the study area, so radii and areas are in
# metres.
#
# ------------------------------------------------------------
# INPUTS
#   pond_2025_post_fieldwork_clean_deep_clean.csv   field sampling positions
#   Outdir/P_*_buf.shp                              hand-drawn large ponds
#   Data/Mud_2024_2025_fieldwork.csv                carapace sediment loads
#   Movebank study 2928116                          downloaded fresh each run
#
# OUTPUTS  all in Outdir/recurse_polygon/
#
# RUNTIME  about 10 minutes: the Movebank download plus the
#          recursion loop. Nothing is cached.
# ============================================================


# ------------------------------------------------------------
# 0. Packages and settings
# ------------------------------------------------------------

library(mapedit)     # only for the one-off drawing step in PART A2
library(leaflet)
library(move)
library(keyring)
library(recurse)
library(sf)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(viridis)
library(patchwork)
library(scales)
library(ggrepel)
library(mapview)
library(htmlwidgets)
library(stringr)
library(terra)
library(bipartite)

# Run from the project root. Every path below is relative to it, so the
# script runs unchanged on any machine with the repository checked out.

study_id      <- 2928116

# Movebank account, used only if a snapshot has to be downloaded.
# The password is NOT stored here - see PART B1.
movebank_user <- "whatsinaname"

# Frozen copy of the Movebank study the analysis runs on. Pin the file
# used for the published results so the analysis reproduces exactly;
# NULL takes the most recent snapshot in Data/, and downloads one if
# there is none.
movebank_snapshot <- NULL
years_to_keep <- 2018:2025

# Field sheet: one row per sampling event, with the GPS position we
# recorded at the pond margin that day.
field_csv <- "Data/pond_2025_post_fieldwork_clean_deep_clean.csv"

# Buffer around each sampling position, in metres.
buffer_m <- 5

# Ponds too large to represent by sampling buffers, drawn by hand in
# PART A2. These are the three the field sheet records as "large" in
# the Size column on every visit.
large_ponds <- c("P_CM", "P_Laguna_Ch", "P_Manzan_1")

# Working projection: equal-area, metres, centred on the study area.
# Set properly in PART C1 once the tortoise data is loaded.
utm_gal <- 32715      # UTM 15S, for areas and for the drawn polygons

pond_file <- "Outdir/buffered_ponds_deep_clean.shp"

# Visits of 0.1 h or less are excluded as brief pass-by events, and
# times are reported in Galapagos local time.
min_visit_hours <- 0.1
local_tz        <- "Pacific/Galapagos"

# Pond type colours, defined once so every figure agrees. The figure
# captions read "blue indicates non-touristic ponds" and "yellow
# circles represent touristic ponds".
pond_type_colors <- c("Non-touristic" = "steelblue",
                      "Touristic"     = "goldenrod1")

# Digital elevation model for the study-area map.
dem_file <- "Data/SRTM_Santa_Cruz.tif"

# Sensitivity switch. Ponds named here are dropped before anything is
# computed, so every table and figure is regenerated without them and
# written to a separate folder. P_SDB_3 carries about two thirds of all
# visits, from two tortoises, so it is the obvious one to test.
exclude_ponds <- character(0)          # e.g. "P_SDB_3"

outdir <- if (length(exclude_ponds) == 0) {
  "Outdir/recurse_polygon"
} else {
  paste0("Outdir/recurse_polygon_no", paste(sub("^P_", "", exclude_ponds),
                                            collapse = "_"))
}
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# ============================================================
# PART A - build the pond zones
# ============================================================

# ------------------------------------------------------------
# A1. Field sampling positions
# ------------------------------------------------------------

# Latin1 because a few free-text columns have accented Spanish.
field <- read_csv(field_csv, show_col_types = FALSE,
                  locale = locale(encoding = "Latin1"))

sampling_points <- field |>
  mutate(GPS_Pond_name = na_if(GPS_Pond_name, ""),
         Latitude      = as.numeric(Latitude),
         Longitude     = as.numeric(Longitude)) |>
  drop_na(GPS_Pond_name, Latitude, Longitude) |>
  # a few rows were typed without the minus sign
  mutate(Longitude = if_else(Longitude > 0, -Longitude, Longitude),
         Latitude  = if_else(Latitude  > 0, -Latitude,  Latitude)) |>
  filter(!GPS_Pond_name %in% c("Poza_vigen", "Poza virgen",
                               "P_Laguna_Ch? REMOVE SEE NOTE")) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

message("sampling positions: ", nrow(sampling_points),
        " at ", n_distinct(sampling_points$GPS_Pond_name), " ponds")

# Field size class, used only to check that every large pond has been
# drawn. Nothing in the analysis depends on it.
pond_size_class <- field |>
  filter(!is.na(GPS_Pond_name), !is.na(Size)) |>
  group_by(pond = GPS_Pond_name) |>
  summarise(size_class = names(sort(table(Size), decreasing = TRUE))[1],
            .groups = "drop")

# Touristic vs non-touristic, from the same field sheet. Used by the
# study-area map and the network figure.
pond_type_lookup <- field |>
  filter(!is.na(GPS_Pond_name), !is.na(Tourist_lagoon)) |>
  mutate(tourist = str_to_lower(str_trim(Tourist_lagoon)),
         pond_type = case_when(tourist == "yes" ~ "Touristic",
                               tourist == "no"  ~ "Non-touristic",
                               TRUE ~ NA_character_)) |>
  distinct(pond = GPS_Pond_name, pond_type) |>
  filter(!is.na(pond_type))


# ------------------------------------------------------------
# A2. Hand-drawn outlines for the large ponds
#
# THIS BLOCK IS INTERACTIVE AND ONLY NEEDS RUNNING ONCE PER POND.
# It opens a map, you draw the pond outline, and it writes a
# shapefile. Every later run of this script just loads that file, so
# leave draw_ponds_now <- FALSE

# Master switch. Nothing is drawn while this is FALSE.
draw_ponds_now <- FALSE

# Ponds already drawn are skipped unless named here. `large_ponds`
# means redraw all three. Use character(0) to draw only what is
# missing, or e.g. "P_CM" for one.
#
redraw <- character(0)

if (draw_ponds_now) {

  for (nm in large_ponds) {

    shp <- file.path("Outdir", paste0(nm, "_buf.shp"))

    if (file.exists(shp) && !nm %in% redraw) {
      message("skipping ", nm, " - already drawn. Put it in `redraw` to replace it.")
      next
    }

    # Never silently destroy an existing outline.
    if (file.exists(shp)) {
      backup_dir <- file.path("Outdir", "pond_outlines_previous")
      dir.create(backup_dir, showWarnings = FALSE, recursive = TRUE)
      stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
      for (f in Sys.glob(file.path("Outdir", paste0(nm, "_buf.*")))) {
        file.copy(f, file.path(backup_dir,
                               sub("_buf\\.", paste0("_buf_", stamp, "."), basename(f))),
                  overwrite = TRUE)
      }
      message("    previous ", nm, " outline backed up to ", backup_dir)
    }

    # The pond being drawn, in red, with every other sampling position
    # in grey for context. Without this the map opens on the whole
    # island and you have to hunt for the right pond.
    this_pond <- filter(sampling_points, GPS_Pond_name == nm)
    others    <- filter(sampling_points, GPS_Pond_name != nm)

    # zoom to 150 m around this pond's own sampling positions
    bb <- this_pond |>
      st_transform(utm_gal) |>
      st_buffer(150) |>
      st_transform(4326) |>
      st_bbox()

    # National Geographic as the default backdrop, with satellite on a
    # toggle. NatGeo is a cartographic map, so it will not show the pond
    # as open water and it stops at about zoom 16; switch to Satellite
    # in the layer control (top right) when you actually trace the
    # outline, especially for Manzanillo, which sits under canopy.
    basemap <- leaflet() |>
      addProviderTiles("Esri.NatGeoWorldMap", group = "National Geographic") |>
      # Esri has no imagery above zoom 17 over these ponds - z18+ returns
      # a blank tile. maxNativeZoom stops it asking for tiles that do not
      # exist and upsamples the z17 tile instead, so you can keep zooming
      # to place vertices accurately. 1.19 m/pixel is the real limit.
      addProviderTiles("Esri.WorldImagery", group = "Satellite",
                       options = providerTileOptions(maxNativeZoom = 17,
                                                     maxZoom       = 21)) |>
      addLayersControl(
        baseGroups    = c("National Geographic", "Satellite"),
        options       = layersControlOptions(collapsed = FALSE)) |>
      addCircleMarkers(data = others, radius = 3, color = "white",
                       stroke = TRUE, weight = 1, fillOpacity = 0.3) |>
      addCircleMarkers(data = this_pond, radius = 7, color = "red",
                       stroke = TRUE, weight = 2, fillOpacity = 0.9,
                       label = nm) |>
      fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])

    message("\n>>> DRAW ", nm, " - it is at the RED markers.")
    message("    Trace the pond outline, then press Done.")

    drawn <- editMap(basemap, editor = "leafpm")

    if (is.null(drawn$finished)) {
      message("    nothing drawn for ", nm, " - skipped")
      next
    }

    st_as_sf(drawn$finished) |>
      st_transform(utm_gal) |>
      st_cast("POLYGON") |>
      st_buffer(dist = buffer_m) |>
      mutate(GPS_Pond_name = nm) |>
      select(GPS_Pond_name) |>
      st_write(shp, delete_layer = TRUE, quiet = TRUE)

    message("    written: ", shp)
  }
}

# ------------------------------------------------------------
# A3. Assemble the pond zones
#
# Sampling positions buffered by 5 m for every pond, plus the
# hand-drawn outline for each large pond, unioned together in PART C.
#
# A drawn pond keeps its sampling positions rather than having them
# replaced. A hand-drawn outline traces the pond as it appeared on one
# date of satellite imagery, i.e. one state of a waterbody that expands
# and contracts with rainfall; the sampling positions record where the
# margin actually was on each field visit. At Cerro Mesa the western
# positions were taken when the pond was flooded and sit up to 43 m
# outside the traced outline, so replacing them would discard the
# pond's wet-season extent - exactly the thing this delineation is
# meant to capture.
# ------------------------------------------------------------

drawn_files <- file.path("Outdir", paste0(large_ponds, "_buf.shp"))
names(drawn_files) <- large_ponds

drawn_ponds <- large_ponds[file.exists(drawn_files)]
missing     <- setdiff(large_ponds, drawn_ponds)

if (length(missing) > 0) {
  warning("no hand-drawn outline yet for: ", paste(missing, collapse = ", "),
          ". Falling back to its sampling buffers, which will understate it. ",
          "Set draw_ponds_now <- TRUE and rerun PART A2.", call. = FALSE)
}

# every pond's sampling positions, drawn ponds included
zones <- sampling_points |>
  st_transform(utm_gal) |>
  st_buffer(dist = buffer_m) |>
  select(GPS_Pond_name)

# The drawn ones. The shapefile format truncates field names to 10
# characters, so GPS_Pond_name comes back as GPS_Pn_. Rather than
# depend on that, take only the geometry and label it with the pond we
# asked for.
for (nm in drawn_ponds) {
  g <- st_read(drawn_files[[nm]], quiet = TRUE) |> st_transform(utm_gal)
  zones <- bind_rows(zones,
                     st_sf(GPS_Pond_name = nm, geometry = st_geometry(g)))
}

ponds_raw <- zones |>
  st_make_valid() |>
  mutate(pond = GPS_Pond_name) |>
  select(pond) |>
  st_transform(4326)

st_write(ponds_raw, pond_file, delete_layer = TRUE, quiet = TRUE)
message("pond features written to ", pond_file, ": ", nrow(ponds_raw))


# ============================================================
# PART B - which tortoises use which ponds
# ============================================================

# ------------------------------------------------------------
# B1. Movebank data
#
# Download every animal in the study, not a pre-selected list, and
# work out which of them use ponds further down. Selecting animals in
# advance would make the set of pond users an input rather than a
# result.
# ------------------------------------------------------------

# The analysis runs against a frozen snapshot of the Movebank study,
# not a live download, so it reproduces exactly. `movebank_snapshot`
# is set at the top: pin the file used for the published results, or
# leave it NULL to take the most recent snapshot in Data/.
#
# If no snapshot exists the study is downloaded once and saved with a
# timestamp, and every later run reuses it. Delete or repin to refresh.

snapshots <- sort(Sys.glob("Data/tortoises_movebank_*.Rdata"))

if (is.null(movebank_snapshot) && length(snapshots) > 0) {
  movebank_snapshot <- tail(snapshots, 1)
}

if (!is.null(movebank_snapshot) && file.exists(movebank_snapshot)) {

  message("loading Movebank snapshot: ", movebank_snapshot)
  load(movebank_snapshot)          # restores `tortugas`

} else {

  # Password comes from the macOS Keychain, not from this script. Set
  # it once from the R console with:
  #   keyring::key_set("movebank", movebank_user)
  movebank_pw <- tryCatch(keyring::key_get("movebank", movebank_user),
                          error = function(e) NULL)

  loginStored <- if (is.null(movebank_pw)) {
    message("no Keychain entry for '", movebank_user,
            "' - falling back to an interactive prompt")
    movebankLogin(username = movebank_user)
  } else {
    movebankLogin(username = movebank_user, password = movebank_pw)
  }
  rm(movebank_pw)

  # Large download; Movebank intermittently drops the connection part
  # way through, so retry with a growing pause.
  tortugas <- NULL
  for (attempt in 1:5) {
    tortugas <- tryCatch(
      getMovebankData(
        study                      = study_id,
        login                      = loginStored,
        includeExtraSensors        = FALSE,
        removeDuplicatedTimestamps = TRUE,
        deploymentAsIndividuals    = FALSE
      ),
      error = function(e) {
        message("Movebank download attempt ", attempt, " failed: ",
                conditionMessage(e))
        NULL
      })
    if (!is.null(tortugas)) break
    if (attempt < 5) Sys.sleep(30 * attempt)
  }

  if (is.null(tortugas)) {
    stop("Movebank download failed after 5 attempts. ",
         "Check the connection and rerun; nothing has been written.")
  }

  movebank_snapshot <- file.path(
    "Data",
    paste0("tortoises_movebank_",
           format(Sys.time(), "%Y%m%d_%H%M%S"), ".Rdata"))

  save(tortugas, file = movebank_snapshot)
  message("Movebank download saved to ", movebank_snapshot)
}

tortugas$name <- tortugas@trackId
tortugas_df   <- data.frame(tortugas)


# ------------------------------------------------------------
# B2. Clean the GPS fixes and make them spatial
# ------------------------------------------------------------

tortoise_sf <- tortugas_df |>
  mutate(
    timestamp    = as.POSIXct(timestamp, tz = "UTC"),
    year         = year(timestamp),
    name         = as.character(name),
    gps_point_id = row_number()
  ) |>
  filter(
    year %in% years_to_keep,
    !is.na(timestamp), !is.na(name),
    !is.na(location_long), !is.na(location_lat)
  ) |>
  st_as_sf(coords = c("location_long", "location_lat"),
           crs = 4326, remove = FALSE)

# ------------------------------------------------------------
# B3. Clean obviously non-tortoise locations
#
# Two filters, both deliberately conservative.
#
# 1. Speed spikes. A fix whose step in AND step out both exceed
#    max_speed_kmh cannot be an animal that walks at a few hundred
#    metres per hour; it is a bad fix, and requiring both sides means
#    a genuine fast move is not clipped. The worst step in the raw
#    data implies about 14,600 km/h.
#
# 2. Puerto Ayora. Collars taken to the research station in town
#    keep logging. These sit at 8-19 m elevation, barely move (23
#    distinct coordinates across 71 fixes), and two collars share an
#    identical eight-day window in 2022 - the signature of units off
#    the animals rather than tortoises in town.
#
# NOT removed: the straight run of fixes west of Bellavista. It looks
# road-like, but it is Baronesa over eight days in March 2020 at a
# median 0.027 km/h and a maximum of 0.22 km/h. That is a tortoise
# walking a trail, not a vehicle.
# ------------------------------------------------------------

max_speed_kmh   <- 1
town_exclusions <- data.frame(place = "Puerto Ayora",
                              lon = -90.3126, lat = -0.7437,
                              radius_m = 1500)

n_before <- nrow(tortoise_sf)

xy <- st_coordinates(st_transform(tortoise_sf, utm_gal))

tortoise_sf <- tortoise_sf |>
  mutate(.x = xy[, 1], .y = xy[, 2]) |>
  arrange(name, timestamp) |>
  group_by(name) |>
  mutate(
    .sp_in  = sqrt((.x - lag(.x))^2  + (.y - lag(.y))^2)  / 1000 /
              as.numeric(timestamp - lag(timestamp),  units = "hours"),
    .sp_out = sqrt((lead(.x) - .x)^2 + (lead(.y) - .y)^2) / 1000 /
              as.numeric(lead(timestamp) - timestamp, units = "hours")
  ) |>
  ungroup() |>
  filter(is.na(.sp_in) | is.na(.sp_out) |
         .sp_in <= max_speed_kmh | .sp_out <= max_speed_kmh) |>
  select(-.x, -.y, -.sp_in, -.sp_out)

message("removed ", n_before - nrow(tortoise_sf),
        " speed-spike fixes (>", max_speed_kmh, " km/h on both sides)")

n_before <- nrow(tortoise_sf)

towns_sf <- st_as_sf(town_exclusions, coords = c("lon", "lat"), crs = 4326) |>
  st_transform(utm_gal) |>
  st_buffer(town_exclusions$radius_m)

in_town <- lengths(st_intersects(st_transform(tortoise_sf, utm_gal), towns_sf)) > 0
tortoise_sf <- tortoise_sf[!in_town, ]

message("removed ", n_before - nrow(tortoise_sf),
        " fixes inside the town exclusion buffer")


message("GPS fixes retained: ", nrow(tortoise_sf))
message("tortoises retained: ", n_distinct(tortoise_sf$name))
message("years present:      ", paste(range(tortoise_sf$year), collapse = " - "))


# ------------------------------------------------------------
# B4. Pond zones
#
# PART A assembled ponds_raw: one 5 m buffer per sampling position for
# every pond, plus the hand-drawn outline for each pond in
# `drawn_ponds`. Dissolving by name unions those into one zone per
# pond, so a drawn pond is its traced outline together with the 5 m
# buffers around the positions where it was actually sampled.
# ------------------------------------------------------------

ponds <- ponds_raw |>
  group_by(pond) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_make_valid()

if (length(exclude_ponds) > 0) {
  ponds     <- filter(ponds,     !pond %in% exclude_ponds)
  ponds_raw <- filter(ponds_raw, !pond %in% exclude_ponds)
  message("SENSITIVITY RUN: excluding ", paste(exclude_ponds, collapse = ", "))
}

message("ponds: ", nrow(ponds), " zones from ", nrow(ponds_raw), " features")

# Check every large pond actually has a drawn outline.
undrawn_large <- setdiff(pond_size_class$pond[pond_size_class$size_class == "large"],
                         drawn_ponds)
if (length(undrawn_large) > 0) {
  warning("large pond(s) with no hand-drawn outline: ",
          paste(undrawn_large, collapse = ", "), call. = FALSE)
} else {
  message("all large ponds have a hand-drawn outline")
}


# ------------------------------------------------------------
# B5. Which GPS fixes fall inside a pond
# ------------------------------------------------------------

tortoise_pond_hits_sf <- st_join(tortoise_sf, ponds,
                                 join = st_intersects, left = FALSE)

tortoise_pond_hits <- tortoise_pond_hits_sf |>
  st_drop_geometry() |>
  arrange(name, timestamp, pond)

message("GPS fixes inside ponds: ", nrow(tortoise_pond_hits))


# ------------------------------------------------------------
# B6. Summary per pond, including ponds with no detections
# ------------------------------------------------------------

pond_use_summary <- tortoise_pond_hits |>
  mutate(date = as.Date(timestamp)) |>
  group_by(pond) |>
  summarise(gps_fixes           = n_distinct(gps_point_id),
            number_of_tortoises = n_distinct(name),
            detection_days      = n_distinct(date),
            first_detection     = min(timestamp),
            last_detection      = max(timestamp),
            .groups = "drop")

pond_intersection_check <- ponds |>
  st_drop_geometry() |>
  left_join(pond_use_summary, by = "pond") |>
  mutate(gps_fixes           = coalesce(gps_fixes, 0L),
         number_of_tortoises = coalesce(number_of_tortoises, 0L),
         detection_days      = coalesce(detection_days, 0L),
         intersected         = gps_fixes > 0) |>
  arrange(desc(intersected), desc(number_of_tortoises), pond)

write_csv(pond_intersection_check, file.path(outdir, "pond_intersection_check.csv"))

cat("\nPONDS USED\n");     print(filter(pond_intersection_check, intersected), n = Inf)
cat("\nPONDS NOT USED\n"); print(filter(pond_intersection_check, !intersected) |> select(pond), n = Inf)


# ------------------------------------------------------------
# B7. Per tortoise x pond, and the list of pond users
# ------------------------------------------------------------

tortoise_pond_detection_summary <- tortoise_pond_hits |>
  mutate(date = as.Date(timestamp)) |>
  group_by(name, pond) |>
  summarise(gps_fixes       = n_distinct(gps_point_id),
            detection_days  = n_distinct(date),
            first_detection = min(timestamp),
            last_detection  = max(timestamp),
            .groups = "drop") |>
  arrange(name, desc(detection_days), pond)

write_csv(tortoise_pond_detection_summary,
          file.path(outdir, "tortoise_pond_detection_summary.csv"))

intersecting_tortoises <- tortoise_pond_hits |> distinct(name) |> arrange(name)
write_csv(intersecting_tortoises, file.path(outdir, "tortoise_names_in_ponds.csv"))

message("tortoises using at least one pond: ", nrow(intersecting_tortoises))


# ------------------------------------------------------------
# B8. Full tracks for the pond-using tortoises
#
# Every fix for these animals, inside ponds and outside. recurse
# counts trajectory SEGMENTS crossing the zone, so it needs to see
# the approach and departure, not only the fixes that landed inside.
# ------------------------------------------------------------

tracks <- tortoise_sf |>
  filter(name %in% intersecting_tortoises$name) |>
  st_drop_geometry() |>
  arrange(name, timestamp)

message("fixes for pond-using tortoises: ", nrow(tracks))


# ============================================================
# PART C - revisitation
# ============================================================

# ------------------------------------------------------------
# C1. Project to equal-area metres
# ------------------------------------------------------------

myprj <- paste0(
  "+proj=laea",
  " +lat_0=", round(median(tracks$location_lat)),
  " +lon_0=", round(median(tracks$location_long)),
  " +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
)

tracks_m_sf <- st_as_sf(tracks, coords = c("location_long", "location_lat"),
                        crs = 4326) |>
  st_transform(myprj)

# recurse wants a plain data frame: x, y, time, id - in that order.
# id must be a factor or numeric; recurse runs as.numeric() on it
# internally, so a character id fails with the unhelpful error
# "missing value where TRUE/FALSE needed".
trk <- data.frame(
  x  = st_coordinates(tracks_m_sf)[, 1],
  y  = st_coordinates(tracks_m_sf)[, 2],
  t  = tracks_m_sf$timestamp,
  id = factor(tracks_m_sf$name)
) |>
  arrange(id, t)

ponds_m     <- st_transform(ponds, myprj)
ponds_raw_m <- st_transform(ponds_raw, myprj)

ponds_m$area_m2   <- as.numeric(st_area(ponds_m))
ponds_m$n_feature <- as.vector(table(ponds_raw_m$pond)[ponds_m$pond])
ponds_m$kind      <- if_else(ponds_m$pond %in% drawn_ponds,
                             "drawn polygon", "sampled locations + 5 m")


# ------------------------------------------------------------
# C2. Recursions
#
# A pond is the UNION of 5 m discs around its sampled locations, so
# a tortoise is in the pond whenever it is within 5 m of any of
# them. recurse cannot take a union directly, so:
#
#   - run getRecursionsAtLocations once over every sampled location
#     with radius 5 m, which gives visits to each disc
#   - then merge, per tortoise per pond, any visits whose time
#     inside overlaps or touches
#
# Merging the time intervals is what turns "visits to discs" into
# "visits to the pond". It is the same idea as the collapse step in
# a tortoise walking from one disc to the next within a single pond.
#
# The two drawn ponds are handled separately with
# getRecursionsInPolygon. That function needs a convex single
# polygon: P_CM is convex already and P_Laguna_Ch is 97.9% of its
# convex hull, so the hull is an accurate stand-in for both.
# ------------------------------------------------------------

# The recursion loop below takes a few minutes. Nothing is cached:
# the script downloads fresh data on every run, so a cached result
# could belong to a different download.

visit_discs <- data.frame()

# --- every sampling position, at every pond ----------------------
#
# Drawn ponds are included here too. PART A keeps their sampling
# positions alongside the traced outline, so they must be analysed as
# well, otherwise the zone used for the intersection in PART B would
# not match what recurse actually measures. A sampling buffer is
# exactly pi * buffer_m^2; anything larger is a traced outline and is
# handled by the polygon loop below.
disc_feats <- ponds_raw_m |>
  filter(abs(as.numeric(st_area(geometry)) - pi * buffer_m^2) < 1)
disc_xy    <- st_coordinates(st_centroid(st_geometry(disc_feats)))

message("recurse: ", nrow(disc_feats), " sampling positions, radius ",
        buffer_m, " m")

rec_discs <- getRecursionsAtLocations(
  trk,
  data.frame(x = disc_xy[, 1], y = disc_xy[, 2]),
  radius    = buffer_m,
  timeunits = "hours",
  verbose   = TRUE
)

# coordIdx indexes the locations we passed in, in the order we
# passed them, so this lookup is just the row number.
disc_lookup <- data.frame(coordIdx = seq_len(nrow(disc_feats)),
                          pond     = disc_feats$pond)

visit_discs <- rec_discs$revisitStats |>
  left_join(disc_lookup, by = "coordIdx") |>
  select(id, pond, entranceTime, exitTime, timeInside)

# --- the hand-drawn outlines -------------------------------------

for (nm in drawn_ponds) {

  message("recurse: ", nm, " (drawn polygon)")

  # for P_CM the shapefile also holds its sampled locations; the
  # drawn outline is the largest feature and contains them
  g <- ponds_raw_m |> filter(pond == nm)
  g <- st_geometry(g)[which.max(as.numeric(st_area(g)))]

  # verbose = TRUE errors on a zero-visit polygon in recurse 1.4.0
  # (it does nrow() on a NULL), so ask quietly first
  r <- getRecursionsInPolygon(trk, st_convex_hull(g),
                              timeunits = "hours", verbose = FALSE)

  if (r$revisits > 0) {
    rv <- getRecursionsInPolygon(trk, st_convex_hull(g),
                                 timeunits = "hours", verbose = TRUE)
    visit_discs <- rbind(visit_discs,
      rv$revisitStats |>
        mutate(pond = nm) |>
        select(id, pond, entranceTime, exitTime, timeInside))
  }
}

message("raw disc-level detections: ", nrow(visit_discs))


# ------------------------------------------------------------
# C3. Merge disc visits into pond visits
#
# Sort by tortoise, pond and entrance time, then start a new visit
# only when a detection begins after the latest exit seen so far.
# cummax() carries that running latest exit; lag() shifts it so each
# row is compared against everything before it.
# ------------------------------------------------------------

visit_stats <- visit_discs |>
  arrange(pond, id, entranceTime) |>
  group_by(pond, id) |>
  mutate(prev_end  = lag(cummax(as.numeric(exitTime))),
         new_visit = is.na(prev_end) | as.numeric(entranceTime) > prev_end,
         visit_grp = cumsum(new_visit)) |>
  group_by(pond, id, visit_grp) |>
  summarise(entranceTime = min(entranceTime),
            exitTime     = max(exitTime),
            discs_used   = n(),
            .groups = "drop") |>
  # recompute duration from the merged interval rather than summing
  # the pieces, so overlapping discs are not counted twice
  mutate(timeInside = as.numeric(difftime(exitTime, entranceTime,
                                          units = "hours")))

message("after merging overlapping discs: ", nrow(visit_stats), " pond visits")

# drop brief pass-by events
visit_stats <- filter(visit_stats, timeInside > min_visit_hours)

visit_stats <- visit_stats |>
  arrange(pond, id, entranceTime) |>
  group_by(pond, id) |>
  mutate(timeSinceLastVisit =
           as.numeric(difftime(entranceTime, lag(exitTime), units = "hours"))) |>
  ungroup()

message("after the ", min_visit_hours, " h filter: ", nrow(visit_stats), " visits")


# ------------------------------------------------------------
# C4. Pond table
# ------------------------------------------------------------

pond_totals <- visit_stats |>
  group_by(pond) |>
  summarise(visits      = n(),
            hours       = sum(timeInside),
            n_tortoises = n_distinct(id),
            .groups = "drop")

pond_recurse <- ponds_m |>
  st_drop_geometry() |>
  select(pond, kind, area_m2, n_feature) |>
  mutate(area_m2 = round(area_m2)) |>
  left_join(pond_size_class, by = "pond") |>
  left_join(pond_totals, by = "pond") |>
  mutate(across(c(visits, hours, n_tortoises), ~ coalesce(.x, 0))) |>
  arrange(desc(area_m2))

print(as.data.frame(pond_recurse), row.names = FALSE)

message("pond zones: ", sum(pond_recurse$visits), " visits at ",
        sum(pond_recurse$visits > 0), " ponds")

write_csv(pond_recurse, file.path(outdir, "pond_recursions_polygon.csv"))
write_csv(visit_stats,  file.path(outdir, "pond_visit_stats.csv"))


# PART D - figures and maps
# ============================================================

theme_set(
  theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank(),
          plot.title       = element_text(face = "bold"),
          strip.text       = element_text(face = "bold", size = 8))
)

used <- filter(pond_recurse, visits > 0)

# Axis label helper. Do NOT use labels = comma on a log axis: comma()
# derives one accuracy for the whole break vector, so a break at 0.001
# forces 10 to print as "10.000", which reads as ten thousand.
log_labels <- label_number(drop0trailing = TRUE, big.mark = ",")


# ------------------------------------------------------------
# Objects shared by the figures and maps below
# ------------------------------------------------------------

big        <- drawn_ponds
big_poly   <- ponds_m[ponds_m$pond %in% big, c("pond", "area_m2")]
big_centre <- st_centroid(st_geometry(big_poly))

fixes_m  <- st_as_sf(trk, coords = c("x", "y"), crs = myprj)
near_big <- fixes_m[apply(st_intersects(fixes_m, st_buffer(big_poly, 120),
                                        sparse = FALSE), 1, any), ]


# ------------------------------------------------------------
# Figure 1: study area
#
# A leaflet map: topographic basemap, labelled elevation contours,
# tortoise movement thinned to one fix per animal per day, and pond
# locations coloured by type. Saved as HTML; open it and export at
# the size you want for the figure.
#
# Thinning keeps the tracks legible - the full data is millions of
# fixes and renders as a solid blot.
# ------------------------------------------------------------

# one fix per tortoise per local day
tracks_daily <- tracks |>
  mutate(date = as.Date(with_tz(timestamp, local_tz), tz = local_tz)) |>
  group_by(name, date) |>
  slice_min(timestamp, n = 1, with_ties = FALSE) |>
  ungroup()

tracks_daily_sf <- st_as_sf(tracks_daily,
                            coords = c("location_long", "location_lat"),
                            crs = 4326)

message("study-area map: ", nrow(tracks_daily), " daily fixes from ",
        n_distinct(tracks_daily$name), " tortoises")

# elevation contours every 100 m
dem <- rast(dem_file)
dem[dem < 0] <- NA                       # drop ocean

contours_sf <- as.contour(dem, levels = seq(100, 900, by = 100)) |>
  st_as_sf() |>
  st_transform(4326)

# pond centres. The figure labels these Natural / Artificial, which is
# how the field sheet's Tourist_lagoon column is presented in the paper.
pond_pts <- ponds |>
  st_centroid() |>
  left_join(pond_type_lookup, by = "pond") |>
  filter(!is.na(pond_type)) |>
  mutate(type_label = if_else(pond_type == "Touristic",
                              "Artificial", "Natural"))

# colorFactor IGNORES the names on a palette vector and assigns colours
# in sorted level order, so a named vector silently reverses here
# ("Artificial" sorts before "Natural"). Pass levels explicitly.
pond_pal <- colorFactor(palette = c("steelblue", "goldenrod1"),
                        levels  = c("Natural", "Artificial"))

# extent covering the tracks and ponds, with a small margin
map_bb <- st_bbox(st_buffer(st_transform(
            st_union(st_geometry(tracks_daily_sf), st_geometry(pond_pts)),
            utm_gal), 1500) |> st_transform(4326))

map_study <- leaflet() |>
  addProviderTiles("Esri.WorldTopoMap") |>

  # contours, labelled with their elevation
  addPolylines(data = contours_sf, color = "grey30", weight = 1, opacity = 0.6,
               label = ~paste0(level, " m"),
               labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE,
                                           direction = "auto",
                                           style = list(color = "grey20",
                                                        "font-size" = "11px"))) |>

  # tortoise movement, one point per animal per day
  addCircleMarkers(data = tracks_daily_sf, radius = 1.5,
                   color = "#1b3a2b", stroke = FALSE, fillOpacity = 0.6,
                   group = "Tortoise movement") |>

  # ponds
  addCircleMarkers(data = pond_pts, radius = 8,
                   fillColor = ~pond_pal(type_label),
                   color = "black", weight = 1.5, fillOpacity = 0.95,
                   popup = ~paste0(pond, "<br>", type_label),
                   group = "Ponds") |>

  addLegend("bottomright", pal = pond_pal, values = c("Natural", "Artificial"),
            title = "Pond type", opacity = 1) |>
  addScaleBar("bottomleft", scaleBarOptions(imperial = FALSE)) |>

  # frame on the tracks and ponds, not the whole archipelago
  fitBounds(map_bb[["xmin"]], map_bb[["ymin"]],
            map_bb[["xmax"]], map_bb[["ymax"]])

saveWidget(map_study,
           file.path(normalizePath(outdir), "00_study_area.html"),
           selfcontained = FALSE, title = "Santa Cruz study area")

message("study-area map written to ", file.path(outdir, "00_study_area.html"))


# ------------------------------------------------------------
# Fig 2. All 21 ponds at the same scale, with the 5 m circle
# ------------------------------------------------------------

# Shift every pond onto 0,0 so they can be compared side by side.
centred <- list()
for (i in 1:nrow(ponds_m)) {
  g  <- st_geometry(ponds_m)[i]
  ct <- as.numeric(st_coordinates(st_centroid(g)))
  centred[[i]] <- st_sf(pond    = ponds_m$pond[i],
                        area_m2 = ponds_m$area_m2[i],
                        geometry = g - ct)
}
centred <- do.call(rbind, centred)
st_crs(centred) <- NA

# Tortoise fixes within 80 m of each pond, shifted by the same centroid
# so they line up with the shifted outlines. One st_intersects call over
# all ponds first, because fixes_m holds millions of points.
pond_hits <- st_intersects(fixes_m, st_buffer(st_geometry(ponds_m), 80))
keep      <- lengths(pond_hits) > 0
near_any  <- fixes_m[keep, ]
near_pond <- sapply(pond_hits[keep], `[`, 1)     # first pond each fix falls near

near_shifted <- list()
for (i in 1:nrow(ponds_m)) {
  sel <- st_geometry(near_any)[near_pond == i]
  if (length(sel) == 0) next
  ct <- as.numeric(st_coordinates(st_centroid(st_geometry(ponds_m)[i])))
  near_shifted[[length(near_shifted) + 1]] <-
    st_sf(pond     = ponds_m$pond[i],
          area_m2  = ponds_m$area_m2[i],
          geometry = sel - ct)
}
near_shifted <- do.call(rbind, near_shifted)
st_crs(near_shifted) <- NA

# same circle repeated once per facet; area_m2 is carried along only
# because the facet expression below sorts on it
circle0 <- st_sf(
  pond     = ponds_m$pond,
  area_m2  = ponds_m$area_m2,
  geometry = st_buffer(st_sfc(st_point(c(0, 0))), buffer_m)[rep(1, nrow(ponds_m))]
)

fig2 <- ggplot() +
  # outline first, then fixes on top of it, then the circle. Otherwise
  # the semi-transparent polygon hides the fixes inside the large ponds.
  geom_sf(data = centred, fill = "#2c7fb8", colour = "#08519c",
          alpha = 0.35, linewidth = 0.2) +
  geom_sf(data = near_shifted, size = 0.25, colour = "grey25", alpha = 0.35) +
  geom_sf(data = circle0, fill = "#e31a1c", colour = NA, alpha = 0.9) +
  facet_wrap(~reorder(pond, -area_m2), ncol = 7) +
  coord_sf(xlim = c(-75, 75), ylim = c(-75, 75)) +
  labs(title    = sprintf("All %d pond zones at the same scale", nrow(ponds_m)),
       subtitle = paste0("blue = pond zone, grey = tortoise GPS fixes within 80 m, ",
                         "red = one 5 m sampling buffer for scale")) +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave(file.path(outdir, "01_pond_zones_at_scale.png"), fig2,
       width = 12, height = 6, dpi = 200, bg = "white")


# ------------------------------------------------------------
# Fig 3. Visits against pond size
# ------------------------------------------------------------

fig3 <- pond_recurse |>
  filter(visits > 0) |>
  ggplot(aes(area_m2, visits)) +
  geom_point(aes(colour = kind, size = n_tortoises), alpha = 0.85) +
  geom_text_repel(aes(label = pond), size = 2.8, seed = 1) +
  scale_x_log10(labels = log_labels) +
  scale_y_log10(labels = log_labels) +
  scale_colour_manual(NULL, values = c("drawn polygon" = "#e6550d",
                                       "sampled locations + 5 m" = "#2c7fb8")) +
  scale_size_continuous(name = "tortoises") +
  labs(title = "Visits against pond size",
       subtitle = "both axes log; pond area is the zone actually analysed",
       x = "pond zone area (m2)", y = "visits") +
  theme(legend.position = "top", legend.text = element_text(size = 8))

ggsave(file.path(outdir, "02_visits_vs_pond_size.png"), fig3,
       width = 8, height = 6, dpi = 200, bg = "white")


# ------------------------------------------------------------
# Fig 4. The corrected pond use numbers
# ------------------------------------------------------------

fig4a <- ggplot(used, aes(visits, reorder(pond, visits), fill = n_tortoises)) +
  geom_col() +
  geom_text(aes(label = comma(visits)), hjust = -0.15, size = 3) +
  scale_fill_viridis(name = "tortoises") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Visits per pond", x = "visits", y = NULL)

fig4b <- ggplot(used, aes(hours, reorder(pond, visits), fill = n_tortoises)) +
  geom_col() +
  geom_text(aes(label = comma(round(hours))), hjust = -0.15, size = 3) +
  scale_fill_viridis(name = "tortoises") +
  scale_x_continuous(labels = comma, expand = expansion(mult = c(0, 0.18))) +
  labs(title = "Residence time per pond", x = "hours inside the pond", y = NULL)

ggsave(file.path(outdir, "03_pond_use.png"),
       fig4a + fig4b + plot_layout(guides = "collect"),
       width = 12, height = 5.5, dpi = 200, bg = "white")


# ------------------------------------------------------------
# Fig 5. Which tortoise used which pond
# ------------------------------------------------------------

heat <- count(visit_stats, pond, id, name = "visits")

fig5 <- ggplot(heat, aes(pond, id, fill = visits)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = visits), size = 2.7,
            colour = ifelse(heat$visits > max(heat$visits) * 0.5, "white", "grey15")) +
  scale_fill_viridis(option = "mako", direction = -1, trans = "log10", labels = comma) +
  labs(title = "Visits by tortoise and pond", x = NULL, y = NULL) +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

ggsave(file.path(outdir, "04_tortoise_pond_heatmap.png"), fig5,
       width = 9, height = 6, dpi = 200, bg = "white")


# ------------------------------------------------------------
# Fig 6. Visit length, gap between visits, and seasonality
# ------------------------------------------------------------

fig6a <- ggplot(visit_stats, aes(as.numeric(timeInside))) +
  geom_histogram(bins = 60, fill = "#2c7fb8", colour = "white", linewidth = 0.2) +
  scale_x_log10(labels = log_labels) +
  labs(title    = "How long is a visit?",
       subtitle = sprintf("median %.1f h, 90%% under %.1f h, longest %.1f h",
                          median(as.numeric(visit_stats$timeInside)),
                          quantile(as.numeric(visit_stats$timeInside), 0.9),
                          max(as.numeric(visit_stats$timeInside))),
       x = "hours inside the pond (log)", y = "visits")

fig6b <- ggplot(filter(visit_stats, !is.na(timeSinceLastVisit)),
                aes(as.numeric(timeSinceLastVisit) / 24)) +
  geom_histogram(bins = 60, fill = "#41ab5d", colour = "white", linewidth = 0.2) +
  scale_x_log10(labels = log_labels) +
  labs(title = "How long between visits?", x = "days since last visit (log)", y = "visits")

fig6c <- visit_stats |>
  mutate(month = month(entranceTime, label = TRUE)) |>
  count(month, pond) |>
  ggplot(aes(month, n, fill = pond)) +
  geom_col() +
  scale_fill_viridis_d(option = "turbo",
                       guide = guide_legend(ncol = 2, keyheight = 0.5)) +
  labs(title = "Seasonality of pond visits", x = NULL, y = "visits") +
  theme(legend.title = element_blank(), legend.text = element_text(size = 6))

visit_stats[! visit_stats$pond == 'P_SDB_3',] |>
  mutate(month = month(entranceTime, label = TRUE)) |>
  count(month, pond) |>
  ggplot(aes(month, n, fill = pond)) +
  geom_col() +
  scale_fill_viridis_d(option = "turbo",
                       guide = guide_legend(ncol = 2, keyheight = 0.5)) +
  labs(title = "Seasonality of pond visits", x = NULL, y = "visits") +
  theme(legend.title = element_blank(), legend.text = element_text(size = 6))



ggsave(file.path(outdir, "05_visit_structure.png"),
       (fig6a + fig6b) / fig6c + plot_layout(heights = c(1, 1.1)),
       width = 12, height = 8, dpi = 200, bg = "white")


# ============================================================
# PART E - overnight stays
#
# A visit is overnight if its local exit date differs from its local
# entrance date.
#
# Everything is in Galapagos local time. Using UTC would shift
# midnight by 6 hours and silently change the answer.
# ============================================================

nights <- visit_stats |>
  mutate(
    local_entrance = with_tz(entranceTime, local_tz),
    local_exit     = with_tz(exitTime,     local_tz),

    hours_inside = as.numeric(timeInside),
    year         = year(local_entrance),
    month_num    = month(local_entrance),
    doy_enter    = yday(local_entrance),

    # decimal hour of day, for the entry-time axis
    time_enter = hour(local_entrance) + minute(local_entrance) / 60 +
                 second(local_entrance) / 3600,
    time_exit  = hour(local_exit)     + minute(local_exit) / 60 +
                 second(local_exit) / 3600,

    # CAREFUL HERE. as.Date() on a POSIXct defaults to tz = "UTC" and
    # IGNORES the tzone attribute, so as.Date(local_exit) silently
    # gives the UTC date back and you end up detecting UTC midnight,
    # which in Galapagos is 18:00 local. That misflags every early
    # evening visit, however short, as overnight.
    #
    # lubridate's yday() and date() DO respect tzone, which is why the
    # original yday(exit) - yday(entrance) version was right. Passing
    # tz explicitly to as.Date() is the same test, written so the
    # timezone cannot be lost.
    overnight = if_else(
      as.Date(local_exit,     tz = local_tz) !=
      as.Date(local_entrance, tz = local_tz),
      "yes", "no"
    ),

    season = if_else(month_num %in% c(12, 1, 2, 3, 4, 5),
                     "wet_hot", "dry_cool")
  ) |>
  mutate(overnight = factor(overnight, levels = c("no", "yes")),
         season    = factor(season, levels = c("wet_hot", "dry_cool")))

n_over  <- sum(nights$overnight == "yes")
pct_over <- 100 * n_over / nrow(nights)

message("overnight visits: ", n_over, " of ", nrow(nights),
        " (", round(pct_over, 2), "%)")

write_csv(nights, file.path(outdir, "pond_visits_overnight.csv"))

message("overnight: ",
        n_over, " of ", nrow(nights),
        " (", round(pct_over, 2), "%)")

# per pond, and per tortoise
overnight_by_pond <- nights |>
  group_by(pond) |>
  summarise(visits          = n(),
            overnight       = sum(overnight == "yes"),
            pct_overnight   = round(100 * mean(overnight == "yes"), 1),
            median_hours    = round(median(hours_inside), 2),
            .groups = "drop") |>
  arrange(desc(overnight))

overnight_by_tortoise <- nights |>
  group_by(id) |>
  summarise(visits        = n(),
            overnight     = sum(overnight == "yes"),
            pct_overnight = round(100 * mean(overnight == "yes"), 1),
            .groups = "drop") |>
  arrange(desc(overnight))

print(overnight_by_pond,     row.names = FALSE)
print(overnight_by_tortoise, row.names = FALSE)

write_csv(overnight_by_pond,     file.path(outdir, "overnight_by_pond.csv"))
write_csv(overnight_by_tortoise, file.path(outdir, "overnight_by_tortoise.csv"))


# ------------------------------------------------------------
# Fig 7. Supplementary Figure 1 - entry time vs visit duration
# ------------------------------------------------------------

fig7 <- ggplot(nights, aes(time_enter, hours_inside, colour = overnight)) +
  geom_point(alpha = 0.55, size = 1.6) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24)) +
  scale_y_log10(labels = log_labels) +
  scale_colour_manual(name = "overnight",
                      values = c(no = "#4292c6", yes = "#e6550d")) +
  labs(title    = "Overnight pond use",
       subtitle = sprintf("%s of %s visits (%.2f%%) crossed local midnight",
                          comma(n_over), comma(nrow(nights)), pct_over),
       x = "pond entry time (hour of day, Galapagos local time)",
       y = "visit duration, hours (log)")

ggsave(file.path(outdir, "06_overnight_entry_vs_duration.png"), fig7,
       width = 9, height = 6, dpi = 200, bg = "white")


# ------------------------------------------------------------
# Fig 7b. Supplementary Figure 1b - presence during darkness
# ------------------------------------------------------------
# The midnight-crossing rule above answers "did the visit span two calendar
# days". That is not the same question as "was the tortoise in the pond
# during the night", and the two disagree at the edges: a tortoise entering
# at 00:30 and staying ten hours is in the water through the last of the
# darkness but exits on the day it arrived, so the midnight rule calls it a
# day visit.
#
# This second classification asks the biological question directly. Galapagos
# sits on the equator, so day length barely varies: sunrise and sunset stay
# within about twenty minutes of 06:00 and 18:00 all year. Night is therefore
# taken as the fixed window 18:00-06:00 local, and a visit counts as
# night-time use if any part of it falls inside that window.
night_start_h <- 18
night_end_h   <- 6

# Hours of each visit that fall in darkness. Visits reach 21.4 h, so one can
# span at most two nights; stepping over the three candidate nights around
# the entry date covers every case.
night_hours_in_visit <- function(start, end, tz) {
  total <- 0
  for (d in seq(as.Date(start, tz = tz) - 1, as.Date(end, tz = tz) + 1, by = "day")) {
    n_start <- as.POSIXct(paste(as.Date(d, origin = "1970-01-01"),
                                sprintf("%02d:00:00", night_start_h)), tz = tz)
    n_end   <- n_start + (24 - night_start_h + night_end_h) * 3600
    lo <- max(start, n_start)
    hi <- min(end,   n_end)
    if (hi > lo) total <- total + as.numeric(difftime(hi, lo, units = "hours"))
  }
  total
}

nights$night_hours <- mapply(night_hours_in_visit,
                             nights$local_entrance, nights$local_exit,
                             MoreArgs = list(tz = local_tz))

nights$night_use <- factor(if_else(nights$night_hours > 0, "yes", "no"),
                           levels = c("no", "yes"))

n_night   <- sum(nights$night_use == "yes")
pct_night <- 100 * n_night / nrow(nights)

# How the two definitions disagree
reclass <- table(midnight = nights$overnight, darkness = nights$night_use)
message("night-time use (18:00-06:00): ", n_night, " of ", nrow(nights),
        " visits (", sprintf("%.2f", pct_night), "%)")
message("  visits crossing midnight but with no darkness overlap: ",
        reclass["yes", "no"])
message("  visits in darkness that did NOT cross midnight: ",
        reclass["no", "yes"])

fig7b <- ggplot(nights, aes(time_enter, hours_inside, colour = night_use)) +
  annotate("rect", xmin = night_start_h, xmax = 24, ymin = 0.05, ymax = 25,
           fill = "grey85", alpha = 0.45) +
  annotate("rect", xmin = 0, xmax = night_end_h, ymin = 0.05, ymax = 25,
           fill = "grey85", alpha = 0.45) +
  geom_point(alpha = 0.55, size = 1.6) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24)) +
  scale_y_log10(labels = log_labels) +
  scale_colour_manual(name = "in pond\nduring darkness",
                      values = c(no = "#4292c6", yes = "#e6550d")) +
  labs(title    = "Pond use during darkness",
       subtitle = sprintf("%s of %s visits (%.2f%%) overlapped 18:00-06:00; shading marks night",
                          comma(n_night), comma(nrow(nights)), pct_night),
       x = "pond entry time (hour of day, Galapagos local time)",
       y = "visit duration, hours (log)")

ggsave(file.path(outdir, "06b_darkness_entry_vs_duration.png"), fig7b,
       width = 9, height = 6, dpi = 200, bg = "white")

write_csv(nights |> select(id, pond, local_entrance, local_exit, hours_inside,
                           overnight, night_hours, night_use),
          file.path(outdir, "pond_visits_night_use.csv"))


# ------------------------------------------------------------
# Fig 7c. Supplementary Figure 1, excluding Steve.Devine
# ------------------------------------------------------------
# Same panel as Fig 7, with one tracked tortoise removed, matching the
# no-Steve.Devine variants of the transition figures further down. Steve.Devine
# contributes 159 of the 2,254 visits (7.1%), so this is a check that the
# overnight pattern is not driven by that individual rather than a different
# result.
nights_nosd <- nights |> filter(id != "Steve.Devine")

n_over_nosd   <- sum(nights_nosd$overnight == "yes")
pct_over_nosd <- 100 * n_over_nosd / nrow(nights_nosd)

message("overnight excluding Steve.Devine: ", n_over_nosd, " of ",
        nrow(nights_nosd), " visits (", sprintf("%.2f", pct_over_nosd), "%)")

fig7c <- ggplot(nights_nosd, aes(time_enter, hours_inside, colour = overnight)) +
  geom_point(alpha = 0.55, size = 1.6) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24)) +
  scale_y_log10(labels = log_labels) +
  scale_colour_manual(name = "overnight",
                      values = c(no = "#4292c6", yes = "#e6550d")) +
  labs(title    = "Overnight pond use, excluding Steve.Devine",
       subtitle = sprintf("%s of %s visits (%.2f%%) were in the pond at local midnight",
                          comma(n_over_nosd), comma(nrow(nights_nosd)), pct_over_nosd),
       x = "pond entry time (hour of day, Galapagos local time)",
       y = "visit duration, hours (log)")

ggsave(file.path(outdir, "06c_overnight_no_SteveDevine.png"), fig7c,
       width = 9, height = 6, dpi = 200, bg = "white")


# ------------------------------------------------------------
# Fig 7d. Supplementary Figure 1, excluding Steve.Blake
# ------------------------------------------------------------
# Steve.Blake alone contributes 1,393 of the 2,254 visits (61.8%), almost all
# at P_SDB_3, so the full-sample panel is largely a portrait of one animal.
# Dropping it is the stronger test of whether the overnight pattern holds
# across the tracked population.
nights_nosb <- nights |> filter(id != "Steve.Blake")

n_over_nosb   <- sum(nights_nosb$overnight == "yes")
pct_over_nosb <- 100 * n_over_nosb / nrow(nights_nosb)

message("overnight excluding Steve.Blake: ", n_over_nosb, " of ",
        nrow(nights_nosb), " visits (", sprintf("%.2f", pct_over_nosb), "%), ",
        n_distinct(nights_nosb$id), " tortoises")

fig7d <- ggplot(nights_nosb, aes(time_enter, hours_inside, colour = overnight)) +
  geom_point(alpha = 0.55, size = 1.6) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(0, 24)) +
  scale_y_log10(labels = log_labels) +
  scale_colour_manual(name = "overnight",
                      values = c(no = "#4292c6", yes = "#e6550d")) +
  labs(title    = "Overnight pond use, excluding Steve.Blake",
       subtitle = sprintf("%s of %s visits (%.2f%%) were in the pond at local midnight",
                          comma(n_over_nosb), comma(nrow(nights_nosb)), pct_over_nosb),
       x = "pond entry time (hour of day, Galapagos local time)",
       y = "visit duration, hours (log)")

ggsave(file.path(outdir, "06d_overnight_no_SteveBlake.png"), fig7d,
       width = 9, height = 6, dpi = 200, bg = "white")


# ------------------------------------------------------------
# Fig 8. When do overnight stays happen, and where
# ------------------------------------------------------------

# entry and exit hour, split by overnight
hours_long <- nights |>
  select(overnight, entry = time_enter, exit = time_exit) |>
  tidyr::pivot_longer(c(entry, exit), names_to = "event", values_to = "hour")

fig8a <- ggplot(hours_long, aes(hour, fill = overnight)) +
  geom_histogram(binwidth = 1, colour = "white", linewidth = 0.2) +
  facet_wrap(~event, ncol = 1) +
  scale_x_continuous(breaks = seq(0, 24, 3), limits = c(-0.5, 24.5)) +
  scale_fill_manual(name = "overnight", values = c(no = "#4292c6", yes = "#e6550d")) +
  labs(title = "Entry and exit hour", x = "hour of day (local)", y = "visits")

fig8b <- ggplot(overnight_by_pond |> filter(visits > 0),
                aes(pct_overnight, reorder(pond, pct_overnight))) +
  geom_col(fill = "#e6550d", alpha = 0.85) +
  geom_text(aes(label = paste0(overnight, "/", visits)), hjust = -0.15, size = 3) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Share of visits that run overnight", subtitle = "label = overnight / total visits",
       x = "% of visits overnight", y = NULL)

fig8c <- nights |>
  count(month_num, overnight) |>
  ggplot(aes(factor(month_num), n, fill = overnight)) +
  geom_col() +
  scale_fill_manual(name = "overnight", values = c(no = "#4292c6", yes = "#e6550d")) +
  labs(title = "Overnight stays by month", x = "month", y = "visits")

ggsave(file.path(outdir, "07_overnight_when_where.png"),
       (fig8a | fig8b) / fig8c + plot_layout(heights = c(1.3, 1), guides = "collect"),
       width = 13, height = 9, dpi = 200, bg = "white")


# ------------------------------------------------------------
# Interactive maps
# ------------------------------------------------------------

# back to lon/lat for mapview.
#
# saveWidget() below uses selfcontained = FALSE: TRUE needs pandoc
# installed. FALSE writes the html plus a sibling <name>_files folder -
# keep the two together when moving or sharing the map.
ponds_map <- ponds_m |>
  st_set_geometry("geometry") |>
  select(pond, kind, area_m2, n_feature) |>
  left_join(select(pond_recurse, pond, size_class, visits, hours,
                   n_tortoises),
            by = "pond") |>
  st_transform(4326)

# every individual sampled location, so the zone can be read as
# "within 5 m of one of these"
sampled_map <- ponds_raw_m |>
  filter(!pond %in% drawn_ponds) |>
  st_centroid() |>
  st_transform(4326)

circle_map <- st_sf(pond     = ponds_m$pond,
                    geometry = st_buffer(st_centroid(st_geometry(ponds_m)), buffer_m)) |>
  st_transform(4326)

# For the points layer use the GPS fixes that fell inside ponds, from
# Part A. Note that revisitStats$x and $y are ALWAYS NA when the
# recursion came from getRecursionsInPolygon - a polygon has no single
# centre location to report, unlike getRecursionsAtLocations - so
# visit_stats cannot be mapped directly.
hits_map <- select(tortoise_pond_hits_sf, name, pond, timestamp)


# Map A - pond use
map_A <-
  mapview(ponds_map, zcol = "visits", layer.name = "Pond (visits)",
          alpha.regions = 0.7, lwd = 2) +
  mapview(hits_map, zcol = "pond", cex = 4, layer.name = "GPS fixes inside ponds")

saveWidget(map_A@map,
           file.path(normalizePath(outdir), "map_A_pond_use.html"),
           selfcontained = FALSE, title = "Pond revisitation")


# Map B - the hand-drawn large ponds
big_map <- filter(ponds_map, pond %in% big)

fixes_map <- st_transform(near_big, 4326)   # the 120 m selection from Fig 1

map_B <-
  mapview(big_map, col.regions = "#2c7fb8", color = "#08519c",
          alpha.regions = 0.5, layer.name = "Pond outline") +
  mapview(fixes_map, cex = 2, col.regions = "grey40", layer.name = "Tortoise fixes")

saveWidget(map_B@map,
           file.path(normalizePath(outdir), "map_B_large_ponds.html"),
           selfcontained = FALSE, title = "Hand-drawn large ponds")


# Map C - the convex hull caveat: orange is what recurse analysed,
# the filled polygon is the real pond. Big orange gaps = that
# pond's visit count is an upper bound.
map_C <-
  mapview(ponds_map, zcol = "kind", alpha.regions = 0.6,
          layer.name = "Pond zone") +
  mapview(sampled_map, col.regions = "black", cex = 3,
          layer.name = "Sampled locations")

saveWidget(map_C@map,
           file.path(normalizePath(outdir), "map_C_pond_zones.html"),
           selfcontained = FALSE, title = "Pond zones and the sampled locations behind them")


message("done - everything written to ", outdir)

# ============================================================
# PART F - seasonality, network, transitions and sediment export
#
# Everything here is built from `nights`, the merged pond visits with
# local times from PART E.
# ============================================================

# pond type, from the field sheet
visits <- nights |>
  left_join(pond_type_lookup, by = "pond") |>
  mutate(visit_date = as.Date(local_entrance, tz = local_tz))

if (any(is.na(visits$pond_type)))
  warning("no pond type for: ",
          paste(unique(visits$pond[is.na(visits$pond_type)]), collapse = ", "),
          call. = FALSE)


# ------------------------------------------------------------
# F1. Seasonality, corrected for tracking effort
#
# A month with more collars running will show more visits even if
# each animal behaves identically, so visits are expressed per 30
# tortoise-days. A tortoise-day is one animal with at least one fix
# on that local date.
# ------------------------------------------------------------

# Note the second year filter. The main filter is on UTC year, but
# converting to Galapagos local time (UTC-6) pulls the first six hours
# of 1 January back into 31 December of the previous year. Without this
# you get a phantom December 2017 of about 4 tortoise-days and no
# visits, averaged in as though it were a whole month.
effort_by_month <- tracks |>
  mutate(local_time = with_tz(timestamp, local_tz),
         date       = as.Date(local_time, tz = local_tz),
         year       = year(local_time),
         month_num  = month(local_time)) |>
  filter(year %in% years_to_keep) |>
  distinct(name, date, year, month_num) |>
  count(year, month_num, name = "tortoise_days")

visits_by_month <- visits |>
  count(year, month_num, name = "n_visits")

monthly_rate <- effort_by_month |>
  left_join(visits_by_month, by = c("year", "month_num")) |>
  mutate(n_visits = replace_na(n_visits, 0),
         visits_per_30_tortoise_days = n_visits / tortoise_days * 30)

# Two ways to summarise. `mean_visit_rate` averages the per-year rates,
# `pooled_visit_rate` divides total
# visits by total tortoise-days. Prefer pooled: telemetry effort has
# collapsed in recent years (December ran from 431 tortoise-days in
# 2023 to 62 in 2025), and a mean of ratios lets those thin months
# swing the answer. Both are written out so the choice is visible.
seasonal <- monthly_rate |>
  group_by(month_num) |>
  summarise(pooled_visit_rate = sum(n_visits) / sum(tortoise_days) * 30,
            mean_visit_rate   = mean(visits_per_30_tortoise_days),
            sd_visit_rate     = sd(visits_per_30_tortoise_days),
            tortoise_days     = sum(tortoise_days),
            visits            = sum(n_visits),
            n_years           = n(),
            .groups = "drop") |>
  mutate(month = factor(month.name[month_num], levels = month.name))

write_csv(seasonal, file.path(outdir, "seasonality_effort_corrected.csv"))

peak_month <- slice_max(seasonal, pooled_visit_rate, n = 1, with_ties = FALSE)
low_month  <- slice_min(seasonal, pooled_visit_rate, n = 1, with_ties = FALSE)

message("seasonality (effort-weighted): peak ", peak_month$month, " ",
        round(peak_month$pooled_visit_rate, 1), " visits / 30 tortoise-days; ",
        "low ", low_month$month, " ", round(low_month$pooled_visit_rate, 1))

fig9 <- ggplot(seasonal, aes(month, pooled_visit_rate)) +
  geom_col(fill = "#2c7fb8", alpha = 0.85) +
  geom_text(aes(label = round(pooled_visit_rate, 1)), vjust = -0.4, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(title    = "Pond visitation corrected for tracking effort",
       subtitle = "total visits / total tortoise-days x 30, pooled across 2018-2025",
       x = NULL, y = "visits per 30 tortoise-days") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

ggsave(file.path(outdir, "08_seasonality.png"), fig9,
       width = 9, height = 5, dpi = 200, bg = "white")


# ------------------------------------------------------------
# F2. Figure 2 - daily patterns of pond use
#
# Built on tort_sp_stats, constructed exactly as in the original
# analysis. Two differences, both deliberate:
#
#   * it is built from the merged pond visits, so the 2,254 visits
#     here are the same 2,254 the Results report. The original used
#     recAtLocs$revisitStats directly, which counts one arrival once
#     per overlapping buffer entered.
#   * `site` was paste("site", coordIdx), a buffer index; the
#     equivalent unit here is the pond.
#
# Everything else - the 0.1 h filter, with_tz to Galapagos, and the
# derived doy/hour/time/overnight columns - is the original code.
# ------------------------------------------------------------

tort_sp_stats <- as.data.frame(visit_stats)
tort_sp_stats <- tort_sp_stats[tort_sp_stats$timeInside > 0.1, ]

tort_sp_stats$local.entranceTime <- with_tz(tort_sp_stats$entranceTime, tz = local_tz)
tort_sp_stats$local.exitTime     <- with_tz(tort_sp_stats$exitTime,     tz = local_tz)
tort_sp_stats$year       <- year(tort_sp_stats$entranceTime)
tort_sp_stats$doy_enter  <- yday(tort_sp_stats$local.entranceTime)
tort_sp_stats$hour_enter <- hour(tort_sp_stats$local.entranceTime)
tort_sp_stats$time_enter <- hour(tort_sp_stats$local.entranceTime) +
  minute(tort_sp_stats$local.entranceTime) / 60 +
  second(tort_sp_stats$local.entranceTime) / 60 / 60
tort_sp_stats$time_exit  <- hour(tort_sp_stats$local.exitTime) +
  minute(tort_sp_stats$local.exitTime) / 60 +
  second(tort_sp_stats$local.exitTime) / 60 / 60
tort_sp_stats$overnight  <- factor(as.logical(yday(tort_sp_stats$local.exitTime) -
                                              yday(tort_sp_stats$local.entranceTime)))
levels(tort_sp_stats$overnight) <- c("no", "yes")
tort_sp_stats$site  <- tort_sp_stats$pond
tort_sp_stats$month <- month(tort_sp_stats$local.entranceTime, label = TRUE)

message("tort_sp_stats: ", nrow(tort_sp_stats), " visits, ",
        n_distinct(tort_sp_stats$id), " tortoises, ",
        n_distinct(tort_sp_stats$site), " ponds")


# --- (a) visit duration by entrance time, smooth per month ----------
# Boxplots are the ggplot five-number summary of visit duration for all
# visits entering in that hour, pooled across months: box = IQR, centre
# line = median, whiskers = 1.5 x IQR, points = values beyond them.
# They are not means, and the points are not raw data.
# One curve per season rather than per month. Twelve monthly loess curves
# were unreadable, and the sparse months swung to implausible durations on a
# handful of visits. January is kept separate from the hot-wet season: it
# sits at the transition and carries the highest pond use in both the camera
# and the telemetry, so pooling it would average away the peak the Results
# describe. Same split and same colours as Figure 4.
tort_sp_stats$season <- factor(
  case_when(month(tort_sp_stats$local.entranceTime) == 1     ~ "January (transition)",
            month(tort_sp_stats$local.entranceTime) %in% 2:5 ~ "Hot-wet (Feb-May)",
            TRUE                                             ~ "Cool-dry (Jun-Dec)"),
  levels = c("Hot-wet (Feb-May)", "January (transition)", "Cool-dry (Jun-Dec)"))

season_cols <- c("Hot-wet (Feb-May)"    = "#D55E00",
                 "January (transition)" = "#CC79A7",
                 "Cool-dry (Jun-Dec)"   = "#0072B2")

pa <- ggplot(tort_sp_stats, aes(x = hour_enter, y = timeInside)) +
  geom_boxplot(aes(group = factor(hour_enter)), fill = "white", alpha = 0.7,
               outlier.size = 0.7, outlier.alpha = 0.35) +
  geom_smooth(aes(colour = season, fill = season), method = "loess",
              se = TRUE, alpha = 0.15, linewidth = 1.1, formula = y ~ x) +
  scale_colour_manual(values = season_cols) +
  scale_fill_manual(values   = season_cols) +
  scale_x_continuous(breaks = seq(0, 24, 5)) +
  # Durations span two orders of magnitude (median 0.9 h, max 21.4 h), so on a
  # linear axis every box is flattened against zero and only the outliers are
  # legible. A log axis shows the hourly structure the panel is about.
  scale_y_log10(breaks = c(0.1, 0.25, 0.5, 1, 2, 5, 10, 20),
                labels = c("0.1", "0.25", "0.5", "1", "2", "5", "10", "20")) +
  labs(x = "Entrance time (hour)", y = "Visit duration (h, log scale)",
       colour = "Season", fill = "Season",
       title = "(b) Visit duration by entrance time") +
  theme_minimal(base_size = 20) +
  theme(axis.text    = element_text(size = 17, colour = "black"),
        axis.title   = element_text(size = 20, face = "bold"),
        plot.title   = element_text(size = 22, face = "bold"),
        legend.text  = element_text(size = 17),
        legend.title = element_text(size = 18, face = "bold"),
        axis.line.x = element_line(colour = "black", linewidth = 0.6),
        axis.line.y = element_line(colour = "black", linewidth = 0.6))


# --- (b) pond residence time by time of day -------------------------
# Each visit's recurse timeInside is credited to the hours it was
# actually spent in, not to the hour it began, so this answers "when
# are tortoises in ponds" rather than "when do visits start". That is
# the quantity comparable to the camera counts.
#
# The walk below is plain arithmetic on tort_sp_stats: for each visit,
# step through the hour boundaries between entrance and exit and add
# the minutes falling in each. The check afterwards enforces that the
# bars total recurse's own residence time.
residence_hour <- data.frame(hour = 0:23, pond_hours = 0)

for (i in seq_len(nrow(tort_sp_stats))) {
  s_i <- tort_sp_stats$local.entranceTime[i]
  e_i <- tort_sp_stats$local.exitTime[i]
  h_i <- floor_date(s_i, "hour")
  while (h_i < e_i) {
    lo <- max(s_i, h_i)
    hi <- min(e_i, h_i + 3600)
    k  <- hour(h_i) + 1
    residence_hour$pond_hours[k] <- residence_hour$pond_hours[k] +
      as.numeric(difftime(hi, lo, units = "hours"))
    h_i <- h_i + 3600
  }
}

if (abs(sum(residence_hour$pond_hours) - sum(tort_sp_stats$timeInside)) > 1) {
  stop("residence by hour does not reconcile with recurse timeInside: ",
       round(sum(residence_hour$pond_hours), 1), " vs ",
       round(sum(tort_sp_stats$timeInside), 1), " h")
}

message("residence by time of day: peak ",
        residence_hour$hour[which.max(residence_hour$pond_hours)], ":00, trough ",
        residence_hour$hour[which.min(residence_hour$pond_hours)], ":00, total ",
        round(sum(residence_hour$pond_hours)), " h (reconciles with recurse)")

write_csv(residence_hour, file.path(outdir, "residence_by_hour_of_day.csv"))

pb <- ggplot(residence_hour, aes(hour, pond_hours)) +
  geom_col(fill = "#2c7fb8", alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, 24, 5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "(a) Pond residence time by time of day",
       x = "Hour of day (Galapagos local time)",
       y = "Pond residence time (h)") +
  theme_minimal(base_size = 20) +
  theme(axis.text   = element_text(size = 17, colour = "black"),
        axis.title  = element_text(size = 20, face = "bold"),
        plot.title  = element_text(size = 22, face = "bold"),
        axis.line.x = element_line(colour = "black", linewidth = 0.6),
        axis.line.y = element_line(colour = "black", linewidth = 0.6))

# panel order: residence by time of day first, visit duration second
# No overall title: the manuscript caption carries it, and repeating it in the
# image wastes vertical space that the two panels need.
fig2 <- pb / pa

ggsave(file.path(outdir, "09_figure2_revisitation.png"), fig2,
       width = 13, height = 12, dpi = 300, bg = "white")


# --- visit day of year, by pond and year ----------------------------
ttime <- ggplot(tort_sp_stats, aes(x = doy_enter)) +
  geom_histogram(binwidth = diff(range(tort_sp_stats$doy_enter))/7,
                 color = "darkgray", fill = "gray") +
  facet_grid(site ~ year) +
  xlab("visit day of year") + ylab("revisit frequency") +
  theme_classic()

ggsave(file.path(outdir, "14_visit_doy_site_year.png"), ttime,
       width = 14, height = 16, dpi = 150, bg = "white", limitsize = FALSE)


# ------------------------------------------------------------
# F3. Bipartite network of tortoises and ponds
#
# Edge weight is the number of distinct local calendar days on
# which that tortoise made at least one visit to that pond, which
# is what the figure caption describes. Ribbon width is log(1 + w)
# so that one very heavily used pond does not flatten the rest.
# ------------------------------------------------------------

edges <- visits |>
  group_by(tortoise = id, pond, pond_type) |>
  summarise(visit_days     = n_distinct(visit_date),
            valid_visits   = n(),
            residence_hours = sum(hours_inside),
            .groups = "drop")

write_csv(edges, file.path(outdir, "tortoise_pond_network_edges.csv"))

message("network: ", n_distinct(edges$tortoise), " tortoises, ",
        n_distinct(edges$pond), " ponds, ", nrow(edges), " links")

# ponds grouped by type then by site, so the figure reads left to right
pond_order <- edges |>
  distinct(pond, pond_type) |>
  arrange(pond_type, pond)

web <- xtabs(visit_days ~ tortoise + pond, data = edges) |> unclass()
web <- web[, pond_order$pond, drop = FALSE]

# order tortoises by the average position of the ponds they use, so
# links cross as little as possible
pond_rank <- setNames(seq_len(ncol(web)), colnames(web))
tortoise_order <- edges |>
  mutate(rank = pond_rank[pond]) |>
  group_by(tortoise) |>
  summarise(pos = weighted.mean(rank, visit_days), .groups = "drop") |>
  arrange(pos) |>
  pull(tortoise)

web <- web[tortoise_order, , drop = FALSE]

pond_box_colors  <- unname(pond_type_colors[pond_order$pond_type])

web_plot <- log1p(web)
rownames(web_plot) <- str_replace_all(rownames(web_plot), "\\.", " ")
colnames(web_plot) <- str_replace_all(str_remove(colnames(web_plot), "^P_"), "_", " ")

pdf(file.path(outdir, "10_tortoise_pond_network.pdf"), width = 11, height = 9)
par(xpd = NA)

plotweb(
  web_plot,
  horizontal    = TRUE,
  sorting       = "normal",
  higher_color  = pond_box_colors,
  higher_border = "grey25",
  lower_color   = "grey20",
  lower_border  = "grey15",
  link_color    = "higher",
  link_border   = "same",
  link_alpha    = 0.45,
  curved_links  = TRUE,
  text_size     = 0.85,
  spacing       = 0.4,
  box_size      = 0.075,
  empty         = FALSE,
  
  # IMPORTANT: margin goes here, not in par()
  mar = c(6, 1, 2, 1)
)

# title(
#   main = "Valid pond visits by tracked giant tortoises",
#   line = 0.3,
#   cex.main = 1.1
# )

# mtext(
#   "Ribbon width is log(1 + unique local days with a valid visit).",
#   side = 1,
#   line = 2,
#   cex = 0.75
# )

legend(
  x = 0.5,
  y = -0.10,
  legend = names(pond_type_colors),
  fill = unname(pond_type_colors),
  border = "grey25",
  bty = "n",
  cex = 0.8,
  horiz = TRUE,
  xjust = 0.5,
  yjust = 1,
  xpd = NA
)

# par(mar = c(4, 7, 3, 7), xpd = NA)
# plotweb(web_plot,
#         horizontal   = TRUE,
#         sorting      = "normal",
#         higher_color = pond_box_colors,
#         higher_border = "grey25",
#         lower_color  = "grey20",
#         lower_border = "grey15",
#         link_color   = "higher",
#         link_border  = "same",
#         link_alpha   = 0.45,
#         curved_links = TRUE,
#         text_size    = 0.85,
#         spacing      = 0.4,
#         box_size     = 0.075,
#         empty        = FALSE)
# title(main = "Valid pond visits by tracked giant tortoises",
#       line = 0.3, cex.main = 1.1)
# mtext("Ribbon width is log(1 + unique local days with a valid visit).",
#       side = 1, line = 1, cex = 0.75)
# legend("bottomright", legend = names(pond_type_colors),
#        fill = unname(pond_type_colors), border = "grey25", bty = "n", cex = 0.8)
dev.off()


# ------------------------------------------------------------
# F4. Pond-to-pond transitions
#
# Collapse each run of consecutive visits to the same pond into one
# bout, then take every pair of successive bouts at DIFFERENT ponds.
# The gap is from leaving the first pond to entering the second.
# ------------------------------------------------------------

bouts <- visits |>
  arrange(id, local_entrance) |>
  group_by(id) |>
  mutate(bout = cumsum(is.na(lag(pond)) | pond != lag(pond))) |>
  group_by(id, bout) |>
  summarise(pond       = first(pond),
            bout_start = min(local_entrance),
            bout_end   = max(local_exit),
            n_visits   = n(),
            .groups = "drop")

transitions <- bouts |>
  arrange(id, bout_start) |>
  group_by(id) |>
  mutate(to_pond     = lead(pond),
         to_start    = lead(bout_start),
         gap_hours   = as.numeric(difftime(to_start, bout_end, units = "hours"))) |>
  ungroup() |>
  filter(!is.na(to_pond), pond != to_pond, gap_hours >= 0) |>
  transmute(tortoise = id, from_pond = pond, to_pond,
            left_at = bout_end, arrived_at = to_start, gap_hours,
            gap_days = gap_hours / 24)

message("transitions: ", nrow(transitions), " by ",
        n_distinct(transitions$tortoise), " tortoises; gap ",
        round(min(transitions$gap_hours), 2), " h to ",
        round(max(transitions$gap_hours) / (24 * 365.25), 2), " years")

multi_pond <- visits |>
  group_by(id) |>
  summarise(ponds = n_distinct(pond), .groups = "drop") |>
  filter(ponds > 1)

message("tortoises using more than one pond: ", nrow(multi_pond))


# ------------------------------------------------------------
# F4b. How far apart are the ponds a tortoise moves between?
#
# Straight-line distance between pond centroids for every observed
# transition. This is the distance an organism carried on or in a
# tortoise could be moved between freshwater habitats, so it is
# paired here with the time the tortoise took: a transition spanning
# months is not a plausible dispersal event for most aquatic taxa,
# whereas one completed within a day or two is.
#
# Distances are between centroids of the pond zones, not the route
# the animal walked, so they are minimum separations.
# ------------------------------------------------------------

pond_centroids <- st_centroid(ponds_m)
pond_dist_m <- st_distance(pond_centroids)
dimnames(pond_dist_m) <- list(ponds_m$pond, ponds_m$pond)

transitions <- transitions |>
  mutate(distance_m  = as.numeric(pond_dist_m[cbind(as.character(from_pond),
                                                    as.character(to_pond))]),
         distance_km = distance_m / 1000)

write_csv(transitions, file.path(outdir, "pond_transitions.csv"))

message("transition distances: median ", round(median(transitions$distance_m)),
        " m, range ", round(min(transitions$distance_m)), "-",
        round(max(transitions$distance_m)), " m")

fig_dist <- ggplot(transitions, aes(distance_m)) +
  geom_histogram(binwidth = 250, boundary = 0,
                 fill = "#2c7fb8", colour = "white", alpha = 0.9) +
  scale_x_continuous(breaks = seq(0, ceiling(max(transitions$distance_m) / 500) * 500, 500)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "Distances moved between successive ponds",
       subtitle = sprintf("%d transitions by %d tortoises; median %s m",
                          nrow(transitions), n_distinct(transitions$tortoise),
                          format(round(median(transitions$distance_m)), big.mark = ",")),
       x = "Distance between ponds (m)", y = "Number of transitions") +
  theme_minimal(base_size = 13) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        axis.line  = element_line(colour = "black", linewidth = 0.6))

ggsave(file.path(outdir, "15_transition_distances.png"), fig_dist,
       width = 8, height = 5.5, dpi = 300, bg = "white")

# Distance against elapsed time, one point per transition. This carries
# both dimensions at once: the top-left corner - far and fast - is where a
# tortoise could plausibly move a living passenger between ponds. Points are
# jittered vertically because transitions between the same pond pair share an
# identical distance and would otherwise overplot.
fig_dist_time <- ggplot(transitions, aes(gap_days, distance_m)) +
  geom_vline(xintercept = c(1, 7), linetype = "dashed", colour = "grey60") +
  annotate("text", x = 1, y = Inf, label = " 1 day", hjust = 0, vjust = 1.6,
           size = 3.4, colour = "grey40") +
  annotate("text", x = 7, y = Inf, label = " 1 week", hjust = 0, vjust = 1.6,
           size = 3.4, colour = "grey40") +
  geom_point(aes(colour = tortoise), size = 2.6, alpha = 0.8,
             position = position_jitter(width = 0, height = 40, seed = 1)) +
  scale_x_log10(breaks = c(0.1, 1, 7, 30, 100, 365),
                labels = c("0.1", "1", "7", "30", "100", "365")) +
  scale_y_continuous(breaks = seq(0, 3500, 500)) +
  scale_colour_viridis_d(option = "D", end = 0.9) +
  labs(title = "Pond-to-pond movements: how far, and how fast",
       # count unordered pairs: A->B and B->A are the same connection
       subtitle = sprintf("%d transitions by %d tortoises among %d pond pairs",
                          nrow(transitions), n_distinct(transitions$tortoise),
                          n_distinct(paste(pmin(as.character(transitions$from_pond),
                                                as.character(transitions$to_pond)),
                                           pmax(as.character(transitions$from_pond),
                                                as.character(transitions$to_pond))))),
       x = "Time between leaving one pond and entering the next (days, log scale)",
       y = "Distance between ponds (m)", colour = "Tortoise") +
  theme_minimal(base_size = 13) +
  theme(axis.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", linewidth = 0.6))

ggsave(file.path(outdir, "16_transition_distance_vs_time.png"), fig_dist_time,
       width = 9, height = 6, dpi = 300, bg = "white")

# The same two figures without Steve Devine, who alone made 25 of the 47
# transitions - nearly all of them the 188 m hop between SDB 2 and SDB 3.
# Dropping him shows how much of the pattern rests on one animal. ggplot's
# %+% swaps the data into the plots already built above, so the two versions
# cannot drift apart.
transitions_no_sd <- transitions |> filter(tortoise != "Steve.Devine")

pairs_no_sd <- n_distinct(paste(
  pmin(as.character(transitions_no_sd$from_pond), as.character(transitions_no_sd$to_pond)),
  pmax(as.character(transitions_no_sd$from_pond), as.character(transitions_no_sd$to_pond))))

ggsave(file.path(outdir, "15b_transition_distances_no_SteveDevine.png"),
       fig_dist %+% transitions_no_sd +
         labs(subtitle = sprintf("%d transitions by %d tortoises; median %s m (Steve Devine excluded)",
                                 nrow(transitions_no_sd),
                                 n_distinct(transitions_no_sd$tortoise),
                                 format(round(median(transitions_no_sd$distance_m)), big.mark = ","))),
       width = 8, height = 5.5, dpi = 300, bg = "white")

ggsave(file.path(outdir, "16b_transition_distance_vs_time_no_SteveDevine.png"),
       fig_dist_time %+% transitions_no_sd +
         labs(subtitle = sprintf("%d transitions by %d tortoises among %d pond pairs (Steve Devine excluded)",
                                 nrow(transitions_no_sd),
                                 n_distinct(transitions_no_sd$tortoise), pairs_no_sd)),
       width = 9, height = 6, dpi = 300, bg = "white")

message("without Steve Devine: ", nrow(transitions_no_sd), " transitions, median ",
        round(median(transitions_no_sd$distance_m)), " m")

# how many of these movements are fast enough to matter for a hitchhiker
for (limit in c(1, 7, 30)) {
  quick <- transitions |> filter(gap_days <= limit)
  message("  within ", limit, " day(s): ", nrow(quick), " transitions",
          if (nrow(quick) > 0)
            paste0(", ", round(min(quick$distance_m)), "-",
                   round(max(quick$distance_m)), " m") else "")
}


# ------------------------------------------------------------
# F5. Visit table for the sediment-export analysis
#
# Written in the column names that the mud-export script expects,
# so it can be run against these visits without modification.
# ------------------------------------------------------------

visit_export_dir <- file.path("Outdir", "recurse_analysis_final", "01_visits")
dir.create(visit_export_dir, recursive = TRUE, showWarnings = FALSE)

pond_visits_export <- visits |>
  transmute(tortoise        = as.character(id),
            pond,
            visit_start_utc = with_tz(entranceTime, "UTC"),
            visit_end_utc   = with_tz(exitTime,     "UTC"),
            residence_hours = hours_inside,
            hours_since_previous_visit = timeSinceLastVisit) |>
  arrange(tortoise, visit_start_utc)

write_csv(pond_visits_export,
          file.path(visit_export_dir,
                    "pond_visits_primary_radius_all_durations.csv"))
write_csv(filter(pond_visits_export, residence_hours > min_visit_hours),
          file.path(visit_export_dir,
                    "pond_visits_primary_analysis_gt_0.1h.csv"))

message("visit tables for the sediment analysis written to ", visit_export_dir)


# ------------------------------------------------------------
# F6. Sediment export
#
# The sediment numbers depend directly on the recurse output: change
# how a visit is defined and the departure rate, the kg per
# tortoise-year and the tonnes all move with it.
#
# Following the Methods: keep tortoises tracked at least 150 days,
# count at most one departure per tortoise, pond and local calendar
# day, and multiply the resulting annual departure rate by the mean
# measured sediment load. Uncertainty from 5,000 bootstraps that
# resample tracked tortoises (keeping each one's departures and
# effort together) and resample sediment loads separately.
# ------------------------------------------------------------

min_tracking_days <- 150
n_boot            <- 5000
set.seed(2026)

# tracking effort per tortoise
effort_by_tortoise <- tracks |>
  mutate(date = as.Date(with_tz(timestamp, local_tz), tz = local_tz)) |>
  distinct(name, date) |>
  count(name, name = "tracked_days") |>
  mutate(tracking_years = tracked_days / 365.25)

# one departure per tortoise, pond and local calendar day
departures <- visits |>
  distinct(id, pond, visit_date) |>
  count(id, name = "departure_days")

export_base <- effort_by_tortoise |>
  filter(tracked_days >= min_tracking_days) |>
  left_join(departures, by = c("name" = "id")) |>
  mutate(departure_days = replace_na(departure_days, 0L))

message("sediment export: ", nrow(export_base), " tortoises with >= ",
        min_tracking_days, " tracked days, ",
        sum(export_base$tracked_days), " tracking days (",
        round(sum(export_base$tracking_years), 1), " tortoise-years), ",
        sum(export_base$departure_days), " departure days")

# measured sediment loads
#
# Sediment was scraped from the anterior carapace only from 2018-2020
# and from both the anterior and posterior portions from 2021 onwards,
# so the earlier loads are doubled to put every year on the same
# whole-carapace basis. The correction is derived here from the raw
# weights rather than read from the stored "adressing half mud" column,
# which doubles 2019 only and carries one fill-down error in 2020.
mud <- read_csv("Data/Mud_2024_2025_fieldwork.csv", show_col_types = FALSE,
                locale = locale(encoding = "Latin1")) |>
  mutate(mud_raw_g = parse_number(as.character(`Mud weight (g)`)),
         mud_g     = if_else(Year <= 2020, mud_raw_g * 2, mud_raw_g)) |>
  filter(!is.na(mud_g), is.finite(mud_g), mud_g >= 0) |>
  mutate(mud_kg = mud_g / 1000)

message("sediment loads: n = ", nrow(mud), ", median ", round(median(mud$mud_g)),
        " g, mean ", round(mean(mud$mud_g), 1), " g")

departure_rate <- sum(export_base$departure_days) / sum(export_base$tracking_years)
export_per_tortoise_year <- departure_rate * mean(mud$mud_kg)

boot <- replicate(n_boot, {
  i <- sample(nrow(export_base), replace = TRUE)
  r <- sum(export_base$departure_days[i]) / sum(export_base$tracking_years[i])
  r * mean(sample(mud$mud_kg, replace = TRUE))
})
boot_ci <- quantile(boot, c(0.025, 0.975), names = FALSE)

# Adults only. These are estimated ADULT population sizes supplied by the
# authors, not a fraction of the published totals (Marquez et al. 2019),
# which are retained alongside for reference. Change `adults` and every
# tonnage below follows.
populations <- tibble(
  species   = c("Chelonoidis porteri", "Chelonoidis donfaustoi"),
  published = c(7198L, 700L),
  adults    = c(3400L,  250L)) |>
  mutate(tonnes       = export_per_tortoise_year * adults / 1000,
         tonnes_lower = boot_ci[1] * adults / 1000,
         tonnes_upper = boot_ci[2] * adults / 1000)

sediment_export <- tibble(
  departures_per_tortoise_year = departure_rate,
  mean_load_kg                 = mean(mud$mud_kg),
  kg_per_tortoise_year         = export_per_tortoise_year,
  kg_lower_95                  = boot_ci[1],
  kg_upper_95                  = boot_ci[2])

print(as.data.frame(sediment_export), row.names = FALSE)
print(as.data.frame(populations),     row.names = FALSE)

write_csv(sediment_export, file.path(outdir, "sediment_export_per_tortoise.csv"))
write_csv(populations,     file.path(outdir, "sediment_export_population.csv"))


# ------------------------------------------------------------
# F7. Sediment loads: pond type and body size
#
# These two results - the Tweedie GLMM of load against pond type
# (Figure 5) and the carapace-length correlation (Supplementary
# Figure 3) - use only the carapace samples and no telemetry, so
# they live in Code/src/pond_mud_camera.R and are not repeated
# here. Only the export calculation above needs the recurse
# departure rate.
# ------------------------------------------------------------


# Data for the two panels below. The `panel` labels must match the
# filter() calls that follow.
export_plot_data <- bind_rows(
  tibble(label = "Per tortoise",
         est   = sediment_export$kg_per_tortoise_year,
         lo    = sediment_export$kg_lower_95,
         hi    = sediment_export$kg_upper_95,
         panel = "Per tortoise sediment transport rate"),
  populations |>
    transmute(label = paste0(sub("Chelonoidis", "C.", species),
                             " (n = ", format(adults, big.mark = ","), " adults)"),
              est   = tonnes,
              lo    = tonnes_lower,
              hi    = tonnes_upper,
              panel = "Potential annual population sediment export (tonnes/yr)")
)

# Per-tortoise panel
p1 <- export_plot_data |>
  filter(panel == "Per tortoise sediment transport rate") |>
  ggplot(aes(est, label)) +
  geom_errorbar(
    aes(xmin = lo, xmax = hi),
    width = 0.14,
    linewidth = 0.8
  ) +
  geom_point(size = 3.2) +
  labs(
    title = "Per tortoise sediment transport rate",
    x = expression("Sediment export (kg tortoise"^{-1}*" yr"^{-1}*")"),
    y = NULL
  ) +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5)
  )

# # Supplementary Figure 4

# Population-level panel
p2 <- export_plot_data |>
  filter(panel == "Potential annual population sediment export (tonnes/yr)") |>
  ggplot(aes(est, label)) +
  geom_errorbar(
    aes(xmin = lo, xmax = hi),
    width = 0.14,
    linewidth = 0.8
  ) +
  geom_point(size = 3.2) +
  labs(
    title = "Potential annual population sediment export",
    x = expression("Sediment export (tonnes yr"^{-1}*")"),
    y = NULL,
    caption = paste(
      "One departure per tortoise, pond and calendar day.",
      "\nPopulations are estimated adult population sizes.",
      "\nThe C. donfaustoi figure applies the C. porteri per-capita rate."
    )
  ) +
  theme(
    plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
    plot.caption = element_text(hjust = 0, size = 8)
  )


# Combine panels
fig13 <- p1 / p2 +
  plot_annotation(
    title = "Potential sediment export",
    subtitle = "Points = estimates, bars = 95% bootstrap intervals"
  ) &
  theme(
    plot.title = element_text(face = "bold")
  )

ggsave(
  file.path(outdir, "13_sediment_export.png"),
  fig13,
  width = 8,
  height = 6,
  dpi = 200,
  bg = "white"
)


# ============================================================
# PART G - the numbers reported in the paper
# ============================================================

cat("\n============================================================\n")
cat("NUMBERS REPORTED IN THE PAPER\n")
cat("============================================================\n")

cat("\nPond delineation\n")
cat("  sampling positions used :", sum(!ponds_raw$pond %in% drawn_ponds), "\n")
cat("  buffer                  :", buffer_m, "m\n")
cat("  hand-drawn ponds        :", paste(drawn_ponds, collapse = ", "), "\n")
if (length(missing) > 0)
  cat("  NOT YET DRAWN           :", paste(missing, collapse = ", "), "\n")

cat("\nRevisitation\n")
cat("  visits                  :", nrow(visit_stats), "\n")
cat("  tortoises               :", n_distinct(visit_stats$id), "\n")
cat("  ponds with >=1 visit    :", n_distinct(visit_stats$pond), "\n")
cat("  total residence time    :", round(sum(visit_stats$timeInside)), "h\n")
cat("  median visit duration   :", round(median(visit_stats$timeInside), 2), "h\n")
cat("  IQR                     :", round(IQR(visit_stats$timeInside), 2), "h\n")
cat("  range                   :", round(min(visit_stats$timeInside), 2), "-",
    round(max(visit_stats$timeInside), 2), "h\n")

cat("\nOvernight use (local time,", local_tz, ")\n")
cat("  overnight visits        :", n_over,
    sprintf("(%.2f%%)", pct_over), "\n")


cat("\nDate range analysed:",
    paste(range(year(visit_stats$entranceTime)), collapse = " - "), "\n")

cat("\nDownstream (PART G)\n")
cat("  seasonality peak        :", as.character(peak_month$month),
    round(peak_month$pooled_visit_rate, 1), "visits / 30 tortoise-days (effort-weighted)\n")
cat("  seasonality low         :", as.character(low_month$month),
    round(low_month$pooled_visit_rate, 1), "visits / 30 tortoise-days\n")
cat("  tortoises using >1 pond :", nrow(multi_pond), "\n")
# how unevenly pond use is spread across individuals and ponds
visits_per_tortoise <- visits |> count(id, name = "n") |> arrange(desc(n))
visits_per_pond     <- visits |> count(pond, name = "n") |> arrange(desc(n))
top_combo           <- visits |> count(id, pond, name = "n") |> slice_max(n, n = 1)

cat("\nIntraspecific variation in pond use\n")
cat("  visits per tortoise     : median", median(visits_per_tortoise$n),
    ", range", min(visits_per_tortoise$n), "-", max(visits_per_tortoise$n), "\n")
cat("  busiest tortoise        :", as.character(visits_per_tortoise$id[1]), "-",
    visits_per_tortoise$n[1], "visits (",
    round(100 * visits_per_tortoise$n[1] / nrow(visits), 1), "% of all )\n")
cat("  busiest pond            :", as.character(visits_per_pond$pond[1]), "-",
    visits_per_pond$n[1], "visits (",
    round(100 * visits_per_pond$n[1] / nrow(visits), 1), "% of all ), by",
    n_distinct(visits$id[visits$pond == visits_per_pond$pond[1]]), "tortoises\n")
cat("  busiest tortoise x pond :", as.character(top_combo$id), "at",
    as.character(top_combo$pond), "-", top_combo$n, "visits (",
    round(100 * top_combo$n / nrow(visits), 1), "% of all )\n")

# Raw visit counts confound behaviour with tracking effort, so the same
# comparison is repeated per 30 tracked days. effort_by_tortoise comes from
# F6 above and counts days with at least one fix.
visit_effort <- effort_by_tortoise |>
  left_join(visits_per_tortoise, by = c("name" = "id")) |>
  mutate(visits = replace_na(n, 0L),
         visits_per_30d = 30 * visits / tracked_days) |>
  select(name, tracked_days, visits, visits_per_30d) |>
  arrange(desc(visits_per_30d))

write_csv(visit_effort, file.path(outdir, "visits_per_tortoise_effort.csv"))

drop_top <- visit_effort |> filter(name != visit_effort$name[which.max(visit_effort$visits)])

cat("  visits per 30 tracked days: median",
    round(median(visit_effort$visits_per_30d), 2), ", range",
    round(min(visit_effort$visits_per_30d), 2), "-",
    round(max(visit_effort$visits_per_30d), 2), "\n")
cat("  same, excluding the busiest tortoise: median",
    round(median(drop_top$visits_per_30d), 2), ", range",
    round(min(drop_top$visits_per_30d), 2), "-",
    round(max(drop_top$visits_per_30d), 2), "\n")

cat("  pond transitions        :", nrow(transitions), "\n")
cat("  transition gap range    :", round(min(transitions$gap_hours), 2), "h to",
    round(max(transitions$gap_hours) / (24 * 365.25), 2), "years\n")
cat("  network links           :", nrow(edges), "\n")

cat("\nSediment export (PART F6)\n")
cat("  tortoises (>=150 d)     :", nrow(export_base), "\n")
cat("  tortoise-years          :", round(sum(export_base$tracking_years), 1), "\n")
cat("  departure days          :", sum(export_base$departure_days), "\n")
cat("  departures / tort-year  :", round(sediment_export$departures_per_tortoise_year, 1), "\n")
cat("  kg / tortoise / year    :", round(sediment_export$kg_per_tortoise_year, 2),
    sprintf("(95%% CI %.2f-%.2f)", sediment_export$kg_lower_95, sediment_export$kg_upper_95), "\n")
for (i in seq_len(nrow(populations)))
  cat(sprintf("  %-22s: %.2f t/yr (95%% CI %.2f-%.2f) for %d adults\n",
      populations$species[i], populations$tonnes[i],
      populations$tonnes_lower[i], populations$tonnes_upper[i], populations$adults[i]))

cat("\nEverything written to", outdir, "\n\n")
