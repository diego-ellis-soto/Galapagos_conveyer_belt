# ============================================================
# Galapagos giant tortoises: pond use and pond revisitation
#
# One script, run top to bottom.
#
#   PART A  which tortoises and which ponds intersect at all
#   PART B  revisitation with recurse
#   PART C  figures and interactive maps
#   PART D  overnight stays
#
# ------------------------------------------------------------
# HOW A POND IS DEFINED HERE
#
# Ponds are not fixed shapes. They shrink and grow between years
# with rainfall, so there is no single correct outline. What we do
# have is where we actually stood at each pond, on every sampling
# trip from 2018 onwards, dropping the YSI and taking water
# samples. Those positions sit around the pond edge, and across
# years they trace out the envelope of where the pond has been.
#
# So a pond is: everything within 5 m of any of its sampled
# locations. The two exceptions are P_CM and P_Laguna_Ch, which
# are far too big for that and were drawn by hand in
# 02_buffer_ponds.R; for those we use the drawn polygon.
#
# ------------------------------------------------------------
# WHAT CHANGED FROM THE PUBLISHED VERSION
#
# The published analysis called
#
#   getRecursionsAtLocations(tortoise_df, pond_coords,
#                            radius = 0.00004486)
#
# then collapsed records sharing tortoise, pond, entrance time and
# exit time. Two things about that:
#
# 1. THE COLLAPSE WORKS, and is not the problem. 7,801 raw
#    detections -> 7,380 after the 0.1 h filter -> 2,243 unique
#    visits, which is exactly the published figure. Duplicate
#    overlapping buffers are not inflating anything.
#
#    But it only removes EXACT duplicates. When a tortoise moves
#    between two sampled locations at the same pond, it leaves one
#    disc and enters another, with different crossing times, so
#    that survives the collapse as two visits. It is one arrival at
#    one pond. Merging overlapping time intervals instead of
#    matching exact times counts visits to the pond rather than
#    visits to individual discs. Total residence time is almost
#    unchanged; the visit count drops by about 15%.
#
# 2. THE RADIUS IS THE REAL PROBLEM. A single scalar radius is
#    applied to every location, so the two hand-drawn ponds were
#    reduced to a 5 m circle at their centroid - 1/176 of
#    P_Laguna_Ch and 1/90 of P_CM - sitting in open water where
#    tortoises do not generate fixes. P_Laguna_Ch returned 0 visits
#    and is missing from the paper's 10 ponds. Its own outline
#    returns 3 visits over 17.3 h.
#
# 3. Degrees, not metres. The vignette: "A geographic projection
#    (i.e., latitude/longitude) is not recommended." The old script
#    built an equal-area projection (myprj) and then never passed
#    it to recurse. Everything here is in metres.
#
# ------------------------------------------------------------
# INPUTS
#   Outdir/buffered_ponds_deep_clean.shp   (built by 02_buffer_ponds.R:
#                                           sampled locations buffered
#                                           by 5 m, plus the two drawn
#                                           polygons, also buffered 5 m)
#   Movebank study 2928116 (cached to Data/ after the first run)
#
# OUTPUTS  all in Outdir/recurse_polygon/
#
# RUNTIME  about 6 minutes the first time, then seconds via the cache.
# ============================================================


# ------------------------------------------------------------
# 0. Packages and settings
# ------------------------------------------------------------

library(move)
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

setwd("/Users/diegoellis/Desktop/Projects/Postdoc/Galapagos_conveyer_belt")

study_id      <- 2928116
years_to_keep <- 2018:2025
pond_file     <- "Outdir/buffered_ponds_deep_clean.shp"

# The ponds that were drawn by hand rather than represented by sampled
# locations. Taken from the shapefile itself: a sampled location is a
# 5 m disc of about 79 m2, so anything much larger is a drawn outline.
#
# NOTE the field size class disagrees here. In the fieldwork sheet,
# THREE ponds are recorded as "large" in every visit: P_CM,
# P_Laguna_Ch and P_Manzan_1. Only the first two were ever drawn, so
# P_Manzan_1 is a large pond still represented by the union of its
# 5 m discs (157 m2). Worth drawing a polygon for it too - see the
# size_class column in the output table.
drawn_ponds <- NULL   # filled in below, once the shapefile is read

# Buffer around each sampled location, in metres. This is the same
# 5 m used in 02_buffer_ponds.R and in the paper.
buffer_m <- 5

# The paper excludes visits of 0.1 h or less as brief pass-by events,
# and reports times in Galapagos local time. Keep both so every number
# here is directly comparable to the manuscript.
min_visit_hours <- 0.1
local_tz        <- "Pacific/Galapagos"

outdir <- "Outdir/recurse_polygon"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Movebank cache.
#
# Data/tortugas_pond_users_2018_2024.Rdata is the SUBSET the published
# analysis was built from: the 19 pond-using tortoises, 2018-2024 only.
# Running on it reproduces the paper exactly (2,243 visits, 18
# tortoises, 10 ponds). It is NOT the whole study and has no 2025 data.
#
# To include 2025, delete that file. The script then downloads the full
# study from Movebank and caches it under the second name.
cache_file <- if (file.exists("Data/tortugas_pond_users_2018_2024.Rdata")) {
  "Data/tortugas_pond_users_2018_2024.Rdata"
} else {
  "Data/tortugas_all_movebank.Rdata"
}


# ============================================================
# PART A - which tortoises use which ponds
# ============================================================

# ------------------------------------------------------------
# A1. Movebank data
#
# Download EVERY animal, not a pre-selected list. The old setup was
# circular: recurse_tortoises.R read the list of pond-using tortoises
# from a CSV that a second script had to produce first. Here we get
# everything once and work out who uses ponds below.
# ------------------------------------------------------------

if (file.exists(cache_file)) {

  message("loading cached Movebank data: ", cache_file)
  load(cache_file)          # restores `tortugas`

} else {

  loginStored <- movebankLogin(username = "Diego_Ellis")

  tortugas <- getMovebankData(
    study                      = study_id,
    login                      = loginStored,
    includeExtraSensors        = FALSE,
    removeDuplicatedTimestamps = TRUE,
    deploymentAsIndividuals    = FALSE
  )

  save(tortugas, file = cache_file)
}

tortugas$name <- tortugas@trackId
tortugas_df   <- data.frame(tortugas)


# ------------------------------------------------------------
# A2. Clean the GPS fixes and make them spatial
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

message("GPS fixes retained: ", nrow(tortoise_sf))
message("tortoises retained: ", n_distinct(tortoise_sf$name))
message("years present:      ", paste(range(tortoise_sf$year), collapse = " - "))


# ------------------------------------------------------------
# A3. Pond zones
#
# The shapefile holds one feature per sampled location (already
# buffered by 5 m) plus one drawn polygon each for P_CM and
# P_Laguna_Ch. Dissolving by name gives the pond zone: the area
# within 5 m of any sampled location.
#
# P_CM has both a drawn polygon and 9 sampled locations. The
# sampled locations sit inside the drawn polygon, so the union is
# just the drawn polygon and nothing is lost.
# ------------------------------------------------------------

ponds_raw <- st_read(pond_file, quiet = TRUE) |>
  st_make_valid() |>
  mutate(pond = GPS_Pn_) |>
  select(pond, geometry)

# Which ponds were drawn by hand, read off the shapefile rather than
# hardcoded, so that drawing another one in 02_buffer_ponds.R is picked
# up here automatically with no edit to this script.
#
# A sampled location buffered by 5 m is exactly pi * 5^2 = 78.5 m2.
# Anything that is not that size is a hand-drawn outline.
disc_m2 <- pi * buffer_m^2
ponds_raw$feature_m2 <- as.numeric(st_area(st_transform(ponds_raw, 32715)))
ponds_raw$is_drawn   <- abs(ponds_raw$feature_m2 - disc_m2) > 1

drawn_ponds <- sort(unique(ponds_raw$pond[ponds_raw$is_drawn]))
message("hand-drawn outlines: ", paste(drawn_ponds, collapse = ", "))

ponds <- ponds_raw |>
  group_by(pond) |>
  summarise(geometry = st_union(geometry), .groups = "drop") |>
  st_make_valid()

message("ponds: ", nrow(ponds), " zones from ", nrow(ponds_raw), " features")


# Field size class, for QC only - nothing downstream depends on it.
pond_size_class <- read_csv(
  "/Users/diegoellis/Desktop/Projects/Postdoc/Pond_2025/Pond_2025_fieldwork_done/pond_2025_post_fieldwork_clean_deep_clean.csv",
  show_col_types = FALSE, locale = locale(encoding = "Latin1")) |>
  filter(!is.na(GPS_Pond_name), !is.na(Size)) |>
  group_by(pond = GPS_Pond_name) |>
  summarise(size_class = names(sort(table(Size), decreasing = TRUE))[1],
            .groups = "drop")

# QC. A pond the field sheet calls "large" should have a drawn outline,
# otherwise it is a big pond being represented by a handful of 5 m
# discs. Draw it in 02_buffer_ponds.R and rerun.
undrawn_large <- setdiff(pond_size_class$pond[pond_size_class$size_class == "large"],
                         drawn_ponds)
if (length(undrawn_large) > 0) {
  warning("large pond(s) with no hand-drawn outline: ",
          paste(undrawn_large, collapse = ", "), call. = FALSE)
} else {
  message("all large ponds have a hand-drawn outline")
}


# ------------------------------------------------------------
# A4. Which GPS fixes fall inside a pond
# ------------------------------------------------------------

tortoise_pond_hits_sf <- st_join(tortoise_sf, ponds,
                                 join = st_intersects, left = FALSE)

tortoise_pond_hits <- tortoise_pond_hits_sf |>
  st_drop_geometry() |>
  arrange(name, timestamp, pond)

message("GPS fixes inside ponds: ", nrow(tortoise_pond_hits))


# ------------------------------------------------------------
# A5. Summary per pond, including ponds with no detections
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
# A6. Per tortoise x pond, and the list of pond users
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
# A7. Full tracks for the pond-using tortoises
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
# PART B - revisitation
# ============================================================

# ------------------------------------------------------------
# B1. Project to equal-area metres
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
# B2. Recursions
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
# the paper, but it also catches a tortoise walking from one disc to
# the next, which an exact entrance/exit match does not.
#
# The two drawn ponds are handled separately with
# getRecursionsInPolygon. That function needs a convex single
# polygon: P_CM is convex already and P_Laguna_Ch is 97.9% of its
# convex hull, so the hull is an accurate stand-in for both.
# ------------------------------------------------------------

recurse_cache <- file.path(outdir, "recurse_results.rds")
recompute     <- FALSE

if (!recompute && file.exists(recurse_cache)) {

  message("loading cached recurse results: ", recurse_cache)
  cached      <- readRDS(recurse_cache)
  visit_discs <- cached$visit_discs

} else {

visit_discs <- data.frame()

# --- the 19 ponds represented by sampled locations ---------------

disc_feats <- ponds_raw_m |> filter(!pond %in% drawn_ponds)
disc_xy    <- st_coordinates(st_centroid(st_geometry(disc_feats)))

message("recurse: ", nrow(disc_feats), " sampled locations, radius ",
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

# --- the two drawn ponds -----------------------------------------

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

saveRDS(list(visit_discs = visit_discs), recurse_cache)
}

message("raw disc-level detections: ", nrow(visit_discs))


# ------------------------------------------------------------
# B3. Merge disc visits into pond visits
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

# the paper's filter: drop brief pass-by events
visit_stats <- filter(visit_stats, timeInside > min_visit_hours)

visit_stats <- visit_stats |>
  arrange(pond, id, entranceTime) |>
  group_by(pond, id) |>
  mutate(timeSinceLastVisit =
           as.numeric(difftime(entranceTime, lag(exitTime), units = "hours"))) |>
  ungroup()

message("after the ", min_visit_hours, " h filter: ", nrow(visit_stats), " visits")


# ------------------------------------------------------------
# B4. Reproduce the published numbers, for comparison
#
# Same disc detections, but collapsed the way the paper does it:
# exact match on tortoise, pond, entrance and exit time.
# ------------------------------------------------------------

published <- visit_discs |>
  filter(as.numeric(timeInside) > min_visit_hours) |>
  distinct(id, pond, entranceTime, exitTime, .keep_all = TRUE)

published_by_pond <- published |>
  group_by(pond) |>
  summarise(published_visits = n(),
            published_hours  = sum(as.numeric(timeInside)),
            .groups = "drop")

message("published-style collapse: ", nrow(published), " visits")


# ------------------------------------------------------------
# B5. Pond table
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
  left_join(pond_totals,       by = "pond") |>
  left_join(published_by_pond, by = "pond") |>
  mutate(across(c(visits, hours, n_tortoises,
                  published_visits, published_hours), ~ coalesce(.x, 0))) |>
  mutate(difference = visits - published_visits) |>
  arrange(desc(area_m2))

print(as.data.frame(pond_recurse), row.names = FALSE)

message("pond zones: ", sum(pond_recurse$visits), " visits   |   ",
        "published style: ", sum(pond_recurse$published_visits), " visits")

write_csv(pond_recurse, file.path(outdir, "pond_recursions_polygon.csv"))
write_csv(visit_stats,  file.path(outdir, "pond_visit_stats.csv"))


# PART C - figures and maps
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
# Fig 1. Why one radius does not work: the two large ponds
# ------------------------------------------------------------

big <- c("P_Laguna_Ch", "P_CM")

big_poly   <- ponds_m[ponds_m$pond %in% big, c("pond", "area_m2")]
big_centre <- st_centroid(st_geometry(big_poly))
big_circle <- st_sf(pond = big_poly$pond, geometry = st_buffer(big_centre, buffer_m))

# tortoise fixes within 120 m of either pond, for context
fixes_m  <- st_as_sf(trk, coords = c("x", "y"), crs = myprj)
near_big <- fixes_m[apply(st_intersects(fixes_m, st_buffer(big_poly, 120),
                                        sparse = FALSE), 1, any), ]

# One panel per pond, then stacked side by side. Not a facet_wrap,
# because the two ponds are far apart so each panel needs its own
# extent, and ggplot refuses free scales together with coord_sf.
panels <- list()
for (i in 1:nrow(big_poly)) {

  this_pond <- big_poly$pond[i]
  n_visits  <- pond_recurse$visits[pond_recurse$pond == this_pond]

  panels[[i]] <- ggplot() +
    geom_sf(data = big_poly[i, ], fill = "#2c7fb8", colour = "#08519c", alpha = 0.35) +
    geom_sf(data = near_big,      size = 0.5, colour = "grey30", alpha = 0.5) +
    geom_sf(data = big_circle[i, ], fill = "#e31a1c", colour = "#a50f15", alpha = 0.9) +
    coord_sf(xlim = st_bbox(st_buffer(big_poly[i, ], 40))[c("xmin", "xmax")],
             ylim = st_bbox(st_buffer(big_poly[i, ], 40))[c("ymin", "ymax")]) +
    labs(title    = this_pond,
         subtitle = sprintf("%s m2   the 5 m circle covers %.1f%% of it   |   %s visits from the outline, %s from the circle",
                            comma(round(big_poly$area_m2[i])),
                            100 * (pi * buffer_m^2) / big_poly$area_m2[i],
                            n_visits,
                            pond_recurse$old_visits[pond_recurse$pond == this_pond])) +
    theme(axis.text = element_text(size = 6),
          plot.subtitle = element_text(size = 8))
}

fig1 <- panels[[1]] + panels[[2]] +
  plot_annotation(
    title    = "Fix 1: a single radius shrinks the two large ponds to a speck",
    subtitle = "blue = true pond outline   red = the 5 m circle the old script actually analysed   grey = tortoise GPS fixes",
    caption  = "The red circle sits in open water, where tortoises do not generate fixes.",
    theme    = theme(plot.title = element_text(face = "bold"))
  )

ggsave(file.path(outdir, "01_radius_problem.png"), fig1,
       width = 11, height = 5.5, dpi = 200, bg = "white")


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

# same circle repeated once per facet; area_m2 is carried along only
# because the facet expression below sorts on it
circle0 <- st_sf(
  pond     = ponds_m$pond,
  area_m2  = ponds_m$area_m2,
  geometry = st_buffer(st_sfc(st_point(c(0, 0))), buffer_m)[rep(1, nrow(ponds_m))]
)

fig2 <- ggplot() +
  geom_sf(data = centred, fill = "#2c7fb8", colour = "#08519c",
          alpha = 0.4, linewidth = 0.2) +
  geom_sf(data = circle0, fill = "#e31a1c", colour = NA, alpha = 0.85) +
  facet_wrap(~reorder(pond, -area_m2), ncol = 7) +
  coord_sf(xlim = c(-75, 75), ylim = c(-75, 75)) +
  labs(title    = "All 21 ponds at the same scale, red = the 5 m analysis circle",
       subtitle = "Correct size for 19 ponds. Not for P_Laguna_Ch and P_CM.") +
  theme(axis.text = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank())

ggsave(file.path(outdir, "02_pond_scale_grid.png"), fig2,
       width = 12, height = 6, dpi = 200, bg = "white")


# ------------------------------------------------------------
# Fig 3. Pond zones vs the published collapse
# ------------------------------------------------------------

steps <- pond_recurse |>
  filter(visits > 0 | published_visits > 0) |>
  select(pond, published_visits, visits) |>
  pivot_longer(-pond, names_to = "step", values_to = "n") |>
  mutate(step = factor(step, levels = c("published_visits", "visits"),
                       labels = c("published: exact entrance/exit match",
                                  "pond zone: merged overlapping discs")))

fig3a <- ggplot(steps, aes(n, reorder(pond, n), colour = step)) +
  geom_line(aes(group = pond), colour = "grey70", linewidth = 1) +
  geom_point(size = 3, alpha = 0.9) +
  scale_x_log10(labels = log_labels, expand = expansion(mult = c(0.08, 0.12))) +
  scale_colour_manual(NULL, values = c("#e31a1c", "#2c7fb8")) +
  labs(title    = "Merging discs counts arrivals at a pond, not disc crossings",
       subtitle = sprintf("totals: %s published  ->  %s pond zones",
                          comma(sum(pond_recurse$published_visits)),
                          comma(sum(pond_recurse$visits))),
       x = "visits (log scale)", y = NULL) +
  theme(legend.position = "top", legend.text = element_text(size = 8))

fig3b <- pond_recurse |>
  filter(visits > 0 | published_visits > 0) |>
  ggplot(aes(difference, reorder(pond, difference), fill = difference > 0)) +
  geom_col() +
  geom_vline(xintercept = 0, colour = "grey40") +
  geom_text(aes(label = ifelse(difference > 0, paste0("+", difference), difference),
                hjust = ifelse(difference > 0, -0.2, 1.2)), size = 3) +
  scale_fill_manual(guide = "none",
                    values = c(`TRUE` = "#2c7fb8", `FALSE` = "#e31a1c")) +
  scale_x_continuous(expand = expansion(mult = 0.15)) +
  labs(title    = "Where the two differ",
       subtitle = "positive = the pond zone finds visits the published method missed",
       x = "pond-zone visits minus published visits", y = NULL)

ggsave(file.path(outdir, "03_method_comparison.png"), fig3a + fig3b,
       width = 14, height = 6, dpi = 200, bg = "white")


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

ggsave(file.path(outdir, "04_pond_use.png"),
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

ggsave(file.path(outdir, "05_tortoise_pond_heatmap.png"), fig5,
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

ggsave(file.path(outdir, "06_visit_structure.png"),
       (fig6a + fig6b) / fig6c + plot_layout(heights = c(1, 1.1)),
       width = 12, height = 8, dpi = 200, bg = "white")


# ============================================================
# PART D - overnight stays
#
# The paper reports 160 of 2,243 visits (7.13%) extending through
# midnight. Same definition here: a visit is overnight if its local
# exit date differs from its local entrance date.
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

# Same calculation on the published-style visits, so the two are
# directly comparable and the paper's 7.13% can be checked.
published_nights <- published |>
  mutate(local_entrance = with_tz(entranceTime, local_tz),
         local_exit     = with_tz(exitTime,     local_tz),
         overnight = as.Date(local_exit,     tz = local_tz) !=
                     as.Date(local_entrance, tz = local_tz))

message("overnight, published method: ",
        sum(published_nights$overnight), " of ", nrow(published_nights),
        " (", round(100 * mean(published_nights$overnight), 2), "%)")
message("overnight, pond zones:        ",
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

ggsave(file.path(outdir, "07_overnight_entry_vs_duration.png"), fig7,
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

ggsave(file.path(outdir, "08_overnight_when_where.png"),
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
                   n_tortoises, published_visits, difference),
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


# Map A - corrected pond use, with the old circles for reference
map_A <-
  mapview(ponds_map, zcol = "visits", layer.name = "Pond (visits)",
          alpha.regions = 0.7, lwd = 2) +
  mapview(circle_map, col.regions = "red", color = "darkred",
          alpha.regions = 0.9, layer.name = "OLD 5 m circle") +
  mapview(hits_map, zcol = "pond", cex = 4, layer.name = "GPS fixes inside ponds")

saveWidget(map_A@map,
           file.path(normalizePath(outdir), "map_A_pond_use.html"),
           selfcontained = FALSE, title = "Pond revisitation")


# Map B - the two large ponds, old circle vs real outline
big_map <- filter(ponds_map, pond %in% big)

fixes_map <- st_transform(near_big, 4326)   # the 120 m selection from Fig 1

map_B <-
  mapview(big_map, col.regions = "#2c7fb8", color = "#08519c",
          alpha.regions = 0.5, layer.name = "TRUE pond outline") +
  mapview(filter(circle_map, pond %in% big), col.regions = "red",
          color = "darkred", alpha.regions = 0.95, layer.name = "OLD 5 m circle") +
  mapview(fixes_map, cex = 2, col.regions = "grey40", layer.name = "Tortoise fixes")

saveWidget(map_B@map,
           file.path(normalizePath(outdir), "map_B_large_ponds.html"),
           selfcontained = FALSE, title = "Large ponds: 5 m circle vs outline")


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
