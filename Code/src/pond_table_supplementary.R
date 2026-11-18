# ===========================================================================
#  pond_table_supplementary.R
#
#  Galapagos giant tortoises as zoogeochemical engineers
#  Ellis-Soto et al. - Ecology Letters
#
#  Supplementary Table 1: one row per pond, giving sampling effort, location,
#  tortoise abundance and the physicochemical and nutrient measurements as
#  mean +/- SD (n).
#
#  Supplementary Table 2: every association between tortoise abundance and
#  pond chemistry in one place. The models behind it are fitted in
#  Code/src/pond_chemistry_pca.R, which writes its results to CSV; this
#  script only formats them, so the two can never disagree. Run that script
#  first - this one stops with a clear message if its output is missing.
#
#  Run from the project root:
#    /Users/diegoellis/Desktop/Projects/Postdoc/Galapagos_conveyer_belt
#
#  The other three scripts of the paper:
#    Code/src/pond_revisitation.R    telemetry, pond use, sediment export
#    Code/src/pond_mud_camera.R      sediment loads and the Montemar camera
#    Code/src/pond_chemistry_pca.R   PCA and the nutrient models
# ===========================================================================


library(tidyverse)
library(janitor)
library(lubridate)
library(terra)

outdir <- "Outdir/pond_table_supplementary"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

num <- function(x) suppressWarnings(parse_number(as.character(x)))


# ---------------------------------------------------------------------------
# 1. Pond data
#
# event_id is assigned before any join, so each original row keeps its
# identity as one sampling event however the joins behave afterwards.
# ---------------------------------------------------------------------------

pond_raw <- read_csv("Data/pond_2025_post_fieldwork_clean_2026_deep_dive.csv",
                     show_col_types = FALSE) |>
  clean_names() |>
  mutate(event_id = row_number())

# The same record dropped everywhere else in the paper: P_Laguna_Ch,
# 21-Jun-18, 10:21. Its conductivity is 1,918 uS/cm against 61-157 at every
# other visit to that pond and a second record at identical coordinates was
# already removed by the field team as a different pond.
pond_raw <- pond_raw |>
  filter(!(gps_pond_name == "P_Laguna_Ch" &
           new_date == "21-Jun-18" &
           !is.na(time) & time == "10:21"))

chl_raw <- read_csv("Data/chlorophyl_data_clean_deep_clean.csv",
                    show_col_types = FALSE) |>
  clean_names()

dat <- pond_raw |>
  left_join(chl_raw, by = c("new_date", "date", "time",
                            "pond_id", "gps_pond_name", "notes"))

# A pond event must not be duplicated by the chlorophyll join.
duplicated_events <- dat |> count(event_id) |> filter(n > 1)
if (nrow(duplicated_events) > 0) {
  print(duplicated_events)
  stop("the chlorophyll join duplicated ", nrow(duplicated_events),
       " pond events; check the join keys")
}


# ---------------------------------------------------------------------------
# 2. Clean variables
# ---------------------------------------------------------------------------

dat <- dat |>
  mutate(
    sample_date = as.Date(parse_date_time(
      new_date, orders = c("d-b-y", "d-B-y", "dmy", "ymd", "mdy"), quiet = TRUE)),
    pond      = as.character(gps_pond_name),
    # Managed = private agricultural or touristic property, maintained or
    # modified by people; natural = inside the Galapagos National Park.
    # Same wording as the manuscript and Figs. 1, 3 and 5.
    pond_type = case_when(tourist_lagoon == "Yes" ~ "Managed",
                          tourist_lagoon == "No"  ~ "Natural"),
    tortoises        = num(number_of_tortoises),
    chlorophyll_a    = num(chl_ugl),
    temperature      = num(ysi_temperature),
    conductivity     = coalesce(num(ysi_us_ccm), num(ysi_m_s_ccm) * 1000),
    dissolved_oxygen = num(do_mg_l),
    tn   = num(tn_mg_l),
    tp   = num(tp_ug_l),
    npoc = num(npoc_mg_l),
    no3  = num(no3_ug_l),
    nh4  = num(nh4_ug_l),
    po4  = num(po4_ug_l),
    longitude_clean = num(longitude),
    latitude_clean  = num(latitude),
    elevation_clean = num(elevation)
  ) |>
  # negative concentrations are below-detection artefacts, not measurements
  mutate(across(c(tn, tp, npoc, no3, nh4, po4),
                ~ if_else(.x < 0, NA_real_, .x)))

# Elevation is taken from the SRTM digital elevation model at the pond
# coordinates, which is the 138-424 m range quoted in the Study Site. The
# handheld GPS readings (elevation_clean, 149-424 m) are kept in the data for
# the PCA but are not reported in this table, so there is only one elevation
# for a reader to check.
srtm <- rast("Data/SRTM_Santa_Cruz.tif")

dat$elevation_srtm <- NA_real_
has_coords <- is.finite(dat$longitude_clean) & is.finite(dat$latitude_clean)

pond_points <- vect(data.frame(lon = dat$longitude_clean[has_coords],
                               lat = dat$latitude_clean[has_coords]),
                    geom = c("lon", "lat"), crs = "EPSG:4326")

dat$elevation_srtm[has_coords] <-
  terra::extract(srtm, project(pond_points, crs(srtm)))[, 2]


# ---------------------------------------------------------------------------
# 3. Three formatters used by the table
# ---------------------------------------------------------------------------

# first non-empty label for a pond (site, pond type)
first_value <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & str_trim(x) != ""]
  if (length(x) == 0) NA_character_ else x[1]
}

# median of the finite values (coordinates, elevation)
median_value <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) == 0) NA_real_ else median(x)
}

# "mean +/- SD (n)", or "value (n=1)" when a pond was measured once.
#
# The separator is plain ASCII "+/-" on purpose. readr::write_csv silently
# mangles non-ASCII characters when the R session locale is C, turning a
# plus-minus sign into the literal text "<c2><b1>" and dropping bytes around
# it. The HTML version written at the end of this script substitutes a real
# plus-minus sign, where it is safe.
mean_sd_n <- function(x, digits = 2) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  n <- length(x)
  if (n == 0) return(NA_character_)
  if (n == 1) return(paste0(formatC(x, format = "f", digits = digits), " (n=1)"))
  paste0(formatC(mean(x), format = "f", digits = digits), " +/- ",
         formatC(sd(x),   format = "f", digits = digits), " (n=", n, ")")
}


# ---------------------------------------------------------------------------
# 4. One row per pond
# ---------------------------------------------------------------------------

pond_table <- dat |>
  filter(!is.na(pond), pond != "") |>
  group_by(pond) |>
  summarise(
    Site              = first_value(site),
    `Pond type`       = first_value(pond_type),
    Longitude         = median_value(longitude_clean),
    Latitude          = median_value(latitude_clean),
    `Elevation (m)`   = median_value(elevation_srtm),
    `Sampling events` = n_distinct(event_id),
    # kept only for the integrity check below, dropped before the table is
    # written so the reader sees one measure of effort rather than three
    sampling_dates    = n_distinct(sample_date, na.rm = TRUE),
    `Tortoise abundance`      = mean_sd_n(tortoises, 2),
    `Temperature (C)`         = mean_sd_n(temperature, 2),
    `Conductivity (uS/cm)`    = mean_sd_n(conductivity, 1),
    `Dissolved oxygen (mg/L)` = mean_sd_n(dissolved_oxygen, 2),
    `Total nitrogen (mg/L)`   = mean_sd_n(tn, 2),
    `Total phosphorus (ug/L)` = mean_sd_n(tp, 1),
    `NPOC (mg/L)`             = mean_sd_n(npoc, 2),
    `Nitrate (ug/L)`          = mean_sd_n(no3, 1),
    `Ammonium (ug/L)`         = mean_sd_n(nh4, 1),
    `Orthophosphate (ug/L)`   = mean_sd_n(po4, 1),
    `Chlorophyll-a (ug/L)`    = mean_sd_n(chlorophyll_a, 2),
    .groups = "drop") |>
  rename(Pond = pond) |>
  mutate(Longitude = round(Longitude, 5),
         Latitude  = round(Latitude, 5),
         `Elevation (m)` = round(`Elevation (m)`, 1)) |>
  arrange(Site, Pond)


# ---------------------------------------------------------------------------
# 5. Checks
# ---------------------------------------------------------------------------

# a pond cannot have been visited on more distinct dates than it has events
bad_counts <- pond_table |> filter(sampling_dates > `Sampling events`)
if (nrow(bad_counts) > 0) {
  print(bad_counts)
  stop("sampling dates exceed sampling events for at least one pond")
}

pond_table <- pond_table |> select(-sampling_dates)

if (nrow(pond_table) != 21) {
  stop("expected 21 ponds, got ", nrow(pond_table))
}

write_csv(pond_table, file.path(outdir, "Suppl_Table_1_ponds.csv"))


# ---------------------------------------------------------------------------
# 6. The same table as HTML, to paste into a manuscript document
#
# Open the file in a browser, select the table and copy: Word and Google Docs
# both keep the cell structure. Written with writeLines, which handles UTF-8
# correctly whatever the session locale.
# ---------------------------------------------------------------------------

header_labels <- names(pond_table) |>
  str_replace(fixed("(C)"),     "(\u00b0C)") |>
  str_replace(fixed("(uS/cm)"), "(\u00b5S/cm)") |>
  str_replace(fixed("(ug/L)"),  "(\u00b5g/L)")

# Cells carry two pieces of information: the summary (mean +/- SD) and the
# number of sampling events behind it. Putting both on one line forces every
# chemistry column wide enough for "1234.56 +/- 789.01 (n=13)", which is what
# made the table unreadable. The count moves to a second line in smaller grey
# type, so each column only has to be as wide as the summary itself and the
# table grows downwards instead of sideways.
cell <- function(x, tag = "td", align = "left") {
  x <- ifelse(is.na(x) | x == "", "\u2014", as.character(x))
  x <- str_replace_all(x, fixed(" +/- "), " \u00b1 ")
  x <- str_replace(x, " ?\\(n=([0-9]+)\\)$",
                   "<br><span style=\"font-size:8pt;color:#666;\">n = \\1</span>")
  paste0("<", tag, " style=\"border:1px solid #999;padding:5px 8px;",
         "text-align:", align, ";white-space:nowrap;vertical-align:top;\">",
         x, "</", tag, ">")
}

# right-align the coordinate, elevation and count columns
align_right <- names(pond_table) %in%
  c("Longitude", "Latitude", "Elevation (m)", "Sampling events")
aligns <- ifelse(align_right, "right", "left")

html_rows <- apply(pond_table, 1, function(row)
  paste0("<tr>", paste(cell(row, "td", aligns), collapse = ""), "</tr>"))

html_table <- c(
  "<meta charset=\"utf-8\">",
  "<div style=\"font-family:Arial,Helvetica,sans-serif;padding:16px;\">",
  paste0("<p style=\"font-size:10pt;max-width:1100px;\"><b>Supplementary Table 1:</b> ",
         "Sampling effort, tortoise abundance and physicochemical characteristics of the ",
         nrow(pond_table), " freshwater ponds studied on Santa Cruz Island, 2018\u20132025. ",
         "Elevation is taken from the SRTM digital elevation model at each pond's coordinates. ",
         "Tortoise abundance and water chemistry are given as mean \u00b1 SD (<i>n</i>), where ",
         "<i>n</i> is the number of sampling events with that measurement; a single measurement ",
         "is shown as the value with (<i>n</i>=1). Em dashes mark variables never measured at ",
         "that pond.</p>"),
  "<table style=\"border-collapse:collapse;font-family:Arial,Helvetica,sans-serif;font-size:9pt;\">",
  paste0("<thead><tr style=\"background:#f0f0f0;\">",
         paste(cell(header_labels, "th", aligns), collapse = ""), "</tr></thead>"),
  "<tbody>", html_rows, "</tbody></table></div>")

writeLines(html_table, file.path(outdir, "Suppl_Table_1_ponds.html"), useBytes = TRUE)


# ---------------------------------------------------------------------------
# 7. Supplementary Table 2: tortoise abundance and pond chemistry
#
# Reads the model results written by Code/src/pond_chemistry_pca.R. The TSS
# pond-type contrast is a footnote rather than a row, because it is a
# comparison between pond types, not a response ratio per doubling of
# tortoise abundance, and would be misread in that column.
# ---------------------------------------------------------------------------

chem_dir <- "Outdir/pond_chemistry_pca"
needed   <- c("response_suite_event_level.csv", "within_between_pond.csv",
              "tss_pond_type_contrast.csv")

if (!all(file.exists(file.path(chem_dir, needed)))) {
  stop("Supplementary Table 2 needs the model results from ",
       "Code/src/pond_chemistry_pca.R. Run that script first.")
}

response_suite <- read_csv(file.path(chem_dir, needed[1]), show_col_types = FALSE)
within_between <- read_csv(file.path(chem_dir, needed[2]), show_col_types = FALSE)
tss_pond_type  <- read_csv(file.path(chem_dir, needed[3]), show_col_types = FALSE)

fmt   <- function(rr, lo, hi) sprintf("%.2f (%.2f-%.2f)", rr, lo, hi)
fmt_p <- function(p) ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))

partitioned <- within_between |>
  transmute(response, component,
            estimate = fmt(response_ratio, lower_95, upper_95),
            p = fmt_p(p_value)) |>
  pivot_wider(names_from = component, values_from = c(estimate, p))

response_order <- c("Ammonium", "Total nitrogen", "Nitrate", "Total phosphorus",
                    "Orthophosphate", "NPOC", "TSS", "Chlorophyll-a")

suppl_table_2 <- response_suite |>
  transmute(Response = response,
            `Sampling events` = n_events,
            Ponds = n_ponds,
            `Response ratio (95% CI)` = fmt(response_ratio, lower_95, upper_95),
            P = fmt_p(p_value)) |>
  left_join(partitioned, by = c("Response" = "response")) |>
  # "not applicable" is a plain hyphen so the CSV stays ASCII; the HTML below
  # turns it into an em dash
  transmute(Response, `Sampling events`, Ponds, `Response ratio (95% CI)`, P,
            `Between ponds (95% CI)` = coalesce(`estimate_Between ponds`, "-"),
            `P ` = coalesce(`p_Between ponds`, "-"),
            `Within ponds (95% CI)` = coalesce(`estimate_Within ponds`, "-"),
            `P  ` = coalesce(`p_Within ponds`, "-")) |>
  arrange(match(Response, response_order))

write_csv(suppl_table_2, file.path(outdir, "Suppl_Table_2_response_ratios.csv"))

s2_cell <- function(x, tag = "td", align = "left") {
  x <- ifelse(x == "-", "\u2014", x)
  paste0("<", tag, " style=\"border:1px solid #999;padding:4px 7px;text-align:",
         align, ";white-space:nowrap;\">", x, "</", tag, ">")
}

s2_align <- ifelse(names(suppl_table_2) == "Response", "left", "right")

writeLines(c(
  "<meta charset=\"utf-8\">",
  "<div style=\"font-family:Arial,Helvetica,sans-serif;padding:16px;\">",
  paste0("<p style=\"font-size:10pt;max-width:1000px;\"><b>Supplementary Table 2:</b> ",
         "Associations between tortoise abundance and pond water chemistry. Response ratios ",
         "give the proportional change in concentration per doubling of 1 + tortoise abundance, ",
         "estimated from Gamma generalized linear mixed-effects models with pond type and season ",
         "as fixed effects and pond identity as a random intercept. Between- and within-pond ",
         "columns partition tortoise abundance into each pond's mean and the deviation of each ",
         "sampling event from that mean. Total suspended solids were additionally ",
         sprintf("%.2f", tss_pond_type$fold), "-fold higher in managed than natural ponds ",
         sprintf("(95%% CI = %.2f-%.2f, P = %.3f)", tss_pond_type$lower_95,
                 tss_pond_type$upper_95, tss_pond_type$p_value),
         ", a pond-type contrast rather than an association with tortoise abundance.</p>"),
  "<table style=\"border-collapse:collapse;font-family:Arial,Helvetica,sans-serif;font-size:9pt;\">",
  paste0("<thead><tr style=\"background:#f0f0f0;\">",
         paste(s2_cell(str_trim(names(suppl_table_2)), "th", s2_align), collapse = ""),
         "</tr></thead><tbody>"),
  apply(suppl_table_2, 1, function(r)
    paste0("<tr>", paste(s2_cell(r, "td", s2_align), collapse = ""), "</tr>")),
  "</tbody></table></div>"),
  file.path(outdir, "Suppl_Table_2_response_ratios.html"), useBytes = TRUE)


# ---------------------------------------------------------------------------
# 8. Numbers reported in the manuscript
# ---------------------------------------------------------------------------

cat("\n--- Supplementary Table 1 ------------------------------------------\n")
cat("ponds                                 :", nrow(pond_table), "\n")
cat("sampling events                       :", sum(pond_table$`Sampling events`), "\n")
cat("elevation, SRTM (m)                   :",
    round(min(pond_table$`Elevation (m)`, na.rm = TRUE)), "-",
    round(max(pond_table$`Elevation (m)`, na.rm = TRUE)),
    " <- the range quoted in the Study Site\n")
cat("ponds by type                          :\n")
print(table(pond_table$`Pond type`))
cat("\nponds with 5 or more sampling events   :",
    sum(pond_table$`Sampling events` >= 5), "\n")

print(as.data.frame(pond_table[, 1:7]), row.names = FALSE)

cat("\n--- Supplementary Table 2 ---\n")
print(as.data.frame(suppl_table_2), row.names = FALSE)

cat("\nTables written to", outdir, "as CSV and as HTML\n")
