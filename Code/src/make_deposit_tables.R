# ===========================================================================
#  Build the tidy data tables that accompany the paper
#
#  The field spreadsheets carry 58-87 columns each, most of them lab
#  bookkeeping, logistics or half-empty. Depositing them as they are would
#  publish a lot of material nobody can interpret and a few columns that are
#  actively misleading. This script turns them into five tables a reader can
#  use, plus a dictionary, and says what it dropped and why.
#
#  Writes to Outdir/deposit/. Run from the project root, after
#  pond_chemistry_pca.R (for the model outputs) and pond_revisitation.R
#  (for the visit table).
# ===========================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr); library(janitor)
  library(stringr); library(lubridate)
})

OUT <- "Outdir/deposit"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)

# Field sheets record numbers as text: "10cm", "20cm?", "17, not deepest
# part", "?" and "Dry" all appear. Pull the first number out and count what
# could not be read, rather than letting as.numeric() drop values silently.
num <- function(x, label = "") {
  raw <- trimws(as.character(x))
  raw[raw %in% c("", "?", "NA", "na", "-")] <- NA
  v <- suppressWarnings(as.numeric(str_extract(raw, "-?[0-9]+\\.?[0-9]*")))
  lost <- sum(!is.na(raw) & is.na(v))
  if (lost > 0 && nzchar(label))
    message("  ", label, ": ", lost, " entries not numeric, set to NA")
  v
}

cat("=== reading field sheets ===\n")
pond_raw <- read_csv("Data/pond_2025_post_fieldwork_clean_2026_deep_dive.csv",
                     show_col_types = FALSE, name_repair = "unique") |> clean_names()
chl_raw  <- read_csv("Data/chlorophyl_data_clean_deep_clean.csv",
                     show_col_types = FALSE) |> clean_names()
tss_raw  <- read_csv("Outdir/final/TSS_pond_identity/TSS_RAW_with_clean_pond_identity.csv",
                     show_col_types = FALSE) |> clean_names()
mud_raw  <- read_csv("Data/Mud_2024_2025_fieldwork.csv", show_col_types = FALSE) |> clean_names()
cam_raw  <- read_csv("Data/Photo_spreadsheet_montemar_all.csv", show_col_types = FALSE) |> clean_names()


# ---------------------------------------------------------------------------
#  1. POND METADATA - one row per pond
# ---------------------------------------------------------------------------

pond_meta <- pond_raw |>
  filter(!is.na(gps_pond_name), gps_pond_name != "") |>
  group_by(pond_id = gps_pond_name) |>
  summarise(
    longitude   = median(longitude, na.rm = TRUE),
    latitude    = median(latitude,  na.rm = TRUE),
    elevation_m = round(median(num(elevation), na.rm = TRUE)),
    pond_type   = if (any(tolower(substr(tourist_lagoon, 1, 1)) == "y", na.rm = TRUE))
                    "managed" else "natural",
    # P_CM (Cerro Mesa) is recorded as large on 4 visits and medium on 5.
    # Taking the mode would silently call it medium, yet the paper's own pond
    # delineation treats it as one of the three ponds too large for a 5 m
    # buffer. The disagreement is flagged rather than resolved here.
    size_class  = names(sort(table(size[size != ""]), decreasing = TRUE))[1],
    size_class_all = paste(sort(unique(size[!is.na(size) & size != ""])), collapse = " / "),
    size_class_consistent = n_distinct(size[!is.na(size) & size != ""]) == 1,
    n_sampling_events = n(),
    .groups = "drop") |>
  arrange(pond_id)

write_csv(pond_meta, file.path(OUT, "pond_metadata.csv"))
cat("pond_metadata.csv       ", nrow(pond_meta), "ponds\n")


# ---------------------------------------------------------------------------
#  2. POND CHEMISTRY - one row per sampling event, analysis ready
#
#  Chlorophyll and TSS are joined on pond and date exactly as
#  pond_chemistry_pca.R does, so the deposited table reproduces the analysis
#  rather than merely resembling it. TSS is averaged first: the raw file has
#  repeated pond-by-date rows that would otherwise duplicate events.
# ---------------------------------------------------------------------------

cat("\n=== coercing chemistry ===\n")

tss_event <- tss_raw |>
  transmute(pond_id = gps_pond_name_clean, new_date,
            tss_mg_l = num(tss_mg_l, "TSS")) |>
  filter(!is.na(pond_id), !is.na(tss_mg_l)) |>
  group_by(pond_id, new_date) |>
  summarise(tss_mg_l = mean(tss_mg_l), .groups = "drop")

chl_event <- chl_raw |>
  transmute(pond_id = gps_pond_name, new_date,
            chlorophyll_a_ug_l = num(chl_ugl, "chlorophyll")) |>
  filter(!is.na(pond_id)) |>
  group_by(pond_id, new_date) |>
  summarise(chlorophyll_a_ug_l = mean(chlorophyll_a_ug_l, na.rm = TRUE), .groups = "drop")

chem <- pond_raw |>
  transmute(
    pond_id             = gps_pond_name,
    new_date,
    # new_date is the authoritative date: consistent d-b-y, complete for every
    # row, and the key the analysis joins on. The sheet's other `date` column
    # mixes 13/08/2019, 11/7/19, 3/31/20, 14.11.2019 and a typo (19.08/2019)
    # in one field and cannot be parsed; it is not deposited.
    date                = suppressWarnings(dmy(new_date)),
    time_local          = time,
    temperature_c       = num(ysi_temperature, "temperature"),
    conductivity_us_cm  = num(ysi_us_ccm,      "conductivity"),
    do_mg_l             = num(do_mg_l,         "dissolved oxygen"),
    do_pct              = num(do_percent,      "DO %"),
    tn_mg_l             = num(tn_mg_l),
    tp_ug_l             = num(tp_ug_l),
    npoc_mg_l           = num(npoc_mg_l),
    no3_ug_l            = num(no3_ug_l),
    nh4_ug_l            = num(nh4_ug_l),
    po4_ug_l            = num(po4_ug_l),
    n_tortoises         = num(number_of_tortoises, "tortoise counts"),
    emergent_veg_pct    = num(emergent_vegetation_in_percent, "emergent vegetation")) |>
  left_join(chl_event, by = c("pond_id", "new_date")) |>
  left_join(tss_event, by = c("pond_id", "new_date")) |>
  filter(!is.na(pond_id), pond_id != "")

# The one record excluded from the analysis is flagged, not deleted, so a
# reader can see it and reproduce the sensitivity check. Its conductivity is
# an order of magnitude above every other measurement at that pond, and a
# twin record at identical coordinates was marked in the field as belonging
# to a different water body.
chem <- chem |>
  mutate(excluded_from_analysis = pond_id == "P_Laguna_Ch" &
           new_date == "21-Jun-18" &
           str_detect(replace_na(time_local, ""), "^10:21"),
         exclusion_reason = if_else(excluded_from_analysis,
           "conductivity inconsistent with pond; field note indicates different water body", NA_character_))

write_csv(chem, file.path(OUT, "pond_chemistry.csv"))
cat("pond_chemistry.csv      ", nrow(chem), "sampling events,",
    sum(chem$excluded_from_analysis), "flagged as excluded\n")


# ---------------------------------------------------------------------------
#  3. SEDIMENT ON CARAPACES
#
#  Sediment was scraped from the anterior carapace only in 2018-2020 and from
#  both portions from 2021, so pre-2021 loads are doubled. We derive that here
#  from the raw weight and the year. The spreadsheet's own corrected column is
#  NOT deposited: it doubles 2019 alone, and one 2020 record (P_Manzan_2,
#  24 July 2020) holds 112 g against a raw 895 g, filled down from the row
#  above.
# ---------------------------------------------------------------------------

mud <- mud_raw |>
  transmute(
    pond_id             = gps_pond_name,
    date                = suppressWarnings(dmy(date)),
    year,
    sex                 = na_if(trimws(sex), ""),
    carapace_length_cm  = num(carapace_length, "carapace length"),
    mud_weight_raw_g    = num(mud_weight_g, "mud weight"),
    scrape_coverage     = if_else(year < 2021, "anterior only", "anterior and posterior"),
    mud_weight_g        = if_else(year < 2021, mud_weight_raw_g * 2, mud_weight_raw_g)) |>
  filter(!is.na(pond_id))

write_csv(mud, file.path(OUT, "sediment_carapace.csv"))
cat("sediment_carapace.csv   ", nrow(mud), "tortoises sampled\n")


# ---------------------------------------------------------------------------
#  4. MONTEMAR CAMERA
# ---------------------------------------------------------------------------

cam <- cam_raw |>
  transmute(
    image            = file_name,
    date             = suppressWarnings(mdy(date)),
    time_local       = time,
    air_temp_f       = num(temperature_f),
    tortoises_water  = num(number_of_tortoises_in_water),
    tortoises_land   = num(number_of_tortoises_on_land),
    tortoises_total  = num(total_number_of_tortoises),
    # The camera also scores how full the wetland is, which is the only
    # repeated measure of water level anywhere in the dataset.
    wetland_level    = num(wetland_level_actual)) |>
  filter(!is.na(date))

write_csv(cam, file.path(OUT, "camera_montemar.csv"))
cat("camera_montemar.csv     ", nrow(cam), "images\n")


# ---------------------------------------------------------------------------
#  5. TORTOISE POND VISITS (derived from telemetry)
#
#  The tracking data themselves are not ours to redistribute; this is the
#  derived visit table, which is what the paper's analyses use.
# ---------------------------------------------------------------------------

vfile <- "Outdir/recurse_polygon/pond_visit_stats.csv"
if (file.exists(vfile)) {
  visits <- read_csv(vfile, show_col_types = FALSE) |>
    transmute(pond_id = pond, tortoise = id,
              entrance_utc = entranceTime, exit_utc = exitTime,
              duration_h = round(timeInside, 4),
              time_since_last_visit_h = round(timeSinceLastVisit, 4))
  write_csv(visits, file.path(OUT, "tortoise_pond_visits.csv"))
  cat("tortoise_pond_visits.csv", nrow(visits), "visits,",
      n_distinct(visits$tortoise), "tortoises,", n_distinct(visits$pond_id), "ponds\n")
} else {
  cat("tortoise_pond_visits.csv  SKIPPED - run pond_revisitation.R first\n")
}


# ---------------------------------------------------------------------------
#  6. DATA DICTIONARY
# ---------------------------------------------------------------------------

dict <- tribble(
  ~table, ~variable, ~units, ~description,
  "pond_metadata", "pond_id", "", "Pond identifier used across all tables",
  "pond_metadata", "longitude; latitude", "decimal degrees, WGS84", "Median of GPS positions recorded at the pond margin during sampling",
  "pond_metadata", "elevation_m", "m above sea level", "Median recorded elevation",
  "pond_metadata", "pond_type", "", "managed (modified or maintained for tourism or farming) or natural",
  "pond_metadata", "size_class", "", "Modal field size category: small, small-medium, medium, large",
  "pond_metadata", "size_class_all; size_class_consistent", "", "Every size label recorded for the pond, and whether they agree. P_CM (Cerro Mesa) disagrees between visits; the pond delineation in the Methods treats it as large",
  "pond_metadata", "n_sampling_events", "count", "Number of water sampling events at this pond",
  "pond_chemistry", "new_date", "", "Sampling date as recorded; join key with chlorophyll and TSS",
  "pond_chemistry", "time_local", "HH:MM", "Galapagos local time (UTC-6)",
  "pond_chemistry", "temperature_c", "degrees C", "YSI multiparameter sonde",
  "pond_chemistry", "conductivity_us_cm", "uS/cm", "YSI multiparameter sonde",
  "pond_chemistry", "do_mg_l; do_pct", "mg/L; % saturation", "Dissolved oxygen, YSI sonde",
  "pond_chemistry", "tn_mg_l", "mg/L", "Total nitrogen",
  "pond_chemistry", "tp_ug_l", "ug/L", "Total phosphorus",
  "pond_chemistry", "npoc_mg_l", "mg/L", "Non-purgeable organic carbon",
  "pond_chemistry", "no3_ug_l; nh4_ug_l; po4_ug_l", "ug/L", "Nitrate, ammonium, orthophosphate",
  "pond_chemistry", "chlorophyll_a_ug_l", "ug/L", "Chlorophyll a, joined on pond and date",
  "pond_chemistry", "tss_mg_l", "mg/L", "Total suspended solids; replicate measurements averaged per event",
  "pond_chemistry", "n_tortoises", "count", "Tortoises present at the pond at sampling",
  "pond_chemistry", "emergent_veg_pct", "%", "Visually estimated emergent vegetation cover",
  "pond_chemistry", "excluded_from_analysis", "TRUE/FALSE", "Flagged record excluded from models; see exclusion_reason",
  "sediment_carapace", "carapace_length_cm", "cm", "Straight carapace length",
  "sediment_carapace", "mud_weight_raw_g", "g", "Dry sediment as weighed",
  "sediment_carapace", "scrape_coverage", "", "Which part of the carapace was scraped in that year",
  "sediment_carapace", "mud_weight_g", "g", "Whole-carapace load: raw weight doubled before 2021, when only the anterior portion was scraped",
  "camera_montemar", "tortoises_water; tortoises_land; tortoises_total", "count", "Tortoises scored by hand in each time-lapse image",
  "camera_montemar", "air_temp_f", "degrees F", "Camera-recorded air temperature",
  "camera_montemar", "wetland_level", "ordinal score", "Visually scored water level at Montemar pond",
  "tortoise_pond_visits", "entrance_utc; exit_utc", "UTC", "Visit boundaries; Galapagos local time is UTC-6",
  "tortoise_pond_visits", "duration_h", "hours", "Time inside the pond polygon",
  "tortoise_pond_visits", "time_since_last_visit_h", "hours", "Interval since that tortoise's previous visit to that pond")

write_csv(dict, file.path(OUT, "DATA_DICTIONARY.csv"))


# ---------------------------------------------------------------------------
#  WHAT WAS LEFT OUT, AND WHY
# ---------------------------------------------------------------------------

cat("\n=== columns deliberately not deposited ===\n")
dropped <- tribble(
  ~columns, ~reason,
  "v1", "spreadsheet row index, no meaning",
  "chlorophyl_syringe, chlorophyl_left_on_siringe, afdm_siringe, afdm_left_on_siringe, weight_filter_afdm, dic_syringle_left",
    "laboratory bookkeeping, not measurements",
  "pictures, fisheye, ease_of_access, notes, site", "field logistics and free-text notes",
  "mud_weight_adressing_half_mud_in_2019_g", "known incorrect: doubles 2019 only and one 2020 record is filled down from the row above",
  "tortoise_sex, tortoise_size, carapace_length, sex (in the pond sheet)",
    "15-23% filled and duplicated in the sediment table; kept there instead",
  "mud_weight_g (in the pond sheet)", "duplicates the sediment table, which is the authoritative source",
  "pond_length, pond_width, pond_area, pond_depth", "free text and inconsistently estimated between visits; not used in the paper",
  "gps_match_method, gps_match_confidence, gps_review_status, gps_raw_name_disagrees, gps_pond_id_disagrees (TSS sheet)",
    "provenance of the pond-identity reconciliation; retained in the raw file, not needed to reuse the data",
  "date (pond sheet)", "unparseable: mixes dd/mm/yyyy, m/d/yy and dot-separated formats in one field, plus a typo; new_date is used instead",
  "initials, date_analyzed, notes (camera sheet)", "scoring provenance, not observations")
print(as.data.frame(dropped), right = FALSE)
write_csv(dropped, file.path(OUT, "columns_not_deposited.csv"))

cat("\n=== variable coverage in pond_chemistry.csv ===\n")
cov <- chem |>
  summarise(across(where(is.numeric), ~sum(!is.na(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_events") |>
  arrange(desc(n_events))
print(as.data.frame(cov), right = FALSE)

cat("\nwritten to", OUT, "\n")
