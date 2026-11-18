# ===========================================================================
#  pond_mud_camera.R
#
#  Galapagos giant tortoises as zoogeochemical engineers
#  Ellis-Soto et al. - Ecology Letters
#
#  This script produces the sediment ("mud") and camera-trap parts of the
#  manuscript, and prints every number that is reported in the text:
#
#    PART A  Pond size categories        -> Suppl. Table 1 lookup
#    PART B  Sediment carried by tortoises
#              Figure 5            sediment by pond type
#              Suppl. Figure 3     sediment vs carapace length
#    PART C  Montemar time-lapse camera
#              Figure 4a           tortoises in water by hour and month
#              Figure 4b           tortoises in water by time of day
#    PART D  Numbers reported in the manuscript
#
#  Run from the project root:
#    /Users/diegoellis/Desktop/Projects/Postdoc/Galapagos_conveyer_belt
#
#  Not in this script (done elsewhere, on purpose):
#    - pond revisitation from telemetry  -> Code/src/pond_revisitation.R
#    - PCA and nutrient models (Fig. 6)  -> Code/13_ordination_and_response_models.R
# ===========================================================================


# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(glmmTMB)
library(patchwork)

outdir <- "Outdir/pond_mud_camera"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Pond type colours, the same convention as Figures 1 and 3:
# Natural (inside the National Park) = blue, managed (private agricultural or
# touristic properties) = gold. Same colours as Figs. 1 and 3.
pal <- c("No" = "#5580B0", "Yes" = "#F5C34C")


# ===========================================================================
#  PART A - Pond size categories
# ===========================================================================
# The field sheets contain a categorical pond size. "small-medium" was only
# ever used for Montemar, so it is folded into "medium" and the column is
# turned into an ordered small < medium < large factor.
# Change the line below if a category should be assigned differently.

size_recode <- c("small-medium" = "medium")

pond <- read.csv("Data/pond_2025_post_fieldwork_clean.csv")

pond_size <- pond |>
  mutate(
    Size = tolower(trimws(Size)),
    Size = ifelse(Size %in% c("", "na"), NA, Size),
    Size = ifelse(Size %in% names(size_recode), size_recode[Size], Size),
    Size = factor(Size, levels = c("small", "medium", "large"))
  ) |>
  filter(!is.na(GPS_Pond_name), GPS_Pond_name != "") |>
  group_by(GPS_Pond_name) |>
  summarise(
    n_sampling_events = n(),
    Size = names(sort(table(Size), decreasing = TRUE))[1],  # one label per pond
    .groups = "drop"
  ) |>
  mutate(Size = factor(Size, levels = c("small", "medium", "large"))) |>
  arrange(Size, GPS_Pond_name)

write.csv(pond_size,
          file.path(outdir, "pond_size_categories.csv"),
          row.names = FALSE)

cat("\n--- Pond size categories -----------------------------------------\n")
print(as.data.frame(pond_size))
print(table(pond_size$Size, useNA = "ifany"))


# ===========================================================================
#  PART B - Sediment carried out of ponds by tortoises
# ===========================================================================

mud <- read.csv("Data/Mud_2024_2025_fieldwork.csv")

# Sediment was scraped from the anterior carapace only from 2018-2020, and
# from both the anterior and posterior portions from 2021 onwards. The
# earlier loads are therefore doubled so that every year is on the same
# whole-carapace basis.
#
# The file also carries a stored column, Mud.weight.adressing.half.mud.in.2019.g.,
# that was meant to do this. It is not used here: it doubles 2019 only, leaves
# the four 2020 records uncorrected, and one 2020 record (P_Manzan_2, 24 Jul
# 2020) holds 112 g against a raw weight of 895 g - the value of the row above
# it, filled down by mistake. Deriving the correction from the raw weights
# fixes both problems in one rule.

mud <- mud |>
  mutate(
    sediment_raw_g = as.numeric(gsub("TBD", NA, Mud.weight..g.)),
    sediment_g     = ifelse(Year <= 2020, sediment_raw_g * 2, sediment_raw_g),
    carapace_cm    = readr::parse_number(as.character(Carapace.length)),
    Tourist_lagoon = factor(Tourist_lagoon, levels = c("No", "Yes"))
  )

# --- Figure 5: sediment by pond type ---------------------------------------

fig5 <- ggplot(mud, aes(x = Tourist_lagoon, y = sediment_g, fill = Tourist_lagoon)) +
  geom_boxplot(width = 0.6, alpha = 0.8, linewidth = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.12, height = 0, size = 2, alpha = 0.55,
              shape = 21, stroke = 0.3) +
  scale_fill_manual(values = pal) +
  scale_x_discrete(labels = c("No" = "Natural", "Yes" = "Managed")) +
  labs(x = "Pond type", y = "Dry sediment (g)") +
  theme_classic(base_size = 20) +
  theme(legend.position = "none",
        axis.text  = element_text(colour = "black", size = 18),
        axis.title = element_text(face = "bold", size = 20))

ggsave(file.path(outdir, "Figure_5_sediment_by_pond_type.png"),
       fig5, width = 7, height = 7, dpi = 300)

# --- Does sediment load differ between managed and natural ponds? ---------
# Tweedie GLMM with a log link and pond identity as a random intercept.

mud_model <- glmmTMB(
  sediment_g ~ Tourist_lagoon + (1 | GPS_Pond_name),
  family = tweedie(link = "log"),
  data = mud
)

mud_model_summary <- summary(mud_model)
mud_model_ci <- confint(mud_model)

# Fold difference managed / natural = exp(coefficient)
mud_fold <- exp(fixef(mud_model)$cond["Tourist_lagoonYes"])
mud_fold_ci <- exp(mud_model_ci["Tourist_lagoonYes", c("2.5 %", "97.5 %")])
mud_p <- mud_model_summary$coefficients$cond["Tourist_lagoonYes", "Pr(>|z|)"]

# "Primicias 1" and "P_Primic_1" are the same pond entered two ways, which
# gives the random effect 14 levels instead of 13. Refit with them merged to
# check that the result does not depend on it.
mud_merged <- mud |>
  mutate(GPS_Pond_name = ifelse(GPS_Pond_name == "Primicias 1",
                                "P_Primic_1", GPS_Pond_name))

mud_model_merged <- glmmTMB(
  sediment_g ~ Tourist_lagoon + (1 | GPS_Pond_name),
  family = tweedie(link = "log"),
  data = mud_merged
)

# --- Suppl. Figure 3: sediment vs carapace length --------------------------

mud_size <- mud |> dplyr::filter(!is.na(sediment_g), !is.na(carapace_cm))

mud_size_cor <- cor.test(mud_size$carapace_cm, mud_size$sediment_g,
                         method = "pearson")

figS3 <- ggplot(mud_size, aes(x = carapace_cm, y = sediment_g)) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
              colour = "black", fill = "grey70", alpha = 0.4, linewidth = 0.6) +
  geom_point(size = 2.5, colour = "#5580B0") +
  labs(x = "Carapace length (cm)", y = "Dry sediment (g)") +
  theme_classic(base_size = 14) +
  theme(axis.text = element_text(colour = "black"))

ggsave(file.path(outdir, "Suppl_Figure_3_sediment_vs_carapace.png"),
       figS3, width = 7, height = 5, dpi = 300)


# ===========================================================================
#  PART C - Montemar time-lapse camera
# ===========================================================================
# One image every 15 min. For each image the number of tortoises in the water
# and on land was scored by hand. All analyses use tortoises in the water.

cam <- read.csv("Data/Photo_spreadsheet_montemar_all.csv") |>
  dplyr::mutate(
    timestamp     = mdy(Date),
    year          = year(timestamp),
    month         = month(timestamp),
    time          = Time,
    hour          = hour(hms(Time)),
    tort_in_water = X..of.tortoises.in.water,
    tort_on_land  = X..of.tortoises.on.land,
    total_n_tort  = total...of.tortoises
  ) |>
  dplyr::select(file = File.Name, time, timestamp, year, month, hour,
         tort_in_water, tort_on_land, total_n_tort)

# Months actually sampled (the camera was off 20 Aug - 15 Nov 2022)
month_values <- sort(unique(cam$month))
month_labels <- month.name[month_values]
cam$month_label <- factor(cam$month, levels = month_values, labels = month_labels)

# The Galapagos year splits into a hot-wet and a cool-dry season
# (Colinvaux 1984). January is kept separate rather than folded into the
# hot-wet season: it sits at the transition and carries the highest pond
# use in both the camera and the telemetry, so pooling it would average
# away the peak the Results describe.
# Colours match Figure 2, which uses the same seasonal split.
cam <- cam |>
  mutate(climatic_season = factor(
    case_when(month == 1     ~ "January (transition)",
              month %in% 2:5 ~ "Hot-wet (Feb-May)",
              TRUE           ~ "Cool-dry (Jun-Dec)"),
    levels = c("Hot-wet (Feb-May)", "January (transition)", "Cool-dry (Jun-Dec)")))

season_cols <- c("Hot-wet (Feb-May)"    = "#D55E00",
                 "January (transition)" = "#CC79A7",
                 "Cool-dry (Jun-Dec)"   = "#0072B2")

# --- Figure 4a: tortoises in water by hour, one curve per month ------------

fig4a <- ggplot(cam, aes(x = hour, y = tort_in_water)) +
  geom_boxplot(aes(group = factor(hour)), alpha = 0.6, outlier.size = 0.7) +
  geom_smooth(aes(colour = climatic_season, fill = climatic_season),
              method = "loess", formula = y ~ x,
              se = TRUE, alpha = 0.15, linewidth = 1.6) +
  scale_colour_manual(values = season_cols) +
  scale_fill_manual(values   = season_cols) +
  labs(x = "Hour", y = "Number of tortoises\nin water") +
  theme_minimal(base_size = 22) +
  theme(legend.title = element_blank(),
        legend.text  = element_text(size = 18),
        axis.text  = element_text(face = "bold", size = 19, colour = "black"),
        axis.title = element_text(face = "bold", size = 22))

ggsave(file.path(outdir, "Figure_4a_montemar_hourly.png"),
       fig4a, width = 10, height = 8, dpi = 300)

# --- Figure 4b: tortoises in water by time of day across months ------------
# Morning 05:00-11:59, Midday 12:00-16:59, Evening 17:00-19:59, Night otherwise.

cam <- cam |>
  mutate(time_of_day = case_when(
    hour >= 5  & hour < 12 ~ "Morning",
    hour >= 12 & hour < 17 ~ "Midday",
    hour >= 17 & hour < 20 ~ "Evening",
    TRUE                   ~ "Night"
  ))

cam_tod <- cam |>
  filter(time_of_day != "Night") |>   # the camera records too few tortoises at night
  group_by(month_label, time_of_day) |>
  summarise(median_tort_in_water = median(tort_in_water, na.rm = TRUE),
            mean_tort_in_water   = mean(tort_in_water, na.rm = TRUE),
            n = n(),
            .groups = "drop") |>
  mutate(time_of_day = factor(time_of_day,
                              levels = c("Morning", "Midday", "Evening")))

fig4b <- ggplot(cam_tod, aes(x = month_label, y = median_tort_in_water,
                             colour = time_of_day, group = time_of_day)) +
  geom_line(linewidth = 1.4) +
  geom_point(size = 3.5) +
  scale_colour_manual(values = c("Morning" = "#00BFC4",
                                 "Midday"  = "#7CAE00",
                                 "Evening" = "#F8766D")) +
  labs(x = "Month", y = "Median number of tortoises\nin water", colour = NULL) +
  theme_minimal(base_size = 22) +
  theme(legend.text  = element_text(size = 18),
        axis.text.x  = element_text(angle = 45, hjust = 1),
        axis.text    = element_text(face = "bold", size = 19, colour = "black"),
        axis.title   = element_text(face = "bold", size = 22))

ggsave(file.path(outdir, "Figure_4b_montemar_time_of_day.png"),
       fig4b, width = 10, height = 8, dpi = 300)

# Combined Figure 4 is assembled further down, once fig4c exists.

# --- Figure 4c: the same pattern by climatic season ------------------------
# The Galapagos year splits into a hot-wet season and a cool-dry season
# (Colinvaux 1984). January is treated separately rather than folded into
# the hot-wet season: it sits at the transition, and it is the month with
# the highest pond use in both the camera and the telemetry, so pooling it
# would hide the peak the Results describe.
cam_season <- cam |>
  filter(time_of_day != "Night") |>
  group_by(climatic_season, time_of_day) |>
  summarise(median_tort_in_water = median(tort_in_water, na.rm = TRUE),
            mean_tort_in_water   = mean(tort_in_water, na.rm = TRUE),
            n_images = n(),
            .groups = "drop")

fig4c <- ggplot(cam_season, aes(x = climatic_season, y = median_tort_in_water,
                                fill = time_of_day)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.9) +
  scale_fill_manual(values = c("Morning" = "#00BFC4",
                               "Midday"  = "#7CAE00",
                               "Evening" = "#F8766D")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(x = NULL, y = "Median number of tortoises\nin water", fill = NULL) +
  theme_minimal(base_size = 22) +
  theme(legend.position = "top",
        legend.text  = element_text(size = 18),
        axis.text    = element_text(face = "bold", size = 19, colour = "black"),
        axis.title   = element_text(face = "bold", size = 22))

ggsave(file.path(outdir, "Figure_4c_montemar_climatic_season.png"),
       fig4c, width = 12, height = 8, dpi = 300)

# --- Figure 4: hourly pattern by season beside season x time of day --------
# (a) gives the hourly pattern pooled into the three seasons; (b) keeps the
# month-by-month resolution so the January peak is visible as a point in the
# series rather than a category. Legends sit on top so the panels align.
fig4 <- (fig4a + theme(legend.position = "top")) |
        (fig4b + theme(legend.position = "top")) +
  plot_annotation(title = "Seasonal and daily presence of tortoises in Montemar Pond",
                  tag_levels = "a", tag_prefix = "(", tag_suffix = ")",
                  theme = theme(plot.title = element_text(size = 26, face = "bold"))) &
  theme(plot.tag = element_text(size = 26, face = "bold"))

ggsave(file.path(outdir, "Figure_4_montemar_combined.png"),
       fig4, width = 20, height = 9, dpi = 300)

write_csv(cam_season, file.path(outdir, "montemar_by_climatic_season.csv"))

cat("\nMONTEMAR BY CLIMATIC SEASON (median tortoises in water)\n")
print(as.data.frame(cam_season), row.names = FALSE, digits = 3)

# --- Seasonal and diel numbers quoted in the Results ------------------------

cam_month <- cam |>
  group_by(month_label) |>
  summarise(n_images = n(),
            mean_tort_in_water = mean(tort_in_water, na.rm = TRUE),
            median_tort_in_water = median(tort_in_water, na.rm = TRUE),
            .groups = "drop") |>
  arrange(desc(mean_tort_in_water))

# Midday (12:00) versus late afternoon (17:00) in January and April
cam_jan_apr <- cam |>
  filter(month %in% c(1, 4), hour %in% c(12, 17)) |>
  group_by(month_label, hour) |>
  summarise(n = n(),
            mean_tort_in_water = mean(tort_in_water, na.rm = TRUE),
            .groups = "drop") |>
  pivot_wider(names_from = hour, values_from = c(n, mean_tort_in_water)) |>
  mutate(fold_change = mean_tort_in_water_17 / mean_tort_in_water_12)


# ===========================================================================
#  PART D - Numbers reported in the manuscript
# ===========================================================================

cat("\n\n===================================================================\n")
cat("  NUMBERS REPORTED IN THE MANUSCRIPT\n")
cat("===================================================================\n")

cat("\n--- Sediment (Methods, Results 'Physical ecosystem engineering') ---\n")
cat("tortoises with a sediment measurement :", sum(!is.na(mud$sediment_g)), "\n")
cat("tortoises with a carapace length      :", nrow(mud_size), "\n")
cat("median dry sediment (g)               :", round(median(mud$sediment_g, na.rm = TRUE), 1), "\n")
cat("mean dry sediment (g)                 :", round(mean(mud$sediment_g, na.rm = TRUE), 1), "\n")
cat("SD dry sediment (g)                   :", round(sd(mud$sediment_g, na.rm = TRUE), 1), "\n")
cat("range dry sediment (g)                :",
    round(min(mud$sediment_g, na.rm = TRUE), 1), "-",
    round(max(mud$sediment_g, na.rm = TRUE), 1), "\n")
cat("IQR dry sediment (g)                  :", round(IQR(mud$sediment_g, na.rm = TRUE), 1), "\n")
cat("ponds sampled for sediment            :", n_distinct(mud$GPS_Pond_name), "\n")

cat("\n--- Managed vs natural sediment loads (Tweedie GLMM) ---\n")
cat("fold difference (managed / natural)   :", round(mud_fold, 2), "\n")
cat("95% CI                                :",
    round(mud_fold_ci[1], 2), "-", round(mud_fold_ci[2], 2), "\n")
cat("P                                     :", signif(mud_p, 3), "\n")
print(mud_model_summary)

cat("\nsame model with Primicias 1 merged into P_Primic_1 (", 
    n_distinct(mud_merged$GPS_Pond_name), "ponds ):\n")
cat("fold difference                       :",
    round(exp(fixef(mud_model_merged)$cond["Tourist_lagoonYes"]), 2), "\n")
cat("95% CI                                :",
    round(exp(confint(mud_model_merged)["Tourist_lagoonYes", c("2.5 %", "97.5 %")]), 2), "\n")
cat("P                                     :",
    signif(summary(mud_model_merged)$coefficients$cond["Tourist_lagoonYes", "Pr(>|z|)"], 3), "\n")

cat("\n--- Carapace length vs sediment load (Suppl. Fig. 3) ---\n")
print(mud_size_cor)

cat("\n--- Montemar camera (Methods and Results) ---\n")
cat("images scored                         :", nrow(cam), "\n")
cat("deployment                            :",
    format(min(cam$timestamp, na.rm = TRUE)), "to",
    format(max(cam$timestamp, na.rm = TRUE)), "\n")
cat("months sampled                        :", paste(month_labels, collapse = ", "), "\n")
cat("\nmean tortoises in water per month (highest first):\n")
print(as.data.frame(cam_month))
cat("\nmidday (12:00) vs late afternoon (17:00), January and April:\n")
print(as.data.frame(cam_jan_apr))
cat("\nmedian tortoises in water by month and time of day:\n")
print(as.data.frame(cam_tod))

cat("\nFigures written to", outdir, "\n")
