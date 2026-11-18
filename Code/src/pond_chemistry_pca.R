# ===========================================================================
#  pond_chemistry_pca.R
#
#  Galapagos giant tortoises as zoogeochemical engineers
#  Ellis-Soto et al. - Ecology Letters
#
#  Pond physicochemistry and its association with tortoise abundance:
#
#    PART A  build the pond-chemistry table
#    PART B  PCA of pond water chemistry            -> Figure 6a
#    PART C  tortoise abundance vs nutrients        -> Figure 6b-d
#    PART D  within- and between-pond partitioning  -> Suppl. Figure 2
#    PART E  numbers reported in the manuscript
#
#  Run from the project root:
#    /Users/diegoellis/Desktop/Projects/Postdoc/Galapagos_conveyer_belt
#
#  The other two scripts of the paper:
#    Code/src/pond_revisitation.R   telemetry, pond use, sediment export
#    Code/src/pond_mud_camera.R     sediment loads and the Montemar camera
# ===========================================================================


# ---------------------------------------------------------------------------
# Setup
# ---------------------------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)
library(vegan)
library(glmmTMB)
library(ggeffects)
library(DHARMa)
library(ggrepel)
library(patchwork)

outdir <- "Outdir/pond_chemistry_pca"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# Pond type colours, the same convention as Figures 1, 3 and 5.
pond_colours <- c("Non-touristic" = "steelblue", "Touristic" = "goldenrod1")

set.seed(1)


# ===========================================================================
#  PART A - build the pond-chemistry table
# ===========================================================================

pond_raw <- read_csv("Data/pond_2025_post_fieldwork_clean_2026_deep_dive.csv",
                     show_col_types = FALSE) |>
  clean_names()

# One record is dropped everywhere in the paper: P_Laguna_Ch, 21-Jun-18, 10:21.
# Its conductivity is 1,918 uS/cm against 61-157 at every other visit to that
# pond, its elevation is 232 m against 197-219, and a second record at the
# identical coordinates was labelled "POTENTIALLY A DIFFERENT POND" by the
# field team and removed earlier. This one is its twin and was missed.
# Dropping it changes no conclusion (no model crosses P = 0.05 and the
# ordination is unchanged to three decimals).
pond_raw <- pond_raw |>
  filter(!(gps_pond_name == "P_Laguna_Ch" &
           new_date == "21-Jun-18" &
           !is.na(time) & time == "10:21"))

chlorophyll <- read_csv("Data/chlorophyl_data_clean_deep_clean.csv",
                        show_col_types = FALSE) |>
  clean_names()

# Total suspended solids. Fifteen pond x date keys hold replicate filters, so
# TSS is averaged to one value per sampling event before joining. Without this
# the join would add 15 rows and duplicate one ordination event.
tss_by_event <- read_csv("Outdir/final/TSS_pond_identity/TSS_RAW_with_clean_pond_identity.csv",
                         show_col_types = FALSE) |>
  clean_names() |>
  select(gps_pond_name = gps_pond_name_clean, new_date, tss_mg_l) |>
  mutate(tss_mg_l = suppressWarnings(as.numeric(tss_mg_l))) |>
  filter(!is.na(gps_pond_name), !is.na(tss_mg_l)) |>
  group_by(gps_pond_name, new_date) |>
  summarise(tss_mg_l = mean(tss_mg_l), .groups = "drop")

dat <- pond_raw |>
  left_join(chlorophyll) |>
  left_join(tss_by_event, by = c("gps_pond_name", "new_date")) |>
  mutate(
    sample_date = as.Date(parse_date_time(
      new_date, orders = c("d-b-y", "d-B-y", "dmy", "ymd", "mdy"), quiet = TRUE)),
    pond      = factor(gps_pond_name),
    pond_type = factor(case_when(tourist_lagoon == "Yes" ~ "Touristic",
                                 tourist_lagoon == "No"  ~ "Non-touristic"),
                       levels = c("Non-touristic", "Touristic")),
    season    = factor(if_else(month(sample_date) %in% 1:5, "wet_hot", "dry_cool"),
                       levels = c("wet_hot", "dry_cool")),
    tortoises        = parse_number(as.character(number_of_tortoises)),
    temperature      = parse_number(as.character(ysi_temperature)),
    conductivity     = coalesce(parse_number(as.character(ysi_us_ccm)),
                                parse_number(as.character(ysi_m_s_ccm)) * 1000),
    dissolved_oxygen = parse_number(as.character(do_mg_l)),
    tn   = as.numeric(tn_mg_l),
    tp   = as.numeric(tp_ug_l),
    npoc = as.numeric(npoc_mg_l),
    nh4  = as.numeric(nh4_ug_l),
    no3  = as.numeric(no3_ug_l),
    po4  = as.numeric(po4_ug_l),
    chlorophyll_a = as.numeric(chl_ugl),
    elevation     = parse_number(as.character(elevation)),
    tss           = suppressWarnings(as.numeric(tss_mg_l))
  ) |>
  # negative concentrations are below-detection artefacts, not measurements
  mutate(across(c(tn, tp, npoc, nh4, no3, po4), ~ if_else(.x < 0, NA_real_, .x)),
         tss = if_else(is.na(tss) | tss <= 0, NA_real_, tss))


# ===========================================================================
#  PART B - PCA of pond water chemistry (Figure 6a)
# ===========================================================================
# Eight active variables. Conductivity and the nutrients are log(1 + x)
# transformed, temperature and dissolved oxygen are left as measured, and
# everything is centred and scaled.
#
# Tortoise abundance, TSS, chlorophyll-a and elevation are SUPPLEMENTARY:
# they are fitted to the finished ordination rather than helping to build it.
# Making TSS active would force listwise deletion, cut the sample from 67
# events at 21 ponds to 46 at 19, and let TSS help define the very axis it is
# then shown to correlate with.

active_variables <- c("temperature", "conductivity", "dissolved_oxygen",
                      "tn", "tp", "npoc", "nh4", "po4")

pca_dat <- dat |>
  select(pond, sample_date, pond_type, season, tortoises,
         chlorophyll_a, elevation, tss, all_of(active_variables)) |>
  filter(if_all(all_of(active_variables), ~ !is.na(.x) & is.finite(.x)))

pca_matrix <- pca_dat |>
  transmute(Temperature        = temperature,
            Conductivity       = log1p(conductivity),
            `Dissolved oxygen` = dissolved_oxygen,
            `Total nitrogen`   = log1p(tn),
            `Total phosphorus` = log1p(tp),
            NPOC               = log1p(npoc),
            Ammonium           = log1p(nh4),
            Orthophosphate     = log1p(po4))

pond_pca     <- prcomp(pca_matrix, center = TRUE, scale. = TRUE)
pca_variance <- summary(pond_pca)$importance[2, 1:2] * 100
pca_scores   <- bind_cols(pca_dat, as.data.frame(pond_pca$x[, 1:2]))

# active variable arrows, scaled by their standard deviations
active_arrows <- as.data.frame(t(t(pond_pca$rotation[, 1:2]) * pond_pca$sdev[1:2])) |>
  rownames_to_column("variable") |>
  mutate(r = sqrt(PC1^2 + PC2^2), p = NA_real_, role = "Active")

# supplementary variables, each fitted to the ordination by permutation
passive_variables <- tribble(
  ~variable,            ~column,         ~transform,
  "Tortoise abundance", "tortoises",     "log1p",
  "Chlorophyll-a",      "chlorophyll_a", "log1p",
  "TSS",                "tss",           "log1p",
  "Elevation",          "elevation",     "identity"
)

passive_arrows <- passive_variables |>
  pmap_dfr(function(variable, column, transform) {
    values <- pca_scores[[column]]
    if (transform == "log1p") values <- log1p(values)
    keep   <- is.finite(values)
    scores <- as.matrix(pca_scores[keep, c("PC1", "PC2")])
    fit    <- envfit(scores, data.frame(v = values[keep]), permutations = 4999)
    tibble(variable = variable,
           PC1  = cor(values[keep], scores[, 1]),
           PC2  = cor(values[keep], scores[, 2]),
           r    = sqrt(fit$vectors$r),
           p    = fit$vectors$pvals,
           n    = sum(keep),
           role = "Passive")
  })

all_arrows <- bind_rows(select(active_arrows,  variable, PC1, PC2, r, p, role),
                        select(passive_arrows, variable, PC1, PC2, r, p, role))

arrow_scale <- 0.85 * max(abs(c(pca_scores$PC1, pca_scores$PC2)), na.rm = TRUE)
arrows_xy   <- all_arrows |> mutate(x = PC1 * arrow_scale, y = PC2 * arrow_scale)

unit_circle <- tibble(theta = seq(0, 2 * pi, length.out = 400)) |>
  mutate(x = arrow_scale * cos(theta), y = arrow_scale * sin(theta))

fig6a <- ggplot(pca_scores, aes(PC1, PC2, colour = pond_type, shape = season)) +
  geom_hline(yintercept = 0, colour = "grey88") +
  geom_vline(xintercept = 0, colour = "grey88") +
  geom_path(data = unit_circle, aes(x, y), inherit.aes = FALSE,
            colour = "grey85", linewidth = 0.4) +
  geom_point(size = 3, alpha = 0.85) +
  geom_segment(data = filter(arrows_xy, role == "Active"),
               aes(x = 0, y = 0, xend = x, yend = y), inherit.aes = FALSE,
               arrow = arrow(length = unit(0.16, "cm")),
               linewidth = 0.5, colour = "grey25") +
  geom_segment(data = filter(arrows_xy, role == "Passive"),
               aes(x = 0, y = 0, xend = x, yend = y), inherit.aes = FALSE,
               arrow = arrow(length = unit(0.18, "cm")),
               linewidth = 0.8, linetype = "dashed", colour = "darkgreen") +
  geom_text_repel(data = filter(arrows_xy, role == "Active"),
                  aes(x, y, label = variable), inherit.aes = FALSE,
                  colour = "grey20", size = 4.6, box.padding = 0.45,
                  min.segment.length = 0, max.overlaps = Inf, seed = 1) +
  geom_text_repel(data = filter(arrows_xy, role == "Passive"),
                  aes(x, y, label = variable), inherit.aes = FALSE,
                  colour = "darkgreen", fontface = "bold", size = 4.6,
                  box.padding = 0.7, point.padding = 0.4,
                  min.segment.length = 0, max.overlaps = Inf, seed = 1) +
  coord_equal(expand = FALSE) +
  scale_colour_manual(values = pond_colours) +
  scale_shape_manual(values = c(wet_hot = 17, dry_cool = 16),
                     labels = c(wet_hot = "Hot-wet", dry_cool = "Cool-dry")) +
  scale_x_continuous(limits = c(-3.4, 3.4)) +
  scale_y_continuous(limits = c(-3.4, 3.4)) +
  labs(x = paste0("PC1 (", round(pca_variance[1], 1), "%)"),
       y = paste0("PC2 (", round(pca_variance[2], 1), "%)"),
       colour = "Pond type", shape = "Season") +
  guides(colour = guide_legend(override.aes = list(size = 3.5)),
         shape  = guide_legend(override.aes = list(size = 3.5))) +
  theme_classic(base_size = 15) +
  theme(legend.position = "bottom",
        legend.box = "horizontal",
        axis.text = element_text(colour = "black"))


# ===========================================================================
#  PART C - tortoise abundance and nutrients (Figure 6b-d)
# ===========================================================================
# One Gamma GLMM with a log link per nutrient: tortoise abundance as
# log(1 + count), pond type and season as fixed effects, pond identity as a
# random intercept. The response ratio is the proportional change in
# concentration for a doubling of 1 + tortoise abundance.
#
# TSS is deliberately not a panel here. Its event-level association is not
# significant (RR = 1.15, P = 0.21); the TSS result is the pond-type contrast
# and the between-pond effect, which this panel type does not show. All
# response ratios including TSS are in Supplementary Figure 2 and its table.

# Axis labels are plotmath, so the micro sign and the superscript render on
# any graphics device instead of coming out as boxes.
nutrient_panels <- tribble(
  ~response, ~label,              ~axis,
  "nh4",     "Ammonium",          "Ammonium~(mu*g~L^-1)",
  "npoc",    "NPOC",              "NPOC~(mg~L^-1)",
  "tp",      "Total phosphorus",  "Total~phosphorus~(mu*g~L^-1)"
)

# model frame for one response: positive values only, no missing predictors
model_frame <- function(data, response) {
  data |>
    filter(!is.na(.data[[response]]), .data[[response]] > 0,
           !is.na(tortoises), !is.na(pond_type), !is.na(season), !is.na(pond)) |>
    mutate(y = .data[[response]],
           log_tortoises = log1p(tortoises),
           pond = droplevels(factor(pond)))
}

nutrient_results <- tibble()
nutrient_plots   <- list()
nutrient_models  <- list()

for (i in seq_len(nrow(nutrient_panels))) {

  response <- nutrient_panels$response[i]
  label    <- nutrient_panels$label[i]
  axis_lab <- nutrient_panels$axis[i]

  d <- model_frame(dat, response)

  m <- glmmTMB(y ~ log_tortoises + pond_type + season + (1 | pond),
               family = Gamma(link = "log"), data = d)

  coefs <- summary(m)$coefficients$cond
  beta  <- coefs["log_tortoises", "Estimate"]
  se    <- coefs["log_tortoises", "Std. Error"]
  p_val <- coefs["log_tortoises", "Pr(>|z|)"]

  nutrient_results <- bind_rows(nutrient_results, tibble(
    response = label,
    n_events = nrow(d),
    n_ponds  = n_distinct(d$pond),
    response_ratio = exp(beta * log(2)),
    lower_95 = exp((beta - 1.96 * se) * log(2)),
    upper_95 = exp((beta + 1.96 * se) * log(2)),
    p_value  = p_val))

  nutrient_models[[label]] <- m

  pred <- ggpredict(m, terms = "log_tortoises [all]", type = "fixed") |>
    as.data.frame() |>
    transmute(tortoises = expm1(x), predicted, conf.low, conf.high)

  annotation <- paste0(sprintf("RR = %.2f", exp(beta * log(2))), "\n",
                       if (p_val < 0.001) "P < 0.001" else sprintf("P = %.3f", p_val))

  nutrient_plots[[label]] <-
    ggplot() +
    geom_point(data = d, aes(tortoises, y, colour = pond_type),
               size = 2.2, alpha = 0.65,
               position = position_jitter(width = 0.04, height = 0)) +
    geom_ribbon(data = pred, aes(tortoises, ymin = conf.low, ymax = conf.high),
                fill = "grey65", alpha = 0.25) +
    geom_line(data = pred, aes(tortoises, predicted),
              colour = "black", linewidth = 0.9) +
    annotate("text", x = Inf, y = Inf, label = annotation,
             hjust = 1.1, vjust = 1.4, size = 4.2, fontface = "bold") +
    scale_x_continuous(trans = "log1p", breaks = c(0, 1, 5, 20, 50)) +
    scale_y_log10(labels = scales::label_number()) +
    scale_colour_manual(values = pond_colours, name = "Pond type") +
    labs(title = label, x = "Tortoise abundance (count)",
         y = parse(text = axis_lab)[[1]]) +
    theme_classic(base_size = 14) +
    theme(legend.position = "none",
          plot.title = element_text(face = "bold", size = 14),
          axis.title = element_text(size = 12),
          axis.text  = element_text(colour = "black", size = 11))
}

# The PCA uses coord_equal, so it renders square whatever slot it is given.
# Width and height below are chosen so that square spans the width of the
# three panels underneath; change them together or the alignment goes.
fig6 <- fig6a / wrap_plots(nutrient_plots, nrow = 1) +
  plot_layout(heights = c(3, 1.15)) +
  plot_annotation(tag_levels = "a",
                  theme = theme(plot.tag = element_text(face = "bold", size = 20)))

ggsave(file.path(outdir, "Figure_6_pond_chemistry_and_tortoises.png"),
       fig6, width = 12, height = 15.5, dpi = 600, bg = "white")


# ===========================================================================
#  PART D - within- and between-pond partitioning (Suppl. Figure 2)
# ===========================================================================
# Tortoise abundance is split into a between-pond component (each pond's own
# mean) and a within-pond component (how far a given visit sits from that
# mean). Persistent differences among ponds can then be told apart from
# temporal variation within them.

# Every response is partitioned, not just the ones with an event-level
# association. TSS is the reason: it is null at the event level yet
# significant between ponds, so a null overall cannot be assumed to hide
# nothing. Supplementary Figure 2 still shows the four nutrients only.
partition_responses <- tribble(
  ~response,       ~label,
  "nh4",           "Ammonium",
  "tn",            "Total nitrogen",
  "no3",           "Nitrate",
  "tp",            "Total phosphorus",
  "po4",           "Orthophosphate",
  "npoc",          "NPOC",
  "tss",           "TSS",
  "chlorophyll_a", "Chlorophyll-a"
)

within_between <- tibble()

for (i in seq_len(nrow(partition_responses))) {

  response <- partition_responses$response[i]
  label    <- partition_responses$label[i]

  d <- model_frame(dat, response) |>
    group_by(pond) |>
    mutate(between_log_tortoises = mean(log_tortoises),
           within_log_tortoises  = log_tortoises - between_log_tortoises) |>
    ungroup()

  m <- glmmTMB(y ~ between_log_tortoises + within_log_tortoises +
                 pond_type + season + (1 | pond),
               family = Gamma(link = "log"), data = d)

  coefs <- summary(m)$coefficients$cond

  for (term in c("between_log_tortoises", "within_log_tortoises")) {
    beta <- coefs[term, "Estimate"]
    se   <- coefs[term, "Std. Error"]
    within_between <- bind_rows(within_between, tibble(
      response  = label,
      component = if (term == "between_log_tortoises") "Between ponds" else "Within ponds",
      response_ratio = exp(beta * log(2)),
      lower_95 = exp((beta - 1.96 * se) * log(2)),
      upper_95 = exp((beta + 1.96 * se) * log(2)),
      p_value  = coefs[term, "Pr(>|z|)"],
      n_events = nrow(d)))
  }
}

within_between <- within_between |>
  mutate(response  = factor(response, levels = partition_responses$label),
         component = factor(component, levels = c("Within ponds", "Between ponds")))

# Deliberately monochrome. Blue and gold mean non-touristic and touristic
# everywhere else in the paper, so reusing them for within and between ponds
# would imply a link that does not exist. The two components are told apart by
# their row labels.
figS2 <- ggplot(filter(within_between, response %in%
                         c("Ammonium", "Total nitrogen", "Total phosphorus", "NPOC")),
                aes(response_ratio, component)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = lower_95, xmax = upper_95), height = 0.16,
                 linewidth = 0.7, colour = "black") +
  geom_point(size = 3.4, colour = "black") +
  # geom_label, not geom_text, so a label sitting near RR = 1 is not struck
  # through by the dashed reference line
  geom_label(aes(label = sprintf("RR = %.2f, P = %.3f", response_ratio, p_value)),
             vjust = -0.9, size = 3.6, colour = "grey20",
             fill = "white", label.size = 0, label.padding = unit(0.1, "lines")) +
  facet_wrap(~ response, ncol = 1, strip.position = "top") +
  scale_x_log10(breaks = c(0.75, 1, 1.5, 2)) +
  labs(x = "Response ratio per doubling of 1 + tortoise abundance", y = NULL) +
  theme_classic(base_size = 14) +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold", size = 13),
        axis.text = element_text(colour = "black"),
        panel.spacing = unit(1, "lines"))

ggsave(file.path(outdir, "Suppl_Figure_2_within_between_pond.png"),
       figS2, width = 8.5, height = 10, dpi = 400, bg = "white")

write_csv(within_between, file.path(outdir, "within_between_pond.csv"))
write_csv(nutrient_results, file.path(outdir, "tortoise_nutrient_models.csv"))
write_csv(all_arrows, file.path(outdir, "pca_variable_loadings.csv"))


# ===========================================================================
#  PART E - the full response suite
# ===========================================================================
# The same event-level Gamma GLMM as Figure 6b-d, fitted to every response
# the Results mention. Only the three supported nutrients are plotted; the
# rest are reported as response ratios.

suite_responses <- tribble(
  ~response, ~label,
  "nh4",     "Ammonium",
  "tn",      "Total nitrogen",
  "no3",     "Nitrate",
  "tp",      "Total phosphorus",
  "po4",     "Orthophosphate",
  "npoc",    "NPOC",
  "chlorophyll_a", "Chlorophyll-a",
  "tss",     "TSS"
)

response_suite <- tibble()

for (i in seq_len(nrow(suite_responses))) {

  d <- model_frame(dat, suite_responses$response[i])

  m <- glmmTMB(y ~ log_tortoises + pond_type + season + (1 | pond),
               family = Gamma(link = "log"), data = d)

  coefs <- summary(m)$coefficients$cond
  beta  <- coefs["log_tortoises", "Estimate"]
  se    <- coefs["log_tortoises", "Std. Error"]

  response_suite <- bind_rows(response_suite, tibble(
    response = suite_responses$label[i],
    n_events = nrow(d),
    n_ponds  = n_distinct(d$pond),
    response_ratio = exp(beta * log(2)),
    lower_95 = exp((beta - 1.96 * se) * log(2)),
    upper_95 = exp((beta + 1.96 * se) * log(2)),
    p_value  = coefs["log_tortoises", "Pr(>|z|)"]))
}


# ===========================================================================
#  PART F - total suspended solids and pond type
# ===========================================================================
# The TSS result in the Results is a pond-type contrast, not a tortoise
# effect: touristic ponds carry more suspended sediment whatever the tortoise
# count. Fitted with REML, which is what the quoted number uses.

d_tss <- model_frame(dat, "tss")

tss_model <- glmmTMB(y ~ log_tortoises + pond_type + season + (1 | pond),
                     family = Gamma(link = "log"), data = d_tss, REML = TRUE)

tss_coefs <- summary(tss_model)$coefficients$cond
tss_beta  <- tss_coefs["pond_typeTouristic", "Estimate"]
tss_se    <- tss_coefs["pond_typeTouristic", "Std. Error"]

tss_pond_type <- tibble(
  contrast = "Touristic / non-touristic",
  n_events = nrow(d_tss),
  n_ponds  = n_distinct(d_tss$pond),
  fold     = exp(tss_beta),
  lower_95 = exp(tss_beta - 1.96 * tss_se),
  upper_95 = exp(tss_beta + 1.96 * tss_se),
  p_value  = tss_coefs["pond_typeTouristic", "Pr(>|z|)"])


# ===========================================================================
#  PART G - is the ammonium result driven by one pond?
# ===========================================================================
# Drop each pond in turn and refit. The Results quote the range of response
# ratios and the largest P across these refits.

d_nh4 <- model_frame(dat, "nh4")

ammonium_loo <- tibble()

for (this_pond in levels(d_nh4$pond)) {

  d <- d_nh4 |> filter(pond != this_pond) |> mutate(pond = droplevels(pond))

  m <- glmmTMB(y ~ log_tortoises + pond_type + season + (1 | pond),
               family = Gamma(link = "log"), data = d)

  coefs <- summary(m)$coefficients$cond

  ammonium_loo <- bind_rows(ammonium_loo, tibble(
    pond_dropped   = this_pond,
    n_events       = nrow(d),
    response_ratio = exp(coefs["log_tortoises", "Estimate"] * log(2)),
    p_value        = coefs["log_tortoises", "Pr(>|z|)"]))
}

# ---------------------------------------------------------------------------
# Model adequacy: simulation-based residual tests (DHARMa)
#
# Uniformity of the scaled residuals and over/under-dispersion, for the three
# models shown in Figure 6b-d. A P above 0.05 means no detectable departure.
# ---------------------------------------------------------------------------

set.seed(1)

residual_checks <- tibble()

for (label in names(nutrient_models)) {
  sim <- simulateResiduals(nutrient_models[[label]], n = 1000, plot = FALSE)
  residual_checks <- bind_rows(residual_checks, tibble(
    model = label,
    uniformity_p = testUniformity(sim, plot = FALSE)$p.value,
    dispersion_p = testDispersion(sim, plot = FALSE)$p.value))
}

write_csv(residual_checks, file.path(outdir, "dharma_residual_checks.csv"))

write_csv(response_suite, file.path(outdir, "response_suite_event_level.csv"))


# The three result tables written above - response_suite_event_level.csv,
# within_between_pond.csv and tss_pond_type_contrast.csv - are what
# Code/src/pond_table_supplementary.R formats into Supplementary Table 2.
# The models are fitted here and only here, so the two scripts cannot drift.
write_csv(tss_pond_type,  file.path(outdir, "tss_pond_type_contrast.csv"))
write_csv(ammonium_loo,   file.path(outdir, "ammonium_leave_one_pond_out.csv"))


# ===========================================================================
#  PART H - numbers reported in the manuscript
# ===========================================================================

cat("\n\n===================================================================\n")
cat("  NUMBERS REPORTED IN THE MANUSCRIPT\n")
cat("===================================================================\n")

cat("\n--- PCA (Methods and Results 'Pond environmental gradients') ---\n")
cat("sampling events in the ordination     :", nrow(pca_dat), "\n")
cat("ponds                                 :", n_distinct(pca_dat$pond), "\n")
cat("PC1 variance explained (%)            :", round(pca_variance[1], 1), "\n")
cat("PC2 variance explained (%)            :", round(pca_variance[2], 1), "\n")
cat("PC1 + PC2 (%)                         :", round(sum(pca_variance), 1), "\n")

cat("\nsupplementary variables fitted to the ordination:\n")
print(as.data.frame(passive_arrows |>
  transmute(variable, n, PC1_correlation = round(PC1, 3),
            r2 = round(r^2, 3), P = p)), row.names = FALSE)

cat("\nactive variable loadings:\n")
print(as.data.frame(active_arrows |>
  transmute(variable, PC1 = round(PC1, 3), PC2 = round(PC2, 3))), row.names = FALSE)

cat("\n--- Tortoise abundance vs nutrients (Figure 6b-d) ---\n")
print(as.data.frame(nutrient_results |>
  mutate(across(c(response_ratio, lower_95, upper_95), ~ round(.x, 3)),
         p_value = signif(p_value, 3))), row.names = FALSE)

cat("\n--- Model adequacy, DHARMa (Methods) ---\n")
print(as.data.frame(residual_checks |>
  mutate(across(c(uniformity_p, dispersion_p), ~ signif(.x, 3)))), row.names = FALSE)

cat("\n--- Full response suite (event level) ---\n")
print(as.data.frame(response_suite |>
  mutate(across(c(response_ratio, lower_95, upper_95), ~ round(.x, 3)),
         p_value = signif(p_value, 3))), row.names = FALSE)

cat("\n--- TSS and pond type ---\n")
print(as.data.frame(tss_pond_type |>
  mutate(across(c(fold, lower_95, upper_95), ~ round(.x, 2)),
         p_value = signif(p_value, 3))), row.names = FALSE)

cat("\n--- Ammonium, dropping each pond in turn (", nrow(ammonium_loo), "refits ) ---\n")
cat("response ratio range                  :",
    round(min(ammonium_loo$response_ratio), 2), "-",
    round(max(ammonium_loo$response_ratio), 2), "\n")
cat("largest P                             :",
    signif(max(ammonium_loo$p_value), 3), "\n")
cat("refits still significant at 0.05      :",
    sum(ammonium_loo$p_value < 0.05), "of", nrow(ammonium_loo), "\n")

cat("\n--- Within and between ponds (Suppl. Figure 2) ---\n")
print(as.data.frame(within_between |>
  mutate(across(c(response_ratio, lower_95, upper_95), ~ round(.x, 3)),
         p_value = signif(p_value, 3))), row.names = FALSE)

cat("\nFigures written to", outdir, "\n")
