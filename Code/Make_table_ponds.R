conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::select)
library(kableExtra)
library(tidyverse)

df_raw <- read_csv("Data.Galapagos Water Sample Data_2018-24.csv")
df_raw <- read_csv("Data/Galapagos Water Sample Data_2018-24_most_up_to_date_2025.csv")
df <- df_raw

df <- df %>%
  mutate(
    # Try parsing both "New Date" and "Date" if needed:
    #  - If your final date is always in the 'Date' column, adjust accordingly.
    #  - Below is just one example.
    Date_parsed = parse_date_time(
      x = coalesce(Date, `New Date`),  # use `Date` if not NA else use `New Date`
      orders = c("dmY", "dmy", "mdY", "Ymd", "d.b.Y", "d/m/y", "d.m.y", "m/d/y"),
      tz = "UTC"
    )
  ) %>%
  filter(!is.na(Date_parsed))


keep = c("P2_Chato_3",  "P_Chato_1",   "P_Laguna_Ch", "P_CM_1" ,     "P_Caseta_1", 
"P_Caseta_2",  "P_Chato_2",   "P_Manzan_1",  "P_Manzan_2",  "P_Montem_1" ,
"P_Pikaia_1",  "P_Primic_1",  "P_Primic_2",  "P_Primic_3",  "P_Primic_4" ,
"P_Primic_5",  "P_Primic_6" , "P_SDB_1" ,    "P_SDB_2" ,    "P_SDB_3"  ,  
"P1_Chato_3" , "P3_Chato_3"  ,"Poza 3"     , "Poza 4" ,     "Poza 5" ,    
"Poza 7"  ,    "R_Chato")

df = df[df$GPS_Pond_name %in% keep,]

# Count unique ponds sampled per field day
df_daily <- df %>%
  group_by(Date_parsed) %>%
  summarize(
    NumberOfPondsSampled = n_distinct(GPS_Pond_name),
    .groups = "drop"
  )


galapagos_season <- function(d) {
  m <- month(d)
  # If month is July–November => "Dry", else => "Wet"
  if_else(m >= 7 & m <= 11, "Dry", "Wet")
}

df_daily$Date_parsed = as.Date(df_daily$Date_parsed)


# Make a daily sequence from min to max date in your dataset:
all_dates <- tibble(Day = seq.Date(
  from = floor_date(min(df_daily$Date_parsed), "day"),
  to   = ceiling_date(max(df_daily$Date_parsed), "day"),
  by   = "day"
)) %>%
  mutate(Season = galapagos_season(Day))


season_intervals <- all_dates %>%
  mutate(
    # If a day is 'Wet', we keep that label; if 'Dry', likewise
    # We'll create an integer ID that increments when the season changes
    season_change_id = cumsum(c(0, diff(as.numeric(as.factor(Season))) != 0))
  ) %>%
  group_by(season_change_id, Season) %>%
  summarize(
    start = first(Day),
    end   = last(Day) + days(1),  # end is exclusive, so add 1 day
    .groups = "drop"
  )


ggplot() +
  # (A) Shaded seasonal rectangles
  geom_rect(
    data = season_intervals,
    aes(
      xmin = start,
      xmax = end,
      ymin = -Inf,
      ymax = Inf,
      fill = Season
    ),
    alpha = 0.2
  ) +
  scale_fill_manual(values = c("Wet" = "lightblue", "Dry" = "lightcoral")) +
  
  # (B) Line + Points for number of ponds sampled each day
  geom_line(
    data = df_daily,
    aes(x = Date_parsed, y = NumberOfPondsSampled),
    color = "black",
    size = 0.8
  ) +
  geom_point(
    data = df_daily,
    aes(x = Date_parsed, y = NumberOfPondsSampled),
    color = "blue",
    size = 2
  ) +
  
  # (C) Formatting
  labs(
    title = "Number of Unique Ponds Sampled Over Time in Galápagos",
    x = "Sampling Date",
    y = "Number of Ponds Sampled",
    fill = "Season"
  ) +
  scale_x_date(
    date_breaks = "1 year", 
    date_labels = "%Y",
    limits = c(
      as.Date("2018-01-01"),  # Adjust if earliest date is earlier
      as.Date("2025-12-31")   # Adjust if you have data through 2025
    )
  ) +
  theme_minimal()+
  
scale_y_continuous(breaks = seq(
  0, max(df_daily$NumberOfPondsSampled, na.rm = TRUE), by = 1))  # Whole number axis


# Next make a summary table:


#--------------------------------------------------
# 1. Read CSV & Initial Cleaning
#--------------------------------------------------

df <- read.csv("Data/Galapagos Water Sample Data_2018-24_most_up_to_date_2025.csv",
               stringsAsFactors = FALSE)
df = df[df$GPS_Pond_name %in% keep,]

#--------------------------------------------------
# 2. Replace "" with NA & Create New Logical Columns
#   (Mud, Macroinvertebrates, Nutrients, YSI, Filters)
#--------------------------------------------------
df <- df %>%
  # Convert "New.Date" to a proper Date
  mutate(
    Date = as.Date(New.Date, format = "%d-%b-%y"),
    Year = format(Date, "%Y")
  ) %>%
  # Convert numeric columns to character temporarily to avoid type mismatch
  mutate(
    across(c("TP..ug.L.", "NPOC..mg.L.", "NO3..ug.L.", "NH4..ug.L.", "PO4..ug.L.",
             "YSI_Temperature", "YSI_mS.ccm", "YSI_us.ccm", "DO.", "DO.mg.L",
             "Chl", "TNTP", "AFDM"),
           as.character)
  ) %>%
  # Replace "" with NA in selected columns
  mutate(
    Mud.weight..g. = na_if(Mud.weight..g., ""),
    Macroinvertebrates = na_if(Macroinvertebrates, ""),
    `TP..ug.L.`  = na_if(`TP..ug.L.`,  ""),
    `NPOC..mg.L.`= na_if(`NPOC..mg.L.`, ""),
    `NO3..ug.L.` = na_if(`NO3..ug.L.`, ""),
    `NH4..ug.L.` = na_if(`NH4..ug.L.`, ""),
    `PO4..ug.L.` = na_if(`PO4..ug.L.`, ""),
    YSI_Temperature = na_if(YSI_Temperature, ""),
    YSI_mS.ccm      = na_if(YSI_mS.ccm, ""),
    YSI_us.ccm      = na_if(YSI_us.ccm, ""),
    `DO.`          = na_if(`DO.`, ""),
    `DO.mg.L`       = na_if(`DO.mg.L`, ""),
    Chl   = na_if(Chl, ""),
    TNTP  = na_if(TNTP, ""),
    AFDM  = na_if(AFDM, "")
  ) %>%
  # Convert these columns back to numeric (if they were originally numeric)
  mutate(
    across(c("TP..ug.L.", "NPOC..mg.L.", "NO3..ug.L.", "NH4..ug.L.", "PO4..ug.L.",
             "YSI_Temperature", "YSI_mS.ccm", "YSI_us.ccm", "DO.", "DO.mg.L",
             "Chl", "TNTP", "AFDM"),
           as.numeric)
  ) %>%
  # Convert "No" in Macroinvertebrates to NA
  mutate(
    Macroinvertebrates = if_else(Macroinvertebrates == "No", NA_character_, Macroinvertebrates)
  ) %>%
  # Existing booleans for Mud & Inverts
  mutate(
    MeasuredMud     = !is.na(Mud.weight..g.),
    MeasuredInverts = !is.na(Macroinvertebrates)
  ) %>%
  # New booleans for Nutrients, YSI, Filters
  mutate(
    # If ANY nutrient column is non-NA, this row had nutrient sampling
    Nutrients = if_any(
      c("TP..ug.L.", "NPOC..mg.L.", "NO3..ug.L.", "NH4..ug.L.", "PO4..ug.L."),
      ~ !is.na(.x)
    ),
    # If ANY YSI column is non-NA, this row had YSI measurement
    YSI = if_any(
      c("YSI_Temperature", "YSI_mS.ccm", "YSI_us.ccm", "DO.", "DO.mg.L"),
      ~ !is.na(.x)
    ),
    # If ANY filter column is non-NA, this row had filter sampling
    Filters = if_any(
      c("Chl", "TNTP", "AFDM"),
      ~ !is.na(.x)
    )
  )

#--------------------------------------------------
# 3. Summarize by Pond (Using GPS_Pond_name)
#--------------------------------------------------
summary_table <- df %>%
  group_by(GPS_Pond_name) %>%
  summarize(
    StartDate = min(Date, na.rm = TRUE),
    EndDate   = max(Date, na.rm = TRUE),
    Longitude = mean(Longitude,na.rm=TRUE),
    Latitude = mean(Latitude,na.rm=TRUE),
    NumberOfSamplingEvents = n(),
    SampledYears = paste(sort(unique(Year)), collapse = ", "),
    MeasuredMudYN     = if_else(any(MeasuredMud,     na.rm = TRUE), "Yes", "No"),
    MeasuredInvertsYN = if_else(any(MeasuredInverts, na.rm = TRUE), "Yes", "No"),
    NutrientsYN       = if_else(any(Nutrients,       na.rm = TRUE), "Yes", "No"),
    YSIYN             = if_else(any(YSI,             na.rm = TRUE), "Yes", "No"),
    FiltersYN         = if_else(any(Filters,         na.rm = TRUE), "Yes", "No"),
    .groups = "drop"
  ) %>%
  # Create a single 'DateRange' column
  mutate(
    DateRange = paste(
      format(StartDate, "%Y-%m-%d"),
      "to",
      format(EndDate,   "%Y-%m-%d")
    )
  ) %>%
  select(
    GPS_Pond_name,
    Longitude,
    Latitude,
    DateRange,
    SampledYears,
    NumberOfSamplingEvents,
    MeasuredMudYN,
    MeasuredInvertsYN,
    NutrientsYN,
    YSIYN,
    FiltersYN
  )

#--------------------------------------------------
# 4. Inspect the Summary Table
#--------------------------------------------------
summary_table_clean = summary_table |> filter(! GPS_Pond_name %in% c('', '_',
                                                                     '-',
                                                                     '3 L.C.',
                                                                     'Poza_vigen')) |>
  distinct(GPS_Pond_name)

summary_table_clean %>%
  kbl(caption = "Summary of Pond Sampling Events") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

summary_table |> 
  kbl(caption = "Summary of Pond Sampling Events") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

write.csv(summary_table, file = '/Users/diegoellis/Desktop/Pond_samples.csv')
#--------------------------------------------------
# 5. Plot: Number of Sampling Events by Pond
#    Rotated/fixed y-axis scale
#--------------------------------------------------
ggplot(summary_table, aes(x = reorder(GPS_Pond_name, -NumberOfSamplingEvents),
                          y = NumberOfSamplingEvents)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Sampling Events by Pond (GPS_Pond_name)",
    x = "Pond",
    y = "Number of Sampling Events"
  ) +
  scale_y_continuous(
    breaks = seq(0, max(summary_table$NumberOfSamplingEvents, na.rm = TRUE), by = 1)
  ) +
  theme_minimal()


# --- --- --- --- --- --- --- --- --- ---
# Find when montemar had no data
# --- --- --- --- --- --- --- --- --- ---

library(tidyverse)
library(lubridate)

montemar = read.csv('Data/Photo_spreadsheet_montemar_all.csv')

# Ensure the Date column is properly formatted
montemar <- montemar %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%y"))  # Adjust format if needed

# Get observed unique dates
observed_dates <- unique(montemar$Date)

# Create a full sequence of dates from min to max observed date
full_date_range <- tibble(Date = seq.Date(
  from = min(observed_dates, na.rm = TRUE),
  to = max(observed_dates, na.rm = TRUE),
  by = "day"
))

range(full_date_range$Date)

# Identify which dates were sampled (1 = Sampled, 0 = Not Sampled)
sampling_status <- full_date_range %>%
  mutate(Sampled = if_else(Date %in% observed_dates, "Sampled", "Not Sampled"))

# Plot the timeline of sampled vs. missing dates with YEAR & MONTH on x-axis
ggplot(sampling_status, aes(x = Date, y = 1, color = Sampled)) +
  geom_point(size = 4, alpha = 0.7) +  # Larger dots to highlight gaps
  scale_color_manual(values = c("Sampled" = "blue", "Not Sampled" = "red")) +  # Color-coded points
  scale_x_date(
    date_breaks = "1 month",  # Show ticks every month
    date_labels = "%b %Y"  # Format as "Jan 2022", "Feb 2022", etc.
  ) +
  labs(
    title = "Timeline of Sampling Dates: Montemar",
    x = "Date (Year-Month)",
    y = "",
    color = "Sampling Status"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),  # Hide Y-axis labels (not needed)
    axis.ticks.y = element_blank(),  # Remove Y-axis ticks
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for better readability
  )

# Find missing dates
missing_dates <- setdiff(full_date_range$Date, observed_dates)

# Identify which dates were sampled (1 = Sampled, 0 = Not Sampled)
sampling_status <- full_date_range %>%
  mutate(Sampled = if_else(Date %in% observed_dates, "Sampled", "Not Sampled"))

# Print Summary of Sampling Range & Gaps
start_date <- min(observed_dates, na.rm = TRUE)
end_date <- max(observed_dates, na.rm = TRUE)
total_days <- length(full_date_range$Date)
missing_days <- length(missing_dates)
sampled_days <- total_days - missing_days
percent_sampled <- round((sampled_days / total_days) * 100, 2)

cat("\n--- Sampling Summary ---\n")
cat("📅 Sampling Period: ", start_date, " to ", end_date, "\n")
cat("📊 Total Days in Period: ", total_days, "\n")
cat("✅ Sampled Days: ", sampled_days, " (", percent_sampled, "%)\n")
cat("❌ Missing Days: ", missing_days, " (", 100 - percent_sampled, "%)\n")


unique(df$GPS_Pond_name)
