# --- --- --- --- --- --- --- --- --- --- --- ---
# 
# Get rain fall data for Location of Montemar Pond: Local data and remotely sensed data
#
# --- --- --- --- --- --- --- --- --- --- --- ---

require(philentropy)
library(tidycensus)
library(tidyverse)
library(dismo)
library(dplyr)
library(sf)
require(lubridate)
require(rgbif)
require(stringi)
library(sf)
library(spData)
require(leaflet)
library(wesanderson)
require(gridExtra)
require(patchwork)
library(dplyr)
require(climateR)
library(tidyr)
library(alphahull)
require(LaplacesDemon)
conflicted::conflicts_prefer(dplyr::filter)
conflicted::conflicts_prefer(dplyr::select)

#
df = data.frame(Longitude = -90.3929,
          Latitude = -0.68196,
          Site = 'Montemar')
		
#
df_sp <- SpatialPointsDataFrame(coords = df[, c("Longitude", "Latitude")],
                                     data = df,
                                     proj4string = CRS("+proj=longlat +datum=WGS84"))


pond_centroid <- df_sp %>% 
st_as_sf() |>
  # convert to projected coord system for better centroid
  st_centroid() %>%
  st_transform(4226)


# Get monthly precipitation from terra clim for montemar pond
tmp = getTerraClim(
  pond_centroid,
  varname = c('tmax','tmin', 'ppt'),
  startDate = '2021-01-01',
  endDate = '2023-12-31',
  verbose = FALSE,
  dryrun = FALSE
) 
tmp$month = month(tmp$date)
tmp$year = year(tmp$date)
ggplot(tmp, aes(x = date, y = ppt_total)) + geom_point()

tmp |> dplyr::filter(year == 2021)
tmp |> dplyr::filter(year == 2023) |> ggplot(aes(x = date, y = ppt_total)) + geom_point()
# Load local precipitation data:
rain_2021 = read.csv('/Users/diegoellis/projects/Ponds_2024/Precipitation_pond/Rainfall_2021_GTMEP weather stations.csv')
# colnames(rain_2021) <- trimws(colnames(rain_2021))

# Convert all columns except 'station' to numeric, coercing any non-numeric values to NA
rain_2021[-1] <- lapply(rain_2021[-1], function(x) (as.character(x)))

df_long <- rain_2021 %>%
  pivot_longer(cols = -station, names_to = "date", values_to = "value") %>%
  drop_na(value) |> dplyr::filter(station == 'LC300')

df_long$date = gsub('X','', df_long$date)
df_long$date = sub("\\..*", "", df_long$date)
nchar(df_long$date)
df_long[nchar(df_long$date) == 7,]$date = paste0(0, df_long[nchar(df_long$date) == 7,]$date)
df_long$time = mdy(df_long$date)
ggplot(df_long, aes(x = time, y = value)) + geom_point()

# Create date string in the format "yyyy-mm-dd"
date <- paste(year, month, day, sep = "-")
# For 2021: 