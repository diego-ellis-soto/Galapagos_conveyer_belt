# -Look at differneces pond utilization in natural vs artificial and how often do they visit multiple or single ponds 
# To do: get recursion at polygon insteaed of at location

 require(move)
require(recurse)
require(tidyverse)
require(sf)
require(sp)
require(mapview)
require(lubridate)
require(conflicted)
 conflicts_prefer(dplyr::mutate)
conflicts_prefer(dplyr::summarise)

loginStored<-movebankLogin(username="Diego_Ellis", password="Atelopus2!")

all_studies <- getMovebank(entity_type = "study", login=loginStored)

# returns a MoveStack object from the specified study
# tortugas <- getMovebankData(study=2928116, animalName=c("Sir David",
#                                                         "Roberto",
#                                                         'Baronesa',
#                                                         'Herbert',
#                                                         'Steve Devine',
#                                                         'Steve Blake',
#                                                         'Speedy Gonzales'),
#                             login=loginStored, includeExtraSensors=FALSE, 
#                             removeDuplicatedTimestamps=TRUE, deploymentAsIndividuals=FALSE
# ) 
# 
# # Keep only records for the years of the study:
# tortugas$year = year(tortugas$timestamp)
# nrow(tortugas)
# tortugas = tortugas[tortugas$year > 2017,]
# nrow(tortugas)

# Read the .csv
inters_tort = read.csv('Outdir/tortoise_names_in_ponds.csv')[,'x']

tortugas <- getMovebankData(study=2928116, animalName=
                              inters_tort,
                            login=loginStored,
                            includeExtraSensors=FALSE, 
                            removeDuplicatedTimestamps=TRUE,
                            deploymentAsIndividuals=FALSE
) 

tortugas$year = year(tortugas@timestamps)
tortugas$id = tortugas@trackId
# Keep only records for the years of the study:
tortugas = tortugas[tortugas$year > 2017,]
# Up next load the pond locations:
tortoise_sf_ponds_df = data.frame(tortugas) |>
  dplyr::mutate(x = location_long, y = location_lat,t = timestamp )|> dplyr::select(x,y,t, id)

tortoise_sf_ponds_df$t = ymd_hms(tortoise_sf_ponds_df$t)



# # Remove some of the outliers:
# source('/Users/diegoellis/projects/Phd_Chapter_1/Tortoise_data/Galapagos_2019/NIDOS2019/Datos/Algoritmo_nidos/generic/lib.r')
# keep<-0.95
# # keep<-0.90
# message(paste("Removing outliers: threshold ", (1-keep)*100, " pct.", sep=""))
# 
# # Function split my moveobject  and apply a function to each tortoise
# all_tortuga_clean <- list()
# all_tortuga_clean <- unlist(lapply(split(tortugas),  
#                                    function(GPS) # GPS is a move object
#                                    {
#                                      oI <- OffIndex(GPS, time.adj=T)
#                                      GPS$OI <- oI
#                                      if(any(oI>keep, na.rm=T))
#                                      {
#                                        outlier <- coordinates(GPS[which(oI>keep),])
#                                        message(paste("Removed", nrow(outlier), "outlier location(s)."))
#                                      }else{
#                                        message("No outliers found")
#                                      }
#                                      GPS_clean <- GPS[which(oI<=keep),]
#                                      #  GPS_clean$speed <- c(NA, speed(all_tortuga_clean[[1]]))
#                                      
#                                    }
# )
# )
# all_tortuga_clean <- moveStack(all_tortuga_clean)
# all_tortuga_clean_df <- as.data.frame(all_tortuga_clean)
# 
# all_tortuga_clean_df$id = all_tortuga_clean_df$local_identifier

# tortoise_sf_ponds_df = data.frame(all_tortuga_clean_df) |> dplyr::mutate(x = location_long, y = location_lat,t = timestamp )|> dplyr::select(x,y,t, id)

tortoise_sf_ponds_df$t = ymd_hms(tortoise_sf_ponds_df$datetime)

# Load pond data: 



# ponds = read.csv('/Users/diegoellis/projects/Ponds_2024/Pond_parameter_data/Galapagos Water Sample Data_2018-24.csv') |> drop_na(Tourist_lagoon, Longitude, Latitude)
# 
# pond.spdf <- SpatialPointsDataFrame(coords= ponds[,c('Longitude', 'Latitude')], data = ponds, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) # For the Points
# 
utm_gal <- "+proj=utm +zone=15 +south +datum=WGS84 +no_defs +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
# sir_david_sp <- spTransform(sir_david, utm_gal)
# roberto_sp <- spTransform(roberto, utm_gal)
# tortugas_sp = spTransform(tortugas, utm_gal)

ponds_all = st_read('Outdir/buffered_ponds.shp')
# mapview(ponds_all[ponds_all$GPS_Pn_ =='P_CM',])

ponds_all = ponds_all %>%
  dplyr::mutate(centroid = st_centroid(geometry)) %>%
  dplyr::mutate(x = st_coordinates(centroid)[, 1],
                y = st_coordinates(centroid)[, 2]) %>%
  dplyr::select(-centroid)  # remove centroid column if you don’t need it

# pond.spdf_UTM <- spTransform(ponds_all, utm_gal)
pond.spdf_UTM_df <- data.frame(ponds_all)

ponds_all_spdf = as(ponds_all, 'Spatial')


tortoise_sf_ponds_df <- tortoise_sf_ponds_df 
# %>%
#   dplyr::rename(datetime = t)

# Make sure it's a POSIXct:
tortoise_sf_ponds_df$datetime <- as.POSIXct(tortoise_sf_ponds_df$datetime)

ponds_sf_wgs84 <- sf::st_transform(ponds_all, crs = 4326)

# rec_in_ponds <- getRecursionsInPolygon(tortoise_sf_ponds_df, ponds_sf_wgs84)




# pond_uniques_sp <- SpatialPointsDataFrame(coords= pond_uniques[,c('x', 'y')], data = pond_uniques, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) # For the Points

myprj <- paste0("+proj=laea +lat_0=", round(median(tortoise_sf_ponds_df$y)),' +lon_0=', round(median(tortoise_sf_ponds_df$x)),' +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0')

tortoises_sp = SpatialPointsDataFrame(coords= tortoise_sf_ponds_df[,c('x', 'y')], data = tortoise_sf_ponds_df, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) # For the Points

tortoises_sp_laea <- spTransform(tortoises_sp, myprj)
# roberto_sp_laea$t = roberto_sp_laea$timestamp
tortoises_sp_laea_sf <- st_as_sf(tortoises_sp_laea)


# pond_uniques_sp <- SpatialPointsDataFrame(coords= pond_uniques[,c('x', 'y')], data = pond_uniques, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) # For the Points

pond_uniques_sp = ponds_all_spdf


pond_uniques_sp_laea <- spTransform(pond_uniques_sp, myprj)
pond_uniques_sp_laea_sf <- st_as_sf(pond_uniques_sp_laea)


pond_uniques_sp_laea_df <- data.frame(pond_uniques_sp)|> dplyr::select(x,y,GPS_Pn_)  |> dplyr::select(x,y, GPS_Pn_)


pond_coords <- pond_uniques_sp_laea_df[, c("x", "y")]


tortoise_sf_ponds_df = tortoise_sf_ponds_df |> dplyr::select(x,y,t,id)

# This one works: Do without Laguna Chato and Cerro Meso and Steve Di Baen !!!!
require(recurse)
recAtLocs <- getRecursionsAtLocations(tortoise_sf_ponds_df, pond_coords, radius = 0.00004486)

summary(recAtLocs$revisitStats$timeInside)



par(mfrow=c(1,1))
plot(recAtLocs, tortoise_sf_ponds_df, 
     alpha = 1, pch = 17, xlim = c(-90.2,-90.45), ylim = c(-0.75, -0.60))
points(tortoise_sf_ponds_df$x, tortoise_sf_ponds_df$y, pch = ".", col = "gray50")
points(recAtLocs$revisitStats$x, recAtLocs$revisitStats$y, col = 'red')

# drawCircle(-0.7, -90.37, 2)
drawCircle(-90.20, -0.7, 0.00004486, lwd = 4)



# pond_uniques <- ponds |>
#   drop_na(Latitude, Longitude) |>
#   distinct(Longitude, Latitude, .keep_all = TRUE) |>
#   distinct(GPS_Pond_name, .keep_all = TRUE) |>
#   dplyr::mutate(x = Longitude, 
#                 y = Latitude) |>
#   # distinct(GPS_Pond_name) |>
#   select(x, y, GPS_Pond_name)

# Some have positive insted of negarive longitude and latitude
# pond_uniques$x <- ifelse(pond_uniques$x > 0, -pond_uniques$x, pond_uniques$x)
# pond_uniques$y <- ifelse(pond_uniques$y > 0, -pond_uniques$y, pond_uniques$y)


# Hasta aca llegue ####

# 
# rec <- getRecursionsAtLocations( tortoise_sf_ponds_df, pond_uniques_sp_laea_df, radius = 0.00004486 )
# 
# rec <- getRecursionsInPolygon( tortoise_sf_ponds_df, pond_uniques_sp_laea_df, radius = 0.00004486 )
# 
# rec <- getRecursionsAtLocations( tortoise_sf_ponds_df, pond_uniques_sp_laea_df, radius = 8.972e-05 )
# 
# par(mfrow=c(1,1))
# plot(rec, pond_uniques_sp_laea_df, 
#      alpha = 1, pch = 17, xlim = c(-90.2,-90.45), ylim = c(-0.75, -0.60))
# points(tortoise_sf_ponds_df$x, tortoise_sf_ponds_df$y, pch = ".", col = "gray50")
# points(rec$revisitStats$x, rec$revisitStats$y, col = 'red')
# 
# # drawCircle(-0.7, -90.37, 2)
# drawCircle(-90.37, -0.7, 0.00004486, lwd = 4)
# 
# summary(rec$revisitStats$timeInside)
# 
# a = rec$revisitStats |> filter(id == 'Roberto') 
# 
# summary(a$timeInside)
# 
# revisit_stats = rec$revisitStats

revisit_stats = recAtLocs$revisitStats

require(plyr)
ddply(revisit_stats,  'id', function(x){
summary(x$timeInside)  
})


IQR(revisit_stats$timeInside)
mean(revisit_stats$timeInside)
median(revisit_stats$timeInside)

summary_stats_by_id <- revisit_stats |> 
  dplyr::group_by(id) |> 
  dplyr::summarize(
    median_time_inside = median(timeInside, na.rm = TRUE),
    mean_time_inside = mean(timeInside, na.rm = TRUE),
    IQR_time_inside = IQR(timeInside, na.rm = TRUE),
    sd_time_inside = sd(timeInside, na.rm = TRUE),
    min_time_inside = min(timeInside, na.rm = TRUE),
    max_time_inside = max(timeInside, na.rm = TRUE)
  )

# Print summary stats by id
print(summary_stats_by_id)

# Create a ggplot visualization of 'time_inside' distribution for each tortoise
ggplot(data = recAtLocs$revisitStats, aes(x = timeInside, fill = id)) + 
  geom_histogram(binwidth = 10, color = 'black', alpha = 0.7) + 
  labs(title = "Distribution of 'Time Inside' for Different Tortoises",
       x = "Time Inside",
       y = "Count") +
  facet_grid(~ id, scales = "free") +  # Separate plots for each tortoise
  theme_minimal()
  

# mapview(pond_uniques_sp)

par(mfrow=c(2,2))
# How often does the tortoises revisits the ponds
hist(recAtLocs$revisits, xlab = "Revisitations", main = "", col = "gray70")
# How much time does roberto the torotise revisits the pond?
hist(recAtLocs$revisitStats$timeInside, xlab = "Time inside (h)", main = "", col = "gray70")


# Each row gives the metrics for one visit. The id specifies the animal making the visit. The x and y locations correspond to the location of the focal coordiate (the center of the circle). The focal coordinate is also specified by the coordIdx column, which gives the index of the focal coordinate into the data frame (either the movement trajectory or the list of specified locations). The visitIdx gives the index of which visit this is to the focal coordinate (so the number of revisits corresponds to the highest visit index). 

# The entrance time and exit time are the times calculated for crossing the radius by interpolating between the points inside and outside the radius. Finally, the timeSinceLastVisit is NA for the first visit and then calculated as the time outside the radius between visits for subsequent visits.
# 
# These additional metrics can be examined on a per visit basis. For example, one could examine the correlation of visit length with visit entrance time. In this case there does not appear to be a strong pattern.

# Giant tortoises
boxplot(recAtLocs$revisitStats$timeInside ~ as.numeric(format(recAtLocs$revisitStats$entranceTime, "%H")),
        xlab = "Entrance time", ylab = "Visit duration (h)")

library(suncalc)
data <- recAtLocs$revisitStats
lat <- -0.5  
lon <- -90.5 
# Extract unique dates from your data
dates <- unique(as.Date(data$entranceTime))
# Get sunrise/sunset times for these dates
sun_times <- getSunlightTimes(date = dates, lat = lat, lon = lon, keep = c("sunrise", "sunset"))
# Merge sun_times back with your data
data <- data %>%
  mutate(date = as.Date(entranceTime)) %>%
  left_join(sun_times, by = c("date" = "date"))

data <- data %>%
  mutate(hour = as.numeric(format(entranceTime, "%H")),
         time_of_day = case_when(
           entranceTime <= sunrise ~ "before sunrise",
           entranceTime > sunrise & entranceTime <= sunset ~ "daytime",
           entranceTime > sunset ~ "after sunset"
         ))

summary_stats <- data %>%
  dplyr::group_by(time_of_day) %>%
  dplyr::summarise(mean_duration = mean(timeInside, na.rm = TRUE),
            sd_duration = sd(timeInside, na.rm = TRUE),
            median_duration = median(timeInside, na.rm = TRUE),
            count = n())


# Another metric that is calculated is time since last visit. This can also be examined to look for patterns in intervisit interval or among locations.

par(mfrow = c(1, 2), mar = c(4, 4, 1, 1))
hist(recAtLocs$revisitStats$timeSinceLastVisit,
     xlab = "Time since last visit (h)", main = "")


# rec = rec[rec$revisitStats$timeSinceLastVisit < 1000]

plot(recAtLocs$revisitStats$timeSinceLastVisit, recAtLocs$revisitStats$timeInside,
     xlab = "Time since last visit (h)", ylab = "Time inside (h)")
lines(lowess(x = recAtLocs$revisitStats$timeSinceLastVisit, y = recAtLocs$revisitStats$timeInside, delta = 0.01 * diff(range(recAtLocs$revisitStats$timeSinceLastVisit, na.rm = TRUE))), col = "red")

# Check ponds revisits:
tort_sp_stats = recAtLocs$revisitStats

tort_sp_stats = tort_sp_stats[tort_sp_stats$timeInside > 0.1,] # to exclude very short 

# TIme of revisitatipns
tort_sp_stats$local.entranceTime = with_tz(tort_sp_stats$entranceTime, tz = "Pacific/Galapagos")
tort_sp_stats$local.exitTime = with_tz(tort_sp_stats$exitTime, tz = "Pacific/Galapagos")
tort_sp_stats$year = year(tort_sp_stats$entranceTime)
tort_sp_stats$doy_enter = yday(tort_sp_stats$local.entranceTime)
tort_sp_stats$hour_enter = hour(tort_sp_stats$local.entranceTime)
tort_sp_stats$time_enter = hour(tort_sp_stats$local.entranceTime) + minute(tort_sp_stats$local.entranceTime) / 60 + second(tort_sp_stats$local.entranceTime) / 60 / 60
tort_sp_stats$time_exit = hour(tort_sp_stats$local.exitTime) + minute(tort_sp_stats$local.exitTime) / 60 + second(tort_sp_stats$local.exitTime) / 60 / 60
tort_sp_stats$overnight = factor(as.logical(yday(tort_sp_stats$local.exitTime) - yday(tort_sp_stats$local.entranceTime)))
levels(tort_sp_stats$overnight) = c("no", "yes")
tort_sp_stats$site = paste("site", tort_sp_stats$coordIdx) # for plotting




# ---  ---  ---  ---  ---  ---  ---  ---  ---  --- 
# inter- and intra-annual visit patterns across 5 sites 
# ---  ---  ---  ---  ---  ---  ---  ---  ---  --- 
# plot entrace time by year and day of year
ttime = ggplot(tort_sp_stats, aes(x = doy_enter)) + 
  geom_histogram(binwidth = diff(range(tort_sp_stats$doy_enter))/7, color = "darkgray", fill = "gray") +
  facet_grid(site ~ year) + 
  xlab("visit day of year") + ylab("revisit frequency") + 
  theme_classic()
print(ttime)

ggsave("Outdir/tortoise_visit_histogram.png", plot = ttime,
       width = 20, height = 40, units = "in", dpi = 300)

# ---  ---  ---  ---  ---  ---  ---  ---  ---  --- 
# visit duration by entrance time of day
# ---  ---  ---  ---  ---  ---  ---  ---  ---  --- 
# We could even split this by season

binhour <- ggplot(tort_sp_stats, aes(x = time_enter, y = timeInside)) +
  geom_density2d(color = "black") +
  geom_point(alpha = 0.2, aes(color = overnight)) +
  scale_color_brewer(palette = "Dark2", name = 'Overnight') +
  ylim(0, 24) +
  xlab("Visit entrance time") +
  ylab("Visit duration (hours)") +
  ggtitle("Evidence for overnight stays for\nTortoises") +
  theme_classic() +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    axis.line.x = element_line(colour = 'black', size = 0.6),
    axis.line.y = element_line(colour = 'black', size = 0.6),
    legend.justification = c(0, 1),
    legend.position = c(0.02, 0.98)
  )

print(binhour)

ggsave("Outdir/overnight_stays.png", plot = binhour,
       width = 16, height = 20, units = "in", dpi = 300)


# data <- recAtLocs$revisitStats

# Figure of the paper:

usage_ponds = ggplot(data, aes(x = hour, y = timeInside)) +
  geom_boxplot(aes(group = factor(hour)), fill = "white", alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue2", linetype = "solid", size = 1) +
  labs(x = "Entrance time (hour)", y = "Visit duration (h)") +
  theme_minimal()+ggtitle('Daily patterns of giant tortoise pond visitation')+
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    axis.line.x = element_line(colour = 'black', size = 0.6),
    axis.line.y = element_line(colour = 'black', size = 0.6)
  )


ggsave("Outdir/usage_ponds.png", plot = usage_ponds,
       width = 16, height = 20, units = "in", dpi = 300)


# data <- recAtLocs$revisitStats

ggplot(tort_sp_stats, aes(x = hour_enter, y = timeInside)) +
  geom_boxplot(aes(group = factor(hour_enter), fill = overnight), alpha = 0.7, color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "blue4", linetype = "solid", size = 1) +
  scale_fill_manual(values = c("FALSE" = "lightgray", "TRUE" = "tomato"),
                    name = "Overnight stay") +
  labs(
    x = "Entrance time (hour)",
    y = "Visit duration (hours)",
    title = "Daily patterns of giant tortoise pond visitation"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    legend.position = "top"
  )

ggsave('/Users/diegoellis/projects/Ponds_2024/Figures/Tortoise_pond_visit_duration.png', width = 10, height = 8)


# Suppose we have tagging data from another individual, `wren`, released at the same time and location as `martin`. Here we plot `martin` in red and `wren` in dark blue.
# Recursions can also be calculated on the population level. If multiple individuals are passed to `getRecursions()` or `getRecursionsAtLocations()` (specified either through the `id` column of a data frame or using a `MoveStack` object), then the revisits at each location are calculated across all individuals. This can be useful for finding locations that are important across the population (e.g., watering holes or foraging areas) versus to a single individual (e.g., dens).

popvisit = getRecursions(tortoise_sf_ponds_df, 6) 
head(popvisit$revisitStats)

roberto <- tortugas[[3]]
popvisit = getRecursions(roberto, 0.0001) 

pdf('/Users/diegoellis/projects/Ponds_2024/Figures/Spatial_robrto_revisitation.pdf')
plot(popvisit, tortoise_sf_ponds_df, legendPos = c(-90.37, -0.7),
     main = 'Revisitation of ponds')
points(pond_uniques_sp, cex = 1, col = 'black')
dev.off() 

plot(popvisit, tortoise_sf_ponds_df, legendPos = c(-90.40, -0.7))

popvisit = getRecursions(tortugas, 0.0001) 

# We can examine how the number of revisitations changes with the changing radius size. Here we used a linear sequence of radii, though a linear sequence of areas may be a better choice in many situations. The radii go from 0.5 to 20 in increments of 0.25. We use such large radii to illustrate what happens, but it does not actually make sense to use a radius that anywhere approaches the size of the study area.

radii = seq(from = 0.5, to = 20, by = 0.25)
visits = NULL

for (i in 1:length(radii))
{
  visits[[i]] = getRecursions(martin, radius = radii[i])
}

plot(x = radii, y = lapply(visits, function(x) mean(x$revisits)), pch = 16, xlab = "radius", ylab = "mean revisits")

# Check ponds revisits:
tort_sp_stats = rec$revisitStats

tort_sp_stats = tort_sp_stats[tort_sp_stats$timeInside > 0.1,] # to exclude very short 

# TIme of revisitatipns
tort_sp_stats$local.entranceTime = with_tz(tort_sp_stats$entranceTime, tz = "Pacific/Galapagos")
tort_sp_stats$local.exitTime = with_tz(tort_sp_stats$exitTime, tz = "Pacific/Galapagos")
tort_sp_stats$year = year(tort_sp_stats$entranceTime)
tort_sp_stats$doy_enter = yday(tort_sp_stats$local.entranceTime)
tort_sp_stats$hour_enter = hour(tort_sp_stats$local.entranceTime)
tort_sp_stats$time_enter = hour(tort_sp_stats$local.entranceTime) + minute(tort_sp_stats$local.entranceTime) / 60 + second(tort_sp_stats$local.entranceTime) / 60 / 60
tort_sp_stats$time_exit = hour(tort_sp_stats$local.exitTime) + minute(tort_sp_stats$local.exitTime) / 60 + second(tort_sp_stats$local.exitTime) / 60 / 60
tort_sp_stats$overnight = factor(as.logical(yday(tort_sp_stats$local.exitTime) - yday(tort_sp_stats$local.entranceTime)))
levels(tort_sp_stats$overnight) = c("no", "yes")
tort_sp_stats$site = paste("site", tort_sp_stats$coordIdx) # for plotting


# ---  ---  ---  ---  ---  ---  ---  ---  ---  --- 
# inter- and intra-annual visit patterns across 5 sites 
# ---  ---  ---  ---  ---  ---  ---  ---  ---  --- 
# plot entrace time by year and day of year
ttime = ggplot(tort_sp_stats, aes(x = doy_enter)) + 
  geom_histogram(binwidth = diff(range(tort_sp_stats$doy_enter))/7, color = "darkgray", fill = "gray") +
  facet_grid(site ~ year) + 
  xlab("visit day of year") + ylab("revisit frequency") + 
  theme_classic()
print(ttime)

# ---  ---  ---  ---  ---  ---  ---  ---  ---  --- 
  # visit duration by entrance time of day
  # ---  ---  ---  ---  ---  ---  ---  ---  ---  --- 
  # This should be split by season?
  binhour = ggplot(tort_sp_stats, aes(x = time_enter, y = timeInside)) +
  geom_density2d(color = "black") + ylim(0, 24) + 
  scale_color_brewer(palette = "Dark2", name = 'overnight') +
  xlab("visit entrance time") + ylab("visit duration (h)") +
  geom_point(alpha = 0.2, aes(col = overnight)) +
  theme_classic() + theme(
    axis.line.x = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
    axis.line.y = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
    legend.justification = c(0, 1), legend.position = c(0, 1))+ggtitle('Evidence for overnight stays for \n  Tortoises')
print(binhour)

data <- rec$revisitStats
# Figure of the paper:
ggplot(data, aes(x = hour, y = timeInside)) +
  geom_boxplot(aes(group = factor(hour)), fill = "white", alpha = 0.7) +
  geom_smooth(method = "loess", se = FALSE, color = "blue2", linetype = "solid", size = 1) +
  labs(x = "Entrance time (hour)", y = "Visit duration (h)") +
  theme_minimal()+ggtitle('Daily patterns of giant tortoise pond visitation')

ggsave('/Users/diegoellis/projects/Ponds_2024/Figures/Tortoise_pond_visit_duration.png', width = 10, height = 8)


# Assuming 'data' is your rec$revisitStats data frame
data_v2 = data  %>%
  mutate(entranceTime = as.POSIXct(entranceTime),
         month = month(entranceTime, label = TRUE))
# Assuming 'data' is your rec$revisitStats
# data <- rec$revisitStats

# Convert entranceTime to POSIXct if it isn't already, then add a month column
data <- data %>%
  mutate(entranceTime = as.POSIXct(entranceTime),
         month = month(entranceTime, label = TRUE))

# Plot with boxplot and a separate geom_smooth by month
p <- ggplot(data, aes(x = hour, y = timeInside)) +
  geom_boxplot(aes(group = factor(hour)), fill = "white", alpha = 0.7) +
  # Add a loess smooth for each month; color mapping by month
  geom_smooth(aes(color = month), method = "loess", se = FALSE,
              linetype = "solid", size = 1) +
  labs(x = "Entrance time (hour)", y = "Visit duration (h)") +
  ggtitle("Daily patterns of giant tortoise pond visitation") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14),
    axis.line.x = element_line(colour = 'black', size = 0.6),
    axis.line.y = element_line(colour = 'black', size = 0.6)
  )

print(p)

# Optionally, save the plot to a file:
ggsave('Outdir/Tortoise_pond_visit_duration_months.png',
       plot = p, width = 10, height = 8, dpi = 300)

# ggplot(tort_sp_stats, aes(x = round(time_enter), y = round(timeInside))) +
#   geom_boxplot(aes(fill = round(time_enter))) + ylim(0, 24) +
#   scale_fill_brewer(palette = "Dark2", name = 'overnight') +
#   xlab("Visit Entrance Time") + ylab("Visit Duration (h)") +
#   theme_classic() +
#   theme(
#     axis.line.x = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
#     axis.line.y = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
#     axis.text.x = element_text(angle = 90, hjust = 1),  # Rotate x-axis labels for better readability
#     legend.justification = c(0, 1), 
#     legend.position = c(0, 1)
#   ) +
#   ggtitle('Evidence for Overnight Stays for Tortoises')

# ggplot(tort_sp_stats, aes(x = time_enter, y = timeInside)) +
#   geom_point(color = "black") + ylim(0, 24) + 
#   scale_color_brewer(palette = "Dark2", name = 'overnight') +
#   xlab("visit entrance time") + ylab("visit duration (h)") +
#   geom_point(alpha = 0.2, aes(col = overnight)) +
#   theme_classic() + theme(
#     axis.line.x = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
#     axis.line.y = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
#     legend.justification = c(0, 1), legend.position = c(0, 1))+ggtitle('Evidence for overnight stays for \n  Tortoises')

library(ggplot2)
library(lubridate)

# Assuming you have a data frame 'df' with a 'datetime' column

ggplot(tort_sp_stats, aes(x = doy_enter, y = timeInside)) +
   geom_density2d(color = "black") + ylim(0, 24) + 
   scale_color_brewer(palette = "Dark2", name = 'overnight') +
   xlab("visit entrance day of year") + ylab("visit duration (h)") +
   geom_point(alpha = 0.2, aes(col = overnight)) +
   theme_classic() + theme(
     axis.line.x = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
     axis.line.y = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
     legend.justification = c(1, 1), legend.position = c(1,1)) +
   facet_grid(~ site)

# does visit duration vary through breeding season?
# ---  ---  ---  ---  ---  ---  ---  ---  ---  --- 
bindoy = ggplot(roberto_sp_stats, aes(x = doy_enter, y = timeInside)) +
  geom_density2d(color = "black") + ylim(0, 24) + 
  scale_color_brewer(palette = "Dark2", name = 'overnight') +
  xlab("visit entrance day of year") + ylab("visit duration (h)") +
  geom_point(alpha = 0.2, aes(col = overnight)) +
  theme_classic() + theme(
    axis.line.x = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
    axis.line.y = element_line(colour = 'black', size = 0.5, linetype = 'solid'),
    legend.justification = c(1, 1), legend.position = c(1,1)) +
  facet_grid(~ site)
print(bindoy)

# if(sum(rec$revisits)>1){print(paste0(unique(tortoise_sf_ponds_df$id), ' has used a pond total of ', sum(rec$revisits), ' times'))}else(print(paste0('No pond used by '), unique(tortoise_sf_ponds_df$id)))
model <- lm(timeInside ~ hour, data = data)

data$predicted_duration <- predict(model, newdata = data)

# Plot the predicted values against actual values
ggplot(data, aes(x = hour, y = timeInside)) +
  geom_point(color = "gray", alpha = 0.6) +  # Actual visit durations
  geom_line(aes(y = predicted_duration), color = "red", size = 1) +  # Predicted visit duration
  labs(x = "Entrance time (hour)", y = "Visit duration (h)", title = "Predicted Visit Duration by Hour") +
  theme_minimal()

mapview(pond)




library(dplyr)
library(ggplot2)

# Example: Filter for one tortoise ("Roberto") and one site ("Montemar").
# Adjust these if your actual data has different values for id/site.
df_roberto <- tort_sp_stats %>%
  # dplyr::filter(id == "Roberto", site == "Montemar") %>%
  dplyr::group_by(hour_enter) %>%
  dplyr::summarize(total_hours = sum(timeInside, na.rm = TRUE)) %>%
  ungroup()

# Create the bar plot
ggplot(df_roberto, aes(x = hour_enter, y = total_hours)) +
  geom_col(fill = "gray", color = "darkgray") +
  scale_x_continuous(breaks = seq(0, 24, 2),  # tick marks every 2 hours
                     limits = c(0, 24), 
                     expand = c(0, 0)) +
  labs(
    title = "Roberto the Tortoise in Montemar",
    x = "hour of day",
    y = "total hours"
  ) +
  theme_classic(base_size = 14)

df_roberto <- tort_sp_stats %>%
  # dplyr::filter(id == "Roberto", site == "Montemar") %>%
  dplyr::group_by(hour_enter) %>%
  dplyr::summarize(total_hours = sum(timeInside, na.rm = TRUE)) %>%
  ungroup()

# Create the bar plot
ggplot(df_roberto, aes(x = hour_enter, y = total_hours)) +
  geom_col(fill = "gray", color = "darkgray") +
  scale_x_continuous(breaks = seq(0, 24, 2),  # tick marks every 2 hours
                     limits = c(0, 24), 
                     expand = c(0, 0)) +
  labs(
    title = "Roberto the Tortoise in Montemar",
    x = "hour of day",
    y = "total hours"
  ) +
  theme_classic(base_size = 14)


df_roberto <- tort_sp_stats %>%
  # dplyr::filter(id == "Roberto", site == "Montemar") %>%
  dplyr::group_by(hour_enter) %>%
  dplyr::summarize(mean_hours = mean(timeInside, na.rm = TRUE)) %>%
  ungroup()

# Create the bar plot
ggplot(df_roberto, aes(x = hour_enter, y = mean_hours)) +
  geom_col(fill = "gray", color = "darkgray") +
  scale_x_continuous(breaks = seq(0, 24, 2),  # tick marks every 2 hours
                     limits = c(0, 24), 
                     expand = c(0, 0)) +
  labs(
    title = "Roberto the Tortoise in Montemar",
    x = "hour of day",
    y = "mean hours"
  ) +
  theme_classic(base_size = 14)




library(dplyr)
library(lubridate)

df_seasonal <- tort_sp_stats %>%
  # Make sure entranceTime is POSIXct or Date
  dplyr::mutate(entranceTime = as.POSIXct(entranceTime),
         month_num = month(entranceTime),
         season = ifelse(month_num %in% c(12, 1, 2, 3, 4, 5),
                         "wet_hot", 
                         "dry_cool")) %>%
  # Group by ID, season, hour of entrance
  dplyr::group_by(id, season, hour_enter) %>%
  # Sum total timeInside
  dplyr::summarize(mean_hours = mean(timeInside, na.rm = TRUE), .groups = "drop",
                   total_hours = sum(timeInside, na.rm = TRUE)
                   ) 
  

library(ggplot2)

ggplot(df_seasonal, aes(x = hour_enter, y = mean_hours)) +
  geom_col(fill = "gray", color = "darkgray") +
  scale_x_continuous(breaks = seq(0, 24, 2),
                     limits = c(0, 24),
                     expand = c(0, 0)) +
  labs(
    x = "Hour of day",
    y = "Total hours",
    title = "Tortoise Pond Visits by Season and Individual"
  ) +
  facet_grid(season ~ id) +
  theme_classic(base_size = 14)

df_seasonal[df_seasonal$id =='Roberto',]
seasonal_recurse = ggplot(df_seasonal, aes(x = hour_enter, y = mean_hours)) +
  geom_col(fill = "gray", color = "darkgray") +
  scale_x_continuous(breaks = seq(0, 24, 2),
                     limits = c(0, 24),
                     expand = c(0, 0)) +
  labs(
    x = "Hour of day",
    y = "Mean hours",
    title = "Tortoise Pond Visits by Season and Individual"
  ) +
  facet_grid(~season) +
  theme_classic(base_size = 14)+
  geom_smooth(method = "loess", se = FALSE, color = "blue2", linetype = "solid", size = 1)

ggsave("Outdir/seasonal_recurse.png", plot = seasonal_recurse,
       width = 20, height = 40, units = "in", dpi = 300)


# Now for roberto
roberto_total_hours = ggplot(df_seasonal[df_seasonal$id =='Roberto',], aes(x = hour_enter, y = total_hours)) +
  geom_col(fill = "gray", color = "darkgray") +
  scale_x_continuous(breaks = seq(0, 24, 2),
                     limits = c(0, 24),
                     expand = c(0, 0)) +
  labs(
    x = "Hour of day",
    y = "Total hours",
    title = "Tortoise Pond Visits by Season and Individual"
  ) +
  # facet_grid(~season) +
  theme_classic(base_size = 14)+
  geom_smooth(method = "loess", se = FALSE, color = "blue2", linetype = "solid", size = 1)

ggsave("Outdir/roberto_total_hours.png", plot = roberto_total_hours,
       width = 20, height = 40, units = "in", dpi = 300)


"Outdir/overnight_stays.png"
