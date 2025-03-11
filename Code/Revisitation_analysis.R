# To do: Fix - and + long/lat ponds 
# Re-run recursive
# Take from other codes: 
# Do a revisitatin analysis for all tortoises
# for recurse add large buffer around pond of laguna chato

require(viridis)
require(sp)
require(raster)
require(tidyverse)
require(move)
require(sf)
require(rgeos)
require(rgdal)
require(maptools)
require(recurse)
require(mapview)
require(rworldmap)
require(lubridate)
require(ggmap)
require(recurse)

calculateTimeOfDay = function(locations, datetime)
{
  require(maptools)
  
  sunrise = sunriset(locations, dateTime = datetime, direction = "sunrise", POSIXct.out = TRUE)$time
  sunset = sunriset(locations, dateTime = datetime, direction = "sunset", POSIXct.out = TRUE)$time
  
  light = ifelse( datetime < sunrise, "night",
                  ifelse( datetime < sunset, "day", "night" ) )
  
  return(light)
}
require(tidyverse)
require(move)
require(recurse)
conflicts_prefer(raster::extract)
loginStored<-movebankLogin(username="XXX", password="XXX!")

all_studies <- getMovebank(entity_type = "study", login=loginStored)

# returns a MoveStack object from the specified study
# tortugas <- getMovebankData(study=2928116, animalName=c("Sir David","Roberto"),
#                             login=loginStored, includeExtraSensors=FALSE, 
#                             removeDuplicatedTimestamps=TRUE, deploymentAsIndividuals=FALSE
# ) 
# 
# roberto <- tortugas[[1]]
# sir_david <- tortugas[[2]]

track_inters = read.csv('Data/tort_in_ponds_inters_less_columns.csv')

pond = read.csv('Data/Galapagos Water Sample Data_2018-24.csv') |>
  drop_na(Tourist_lagoon, Longitude, Latitude)

pond.spdf <- SpatialPointsDataFrame(coords= pond[,c('Longitude', 'Latitude')],
                                    data = pond,
                                    proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")) # For the Points

utm_gal <- "+proj=utm +zone=15 +south +datum=WGS84 +no_defs +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
# sir_david_sp <- spTransform(sir_david, utm_gal)
# roberto_sp <- spTransform(roberto, utm_gal)
tortugas_sp = spTransform(tortugas, utm_gal)

pond.spdf_UTM <- spTransform(pond.spdf, utm_gal)
pond.spdf_UTM_df <- data.frame(pond.spdf_UTM)

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Plot the object of interest (waterhole) and the animal trajectory for roberto the tortoise
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# require(mapview)
# mapview(pond.spdf, color = 'grey40',col.regions = 'blue',layer.name=c('Ponds')) +
# mapview(roberto, color = 'black',
#         layer.name=c('Roberto La tortuga'),
#         col.regions = 'black', cex = 0.5)

roberto

popvisit = getRecursions(animals, 2) 





rec <- getRecursionsAtLocations( tortugas_sp, pond.spdf_UTM_df[,c('Longitude', 'Latitude')], radius = 100 )

plot(rec, tortugas_sp, main = 'Robert pond revisits 5m radius', pch = 17, alpha = 1,
     xlim = c(790050, 790300)  )


points(pond.spdf_UTM, pch = 16, cex = 2, col = alpha('blue', 0.5)) # Plot the pond aka region of interest
points(roberto_sp, cex = 0.35, col = alpha('grey49', 0.25))   # Tracking data of roberto the tortoise
drawCircle(x = 790270, 9924500, radius = 5) # A 5m radius for reference





# Lets start with one:
# Interesting: If we get within 5 meters tortoises spend less time inside the ponds up to 5 hours approx 1-2 hours, is this consistent with Kyanas data? 


rec <- getRecursionsAtLocations( roberto_sp, pond.spdf_UTM_df[,c('Longitude.1', 'Latitude.1')], radius = 5 )

rec <- getRecursionsAtLocations( roberto_sp, pond.spdf_UTM_df[,c('Longitude', 'Latitude')], radius = 5 )

if(sum(rec$revisits)>1){print(paste0(unique(roberto_sp@idData$individual.local.identifier), ' has used a pond total of ', sum(rec$revisits), ' times'))}else(print(paste0('No pond used by '), unique(roberto_sp@idData$individual.local.identifier)))

plot(rec, roberto_sp, main = 'Robert pond revisits 5m radius', pch = 17, alpha = 1,
     xlim = c(790050, 790300)  )
points(pond.spdf_UTM, pch = 16, cex = 2, col = alpha('blue', 0.5)) # Plot the pond aka region of interest
points(roberto_sp, cex = 0.35, col = alpha('grey49', 0.25))   # Tracking data of roberto the tortoise
drawCircle(x = 790270, 9924500, radius = 5) # A 5m radius for reference

# mapview(pond.spdf) + mapview(tortugas_sp)
mapview(pond.spdf) + mapview(tortugas_sp)

head(track_inters)
head(track_inters)


# Convert to a move object
move_obj <- move(x = track_inters$location_long, 
                 y = track_inters$location_lat, 
                 time = as.POSIXct(track_inters$timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC"),
                 proj = CRS("+proj=longlat +datum=WGS84 +no_defs"),
                 animal = track_inters$tag_id)

# Convert the move object to sp object in UTM coordinates
utm_zone <- "+proj=utm +zone=15 +south +datum=WGS84 +units=m +no_defs"
move_sp <- spTransform(move_obj, utm_zone)

# Calculate revisitations using recurse
revisits <- getRecursions(move_sp, radius=50) # Adjust radius as needed

# length(unique(pond.spdf_UTM_df[,c('Longitude', 'Latitude')]$Longitude))

pond.spdf_UTM_df[,c('Longitude', 'Latitude')]
track_inters_sub$id = as.character(track_inters_sub$id)

rec <- getRecursionsAtLocations( track_inters_sub, pond.spdf_UTM_df[,c('Longitude', 'Latitude')], radius = 0.05 )


revisits_torts <- getRecursions(track_inters_sub, radius=50) # Adjust radius as needed

track_inters_sub$t = ymd_hms(track_inters_sub$t)

head(track_inters_sub)
head(martin$t )
head(martin)
data(martin)
locations = data.frame(x = c(-10, 0, 20), y = c(5, 0, 0))

plot(revisits, locations, legendPos = c(10, -15), 
     alpha = 1, pch = 17, xlim = range(track_inters_sub$x), ylim = range(track_inters_sub$y))
points(track_inters_sub$x, track_inters_sub$y, pch = ".", col = "gray50")
drawCircle(10, -10, 1)

pond |> distinct(Pond.ID)
head(pond_uniques)



# Recurse all tortoises:


# track_inters_sub = track_inters |> mutate(id = tag_id, 
#                                           x = tag_local_identifier,
#                                           y = location_lat,
#                                           t = timestamp) |>
#   select(
#     x,
#     y,
#     t,
#     id)


pond_uniques <- pond |>
  drop_na(Latitude, Longitude) |>
  distinct(Longitude, Latitude, .keep_all = TRUE) |>
  mutate(x = Longitude, 
         y = Latitude) |>
  select(x, y)

locations = track_inters_sub

revisits = getRecursionsAtLocations(track_inters_sub, pond_uniques, radius = 5)

plot(revisits, locations,  legendPos = c(-90.40, -15), 
     alpha = 1, pch = 17, xlim = range(track_inters_sub$x), ylim = range(track_inters_sub$y))


# https://cran.r-project.org/web/packages/recurse/vignettes/recurse.html
getRecursionsInPolygon(martin, protectedArea)

# --- --- --- --- --- --- --- --- --- --- --- ---
# Recursive revisitation analysis
# --- --- --- --- --- --- --- --- --- --- --- ---


data(wren)
animals = rbind(martin, wren)
plot(animals$x, animals$y, col = c("red", "darkblue")[as.numeric(animals$id)], 
pch = ".", xlab = "x", ylab = "y", asp = 1)
popvisit = getRecursions(animals, 2) 
plot(popvisit, animals, legendPos = c(15, -10))
locations = data.frame(x = c(0, 10, 20), y = c(0, 10, 10))
locvisit = getRecursionsAtLocations(wren, locations, 2) 

locvisit$revisits
st_buffer()

getRecursionsInPolygon(roberto, pond.spdf_UTM_df)
?getRecursionsInPolygon

head(popvisit$revisitStats)

roberto <- tortugas[[1]]
sir_david <- tortugas[[2]]


head(track_inters_sub)

roberto$id = roberto@idData$individual_id
roberto$x = roberto$location_long
roberto$y = roberto$location_lat
roberto$t = roberto$timestamp
roberto_df = data.frame(roberto)
roberto_df = roberto_df |> select(x, y, t, id 
                     )
rec <- getRecursionsAtLocations( roberto_df, pond_uniques, radius = 5 )

plot(rec, roberto_df, main = 'Robert pond revisits 5m radius', pch = 17, alpha = 1
     )
plot(roberto_df)

if(sum(rec$revisits)>1){print(paste0(unique(roberto_df$id), ' has used a pond total of ', sum(rec$revisits), ' times'))}else(print(paste0('No pond used by '), unique(roberto_df$id)))

