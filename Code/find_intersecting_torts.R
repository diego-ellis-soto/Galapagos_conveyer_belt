# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
#
# Find tortoise tracking data intersecting with our pond data
#
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
require(move)
require(sp)
require(sf)
require(lubridate)
require('mapview')

loginStored<-movebankLogin(username="Diego_Ellis", password="Atelopus2!")

# Download all tortoises:
tortugas <- getMovebankData(study=2928116,
                            login=loginStored, includeExtraSensors=FALSE,
                            removeDuplicatedTimestamps=TRUE, deploymentAsIndividuals=FALSE
)
tortugas$name = tortugas@trackId
tortugas_df = data.frame(tortugas)

tortoise_sp <- SpatialPointsDataFrame(coords = tortugas_df[, c("location_long", "location_lat")],
                                      data = tortugas_df,
                                      proj4string = CRS("+proj=longlat +datum=WGS84"))

tortoise_sf = st_as_sf(tortoise_sp)

tortoise_sv <- vect(tortugas_df, geom = c("location_long", "location_lat"), crs = "+proj=longlat +datum=WGS84")

# Load pond location data:
ponds = st_read("Outdir/buffered_ponds.shp")

# ponds_sv <- vect("Outdir/buffered_ponds.shp")
# poza_buff_sv <- project(ponds_sv, crs(tortoise_sv))


poza_buff_sf  = st_transform(ponds, st_crs(tortoise_sf))

tortoise_sf$year = year(tortoise_sf$timestamp)

# Subset data for 2018 onwards when we started sampling pond data:
tortoise_sf_v2 = tortoise_sf |> dplyr::filter(year >=2018 & year <= 2025)

intersecting_tortoises <- character()


tortoise_sf_v2$local_identifier = tortoise_sf_v2$name

# sort(unique(tortoise_sf_v2$local_identifier))


for(i in unique(tortoise_sf_v2$local_identifier)){
  
  print(i)
  
  tmp = tortoise_sf_v2 |> dplyr::filter(local_identifier == i)
  
  tmp_df = tmp |> st_intersection(poza_buff_sf)
  
  if(nrow(tmp_df) > 0){ print(paste0(i, ' intersected with pond data'))
    
    intersecting_tortoises <- c(intersecting_tortoises, i)  
  }
  
  rm(tmp)
  rm(tmp_df)
  
}



# Arturito, Baronesa, Charlie, Francisco, Gessica, Guillaume, Harry, Helena, Herbert, Iggy, Laura, Melina, Patrick, Randy, Roberto, Ronnierake
# Speedy.Gonzales, Steve.Devine, Steve.Blake



tortoise_sf_ponds = tortoise_sf |> 
  dplyr::filter(name %in% c('Arturito', 'Baronesa', 'Charlie',
                            'Francisco', 'Gessica', 'Guillaume',
                            'Harry', 'Helena', 'Herbert', 'Iggy',
                            'Laura', 'Melina', 'Patrick', 
                            'Randy', 'Roberto', 'Ronnierake',
                            'Speedy.Gonzales', 'Steve.Devine', 'Steve.Blake')
  ) |> 
  dplyr::filter(year >=2018 & year <= 2025)

# save(tortoise_sf_ponds, file = 'Outdir/tort_in_ponds_inters_2025.Rdata')

tortoise_sf_pnds_df = data.frame(tortoise_sf_ponds)

write.csv(tortoise_sf_pnds_df , file =  'Outdir/tort_in_ponds_inters.csv')
# Save intersecting la caseta and intersecting mondemar:

tortoise_sf_pnds_df$yday = yday(tortoise_sf_pnds_df$timestamp)
tortoise_sf_pnds_df$year_yday = paste0(tortoise_sf_pnds_df$year,'-' ,tortoise_sf_pnds_df$yday)

require(plyr)

tortoise_sf_pnds_df$location_lat
  
tortoise_sf_pnds_df_daily = ddply(tortoise_sf_pnds_df, 'name', function(x){
  ddply(x, 'year_yday', function(y){

    data.frame(
      name = unique(y$name),
      timestamp = y[1,]$timestamp,
      year = y[1,]$year,
      location_lat = y[1,]$location_lat,
      location_long = y[1,]$location_long
    )
    
  })  
})

# tortoise_sf_pnds_df_one_p_p_day = tortoise_sf_pnds_df |> 
#   dplyr::select(year_yday, name, location_long, location_lat, timestamp) |>
#   group_by(year_yday) |> 
#   slice(1)
# unique(tortoise_sf_pnds_df_one_p_p_day$name)

write.csv(tortoise_sf_pnds_df_daily , file =  'Outdir/tortoise_sf_pnds_df_one_p_p_day.csv')
write.csv(intersecting_tortoises, file = 'Outdir/tortoise_names_in_ponds.csv')

# Make a Figure:
# Remove 2019 from Baronesa:

tortoise_sf_pnds_df_daily_sub = tortoise_sf_pnds_df_daily %>%
  dplyr::filter(!(name == "Baronesa" & year == 2019)) |>
  dplyr::filter(!name == 'Helena')

df_sp <- SpatialPointsDataFrame(coords = tortoise_sf_pnds_df_daily_sub[, c("location_long", "location_lat")],
                                data = tortoise_sf_pnds_df_daily_sub,
                                proj4string = CRS("+proj=longlat +datum=WGS84"))

df_sp_sf = st_as_sf(df_sp)

tortoise_sf_pnds_df_daily_sub_as_df = as.data.frame(tortoise_sf_pnds_df_daily_sub)


SRTM <- raster('/Users/diegoellis/Desktop/Projects/Postdoc/Galapagos_conveyer_belt/Data/SRTM_Santa_Cruz.tif')

# # Clip to Santa Cruz
tortoise_sf_pnds_df_daily_sub_as_df = tortoise_sf_pnds_df_daily_sub_as_df[tortoise_sf_pnds_df_daily_sub_as_df$location_long >= bbox(SRTM)[1],]
tortoise_sf_pnds_df_daily_sub_as_df = tortoise_sf_pnds_df_daily_sub_as_df[tortoise_sf_pnds_df_daily_sub_as_df$location_long <= bbox(SRTM)[3],]
tortoise_sf_pnds_df_daily_sub_as_df = tortoise_sf_pnds_df_daily_sub_as_df[tortoise_sf_pnds_df_daily_sub_as_df$location_lat >= bbox(SRTM)[2],]
tortoise_sf_pnds_df_daily_sub_as_df = tortoise_sf_pnds_df_daily_sub_as_df[tortoise_sf_pnds_df_daily_sub_as_df$location_lat <= bbox(SRTM)[4],]
# 
df_sp <- SpatialPointsDataFrame(coords = tortoise_sf_pnds_df_daily_sub_as_df[, c("location_long", "location_lat")],
                                data = tortoise_sf_pnds_df_daily_sub_as_df,
                                proj4string = CRS("+proj=longlat +datum=WGS84"))

df_sp_sf = st_as_sf(df_sp)

mapview(df_sp_sf)

# Ronnierake
# Herbert 
# timestamp 	2021-05-19T17:01:42Z 
# year 	2021

# Guillaume

# --- --- --- --- --- --- --- ---
# Remove 95 of tracking data
# --- --- --- --- --- --- --- ---

