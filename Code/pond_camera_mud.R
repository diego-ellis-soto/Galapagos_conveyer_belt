###########################################################
###
###  Galapagos tortoise project - Ecosystem functioning
###  
###########################################################

require(tidyverse);require(raster); require(sp);require(ggplot2); require('ggfortify');require(cluster);library(lfda);require(mapview)
library(ggsignif)
require(conflicted)
require(tidyverse)
require(lubridate)
require(plyr)
require(raster)
conflicts_prefer(dplyr::mutate)
conflicts_prefer(raster::extract)
conflicts_prefer(dplyr::desc)
conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::arrange)

# Trap camera data Montemar
# mud = read.csv('/Users/diegoellis/projects/Ponds_2024/Pond_parameter_data/Mud_2024.csv')
mud = read.csv('/Users/diegoellis/Desktop/Projects/Postdoc/Pond_2025/Ponds_2024/Pond_parameter_data/Mud_2024.csv')
mud$Tourist_lagoon
sd(mud$Mud.weight..g., na.rm=T)


ggplot(mud, aes(x = Tourist_lagoon, y =Mud.weight..g., fill = Tourist_lagoon)) +
  geom_boxplot(alpha = .5,scale = "width", trim=FALSE, notch = FALSE) + theme_classic() + ggtitle('Dry mud transported by tortoises \n (n = 35, µ = 412.33, σ = 388.42)') +   theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Touristic lagoon') + ylab('Mud weight in gramm')


ggplot(mud, aes(x = Tourist_lagoon, y =Mud.weight..g., fill = Tourist_lagoon)) +
  geom_boxplot(alpha = .5,scale = "width", trim=FALSE, notch = FALSE) + theme_bw() + ggtitle(
    paste0(
      'Dry mud transported by tortoises \n (n = ', nrow(mud[!is.na(mud$Mud.weight..g.),]),', µ = ', round(mean(mud$Mud.weight..g., na.rm=T)),' , σ = ', round(sd(mud$Mud.weight..g., na.rm=T)), ')' ) 
  ) +   theme(plot.title = element_text(hjust = 0.5)) + xlab('Touristic lagoon') + ylab('Mud weight in gramm')


mud_tort = ggplot(mud, aes(x = Tourist_lagoon, y = Mud.weight..g., fill = Tourist_lagoon)) +
  geom_boxplot(alpha = .5, scale = "width", trim = FALSE, notch = FALSE) +
  geom_signif(comparisons = list(c("No", "Yes")), 
              map_signif_level = TRUE) +
  theme_classic() + 
  ggtitle('Dry mud transported by tortoises \n (n = 35, µ = 412.33, σ = 388.42)') +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Touristic lagoon') + 
  ylab('Mud weight in gram')

ggsave(mud_tort, file = '/Users/diegoellis/projects/Ponds_2024/Figures/mud_tort.png', width = 10, height = 8)



# Trap camera data Montemar
cam_montemar = read.csv('/Users/diegoellis/Desktop/Projects/Postdoc/Pond_2025/Ponds_2024/Camera_Traps/Photo_spreadsheet_montemar_all.csv') |>
# cam_montemar = read.csv('/Users/diegoellis/projects/Ponds_2024/Camera_Traps/Photo_spreadsheet_montemar_all.csv') |>
  dplyr::mutate(timestamp = mdy(Date),
                year = year(timestamp),
                month = month(timestamp),
                # hour = hour(hms(time)),
                file = File.Name,
                time = Time,
                temperature_F = Temperature..F.,
                tort_pres = Tortoises.present.,
                tort_in_water = X..of.tortoises.in.water,
                tort_on_land = X..of.tortoises.on.land,
                total_n_tort = total...of.tortoises,
                other_animals = Other.animals.present..Y.N,
                which_other_animal = What.animals.,
                wetland_lvl_raw = Wetland.level.raw.score,
                wetland_lvl_actual = Wetland.level.actual
  ) |>
  dplyr::select(file, time, timestamp,
                year, month, 
                temperature_F,
                tort_pres, tort_on_land, tort_in_water, total_n_tort,
                other_animals, which_other_animal,
                wetland_lvl_raw, wetland_lvl_actual)


cam_montemar$hour = hour(lubridate::hms(cam_montemar$time))


which(cam_montemar$timestamp == max(cam_montemar$timestamp))

ggplot(cam_montemar, aes(x = timestamp, y =total_n_tort)) + geom_point() + geom_smooth()+ylim(0, max(cam_montemar$total_n_tort))

# Median per day
cam_df = plyr::ddply(cam_montemar, 'timestamp', function(x){
  data.frame(
    total_n_tort = median(x$total_n_tort, na.rm=T),
    tort_in_water = median(x$tort_in_water, na.rm=T),
    tort_on_land = median(x$tort_on_land, na.rm=T)
  )
})

plyr::ddply(cam_montemar, 'year', function(x){
  plyr::ddply(x, 'month', function(y){
    data.frame(
      total_n_tort = median(y$total_n_tort, na.rm=T),
      tort_in_water = median(y$tort_in_water, na.rm=T),
      tort_on_land = median(y$tort_on_land, na.rm=T)
    )
  })
})

# Hourly and daily:

# ggplot(cam_df, aes(x = timestamp, y =total_n_tort)) + geom_point() + geom_smooth()+ylim(0, max(cam_df$total_n_tort))
# 
# ggplot(cam_df, aes(x = timestamp, y =tort_in_water)) + geom_point() + geom_smooth()+ylim(0, max(cam_df$total_n_tort))+
#   ggtitle('Median number of tortoises on water per day')
# 


# Get for december only: Hour of the day by month:

# ggplot( cam_montemar[cam_montemar$month ==12,], aes(x = hour, y = tort_in_water)) + geom_smooth() + geom_point()

ggplot( cam_montemar, aes(x = hour, y = tort_in_water)) + geom_smooth() + geom_point() + facet_grid(year~month)


# ggplot( cam_montemar, aes(x = hour, y = tort_in_water)) + geom_smooth() + geom_point() + facet_grid(~month)

library(ggplot2)
library(RColorBrewer)

color_scale <- scale_color_viridis_d(option = "D", end = 0.9)

month_labels <- c("January", "April", "May", "June", "July", "August", "September", "November", "December")
month_values <- c(1, 4, 5, 6, 7, 8, 9, 11, 12)

cam_montemar$month_label <- factor(cam_montemar$month, levels = month_values, labels = month_labels)

# Figure for the paper:
figure_pond_montemar = ggplot(cam_montemar, aes(x = hour, y = tort_in_water)) +
  geom_boxplot(aes(group = factor(hour)), alpha = 0.6) + 
  geom_smooth(aes(color = month_label), method = "loess", linetype = "solid") +
  labs(x = "Hour", y = "Number of tortoises \n in Water") +
  color_scale +
  theme_minimal() +
  theme(legend.title = element_blank())+
  ggtitle('Number of tortoises \n inside Montemar Pond')+
  theme(axis.text.x = element_text(face = "bold", size = 16),
        axis.title.x = element_text(face = "bold", size = 16),
        axis.text.y = element_text(face = "bold", size = 16),
        axis.title.y = element_text(face = "bold", size = 16))
  
  
  ggsave(figure_pond_montemar, file = '/Users/diegoellis/projects/Ponds_2024/Figures/figure_pond_montemar.png', width = 10, height = 8)

conflicts_prefer(dplyr::summarize)
conflicts_prefer(dplyr::arrange)
# Days with the most pond acitibty: 
cam_montemar <- cam_montemar %>%
  mutate(date = as.Date(timestamp))

# Summarize data by date
daily_summary <- cam_montemar %>%
  group_by(date) %>%
  summarize(total_tort_in_water = sum(tort_in_water, na.rm = TRUE), .groups = 'drop')

# Find the day with the highest number of tortoises
day_with_most_tortoises <- daily_summary %>%
  filter(total_tort_in_water == max(total_tort_in_water)) %>%
  arrange(desc(total_tort_in_water))

print(day_with_most_tortoises)



colors <- brewer.pal(12, "Set1")
# cam_montemar$month <- factor(cam_montemar$month, levels = 1:12, labels = month.name)

ggplot(cam_montemar, aes(x = (hour), y = tort_in_water)) +
  geom_boxplot(aes(group = factor(hour),alpha = 0.6)) + # Adjust transparency for better overlay visibility
  geom_smooth(aes(color = factor(month)), method = "loess", linetype = "solid") + # Add smoothing line for each month
  labs(x = "Hour", y = "Tortoises in Water") +
  theme_minimal() +
  theme(legend.title = element_blank()) # Optional: removes legend title


ggplot(cam_montemar, aes(x = factor(hour), y = tort_in_water)) +
  geom_boxplot() +
  labs(x = "Hour", y = "Tortoises in Water") +
  theme_minimal()


ggplot( cam_montemar, aes(x = hour, y = tort_in_water)) + geom_smooth() + geom_point() + facet_grid(year~month)


ggplot( cam_montemar, aes(x = hour, y = tort_in_water)) + geom_smooth()  + facet_grid(year~month)+ggtitle('Tortoises in water per hour')


ggplot( cam_montemar, aes(x = hour, y = tort_in_water)) + geom_line()  + facet_grid(year~month)+ggtitle('Tortoises in water per hour')

ggplot( cam_montemar, aes(x = hour, y = tort_in_water)) + geom_line()  + facet_grid(~month)+ggtitle('Tortoises in water per hour')


# Physicochemical parameters:

# pond = read.csv('/Users/diegoellis/projects/Ponds_2024/Pond_parameter_data/Galapagos Water Sample Data_2018-24.csv') |> drop_na(Tourist_lagoon, Longitude, Latitude)
pond = read.csv('/Users/diegoellis/Desktop/Projects/Postdoc/Pond_2025/Ponds_2024/Pond_parameter_data/Galapagos Water Sample Data_2018-24_most_up_to_date_2025.csv') |> drop_na(Tourist_lagoon, Longitude, Latitude)


pond$Number_of_tortoises <- gsub("\\+", "", pond$Number_of_tortoises)
pond$Number_of_tortoises = as.numeric(pond$Number_of_tortoises)

pond[which(is.na(pond$Number_of_tortoises)),]$Number_of_tortoises = 0

pond |> group_by(Tourist_lagoon) |> dplyr::summarise(
  
  mean(Number_of_tortoises)
)


add_negative_sign <- function(x) {
  ifelse(x > 0, paste0('-', x), x)
}

# Apply the function to the longitude and latitude columns
pond$Longitude <- add_negative_sign(pond$Longitude)
pond$Latitude <- add_negative_sign(pond$Latitude)
pond$Longitude  = as.numeric(pond$Longitude)
pond$Latitude  = as.numeric(pond$Latitude)
pond$Elevation = NA

pond_sp <- SpatialPointsDataFrame(coords = pond[,c('Longitude','Latitude')],data = pond,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

SRTM <- raster('/Users/diegoellis/projects/Manuscripts_collab/EcosystemServicesAnimals/2021_marius_tortuga_submision/Tortugas/Data/Elevation/SRTM_Santa_Cruz.tif')

pond_sp <- spTransform(pond_sp, paste0(proj4string(SRTM)))


# Add a - if there is not a - in the longitude and same for latitude: ####
pond_sp$Elevation <- extract(SRTM, pond_sp)

# gsub('30+', '30', pond$Number_of_tortoises)
# gsub('30+', '30', pond$Number_of_tortoises)
# gsub('30+', '30', pond$Number_of_tortoises)
# gsub('', 0, pond$Number_of_tortoises)
# 
# |>
#   mutate( Tourist_lagoon = as.factor(Tourist_lagoon),
#           Tortoise_presence = as.factor(Tortoise_presence),
#           Number_of_tortoises = as.integer(Number_of_tortoises))

# Keep only a few variables:
names(pond)
pond_sp_df = data.frame(pond_sp)



pond_sp_df_nutrients = pond_sp_df |> drop_na(NO3..ug.L.,TN..mg.L.,TP..ug.L.,
                                             NPOC..mg.L., NH4..ug.L., PO4..ug.L.)

# Remove later the stable isotope samples:
pond_sub = pond_sp_df |> dplyr::select(GPS_Pond_name, Elevation, Tourist_lagoon,
                                       YSI_Temperature, YSI_us.ccm,YSI_mS.ccm,
                                       DO., Tortoise_presence, Number_of_tortoises,
                                       NO3..ug.L.,TN..mg.L.,TP..ug.L.,
                                       NPOC..mg.L., NH4..ug.L., PO4..ug.L.) 
# Ojo double check P2 chato 3 where it was if its touristic or not #####
pond_sub[pond_sub$GPS_Pond_name == 'P2_Chato_3',]$Tourist_lagoon <- 'Yes'
pond_sub[pond_sub$GPS_Pond_name == 'P_Laguna_Ch',]$Tourist_lagoon <- 'No'
# pond_sub[,c('GPS_Pond_name', 'Tourist_lagoon')]
pond_sub$Tourist_lagoon = as.factor(pond_sub$Tourist_lagoon)

pond_sub[which(is.na(pond_sub$Tortoise_presence)),]$Tortoise_presence = 'No'
pond_sub[pond_sub$Tortoise_presence == '',]$Tortoise_presence = 'No'
pond_sub[136,]$Tortoise_presence = 'No' # "(1 tortuga fuea 20m no turtoguas dentro)"
pond_sub$Tortoise_presence = as.factor(pond_sub$Tortoise_presence)


pond_sub %>% 
  mutate( Tourist_lagoon = as.integer(Tourist_lagoon),Tortoise_presence = as.integer(Tortoise_presence),
          Number_of_tortoises = as.integer(Number_of_tortoises)) %>%
  drop_na()

pond_sub[136,]$YSI_us.ccm = ''
pond_sub[136,]$YSI_mS.ccm = ''
pond_sub[136,]$DO. = ''

# Ojo cambiar esto! Camiar todo a NA manualmente en la tabla ######
pond_sub$YSI_Temperature = as.numeric(pond_sub$YSI_Temperature)
pond_sub$YSI_us.ccm = as.numeric(pond_sub$YSI_us.ccm)
pond_sub$YSI_mS.ccm = as.numeric(pond_sub$YSI_mS.ccm)
pond_sub$DO.=as.numeric(pond_sub$DO.)

pond_sub[pond_sub$YSI_Temperature == '',]$YSI_Temperature = 'NA'
nrow(pond_sub[pond_sub$YSI_Temperature == '',])
table(is.na(as.numeric(pond_sub$YSI_Temperature)))

pond_sub_clean = pond_sub %>% 
  mutate( Tourist_lagoon = as.integer(Tourist_lagoon),Tortoise_presence = as.integer(Tortoise_presence),
          Number_of_tortoises = as.integer(Number_of_tortoises)) %>%
  drop_na()


# Load SRTM and extract elevaiton for missing values:
pond_sub_clean[pond_sub_clean$YSI_Temperature=="To shallow to measure",]$YSI_Temperature =''


require(corrplot)
str(pond_sub_clean)

pond_sub_clean_no_nutrients = pond_sub_clean |> dplyr::select(GPS_Pond_name, Elevation, Tourist_lagoon,
                                                              YSI_Temperature, YSI_us.ccm,YSI_mS.ccm,
                                                              DO., Tortoise_presence, Number_of_tortoises)

cors=round(cor(pond_sub_clean[,c(-1)]),2)
corrplot(cors,order = "AOE", addCoef.col = "grey",number.cex=.6, title='With nutrients') # Drop TRI and Slope?!

cors=round(cor(pond_sub_clean_no_nutrients[,c(-1)]),2)
corrplot(cors,order = "AOE", addCoef.col = "grey",number.cex=.6, title='No nutrients') # Drop TRI and Slope?!


pc1=princomp(pond_sub_clean[,c(-1)], cor=TRUE) # princomp: Make a principal component analyisis. First column of pond is Site and we dont want that. I dont want the country information. 
names(pc1) # gives us the square roots of the eigenvalues. The laodings the the weights, the coefficient tell us what direciton w eneed to look in. 
#print results
print(summary(pc1),digits=2,loadings=pc1$loadings,cutoff=0)
#make a screeplot  
screeplot(pc1,type="lines",col="red",lwd=2,pch=19,cex=1.2,main="Scree Plot")
# How to look if our data has multivariate normal distribution -> Chi-Square plot. ciscoreplot(pc1,c(1,2),WB2[,1])
#make a biplot for first two components
biplot(pc1,choices=c(1,2),pc.biplot=T)

#################################
#SECOND, use transformed data
pc2=princomp(pond_sub_clean[,c(-1)], cor=TRUE)
#print results
print(summary(pc2),digits=2,loadings=pc2$loadings,cutoff=0)
#make a screeplot  
screeplot(pc2,type="lines",col="red",lwd=2,pch=19,cex=1.2,main="Scree Plot")
biplot(pc2,choices=c(1,2),pc.biplot=T)


ggplot(mud) +
  geom_histogram(aes(x = Mud.weight..g., y = ..density..),
                 binwidth = 100, fill = "grey", color = "black")


plot(density(mud$Mud.weight..g.), main = 'Density plot \n dry mud transported by tortoises')

summary(mud$Mud.weight..g.)
summary(mud$Mud.weight..g.)
sd(mud$Mud.weight..g., na.rm=T)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Plot PCA with colours
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
names(pond_sub_clean)
df <- pond_sub_clean[,c(-1, -3)]
require(ggplot2)

autoplot(prcomp(df), data = pond_sub_clean, colour = 'Tourist_lagoon')

autoplot(prcomp(df), data = pond_sub_clean, colour = 'Tourist_lagoon', label = TRUE, label.size = 3)
autoplot(prcomp(df), data = pond_sub_clean, colour = 'Tourist_lagoon', loadings = TRUE)

autoplot(prcomp(df), data = pond_sub_clean, colour = 'Tourist_lagoon',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

autoplot(kmeans(df, 3), data = pond_sub_clean, label = TRUE, label.size = 3)

autoplot(fanny(df, 3), frame = TRUE)
autoplot(fanny(df, 3), frame = TRUE, frame.type = 'norm')

# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
model <- lfda(df, pond_sub_clean[, 3], r = 3, metric="plain")
autoplot(model, data = pond_sub_clean, frame = TRUE, frame.colour = 'Tourist_lagoon')





# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Progression of tortoises in ponds 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---



cam_montemar = read.csv('/Users/diegoellis/projects/Ponds_2024/Camera_Traps/Photo_spreadsheet_montemar_all.csv') |>
  dplyr::mutate(timestamp = mdy(Date),
                year = year(timestamp),
                month = month(timestamp),
                # hour = hour(hms(time)),
                file = File.Name,
                time = Time,
                temperature_F = Temperature..F.,
                tort_pres = Tortoises.present.,
                tort_in_water = X..of.tortoises.in.water,
                tort_on_land = X..of.tortoises.on.land,
                total_n_tort = total...of.tortoises,
                other_animals = Other.animals.present..Y.N,
                which_other_animal = What.animals.,
                wetland_lvl_raw = Wetland.level.raw.score,
                wetland_lvl_actual = Wetland.level.actual
  ) |>
  dplyr::select(file, time, timestamp,
                year, month, 
                temperature_F,
                tort_pres, tort_on_land, tort_in_water, total_n_tort,
                other_animals, which_other_animal,
                wetland_lvl_raw, wetland_lvl_actual)


cam_montemar$hour = hour(lubridate::hms(cam_montemar$time))


library(dplyr)

# Define time of day categories
categorize_time_of_day <- function(hour) {
  if (hour >= 5 & hour < 12) {
    return("Morning")
  } else if (hour >= 12 & hour < 17) {
    return("Midday")
  } else if (hour >= 17 & hour < 20) {
    return("Evening")
  } else {
    return("Night")
  }
}

# Add a time_of_day column to the dataset
cam_montemar_tod <- cam_montemar %>%
  mutate(time_of_day = sapply(hour, categorize_time_of_day))


head(cam_montemar_tod)
ddply(cam_montemar_tod, 'month',function(x){
  ddply(x, 'time_of_day', function(y){
    Median_number_of_tortoises = median(x$tort_in_water, na.rm=T)
  })
})

library(plyr)
library(ggplot2)

# Rename 'V1' to 'Median_tort_in_water'
cam_montemar_tod_summary <- ddply(cam_montemar_tod, 'month', function(x) {
  ddply(x, 'time_of_day', function(y) {
    
    data.frame(time_of_day = unique(y$time_of_day),
               Median_tort_in_water = median(y$tort_in_water, na.rm = TRUE))
  })
})



# Update month names for readability
cam_montemar_tod_summary$month <- factor(cam_montemar_tod_summary$month, 
                                         levels = 1:12, 
                                         labels = c("January", "February", "March", "April", 
                                                    "May", "June", "July", "August", 
                                                    "September", "October", "November", "December"))

# Create the plot
ggplot(cam_montemar_tod_summary, aes(x = month, y = Median_tort_in_water, color = time_of_day, group = time_of_day)) +
  geom_line(size = 1) + 
  geom_point(size = 2) +
  labs(title = "Progression of Tortoises in Water by Time of Day",
       x = "Month",
       y = "Median Number of Tortoises in Water",
       color = "Time of Day") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Calculate monthly mean of tort_in_water grouped by time_of_day and month
monthly_mean_tortoises <- cam_montemar_tod %>%
  group_by(month, time_of_day) %>%
  summarise(mean_tort_in_water = mean(tort_in_water, na.rm = TRUE)) %>%
  arrange(month, time_of_day)

# View the results
print(monthly_mean_tortoises)

