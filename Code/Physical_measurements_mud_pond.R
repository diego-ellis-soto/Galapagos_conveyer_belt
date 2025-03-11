###########################################################
###
###  Galapagos tortoise project - Ecosystem functioning
###  
###########################################################
require(tidyverse)
require(raster)
require(sp)
require(ggplot2)
# require('ggfortify')
require(cluster)
library(lfda)
require(mapview)
require(raster)

# Load SRTM and extract elevaiton for missing values:
pond <- read.csv('/gpfs/gibbs/pi/jetz/from_loomis/de293/Gal_Pond/Indir/Pond_parameter_data/Master_sheet_2018_2019_ponds.csv')
pond_sp <- SpatialPointsDataFrame(coords = pond[,c('Longitude','Latitude')],data = pond,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
SRTM <- raster('/gpfs/gibbs/pi/jetz/from_loomis/de293/Gal_Pond/Indir/RS_Data_Santa_Cruz/SRTM/SRTM_Santa_Cruz.tif')
pond_sp <- spTransform(pond_sp, paste0(proj4string(SRTM)))

pond_sp$elevation = raster::extract(SRTM, pond_sp)


pond <- read.csv('/gpfs/gibbs/pi/jetz/from_loomis/de293/Gal_Pond/Indir/Pond_parameter_data/Master_sheet_2018_2019_2020_ponds.csv') 
# %>% 
#   dplyr::select(GPS_Pond_name, Elevation, Tourist_lagoon, YSI_Temperature,
#                 YSI_us.ccm,YSI_mS.ccm, DO., Tortoise_presence, Number_of_tortoises) %>%
#   dplyr::mutate( Tourist_lagoon = as.integer(Tourist_lagoon),
#           Tortoise_presence = as.integer(Tortoise_presence),
#           Number_of_tortoises = as.integer(Number_of_tortoises)) # %>% drop_na()


pond_sp <- SpatialPointsDataFrame(coords = pond[,c('Longitude','Latitude')],data = pond,
                                  proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
SRTM <- raster('/gpfs/gibbs/pi/jetz/from_loomis/de293/Gal_Pond/Indir/RS_Data_Santa_Cruz/SRTM/SRTM_Santa_Cruz.tif')
pond_sp <- spTransform(pond_sp, paste0(proj4string(SRTM)))

pond_sp$elevation = raster::extract(SRTM, pond_sp)
Montemar = pond_sp[pond_sp$Site == 'Montemar',]
# mapview(pond_sp[pond_sp$GPS_Pond_name == 'P_Montem_1',], col.regions='red') + mapview(pond_sp[!pond_sp$GPS_Pond_name == 'P_Montem_1',], col.regions='navyblue')

pond_sp$Tourist_lagoon = gsub('No','National Park',pond_sp$Tourist_lagoon)
pond_sp$Tourist_lagoon = gsub('Yes','Private',pond_sp$Tourist_lagoon)
pond_sp$Tourist_lagoon = factor(pond_sp$Tourist_lagoon)

pond_sp_df= data.frame(pond_sp) |> dplyr::select(GPS_Pond_name,
                      elevation,
                      Tourist_lagoon,
                      YSI_Temperature,
                      YSI_us.ccm,YSI_mS.ccm, DO.,
                      Tortoise_presence, 
                      Number_of_tortoises,
                      Pond.coverr,
                      DO.,
                      DO.mg.L,
                      Emergent.vegetation.in..) |>
  mutate(Number_of_tortoises = as.numeric(Number_of_tortoises),
         Pond.cover = as.numeric(gsub('%','',Pond.coverr)))

pond_sp_df[which(is.na(pond_sp_df$Number_of_tortoises)),]$Number_of_tortoises = 10 # was 10+ see initially
pond_sp_df[which(is.na(pond_sp_df$Pond.cover)),]$Pond.cover = 0 # Check 


# There are more tortoises in ponds in private ponds than in national park ponds
# Tourist_lagoon `mean(Number_of_tortoises, na.rm = T)`
# <fct>                                           <dbl>
#   1 National Park                                    2.56
# 2 Private                                          4.97
pond_sp_df |> group_by(Tourist_lagoon) |> summarise(mean(Number_of_tortoises, na.rm=T)) # Less number of tortoises present each time


require(corrplot)

pond_sp_df |> dplyr::select(elevation, YSI_Temperature, YSI_us.ccm, YSI_mS.ccm, Number_of_tortoises, Pond.cover) |> cor() |>
  corrplot.mixed(order = 'AOE')# corrplot::corrplot(method = 'ellipse')

# Run PCA
pc1= pond_sp_df |>
  dplyr::select(elevation, YSI_Temperature, YSI_us.ccm,
                YSI_mS.ccm, Number_of_tortoises, Pond.cover) |> 
  princomp(, cor=TRUE)

# princomp: Make a principal component analyisis. First column of pond is Site and we dont want that. I dont want the country information. 
names(pc1) # gives us the square roots of the eigenvalues. The laodings the the weights, the coefficient tell us what direciton w eneed to look in. 
#print results
print(summary(pc1),digits=2,loadings=pc1$loadings,cutoff=0)
#make a screeplot  
screeplot(pc1,type="lines",col="red",lwd=2,pch=19,cex=1.2,main="Scree Plot")
# How to look if our data has multivariate normal distribution -> Chi-Square plot. 

#SECOND, use transformed data
pc2= pond_sp_df |>
  dplyr::select(elevation, YSI_Temperature, YSI_us.ccm,
                YSI_mS.ccm, Number_of_tortoises, Pond.cover) |> 
  princomp(, cor=TRUE)

#print results
print(summary(pc2),digits=2,loadings=pc2$loadings,cutoff=0)
#make a screeplot  
screeplot(pc2,type="lines",col="red",lwd=2,pch=19,cex=1.2,main="Scree Plot")

# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Mud:
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Look at how much mud there is:



pond <- read.csv('/gpfs/gibbs/pi/jetz/from_loomis/de293/Gal_Pond/Indir/Pond_parameter_data/Master_sheet_2018_2019_2020_ponds.csv')

pond$Tourist_lagoon <- as.factor(pond$Tourist_lagoon)

ggplot(pond, aes(x = Tourist_lagoon, y =Mud.weight, fill = Tourist_lagoon)) +
  geom_boxplot(alpha = .5,scale = "width", trim=FALSE, notch = FALSE) + theme_bw() + ggtitle('Dry mud transported by tortoises \n (n = 13, µ = 310.67, σ = 300.82)') +   theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Touristic lagoon') + ylab('Mud weight in gramm')


ggplot(pond) +
  geom_histogram(aes(x = Mud.weight, y = ..density..),
                 binwidth = 100, fill = "grey", color = "black")
pond <- pond[!is.na(pond$Mud.weight),]
plot(density(pond$Mud.weight), main = 'Density plot \n dry mud transported by tortoises')

summary(pond$Mud.weight)
summary(pond$Tortoise.Mud)
sd(pond$Mud.weight, na.rm=T)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Plot PCA with colours
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# 2018 data
# OJO
# OJo
pond <- read.csv('/gpfs/gibbs/pi/jetz/from_loomis/de293/Gal_Pond/Indir/Pond_parameter_data/Master_sheet_2018_2019_ponds.csv')  %>%
  dplyr::select(GPS_Pond_name,
                Elevation,
                Tourist_lagoon,
                YSI_Temperature,
                YSI_us.ccm,YSI_mS.ccm,
                DO.,
                Tortoise_presence,
                Number_of_tortoises) %>% 
  mutate( Tortoise_presence = as.integer(as.factor(Tortoise_presence)),
          Number_of_tortoises = as.integer(as.factor(Number_of_tortoises))) %>%
  drop_na
df <- pond[,c(-1, -3)]

autoplot(prcomp(df), data = pond, colour = 'Tourist_lagoon')
autoplot(prcomp(df), data = pond, colour = 'Tourist_lagoon', label = TRUE, label.size = 3)
autoplot(prcomp(df), data = pond, colour = 'Tourist_lagoon', loadings = TRUE)

autoplot(prcomp(df), data = pond, colour = 'Tourist_lagoon',
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3)

autoplot(kmeans(df, 3), data = pond, label = TRUE, label.size = 3)

autoplot(fanny(df, 3), frame = TRUE)
autoplot(fanny(df, 3), frame = TRUE, frame.type = 'norm')

# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
model <- lfda(df, pond[, 3], r = 3, metric="plain")
autoplot(model, data = pond, frame = TRUE, frame.colour = 'Tourist_lagoon')
