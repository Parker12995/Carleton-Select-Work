###
### Basic code to load wards
###

#start with clean environment
rm(list = ls())

### libraries
library(tidyverse)
library(sf)
library(sp)
library(ggmap)

### load wards (Claire's wd)
setwd("/Users/ayaklos/Documents/GitHub/carleton_comps_22_23/")

minn_wards <- st_read("./data/original/City_Council_Wards/WARDS.shp") %>%
  as_Spatial()

### plotting wards (same as census)
plot(minn_wards)


#plot with ggplot
minn_map <- ggmap(get_map(c(left = -93.35, bottom = 44.88, 
                            right = -93.18, top = 45.06), source = "stamen"))

#plotting code from last time
wards_fortify <- broom::tidy(minn_wards)
minn_wards$id <- row.names(minn_wards)
wards_fortify <- left_join(wards_fortify, minn_wards@data)

#replot the shapefile
minn_map + geom_polygon(data= wards_fortify, 
                        aes(x = long, y = lat, group = group), 
                        fill = NA, col = "black")

#plot wards by ward number
minn_map + geom_polygon(data= wards_fortify, 
                        aes(x = long, y = lat, group = group,
                            fill = BDNUM), 
                        col = "black")


# overlap point level dataset onto wards
crime_data <- read.csv("./data/original/Crime_Data.csv")

#make spatial object 
sp_crime_data <- crime_data
coordinates(sp_crime_data) <- ~X+Y

#need to project these datasets to have the same projection
proj4string(minn_wards) #check projection
proj4string(sp_crime_data) <- CRS("+proj=longlat +datum=NAD83 +no_defs")

#transform the projection of the crime data to match the wards
sp_crime_data <- spTransform(sp_crime_data, proj4string(minn_wards))

#overlay the two
over_data <- over(sp_crime_data, minn_wards)

#how many points are outside of these wards?
length(which(is.na(over_data$FID))) #1611

#combine ward information with crime information
crime_data_full <- cbind(crime_data, over_data)

#get count of events per ward 
agg_data <- over_data %>% group_by(BDNUM) %>% tally() %>%
  rename(num_crime = "n")

wards_fortify <- left_join(wards_fortify, agg_data)

#plot number of crimes by wards
minn_map + geom_polygon(data= wards_fortify, 
                        aes(x = long, y = lat, group = group,
                            fill = num_crime), 
                        col = "black") +
  scale_fill_distiller(palette = "Spectral")


#You can do basically the same thing with census tracts! 
# The projections are actually a bit easier in that case.