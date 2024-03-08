library(tidyverse)
library(sp)
library(tigris)
library(ggmap)
library(tidycensus)
library(maditr)
library(spdep)
library(spatialreg)
library(gridExtra)
library(sf)
library(spatstat)

#########Load in data########################################################
load("/Users/ayaklos/Documents/GitHub/carleton_comps_22_23/data/working/Census_Tracts_Data.Rdata")
use_of_force_data <- read.csv("/Users/ayaklos/Documents/GitHub/carleton_comps_22_23/data/original/Police_Use_of_Force.csv")
uof_suspicious_data <- use_of_force_data %>% filter(Problem == "Suspicious Person ")

coordinates(uof_suspicious_data) <- ~X+Y

proj4string(uof_suspicious_data) <- proj4string(minneapolis_tracts)

over_minn_uof_sus <- over(uof_suspicious_data, minneapolis_tracts)

#work with overlay data
over_minn_uof_sus$perc_white <- over_minn_uof_sus$total_white/over_minn_uof_sus$total_pop
over_minn_uof_sus$perc_poverty <- over_minn_uof_sus$below_pov_level/over_minn_uof_sus$total_pop
###########################################################################

#######Make window#########################################################
minn_wards <- st_read("/Users/ayaklos/Documents/GitHub/carleton_comps_22_23/data/original/City_Council_Wards/WARDS.shp") %>%
  as_Spatial()

#proj4string(minn_wards)
#all_wards <- st_transform(st_union(st_as_sf(minn_wards)),crs = 6345)
#all_wards_win <- all_wards %>% as.owin()
###########################################################################

set.seed(1342343)
cov1 <- over_minn_uof_sus$perc_white
cov2 <- over_minn_uof_sus$perc_poverty
beta0 <- 1
beta1 <- 1
beta2 <- 1

#Getting integration points for all tracts
minn_tracts.sf <- st_as_sf(minneapolis_tracts)
#Calculating the area for each tract
for (i in 1:nrow(minn_tracts.sf)) {
  minn_tracts.sf[i, 39] <- area(st_transform(st_union(minn_tracts.sf[i, ]),crs = 6345))
}
minn_tracts.sf$tract_area <- minn_tracts.sf$V39
minn_tracts.sf <- minn_tracts.sf[, -39]

#One method of getting number of integration points per tract
minn_tracts.sf$num_sim_points <- round(minn_tracts.sf$tract_area / min(minn_tracts.sf$tract_area), digits = 0)

#sim_points_coor will get appended to in the for loop
sim_points_coor <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(sim_points_coor) <- c('x', 'y')

set.seed(1342343)
for (i in 1:nrow(minn_tracts.sf)) {
  all_tracts_loop <- st_transform(st_union(minn_tracts.sf[i, ]),crs = 6345)
  all_tracts_win_loop <- all_tracts_loop %>% as.owin()
  sim_points_loop <- runifpoint(minn_tracts.sf[i, ]$num_sim_points, win=all_tracts_win_loop)
  sim_points_coor_loop <- sim_points_loop %>% as.data.frame()
  sim_points_coor <- rbind(sim_points_coor_test2, sim_points_coor_loop)
}

coordinates(sim_points_coor) <- ~x+y
proj4string(sim_points_coor) <- proj4string(as_Spatial(all_tracts_loop))
sim_points_coor <- spTransform(sim_points_coor, proj4string(minneapolis_tracts))

over_sim_covs <- over(sim_points_coor, minneapolis_tracts) 

over_sim_covs$perc_white <- over_sim_covs$total_white/over_sim_covs$total_pop
over_sim_covs$perc_poverty <- over_sim_covs$below_pov_level/over_sim_covs$total_pop

sim_cov1 <- over_sim_covs$perc_white
sim_cov2 <- over_sim_covs$perc_poverty
beta0_int <- 1
beta1_int <- 1
beta2_int <- 1

int <- (1 / m) * sum(beta0_int + beta1_int*sim_cov1 + beta2_int*sim_cov2)

sum(beta0 + beta1*cov1 + beta2*cov2) - int

