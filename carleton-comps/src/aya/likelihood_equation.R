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

##############################################################################

coordinates(use_of_force_data) <- ~X+Y

proj4string(use_of_force_data) <- proj4string(minneapolis_tracts)

over_minn_uof <- over(use_of_force_data, minneapolis_tracts)

#work with overlay data
over_minn_uof$perc_white <- over_minn_uof$total_white/over_minn_uof$total_pop
over_minn_uof$perc_poverty <- over_minn_uof$below_pov_level/over_minn_uof$total_pop

##############################################################################

coordinates(uof_suspicious_data) <- ~X+Y

proj4string(uof_suspicious_data) <- proj4string(minneapolis_tracts)

over_minn_uof_sus <- over(uof_suspicious_data, minneapolis_tracts)

#work with overlay data
over_minn_uof_sus$perc_white <- over_minn_uof_sus$total_white/over_minn_uof_sus$total_pop
over_minn_uof_sus$perc_poverty <- over_minn_uof_sus$below_pov_level/over_minn_uof_sus$total_pop
###########################################################################
###########################################################################
load("/Users/ayaklos/Documents/GitHub/carleton_comps_22_23/data/working/Ward9_Blocks.Rdata")
minn_bg.sf <- st_as_sf(ward9_blocks)

for (i in 1:nrow(minn_bg.sf)) {
  minn_bg.sf[i, 39] <- area(st_transform(st_union(minn_bg.sf[i, ]),crs = 6345))
}

minn_bg.sf$tract_area <- minn_bg.sf$V39
minn_bg.sf <- minn_bg.sf[, -39]

#One method of getting number of integration points per tract
minn_bg.sf$num_sim_points <- 15

sim_points_coor <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(sim_points_coor) <- c('x', 'y')

for (i in 1:nrow(minn_bg.sf)) {
  all_bg_loop <- st_transform(st_union(minn_bg.sf[i, ]),crs = 6345)
  all_bg_win_loop <- all_bg_loop %>% as.owin()
  sim_points_loop <- runifpoint(minn_bg.sf[i, ]$num_sim_points, win=all_bg_win_loop)
  sim_points_coor_loop <- sim_points_loop %>% as.data.frame()
  sim_points_coor <- rbind(sim_points_coor, sim_points_coor_loop)
}


coordinates(sim_points_coor) <- ~x+y
proj4string(sim_points_coor) <- proj4string(as_Spatial(all_bg_loop))
sim_points_coor <- spTransform(sim_points_coor, proj4string(ward9_blocks))
sim_points_coor.df <- as.data.frame(sim_points_coor@coords)

write_csv(data.frame(sim_points_coor.df), "/Users/ayaklos/Documents/GitHub/carleton_comps_22_23/data/working/integration_points/simpoints_15each.csv")

###########################################################################
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
minn_tracts.sf$num_sim_points <- floor(minn_tracts.sf$tract_area / min(minn_tracts.sf$tract_area)) * 1
m <- sum(minn_tracts.sf$num_sim_points)

#sim_points_coor will get appended to in the for loop
sim_points_coor <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(sim_points_coor) <- c('x', 'y')

for (i in 1:nrow(minn_tracts.sf)) {
  all_tracts_loop <- st_transform(st_union(minn_tracts.sf[i, ]),crs = 6345)
  all_tracts_win_loop <- all_tracts_loop %>% as.owin()
  sim_points_loop <- runifpoint(minn_tracts.sf[i, ]$num_sim_points, win=all_tracts_win_loop)
  sim_points_coor_loop <- sim_points_loop %>% as.data.frame()
  sim_points_coor <- rbind(sim_points_coor, sim_points_coor_loop)
}


coordinates(sim_points_coor) <- ~x+y
proj4string(sim_points_coor) <- proj4string(as_Spatial(all_tracts_loop))
sim_points_coor <- spTransform(sim_points_coor, proj4string(minneapolis_tracts))
sim_points_coor.df <- as.data.frame(sim_points_coor@coords)

# write_csv(sim_points_coor, "/Users/ayaklos/Documents/GitHub/carleton_comps_22_23/data/working/integration_points/1482points.csv")

# write_csv(data.frame(sim_points_coor.df), "/Users/ayaklos/Documents/GitHub/carleton_comps_22_23/data/working/integration_points/494points_coord.csv")

over_sim_covs <- over(sim_points_coor, minneapolis_tracts)
sim_points_census <- cbind(sim_points_coor.df, over_sim_covs)

#should this be giving us new covariate values with different points?
write_csv(sim_points_census, "data/working/over_sim_points_census.csv")

over_sim_covs$perc_white <- over_sim_covs$total_white/over_sim_covs$total_pop
over_sim_covs$perc_poverty <- over_sim_covs$below_pov_level/over_sim_covs$total_pop

sim_cov1 <- over_sim_covs$perc_white
sim_cov2 <- over_sim_covs$perc_poverty
beta0_int <- 1
beta1_int <- 1
beta2_int <- 1
area <- sum(minn_tracts.sf$tract_area)

# GP

# b0 + b1 * x1(s) + b2 * x2(s) + w(s)
int <- (area / m) * sum(beta0_int + beta1_int*sim_cov1 + beta2_int*sim_cov2)

sum(beta0 + beta1*cov1 + beta2*cov2, na.rm = T) - int



