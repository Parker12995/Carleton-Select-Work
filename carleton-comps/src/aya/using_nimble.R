library(nimble)
library(sp)
library(tidyverse)

######################load data###########################################################################

#bind in another script sim_points and the overlayed data below
sim_points <- read.csv("/Users/ayaklos/Documents/GitHub/carleton_comps_22_23/data/working/integration_points/494points_coord.csv")
over_sim_points_census <- read.csv("/Users/ayaklos/Documents/GitHub/carleton_comps_22_23/data/working/over_sim_points_census.csv")
over_uof_census <- read.csv("/Users/ayaklos/Documents/GitHub/carleton_comps_22_23/data/working/overlay_pointdatasets_censusdata/over_uof_census.csv")
minn_tracts.sf <- read.csv("/Users/ayaklos/Documents/GitHub/carleton_comps_22_23/data/working/minn_tracts_sf.csv")
#add original uof information to over_uof_census in another script as well

over_uof_census$perc_white <- over_uof_census$total_white/over_uof_census$total_pop
over_uof_census$perc_poverty <- over_uof_census$below_pov_level/over_uof_census$total_pop

############################################################################################################

#build the model
code <- nimbleCode({
  beta0 ~ dnorm(0, sd = 1000)
  beta1 ~ dnorm(0, sd = 1000)
  beta2 ~ dnorm(0, sd = 1000)
  sigma ~ dinvgamma(2, 0.5)
  
  #include prior for gaussian process--dmvnorm
  #parameters--sigma, fixed parameter phi, distance matrix for knots
  #transform gaussian process over knots to dimension of integration points and to data points (do two separately)
})

Rmodel <- nimbleModel(code, data = list(sim_cov1=over_sim_points_census$perc_white,
                                        sim_cov2=over_sim_points_census$perc_poverty,
                                        cov1=over_uof_census$perc_white,
                                        cov2=over_uof_census$perc_poverty), #add the distance matrices in data as well
                      inits = list(beta0 = 1,
                                   beta1 = 1,
                                   beta2 = 1,
                                   sigma = 1),
                      constants = list(area = sum(minn_tracts.sf$tract_area),
                                       m = sum(minn_tracts.sf$num_sim_points))) #area and m specified as constants or data

llFun <- nimbleFunction(
  setup = function(model) { },
  run = function() {
    beta0 <- model$beta0
    beta1 <- model$beta1
    beta2 <- model$beta2
    sigma <- model$sigma

    # area <- model$area
    # m <- model$m
    # sim_cov1 <- model$sim_cov1
    # sim_cov2 <- model$sim_cov2
    # cov1 <- model$cov1
    # cov2 <- model$cov2
    
    int <- (area / m) * sum(beta0 + beta1*sim_cov1 + beta2*sim_cov2) 
    
    ll <- sum(beta0 + beta1*cov1 + beta2*cov2) - int
    
    returnType(double())
    return(ll[1])
  }
)

RllFun <- llFun(Rmodel)

mcmcConf <- configureMCMC(Rmodel, nodes = NULL)

mcmcConf$addSampler(target = "beta0", type = "RW_llFunction",
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = "beta1", type = "RW_llFunction",
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = "beta2", type = "RW_llFunction",
                    control = list(llFunction = RllFun, includesTarget = FALSE))
mcmcConf$addSampler(target = "sigma", type = "RW_llFunction",
                    control = list(llFunction = RllFun, includesTarget = FALSE))

Rmcmc <- buildMCMC(mcmcConf)

runMCMC(Rmcmc, 100)

samples1 <- as.matrix(Rmcmc$mvSamples)

plot(samples1[, 1], type="l")

#check with trace plot





