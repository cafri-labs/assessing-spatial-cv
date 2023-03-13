make_simulated_landscapes <- function(n.sims = 100, gridsize = c(50L, 50L)) {
  RandomFields::RFoptions(install="no")
  
  # Simulation code below adapted (lightly) from:
  #
  # Roberts, D. R., V. Bahn, S. Ciuti, M. S. Boyce, J. Elith, 
  # G. Guillera-Arroita, S. Hauenstein, J. J. Lahoz-Monfort, B. Schroder, 
  # W. Thuiller, D. I. Warton, B. A. Wintle, F. Hartig, and C. F. Dormann. 2017. 
  # Cross-validation strategies for data with temporal, spatial, hierarchical or 
  # phylogenetic structure. - Ecography doi: 10.1111/ecog.02881.
  
  ## PART 1 - Simulate the landscapes
  ## -------------------------------
  ##
  ## These landscapes have:
  ##    - One long range SAC as "temp" with a RMgauss covariance model,
  ##    - A "precip" variable with a bit more noise and a bit shorter range, and
  ##    - RMexp and another intermediate RMexp as "noise"
  
  Xvec <- seq(0, 1, len = gridsize[1])
  Yvec <- seq(0, 1, len = gridsize[2])
  grd <- expand.grid(Y = Yvec, X = Xvec)
  lon <- grd$X
  lat <- grd$Y
  
  sim.data <- vector("list", n.sims)
  ## Simulate landscapes
  # LOOP for all simulated surfaces
  for(i in 1:n.sims){
    
    ## FIRST, simulate the spatially structured environment
    
    # Some other env var is just standard (could be soil or topography or whatever)
    expCov <- RandomFields::RMexp(var = 0.1, scale = 0.1)
    x.1 <- as.vector(t(RandomFields::RFsimulate(expCov, x = Xvec, y = Yvec, spConform = FALSE)))
    x.1 <- scale(x.1)
    
    # "Precip" has a higher variance
    expCov <- RandomFields::RMexp(var = 0.3, scale = 0.1)
    x.2 <- as.vector(t(RandomFields::RFsimulate(expCov, x = Xvec, y = Yvec, spConform = FALSE)))
    x.2 <- scale(x.2)
    
    # "Temp" has a longer range
    expCov <- RandomFields::RMgauss(var = 0.1, scale = 0.3) #0.4 was a bit much
    x.3 <- as.vector(t(RandomFields::RFsimulate(expCov, x = Xvec, y = Yvec, spConform = FALSE)))
    x.3 <- scale(x.3)
    
    
    ## SECOND, simulate the biotic interaction and the disease
    
    # Prevalence of disease
    p.d <- 0.05
    
    # Add constant to x.2 and x.3 to keep them positive
    t.ps <- -floor(min(x.2, x.3))
    
    # Set up disease with 1 where it occurs
    x.4 <- rep(1, prod(gridsize))
    
    # Change locations to zero where the ratio of precip to temp isn't large enough to be 
    # in the percentile specified by prevalence (the top wet and warm locations have disease)
    x.4[((x.2 + t.ps)/(x.3 + t.ps)) < quantile(((x.2 + t.ps)/(x.3 + t.ps)), (1 - p.d))] <- 0
    
    # Putting in "food" as a linear combination of previous variables
    x.5 <- (x.1 + x.2 + x.3 + x.2*x.3)
    x.5 <- scale(x.5)
    
    #  Add more "normal" variables as additional material for overfitting
    expCov <- RandomFields::RMexp(var = 0.1, scale = 0.1)
    x.6 <- as.vector(t(RandomFields::RFsimulate(expCov, x = Xvec, y = Yvec, spConform = FALSE)))
    x.6 <- scale(x.6)
    x.7 <- as.vector(t(RandomFields::RFsimulate(expCov, x = Xvec, y = Yvec, spConform = FALSE)))
    x.7 <- scale(x.7)
    x.8 <- as.vector(t(RandomFields::RFsimulate(expCov, x = Xvec, y = Yvec, spConform = FALSE)))
    x.8 <- scale(x.8)
    expCov <- RandomFields::RMgauss(var = 0.1, scale = 0.3)
    x.9 <- as.vector(t(RandomFields::RFsimulate(expCov, x = Xvec, y = Yvec, spConform = FALSE)))
    x.9 <- scale(x.9)
    expCov <- RandomFields::RMgauss(var = 0.1, scale = 0.3)
    x.10 <- as.vector(t(RandomFields::RFsimulate(expCov, x = Xvec, y = Yvec, spConform = FALSE)))
    x.10 <- scale(x.10)
    
    
    ## THIRD, simulate the species
    
    # Species should depend linearly on food (x.5).
    # Species should be limited by an interaction between precip and temp.
    # In reality, water potential depends linearly on temp, but temp is in K, so the
    # real variance is small and relatively unimportant compared to precip. I will simulate that
    # here by turning the simulated temp into somewhat realistic temp and then doing calcs (precip/temp)
    # as if it were in K (so add 273 first and in the end standardize the variable again)
    # dependence on temp is unimodal f(x) = 1/(sqrt(2*pi)*sigma)*e^(-((x - mean)^2/(2*sigma^2)))
    # I'll have sigma = 1 here and mean = 0
    
    # "water availability"
    x.11 <- x.2/(x.3 + 273)
    x.11 <- scale(x.11)
    
    # Gaussian dependence on temperature
    x.12 <- 1/(sqrt(2*pi))*exp(-(x.3^2/4))
    x.12 <- scale(x.12)
    
    # Gaussian dependence on water
    x.13 <- 1/(sqrt(2*pi))*exp(-(x.2^2/4))
    x.13 <- scale(x.13)
    
    # x.1 = some standard var such as soil - unknown to model
    # x.5 = "food" a combination of x.1, x.2, x.3 and x.2*x.3
    # x.12 and x.13, Gaussian response of temp and precip
    y <- x.1 + x.5 + x.12 + x.13 + x.6
    y <- scale(y)
    
    # Then use the water potential as limiting factor by reducing y to water potential where 
    # water potential is lower (x.11, "water potential" derived from x.2 and x.3)
    y[y>x.11] <- x.11[y>x.11]
    y[x.4 == 1] <- min(y)
    
    sim.data[[i]] <- data.frame(y, mget(paste0("x.", 1:13)))
    sim.data[[i]] <- cbind(grd, sim.data[[i]])
    sim.data[[i]] <- sf::st_as_sf(sim.data[[i]], coords = c("X", "Y"))
    
  } # LOOP for all simulated surfaces 
  sim.data
}
