
rm(list = ls())

# Load libraries
library(TropFishR)
library(tidyverse)
library(patchwork)

# Set theme for output plots
theme_set(theme_classic())
theme_update(axis.text.x = element_text(colour = "black", size = 12), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(size = 12))

# Load functions
source("Scripts/fish_size_functions v2.R")

# Parameters
case_study_parameters <- read.csv("Parameters/case_study_species_parameters1.csv")

target_species <- case_study_parameters[case_study_parameters$species == "Lutjanus fulgens" & case_study_parameters$id == "lf",]

n <- nrow(target_species)

# Parameters for scenarios of changes in fish growth due to climate change
#multiple_tempr <- seq(1.5, 4, by = 0.1) # temperatures of 1.5oC, 2oC and 4oC anomalise from the IPCC CMIP6 projections regarding (source: https://www.ipcc.ch/report/ar6/wg1/downloads/factsheets/IPCC_AR6_WGI_Regional_Fact_Sheet_Australasia.pdf)
#tempr_coef <- 0.02 #temperature coefficient for growth (Cheung et al 2013) (Pauly 1980) (Portner and Peck 2010)
#scnr_tempr <- length(multiple_tempr)

tempr <- seq(4, 30, by = 0.1) # temperature range (made-up numbers)
tempr_min <- 4 # minimum temperature (made-up number)
tempr_max <- 30 # maximum temperature (made-up number)
tempr_opt <- 20 # optimal temperature (made-up number)
K_opt <- 0.3 # optimal growth coefficient (made-up number)
scnr_tempr <- length(tempr)

# Calculate indicators for each species

gall <- all_scnrs <- NULL


# Loop through each species
for (irow in 1:n){
  ## Add climate change impacts on Linf, Winf and K
  scnr_clim <- data.frame(tempr = tempr,
                          size_indicator_Linf = NA,
                          size_indicator_Winf = NA,
                          size_indicator_K = NA)
  
  
  scnr_clim$size_indicator_Linf <- numeric(scnr_tempr)
  scnr_clim$size_indicator_Winf <- numeric(scnr_tempr)
  scnr_clim$size_indicator_K <- numeric(scnr_tempr)
  
  # Fish biology parameters
  Linf <- target_species$Linf..cm.[irow] 
  Winf <- target_species$Winf..g.[irow]
  K <- target_species$growth_coef.[irow]
  M <- target_species$M.[irow]
  t0 <- target_species$t0.[irow]
  max_age <- target_species$max_age.[irow] 
  L50 <- target_species$L50..cm.[irow]
  a <- 0.001 # growth performance index - this is scaling that determines units of weight. The unit of the weight is in grams, so a = 0.001
  b <- 3 # growth performance index
  Fmort_overall <- target_species$Fmort.[irow]

  k_length <- 0.2 # slope of the logistic curve
  N0 <- 42000 # initial abundance of fish in tonnes

  # Loop through each temperature scenario
  for (i in 1:scnr_tempr){
    Linf_new_clim <- Linf * K_tempr(tempr[i], tempr_max, tempr_min, tempr_opt, K_opt)
    Winf_new_clim <- Winf * K_tempr(tempr[i], tempr_max, tempr_min, tempr_opt, K_opt)
    K_new_clim <- K_tempr(tempr[i], tempr_max, tempr_min, tempr_opt, K_opt)
    
    ## Calculate YPR model with new Linf growth parameters
    dat_clim <- abundance_catch_at_age(max_age, N0, Linf_new_clim, Winf, K, t0, a, b, M, L50, k_length, Fmort_overall)
    ind_tempr <- stock_indicators(dat_clim, Linf, Winf)
    scnr_clim$size_indicator_Linf[i] <- ind_tempr$mlength_indicator

    ## Calculate YPR model with new Winf growth parameters
    dat_clim <- abundance_catch_at_age(max_age, N0, Linf, Winf_new_clim, K, t0, a, b, M, L50, k_length, Fmort_overall)  
    ind_tempr <- stock_indicators(dat_clim, Linf, Winf)
    scnr_clim$size_indicator_Winf[i] <- ind_tempr$mweight_indicator

    ## Calculate YPR model with new K growth parameters
    dat_clim <- abundance_catch_at_age(max_age, N0, Linf, Winf, K_new_clim, t0, a, b, M, L50, k_length, Fmort_overall)
    ind_tempr <- stock_indicators(dat_clim, Linf, Winf)
    scnr_clim$size_indicator_K[i] <- ind_tempr$mlength_indicator

   }

    scnr_clim <- target_species[irow]
    
    
    all_scnrs <- c(all_scnrs, list(scnr_clim))


    # Plot results
    g2 <- ggplot(scnr_clim, aes(x = tempr)) +
      geom_line(aes(y = size_indicator_Linf), color = "black") +
      geom_line(aes(y = size_indicator_Winf), color = "blue") +
      geom_line(aes(y = size_indicator_K), color = "red") +
      labs(title = target_species, x = "Temperature change", y = "Size indicator") 

 gall <- c(gall, list(g2))

}

gall 

wrap_plots(gall)

all_scnr_data <- bind_rows(all_scnrs)
