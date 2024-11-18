
rm(list = ls())

# Load libraries
library(TropFishR)
library(tidyverse)
library(patchwork)
library(reshape2)

# Set theme for output plots
theme_set(theme_classic())
theme_update(axis.text.x = element_text(colour = "black", size = 12), 
              axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(size = 12))

# Load functions
source("Scripts/fish_size_functions v2.R")

# Parameters
case_study_parameters <- read.csv("Parameters/case_study_species_parameters1.csv")

target_species <- case_study_parameters[case_study_parameters$species == "Sardinella aurita" & case_study_parameters$id == "sa", ]

n <- nrow(target_species)

# Parameters for scenarios of changes in temperature, optimum temperature and growth performance due to climate change
tempr <- seq(4, 40, by = 0.1) # temperature range (made-up numbers)
tempr_min <- 4 # minimum temperature (made-up number)
tempr_max <- 40 # maximum temperature (made-up number)
tempr_opt <- c(28, 29.5, 30, 32) # optimal temperature based on ipcc (CMIP6) projections (source: Sohou et al. 2020; https://www.ipcc.ch/report/ar6/wg1/downloads/factsheets/IPCC_AR6_WGI_Regional_Fact_Sheet_Australasia.pdf)
k_opt <- 0.3 # optimal growth coefficient (made-up number)
Fmort <- target_species$Fmort.[1] # fishing mortality rate 
scnr_tempr <- length(tempr)
phi_1 <- 0.0438 # growth performance per degree change in temperature (source: Kielbassa et al. 2010; Mallet et al. 1999)

# Calculate indicators for each species

gall <- all_scnrs <- NULL


# Loop through each species
for (irow in 1:n){
  ## Add climate change impacts on Linf, Winf and K
  #one way to get all fmors and all temprs
  scnr_clim <- expand.grid(tempr = tempr, Fmort = Fmort, target_species = target_species$species[irow])
  
  scnr_clim$size_indicator_Linf <- NA
  scnr_clim$size_indicator_Winf <- NA
  scnr_clim$size_indicator_K <- NA
 
  
  scnr_clim$size_indicator_Linf <- numeric(scnr_tempr)
  scnr_clim$size_indicator_Winf <- numeric(scnr_tempr)
  scnr_clim$size_indicator_K <- numeric(scnr_tempr)

  # Fish biology parameters
  Linf <- target_species$Linf..cm.[irow] 
  Winf <- target_species$Winf..g.[irow]
  k <- target_species$growth_coef.[irow]
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
    
    # Calculate new growth parameters
    k_new_clim <- k_tempr(tempr[i], tempr_max, tempr_min, tempr_opt, k_opt)
    #make sure K doesn't go less than zero
    k_new_clim <- max(c(0, k_new_clim))

    #update this with linf model... 
    Linf_new_clim <- Linf_tempr(k, Linf, k_new_clim)
    Winf_new_clim <- Winf_tempr(Winf, Linf, Linf_new_clim)

    ## Calculate YPR model with new Linf growth parameters
    dat_clim <- abundance_catch_at_age(max_age, N0, Linf_new_clim, Winf, k, t0, a, b, M, L50, k_length, Fmort_overall)
    ind_tempr <- stock_indicators(dat_clim, Linf, Winf)
    scnr_clim$size_indicator_Linf[i] <- ind_tempr$mlength_indicator

    ## Calculate YPR model with new Winf growth parameters
    dat_clim <- abundance_catch_at_age(max_age, N0, Linf, Winf_new_clim, k, t0, a, b, M, L50, k_length, Fmort_overall)  
    ind_tempr <- stock_indicators(dat_clim, Linf, Winf)
    scnr_clim$size_indicator_Winf[i] <- ind_tempr$mweight_indicator

    ## Calculate YPR model with new K growth parameters
    dat_clim <- abundance_catch_at_age(max_age, N0, Linf, Winf, k_new_clim, t0, a, b, M, L50, k_length, Fmort_overall)
    ind_tempr <- stock_indicators(dat_clim, Linf, Winf)
    scnr_clim$size_indicator_K[i] <- ind_tempr$mlength_indicator
    
   }
 
        
    all_scnrs <- c(all_scnrs, list(scnr_clim))

print(all_scnrs)

    # Plot results with all scenarios in one graph
    
    g2 <- ggplot(scnr_clim, aes(x = tempr)) +
      #geom_line(aes(y = size_indicator_Linf), color = "black") +
      #geom_line(aes(y = size_indicator_Winf), color = "blue") +
      geom_vline(xintercept = tempr_opt, linetype = 2) + 
      #geom_hline(yintercept = k_opt, linetype = 2) +
      geom_line(aes(y = size_indicator_K), color = "red") +
      labs(title = target_species$species, x = "Temperature", y = "Size indicator") 

 gall <- c(gall, list(g2))
  
}

gall

wrap_plots(gall)

all_scnr_data <- bind_rows(all_scnrs)

#ggsave(g2, filename = "Shared/Outputs/Lutjanus_sebae.tiff", width = 6, height = 6, dpi = 300)
