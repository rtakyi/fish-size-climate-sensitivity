
rm(list = ls())

# Load libraries
library(TropFishR)
library(tidyverse)
library(patchwork)
library(reshape2)

# Set theme for output plots
theme_set(theme_classic())
theme_update(axis.text.x = element_text(colour = "black", size = 16),
              axis.text.y = element_text(colour = "black", size = 16),
              axis.title = element_text(size = 12),
              strip.text = element_text(size = 16))

# Load functions
source("Scripts/fish_size_functions v3.R")


# Parameters of case study species
# target_species <- read.csv("Parameters/cs_sp_parameters_fast.csv")
case_study_parameters <- read.csv("Parameters/cs_sp_parameters_fast.csv")

target_species <- case_study_parameters[case_study_parameters$species == "Scomber colias" & case_study_parameters$id == "sc",]

# Number of scenarios
n <- nrow(target_species)

# Parameters for environmental temperature change scenarios (minimum, maximum and optimal temperature)
temp <- seq(5, 32, by = 0.1)   # temperature range (Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea; Idike and Lupo (2024). Analysis of sea surface temperature patterns, vari...(29.34))
min_temp_dev <- -17.3   # minimum temperature deviation from optimal temperature
max_temp_dev <- 10.7  # maximum temperature deviation from optimal temperature
sp_opt_temp_optk <- target_species$sp_opt_temp # species optimal temperature (source: fishbase.se/manual/key%20facts.htm)

# IPCC scenarios with optimal temperature 
IPCC_scnrs <- (sp_opt_temp_optk - sp_opt_temp_optk) + c(1, 2, 4) # optimal temperature based on ipcc (CMIP6) projections (source: Sohou et al. 2020; https://www.ipcc.ch/report/ar6/wg1/downloads/factsheets/IPCC_AR6_WGI_Regional_Fact_Sheet_Australasia.pdf)

# Temperature difference between optimal and temperature sequence
#  optimal_temp_dev <- sp_opt_temp_optk - sp_opt_temp_optk # optimal temperature (source: fishbase.se/manual/key%20facts.htm)

# Temperature scenario length
scnr_temp <- length(temp)


# Loop through each species
gall <- all_scnrs <- NULL

for (irow in 1:n){
  ## Add climate change impacts on Linf, Winf and K
  #one way to get all fmors and all temprs
  Fmort <- target_species$Fmort.[irow]
  temp_dev <- temp - sp_opt_temp_optk # temperature deviation from optimal temperature
  
  scnr_clim <- expand.grid(temp_dev = temp_dev, Fmort = target_species$Fmort.[irow], target_species = target_species$species[irow])
  

  scnr_clim$size_indicator_Linf <- NA
  scnr_clim$size_indicator_Winf <- NA
  scnr_clim$size_indicator_K <- NA
 
  
  scnr_clim$size_indicator_Linf <- numeric(scnr_temp)
  scnr_clim$size_indicator_Winf <- numeric(scnr_temp)
  scnr_clim$size_indicator_K <- numeric(scnr_temp)

  # Baseline fish biology parameters
  Linf <- target_species$Linf..cm.[irow] 
  Winf <- target_species$Winf..g.[irow]
  k_opt <- k <- target_species$growth_coef.[irow]
  M <- target_species$M.[irow]
  t0 <- target_species$t0.[irow]
  max_age <- target_species$max_age.[irow] 
  L50 <- target_species$L50..cm.[irow]
  a <- 0.001 # a scaling factor that converts length to weight. The unit of the weight is in grams, so a = 0.001
  b <- 3.07 # exponent of the length-weight relationship (source: Pauly 1983; Froese and Pauly 2023)
  
  decline_gro_temp <- target_species$dec_gr[irow] # decline in growth at the extreme temperatures

  # sp_opt_temp_optk <- target_species$sp_opt_temp[irow] - optimal_temp # optimal temperature for each species (source: Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea; Idike and Lupo (2024). Analysis of sea surface temperature patterns, vari...(29.34))
  sp_opt_temp_optk_dev <- sp_opt_temp_optk[irow] - sp_opt_temp_optk # optimal temperature for each species (source: Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea; Idike and Lupo (2024). Analysis of sea surface temperature patterns, vari...(29.34))

  
  k_length <- 0.2 # slope of the logistic curve
  N0 <- 42000 # initial abundance of fish in tonnes

  

  # Loop through each temperature scenario
  for (i in 1:scnr_temp){
    
    # Calculate new growth parameters
    k_new_clim <- k_temp_function(temp_dev[i], max_temp_dev, min_temp_dev, sp_opt_temp_optk_dev, k_opt)
    #make sure K doesn't go less than zero
    k_new_clim <- max(c(0, k_new_clim))

    #update this with linf model...
    Linf_new_clim <- Linf_temp_function(k, Linf, k_new_clim, temp_dev[i], decline_gro_temp)
    scnr_clim$Linf_new_clim[i] <- Linf
    scnr_clim$k_new_clim[i] <- Linf_new_clim
    
    # update Linf_new_clim with climate change by reducing Linf by 1% for every degree change in temperature
    # dyna_Linf_new_clim <- Linf_new_clim - (Linf_new_clim * 0.01 * temp_dev[i])
    dyna_Linf_new_clim <- Linf_new_clim - (Linf_new_clim * 0.01 * abs(temp_dev[i]))
 
    Winf_new_clim <- a*Linf_new_clim^b
    

    ## Calculate YPR model with new Linf growth parameters
    dat_clim <- abundance_catch_at_age(max_age, N0, dyna_Linf_new_clim, Winf_new_clim, k_new_clim, t0, a, b, M, L50, k_length, Fmort)
    # ind_temp <- stock_indicators(dat_clim, Linf, dyna_Linf_new_clim, Winf_new_clim)
    ind_temp <- stock_indicators(dat_clim, Linf, dyna_Linf_new_clim, Winf)
    scnr_clim$size_indicator_Linf[i] <- ind_temp$mlength_indicator
    scnr_clim$Fmort <- Fmort
    # scnr_clim$size_indicator_Winf[i] <- ind_tempr$mweight_indicator
    
   }
 
        
    all_scnrs <- c(all_scnrs, list(scnr_clim))



    # Plot results with all scenarios in one graph
    
    
g4 <- ggplot(scnr_clim, aes(x = temp_dev, y = size_indicator_Linf, group = Fmort)) +
      geom_line(aes(color = Fmort), linetype = "solid") +
      # geom_line(aes(y = size_indicator_Winf), color = "blue") +
      # geom_vline(xintercept = optimal_temp, linetype = 2) + 
      # geom_vline(xintercept = sp_opt_temp_optk, linetype = 2, col = "grey") +
      # geom_hline(yintercept = k_opt, linetype = 2) +
      #geom_line(aes(y = size_indicator_K), color = "red") +
      labs(title = target_species$species[i], x = "Deviation in temperature", y = "Size indicator")



  gall <- c(gall, list(g4))
  
}
# print(scnr_clim)
# debugonce(ggplot(g4))
# ls(scnr_clim)

# gall

# wrap_plots(gall)



all_scnr_data <- bind_rows(all_scnrs)

g5 <- ggplot(all_scnr_data) + 
      aes(x = temp_dev, y = size_indicator_Linf, color = Fmort, group = Fmort) +
      geom_line() +
      facet_wrap(~target_species) + 
      geom_vline(xintercept = sp_opt_temp_optk_dev, linetype = 2) +
      geom_vline(xintercept = IPCC_scnrs, linetype = 2, col = "grey") +
      # geom_vline(xintercept = sp_opt_temp_optk, linetype = 2, col = "brown") +
      geom_hline(yintercept = k_opt, linetype = 2) +
      annotate("text", x = sp_opt_temp_optk_dev, y = max(all_scnr_data$size_indicator_Linf), 
               label = "sp_opt_temp_optk_dev", angle = 90, vjust = -0.5, hjust = 1, color = "black") +
      # annotate("text", x = sp_opt_temp_optk, y = max(all_scnr_data$size_indicator_Linf),
      #          label = "sp_opt_temp_optk", angle = 90, vjust = -0.5, hjust = 1, color = "black") +
      annotate("text", x = min(all_scnr_data$Fmort), y = k_opt, 
               label = "k_opt", hjust = -0.2, vjust = 1.5, color = "black") +
      labs(title = "Sensitivity of size indicators to changes in Linf due to climate change", 
          x = "Deviation in temperature", y = "Size indicator") +
  #
  scale_color_distiller(palette = "RdBu")
      
 g5

# Save plot
ggsave(g5, filename = paste0("Shared/Outputs/size_indicator_sensitivity", target_species$species, ".tiff"), width = 6, height = 9, units = "in", dpi = 300)


