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
source("Scripts/fish_size_functions v3.R")

# Parameters
case_study_parameters <- read.csv("Parameters/trial_parameters.csv")


target_species <- case_study_parameters


# Number of scenarios
n <- nrow(target_species)

# Parameters for scenarios of changes in temperature, optimum temperature and growth performance due to climate change
temp <- seq(7.1, 34, by = 0.1) # temperature range (Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea; Idike and Lupo (2024). Analysis of sea surface temperature patterns, vari...(29.34))
min_temp_dev <- -17.3 # minimum temperature deviation from optimal temperature    
max_temp_dev <- 10.7 # maximum temperature deviation from optimal temperature   
# tempr_opt <- 27.13 #  source: Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea;
optimal_temp <- target_species$tempr_opt
IPCC_scnrs <- (optimal_temp - optimal_temp) + c(1.5, 2, 4) # optimal temperature based on ipcc (CMIP6) projections (source: Sohou et al. 2020; https://www.ipcc.ch/report/ar6/wg1/downloads/factsheets/IPCC_AR6_WGI_Regional_Fact_Sheet_Australasia.pdf)

# Temperature difference between optimal and temperature sequence
temp_dev <- temp - optimal_temp
optimal_temp_dev <- tempr_opt - tempr_opt
 
k_optimal <- target_species$growth_coef..yr.1. # optimal growth coefficient (source: fishbase.se/manual/key%20facts.htm)
scnr_temp <- length(temp_dev)
# phi_t <- 0.0438 # growth performance per degree change in temperature (source: Kielbassa et al. 2010; Mallet et al. 1999)


# Calculate indicators for each species

# Loop through each species
gall <- all_scnrs <- NULL

for (irow in 1:n){
  ## Add climate change impacts on Linf, Winf and K
  #one way to get all fmors and all temprs
  Fmort <- target_species$Fmort.[irow]

# Parameters for fish growth performance index per degree change in temp with Arhenius quadratic model dervative
  decline_growth <- target_species$dec_gr.[irow] # decline in growth at the extreme temperatures (made up)
  # sln_opt <- target_species$sln_opt.[irow] # slope of the growth performance curve at the optimal temperature (made up)

  scnr_clim <- expand.grid(temp_dev = temp_dev, Fmort = target_species$Fmort.[irow], target_species = target_species$species[irow])
    

  scnr_clim$size_indicator_Linf <- NA
  scnr_clim$size_indicator_Linf_dynamic <- NA
  scnr_clim$size_indicator_Winf <- NA
  scnr_clim$size_indicator_K <- NA
  scnr_clim$Linf <- NA
  scnr_clim$Linf_new_clim <- NA
  scnr_clim$k_new_clim <- NA  # Add this line

  
  scnr_clim$size_indicator_Linf <- numeric(scnr_tempr)
  scnr_clim$size_indicator_Winf <- numeric(scnr_tempr)
  scnr_clim$size_indicator_K <- numeric(scnr_tempr)

  # Fish biology parameters
  Linf_baseline <- target_species$Linf..cm.[irow] 
  Winf_baseline <- target_species$Winf..g.[irow]
  k_baseline <- target_species$growth_coef.[irow]
  M <- target_species$M.[irow]
  t0 <- target_species$t0.[irow]
  max_age <- target_species$max_age.[irow] 
  L50 <- target_species$L50..cm.[irow]
  a <- 0.001 # growth performance index - this is scaling that determines units of weight. The unit of the weight is in grams, so a = 0.001
  b <- 3 # growth performance index
  

  k_length <- 0.2 # slope of the logistic curve
  N0 <- 42000 # initial abundance of fish in tonnes

      
  # Loop through each temperature scenario
  for (i in 1:scnr_tempr){
    
    # Calculate new growth parameters
    k_new_clim <- k_temp_function(temp_dev[i], max_temp_dev, min_temp_dev, optimal_temp_dev, k_optimal)
    #make sure K doesn't go less than zero
    k_new_clim <- max(c(0, k_new_clim))

    # Store k value
    scnr_clim$k_new_clim[i] <- k_new_clim

    #update this with linf model... 
    # Linf_new_clim <- Linf_tempr(k, Linf, k_new_clim, phi_t)
    Linf_new_clim <- Linf_temp_function(k_baseline, Linf_baseline, k_new_clim, temp_dev[i], dec_gr)
    Winf_new_clim <- a*Linf_new_clim^b
    
    # Store Linf values
    scnr_clim$Linf[i] <- Linf
    scnr_clim$Linf_new_clim[i] <- Linf_new_clim

    ## Calculate YPR model with new Linf growth parameters
    dat_clim <- abundance_catch_at_age(max_age, N0, Linf_new_clim, Winf_new_clim, k_new_clim, t0, a, b, M, L50, k_length, Fmort)
    ind_tempr <- stock_indicators(dat_clim, Linf, Linf_new_clim, Winf)
    scnr_clim$size_indicator_Linf[i] <- ind_tempr$mlength_indicator
    scnr_clim$size_indicator_Linf_dynamic[i] <- ind_tempr$mlength_indicator_dynamic
    
    scnr_clim$Fmort <- Fmort
    #scnr_clim$size_indicator_Winf[i] <- ind_tempr$mweight_indicator
    
   }
 
        
    all_scnrs <- c(all_scnrs, list(scnr_clim))



    # Plot results with all scenarios in one graph
    
    
g4 <- ggplot(scnr_clim, aes(x = tempr_dev, y = size_indicator_Linf, group = Fmort)) +
      geom_line(aes(color = Fmort), linetype = "solid") +
      #geom_line(aes(y = size_indicator_Winf), color = "blue") +
      #geom_vline(xintercept = tempr_opt, linetype = 2) + 
      #geom_hline(yintercept = k_opt, linetype = 2) +
      #geom_line(aes(y = size_indicator_K), color = "red") +
      labs(title = target_species$species[i], x = "Deviation in temperature", y = "Size indicator")



  gall <- c(gall, list(g4))#, target_species$species[irow]))
  
}

debugonce(scnr_clim)
ls(scnr_clim)

# gall

# wrap_plots(gall)



all_scnr_data <- bind_rows(all_scnrs)

g5 <- ggplot(all_scnr_data) + 
      aes(x = tempr_dev, y = size_indicator_Linf, color = Fmort, group = Fmort) +
      geom_line() +
      facet_wrap(~target_species) + 
      geom_vline(xintercept = tempr_opt_dev, linetype = 2) +
      geom_vline(xintercept = IPCC_scnrs, linetype = 2, col = "grey") +
      geom_hline(yintercept = k_opt, linetype = 2) +
      annotate("text", x = tempr_opt_dev, y = max(all_scnr_data$size_indicator_Linf), 
               label = "tempr_opt_dev", angle = 90, vjust = -0.5, hjust = 1, color = "black") +
      annotate("text", x = min(all_scnr_data$Fmort), y = k_opt, 
               label = "k_opt", hjust = -0.2, vjust = 1.5, color = "black") +
      labs(title = "Sensitivity of size indicators to changes in Linf due to climate change", 
          x = "Deviation in temperature", y = "Size indicator") +
  #
  scale_color_distiller(palette = "RdBu")

g5
      # scale_colour
  # scale_colour_continuous(trans = "reverse")
      
      #labs(title = paste("Sensitivity of size indicators to changes in Linf due to climate change", 
                #target_species$species[1], "ID:", id), x = "Temperature", y = "Size indicator") + 
                #scale_colour_continuous(trans = "reverse")

 g5

# Save plot
ggsave(g5, filename = paste0("Shared/Outputs/size_indicator_sensitivity", target_species$species, ".tiff"), width = 6, height = 9, units = "in", dpi = 300)


#
# Compare dynamic and static linf indicators 
#


g6 <- 
  all_scnr_data %>%
  filter(Fmort %in% c(0.09, 0.35, 0.43)) %>%
  ggplot() + 
  aes(x = tempr_dev, y = size_indicator_Linf, group = Fmort) +
  geom_line() +
  geom_line(aes(y = size_indicator_Linf_dynamic), color = "red") +
  facet_grid(Fmort~target_species) + 
  geom_vline(xintercept = tempr_opt_dev, linetype = 2) +
  geom_vline(xintercept = IPCC_scnrs, linetype = 2, col = "grey") +
  geom_hline(yintercept = k_opt, linetype = 2) +
  # annotate("text", x = tempr_opt_dev, y = max(all_scnr_data$size_indicator_Linf), 
           # label = "tempr_opt_dev", angle = 90, vjust = -0.5, hjust = 1, color = "black") +
  # annotate("text", x = min(all_scnr_data$Fmort), y = k_opt, 
           # label = "k_opt", hjust = -0.2, vjust = 1.5, color = "black") +
  labs(title = "Sensitivity of size indicators to changes in Linf due to climate change", 
       x = "Deviation in temperature", y = "Size indicator") +
  #
  scale_color_distiller(palette = "RdBu")

g6

# Plot comparing original and new Linf values
g7 <- 
  all_scnr_data %>%
  filter(Fmort %in% c(0.09, 0.35, 0.43)) %>%
  ggplot() + 
  aes(x = tempr_dev) +
  geom_line(aes(y = Linf, color = "Original Linf")) +
  geom_line(aes(y = Linf_new_clim, color = "Modified Linf")) +
  facet_grid(Fmort~target_species) + 
  geom_vline(xintercept = tempr_opt_dev, linetype = 2) +
  geom_vline(xintercept = IPCC_scnrs, linetype = 2, col = "grey") +
  ylim(0, 300) + 
  scale_color_manual(name = "Linf Type",
                    values = c("Original Linf" = "black", "Modified Linf" = "red")) +
  labs(title = "Comparison of Original and Temperature-Modified Linf", 
       x = "Deviation in temperature", 
       y = "Length (cm)")

g7

# Plot comparing k_new_clim with k_opt
g8 <- 
  all_scnr_data %>%
  filter(Fmort %in% c(0.09, 0.35, 0.43)) %>%
  ggplot() + 
  aes(x = tempr_dev) +
  geom_line(aes(y = k_new_clim, color = "Modified k")) +
  geom_hline(yintercept = k_opt, linetype = 2, color = "blue") +
  facet_grid(Fmort~target_species) + 
  geom_vline(xintercept = tempr_opt_dev, linetype = 2) +
  geom_vline(xintercept = IPCC_scnrs, linetype = 2, col = "grey") +
  scale_color_manual(name = "Growth Parameter",
                    values = c("Modified k" = "red")) +
  labs(title = "Comparison of Temperature-Modified k with k_opt", 
       x = "Deviation in temperature", 
       y = "Growth coefficient (k)") +
  ylim(0, max(c(k_opt * 1.5, max(all_scnr_data$k_new_clim, na.rm = TRUE))))

g8

# g6 <- ggplot(all_scnr_data) + 
#   aes(x = tempr_dev, y = size_indicator_Linf, color = Fmort, group = Fmort) +
#   geom_line() +
#   facet_wrap(~target_species) + 
#   #geom_vline(xintercept = tempr_opt_dev, linetype = 2) +
#   geom_vline(xintercept = IPCC_scnrs, linetype = 2, col = "grey") +
#   geom_hline(yintercept = k_opt, linetype = 2) +
#   annotate("text", x = tempr_opt_dev, y = max(all_scnr_data$size_indicator_Linf), 
#            label = "tempr_opt_dev", angle = 90, vjust = -0.5, hjust = 1, color = "black") +
#   annotate("text", x = min(all_scnr_data$Fmort), y = k_opt, 
#            label = "k_opt", hjust = -0.2, vjust = 1.5, color = "black") +
#   labs(title = "Sensitivity of size indicators to changes in Linf due to climate change", 
#        x = "Temperature relative to optimum", y = "Size indicator") +
#   #
#   scale_color_distiller(palette = "Reds")
# g6

# scale_colour_continuous(trans = "reverse")

#labs(title = paste("Sensitivity of size indicators to changes in Linf due to climate change", 
#target_species$species[1], "ID:", id), x = "Temperature", y = "Size indicator") + 
#scale_colour_continuous(trans = "reverse")

# g5


#ggsave(g3, filename = paste0("Shared/Outputs/size_indicator_sensitivity_", target_species$species,
         #"_", target_species$id, ".tiff"), width = 6, height = 9, units = "in", dpi = 300)

