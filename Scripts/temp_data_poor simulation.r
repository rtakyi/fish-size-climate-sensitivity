# Simulation of the impact of climate change on fish size indicators using TropFishR package in R.
# Created by: Richard Takyi and Christopher J. Brown


rm(list = ls())

# Load libraries
library(TropFishR)
library(tidyverse)
library(patchwork)
library(reshape2)

# Define reusable palette
palette_name <- "RdBu"

# Set theme for output plots
theme_set(theme_classic())
theme_update(axis.text.x = element_text(colour = "black", size = 17),
              axis.text.y = element_text(colour = "black", size = 17),
              axis.title = element_text(size = 17),
              strip.text = element_text(size = 16),
              legend.text = element_text(size = 17),
              legend.title = element_text(size = 17),
              legend.key.size = unit(1, "cm"))

# Load functions
source("Scripts/fish_size_functions v3.R")


# Parameters of case study species
# target_species <- read.csv("Parameters/cs_sp_parameters_fast.csv")
case_study_parameters <- read.csv("Parameters/cs_sp_parameters_slow.csv")

target_species <- case_study_parameters[case_study_parameters$species == "Pseudotolithus senegalensis" & case_study_parameters$id == "ps",]

# Number of scenarios
n <- nrow(target_species)

# Parameters for environmental temperature change scenarios (minimum, maximum and optimal temperature)
temp <- seq(17.5, 32.1, by = 0.1)   # temperature range (Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea; Idike and Lupo (2024). Analysis of sea surface temperature patterns, vari...(29.34))
min_temp_dev <- -9.5   # minimum temperature deviation from optimal temperature 
max_temp_dev <- 6.0  # maximum temperature deviation from optimal temperature
sp_opt_temp <- target_species$sp_opt_temp # species optimal temperature (source: fishbase.se/manual/key%20facts.htm)

# IPCC scenarios with optimal temperature 
IPCC_scnrs <- (sp_opt_temp - sp_opt_temp) + c(1, 2, 4) # optimal temperature based on ipcc (CMIP6) projections (source: Sohou et al. 2020; https://www.ipcc.ch/report/ar6/wg1/downloads/factsheets/IPCC_AR6_WGI_Regional_Fact_Sheet_Australasia.pdf)

# Temperature difference between optimal and temperature sequence
#  optimal_temp_dev <- sp_opt_temp_optk - sp_opt_temp_optk # optimal temperature (source: fishbase.se/manual/key%20facts.htm)


# Temperature scenario length
scnr_temp <- length(temp)


# Loop through each species
gall <- all_scnrs <- NULL

#Loop over target species parameter and f values
for (irow in 1:n){
  ## Add climate change impacts on Linf, Winf and K
  #one way to get all fmors and all temprs
  Fmort <- target_species$Fmort.[irow]
  temp_dev <- temp - sp_opt_temp # temperature deviation from optimal temperature
  
  scnr_clim <- expand.grid(temp_dev = temp_dev, Fmort = target_species$Fmort.[irow], target_species = target_species$species[irow])
  

  scnr_clim$size_indicator_Linf <- NA
  scnr_clim$size_indicator_Linf_dynamic <- NA
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

  a <- target_species$scaler[irow] # a scaling factor that converts length to weight. The unit of the weight is in grams, so a = 0.001
  b <- target_species$exponent[irow] # exponent of the length-weight relationship (source: Pauly 1983; Froese and Pauly 2023)
  
  decline_gro_temp <- target_species$dec_gr[irow] # decline in growth at the extreme temperatures

  # sp_opt_temp_optk <- target_species$sp_opt_temp[irow] - optimal_temp # optimal temperature for each species (source: Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea; Idike and Lupo (2024). Analysis of sea surface temperature patterns, vari...(29.34))
  sp_opt_temp_dev <- sp_opt_temp[irow] - sp_opt_temp # optimal temperature for each species (source: Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea; Idike and Lupo (2024). Analysis of sea surface temperature patterns, vari...(29.34))

  
  k_length <- 0.2 # slope of the logistic curve
  N0 <- 42000 # initial abundance of fish in tonnes

  

  # Loop through each temperature scenario
  for (i in 1:scnr_temp){
    
    # Calculate new growth parameters
    # RT: use 0 here for optimal_temp as we are looking at deviations
    # your sp_opt_temp_optk_dev was a vector of zeros, we just want a single value
    k_new_clim <- k_temp_function(temp_dev[i], max_temp_dev, min_temp_dev, 0, k_opt)
    #make sure K doesn't go less than zero
    k_new_clim <- max(c(0, k_new_clim))

    
    #update this with linf model...
    Linf_new_clim <- Linf_temp_function(k, Linf, k_new_clim, temp_dev[i], decline_gro_temp)
    scnr_clim$Linf_new_clim[i] <- Linf
    scnr_clim$k_new_clim[i] <- Linf_new_clim
    
    #update natural mortality with temperature
    # M_new_clim <- M_temp_function(temp[i], Linf, Linf_new_clim, k_new_clim, k, temp[i], M)
    M_new_clim <- M_temp_function(temp[i], Linf_new_clim, Linf, k_new_clim, k, sp_opt_temp, M)


    # update Linf_new_clim with climate change by reducing Linf by 1% for every degree change in temperature
    # dyna_Linf_new_clim <- Linf_new_clim - (Linf_new_clim * 0.01 * temp_dev[i])
    # dyna_Linf_new_clim <- Linf_new_clim - (Linf_new_clim * 0.01 * abs(temp_dev[i]))
 
    Winf_new_clim <- a*Linf_new_clim^b
    

    ## Calculate YPR model with new Linf growth parameters
    # dat_clim <- abundance_catch_at_age(max_age, N0, Linf_new_clim, Winf_new_clim, k_new_clim, t0, a, b, M, L50, k_length, Fmort)
      dat_clim <- abundance_catch_at_age(max_age, N0, Linf_new_clim, Winf_new_clim, k_new_clim, t0, a, b, M_new_clim, L50, k_length, Fmort)
    # ind_temp <- stock_indicators(dat_clim, Linf, dyna_Linf_new_clim, Winf_new_clim)
    ind_temp <- stock_indicators(dat_clim, Linf, Linf_new_clim, Winf)
    scnr_clim$size_indicator_Linf[i] <- ind_temp$mlength_indicator
    scnr_clim$size_indicator_Linf_dynamic[i] <- ind_temp$mlength_indicator_dynamic
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
      labs(title = target_species$species[i], x = "Deviation in temperature", y = "Length-based indicator")



  gall <- c(gall, list(g4))
  
}

#  print(scnr_clim)
# debugonce(ggplot(g4))
# ls(scnr_clim)

# gall

# wrap_plots(gall)

xlab <- "Deviation in temperature\n from optimal"

all_scnr_data <- bind_rows(all_scnrs)

g5 <- ggplot(all_scnr_data) + 
      aes(x = temp_dev, y = size_indicator_Linf, color = Fmort, group = Fmort) +
      geom_line() +
      facet_wrap(~target_species) + 
      geom_vline(xintercept = sp_opt_temp_dev, linetype = 2) +
      annotate("text", x = sp_opt_temp_dev, y = max(all_scnr_data$size_indicator_Linf), 
              label = "sp_opt_temp_dev", angle = 90, vjust = -0.5, hjust = 1, size = 6, color = "black") +
      labs(title = "Sensitivity of length indicators to changes in L∞ due to climate change", 
          x = "Deviation in temperature (°C)", y = "Stationary length indicator") +
  
  scale_color_distiller(palette = "RdBu",)

  
 g5 <- g5 + theme(strip.text = element_text(face = "italic"))

# # Save plots
#  ggsave(g5, filename = paste0("Shared/Outputs/size_indicator_sensitivity", target_species$species, ".png"), width = 12, height = 9, units = "in", dpi = 600)


# unique(all_scnr_data$temp_dev)
#  all_scnr_data %>%
#    filter(temp_dev %in% c(-5, -3, -1, 0, 1, 3, 5)) %>%
#  ggplot() + 
#  aes(x = Fmort, y = size_indicator_Linf, group = temp_dev, color = temp_dev) +
#   #  aes(x = size_indicator_Linf, y = Fmort, group = temp_dev,color = temp_dev) +
#    geom_line() +
#    facet_wrap(~target_species) + 
#    labs(title = "Sensitivity of length indicators to changes in L∞ due to climate change", 
#         y= "static length indicator", x = "Fmort") +

#    scale_color_distiller(palette = "Dark2",) + 
#    theme_bw()
# # Save the plot
# ggsave(filename = paste0("Shared/Outputs/size_indicator_sensitivity", target_species$species, "_Fmort.png"), width = 12, height = 9, units = "in", dpi = 600)





# Filter for specific fishing mortality rates before plotting for conference presentation


# filtered_scnr_data <- all_scnr_data %>%
#   filter(Fmort %in% c(0.01, 1.77))

# g5 <- ggplot(filtered_scnr_data) +
#   aes(x = temp_dev, y = size_indicator_Linf, color = factor(Fmort), group = Fmort) +
#   geom_line() +
#   facet_wrap(~target_species) +
#   geom_vline(xintercept = sp_opt_temp_dev, linetype = 2) +
#   annotate("text", x = sp_opt_temp_dev, y = max(filtered_scnr_data$size_indicator_Linf, na.rm = TRUE),
#     label = "sp_opt_temp_dev", angle = 90, vjust = -0.5, hjust = 1,
#     size = 6, color = "black") +
#   labs(title = "Sensitivity of length-based indicators to changes in L∞ due to climate change",
#     x = "Deviation in temperature (°C)",
#     y = "Static Length-based indicator",
#     color = "Fmort") +
#   scale_color_manual(values = c("0.01" = "blue", "1.77" = "red")) 

# g5 <- g5 + theme(strip.text = element_text(face = "italic"))

# # # Save plots
# ggsave(g5, filename = paste0("Shared/Outputs/size_indicator_sensitivity", target_species$species, ".png"), width = 12, height = 9, units = "in", dpi = 600)



# # Dynamic Linf (Non-stationary length indicator)
g6 <- ggplot(all_scnr_data) + 
  aes(x = temp_dev, y = size_indicator_Linf_dynamic, color = Fmort, group = Fmort) +
  geom_line() +
  facet_wrap(~target_species) + 
  geom_vline(xintercept = sp_opt_temp_dev, linetype = 2) +
  annotate("text", x = sp_opt_temp_dev, y = max(all_scnr_data$size_indicator_Linf_dynamic), 
          label = "sp_opt_temp_dev", angle = 90, vjust = -0.5, hjust = 1, size = 6, color = "black") +
  labs(title = "Sensitivity of length-based indicators to changes in L∞ due to climate change", 
       x = "Deviation in temperature (°C)", y = "Non-stationary length indicator") +

  scale_color_distiller(palette = "RdBu") 

g6 <- g6 + theme(strip.text = element_text(face = "italic"))



# unique(all_scnr_data$temp_dev)
#  all_scnr_data %>%
#    filter(temp_dev %in% c(-5, -3, -1, 0, 1, 3, 5)) %>%
#  ggplot() + 
#  aes(x = Fmort, y = size_indicator_Linf_dynamic, group = temp_dev, color = temp_dev) +
#   #  aes(x = size_indicator_Linf, y = Fmort, group = temp_dev,color = temp_dev) +
#    geom_line() +
#    facet_wrap(~target_species) + 
#    labs(title = "Sensitivity of length indicators to changes in L∞ due to climate change", 
#         y= "dynamic length indicator", x = "Fmort") +

#    scale_color_distiller(palette = "Dark2",) + 
#    theme_bw()
# # Save the plot
# ggsave(filename = paste0("Shared/Outputs/size_indicator_sensitivity", target_species$species, "_Fmort_dynamic.png"), width = 12, height = 9, units = "in", dpi = 600)

all_scnr_data$Linf_diff <- with(all_scnr_data, size_indicator_Linf - size_indicator_Linf_dynamic)


g7 <- ggplot(all_scnr_data) + 
  aes(x = temp_dev, y = Linf_diff, color = Fmort, group = Fmort) +
  geom_line() +
  facet_wrap(~target_species) + 
  geom_vline(xintercept = sp_opt_temp_dev, linetype = 2) +
  geom_hline(yintercept = 0, linetype = 2) +
  annotate("text", x = sp_opt_temp_dev, y = max(all_scnr_data$size_indicator_Linf), 
          label = "sp_opt_temp_dev", angle = 90, vjust = -0.5, hjust = 1, size = 6, color = "black") +
    labs(title = "Sensitivity of length-based indicators to changes in L∞ due to climate change", 
       x = "Deviation in temperature (°C)", y = "Estimation bias \n (Stationary vs non-stationary length indicator)") +

  scale_color_distiller(palette = "RdBu")

g7 <- g7 + theme(strip.text = element_text(face = "italic"))

# sen_temp_dev <- (g5 + g6 + g7) +
#   plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') &
#   theme(plot.tag = element_text(size = 20, vjust = -0.2))

# sen_temp_dev

# # Save plots as one
ggsave(g5 + g6 + g7, filename = paste0("Shared/Outputs/size_indicator_sensitivity", target_species$species, ".png"), width = 15, height = 8, units = "in", dpi = 600)
# ggsave(sen_temp_dev, filename = paste0("Shared/Outputs/size_indicator_sensitivity", target_species$species, ".tiff"), width = 15, height = 8, units = "in", dpi = 300)


#  g8 <- all_scnr_data %>%
#   filter(Fmort %in% c(0.01, 0.4, 1.77)) %>%
#   ggplot() +
#   aes(x = temp_dev, y = size_indicator_Linf, group = Fmort) +
#   geom_line() +
#   geom_line(aes(y = size_indicator_Linf_dynamic), color = "red") +
#   facet_grid(Fmort~target_species) +
#   geom_vline(xintercept = sp_opt_temp_dev, linetype = 2) +
#   annotate("text", x = sp_opt_temp_dev, y = max(all_scnr_data$size_indicator_Linf), 
#           label = "sp_opt_temp_dev", angle = 90, vjust = -0.5, hjust = 1, color = "black") +
#           labs(title = "Sensitivity of size indicators to climate change at low_medium_high fishing mortality",
#        x = "Deviation in temperature (°C)", y = "Static vs dynamic size indicator differences") +
  
#   scale_color_distiller(palette = "RdBu")

# # g8
# # # Save plots
# # # ggsave(g8, filename = paste0("Shared/Outputs/size_indicator_sensitivity", target_species$species, "_Fmort.png"), width = 12, height = 9, units = "in", dpi = 600)


g9 <- all_scnr_data %>%
  filter(temp_dev %in% c(-5, 0, +5), Fmort %in% c(0.01, 0.4, 1.77)) %>%
  ggplot() +
  aes(x = factor(target_species, levels = unique(target_species)), y = Linf_diff, color = factor(temp_dev)) +
  geom_point(size = 10) +
  geom_line() +
  facet_wrap(~ Fmort) +
  labs(title = "Sensitivity of L∞ diff to temperature and fishing mortality", 
       x = "Species (fast to slow growing)", y = "Stationary vs non-stationary length-based indicator differences", color = "Deviation in temperature (°C)") +
  scale_color_brewer(palette = "RdBu")

g9 <- g9 + theme(axis.text.x = element_text(face = "italic"))

# # Save plots 
ggsave(g9, filename = paste0("Shared/Outputs/size_indicator_sensitivity", target_species$species, "Linf_diff.png"), width = 15, height = 8, units = "in", dpi = 600)


# # Save plots as one
# g9 <- g9 + theme(axis.text.x = element_text(angle = 10, hjust = 1))

# ggsave(g8 + g9, filename = paste0("Shared/Outputs/size_indicator_sensitivity", target_species$species, "_Fmort.tiff"), width = 12, height = 9, units = "in", dpi = 300)


# g10 <- all_scnr_data %>%
#   filter(temp_dev %in% c(-5, 0, +5), Fmort %in% c(0.01, 0.4, 1.77)) %>%
#   ggplot() +
#   aes(x = factor(Fmort, levels = c(0.01, 0.4, 1.77)), y = Linf_diff, color = factor(temp_dev), group = interaction(target_species, temp_dev)) +
#   geom_point(size = 10) +
#   facet_wrap(~ target_species) +
#   labs(title = "Sensitivity of L∞ diff to temperature and fishing mortality", 
#        x = "Fishing mortality rate (yr¯¹)", y = "Stationary vs non-stationary length-based indicator differences", color = "Deviation in temperature (°C)") +
#   scale_color_brewer(palette = "RdBu")

# g10 <- g10 + theme(strip.text = element_text(face = "italic"))

# # # Save plots 
# ggsave(g10, filename = paste0("Shared/Outputs/size_indicator_sensitivity", target_species$species, "Linf_diff.png"), width = 15, height = 8, units = "in", dpi = 600)
