
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
case_study_parameters <- read.csv("Parameters/cs_sp_parameters_slow.csv")


target_species <- case_study_parameters


# Number of scenarios
n <- nrow(target_species)

# Parameters for scenarios of changes in temperature, optimum temperature and growth performance due to climate change
tempr <- seq(12, 38, by = 0.1) # temperature range (Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea; Idike and Lupo (2024). Analysis of sea surface temperature patterns, vari...(29.34))
tempr_min_dev <- -15.3 # minimum temperature deviation from optimal temperature    
tempr_max_dev <- 10.7 # maximum temperature deviation from optimal temperature   
tempr_opt <- 27.13 #  source: Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea;
IPCC_scnrs <- (tempr_opt - tempr_opt) + c(1.5, 2, 4) # optimal temperature based on ipcc (CMIP6) projections (source: Sohou et al. 2020; https://www.ipcc.ch/report/ar6/wg1/downloads/factsheets/IPCC_AR6_WGI_Regional_Fact_Sheet_Australasia.pdf)

# Temperature difference between optimal and temperature sequence
tempr_dev <- tempr - tempr_opt
tempr_opt_dev <- tempr_opt - tempr_opt
 
k_opt <- 0.3 # optimal growth coefficient (source: fishbase.se/manual/key%20facts.htm)
scnr_tempr <- length(tempr_dev)
phi_1 <- 0.0438 # growth performance per degree change in temperature (source: Kielbassa et al. 2010; Mallet et al. 1999)


# Calculate indicators for each species

# Loop through each species
gall <- all_scnrs <- NULL

for (irow in 1:n){
  ## Add climate change impacts on Linf, Winf and K
  #one way to get all fmors and all temprs
  Fmort <- target_species$Fmort.[irow]
  scnr_clim <- expand.grid(tempr_dev = tempr_dev, Fmort = target_species$Fmort.[irow], target_species = target_species$species[irow])
  

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
  

  k_length <- 0.2 # slope of the logistic curve
  N0 <- 42000 # initial abundance of fish in tonnes

  # Loop through each temperature scenario
  for (i in 1:scnr_tempr){
    
    # Calculate new growth parameters
    k_new_clim <- k_tempr(tempr_dev[i], tempr_max_dev, tempr_min_dev, tempr_opt_dev, k_opt)
    #make sure K doesn't go less than zero
    k_new_clim <- max(c(0, k_new_clim))

    #update this with linf model... 
    Linf_new_clim <- Linf_tempr(k, Linf, k_new_clim, phi_1)
    Winf_new_clim <- a*Linf_new_clim^b
    

    ## Calculate YPR model with new Linf growth parameters
    dat_clim <- abundance_catch_at_age(max_age, N0, Linf_new_clim, Winf_new_clim, k_new_clim, t0, a, b, M, L50, k_length, Fmort)
    ind_tempr <- stock_indicators(dat_clim, Linf, Winf)
    scnr_clim$size_indicator_Linf[i] <- ind_tempr$mlength_indicator
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

# debugonce(ggplot(g4))
# ls(scnr_clim)

gall

wrap_plots(gall)



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
      # scale_colour
  # scale_colour_continuous(trans = "reverse")
      
      #labs(title = paste("Sensitivity of size indicators to changes in Linf due to climate change", 
                #target_species$species[1], "ID:", id), x = "Temperature", y = "Size indicator") + 
                #scale_colour_continuous(trans = "reverse")

 g5

# Save plot
ggsave(g5, filename = paste0("Shared/Outputs/size_indicator_sensitivity", target_species$species, ".tiff"), width = 6, height = 9, units = "in", dpi = 300)


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

