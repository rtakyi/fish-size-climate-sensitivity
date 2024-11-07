
rm(list = ls())

# LOAD LIBRARIES
library(TropFishR) 
library(tidyverse)
library(patchwork)

# Set theme for output plots
theme_set(theme_classic())
theme_update(axis.text.x = element_text(colour = "black", size = 12), axis.text.y = element_text(colour = "black", size = 12), axis.title = element_text(size = 12))


#
source("Scripts/fish_size_functions 1.R") #load the functions


# PARAMETERS
case_study_parameters <- read.csv("case_study_species_parameters1.csv")

target_species <- case_study_parameters[case_study_parameters$species == "Lutjanus fulgens" & case_study_parameters$id == "lf",]

n <- nrow(target_species)

# Parameters for scenarios of changes in fish growth due to climate change
multiple_tempr <- seq(1.5, 4, by = 0.1) # temperatures of 1.5oC, 2oC and 4oC anomalise from the IPCC CMIP6 projections regarding (source: https://www.ipcc.ch/report/ar6/wg1/downloads/factsheets/IPCC_AR6_WGI_Regional_Fact_Sheet_Australasia.pdf)
tempr_coef <- 0.02 #temperature coefficient for growth (Cheung et al 2013) (Pauly 1980) (Portner and Peck 2010)
scnr_tempr <- length(multiple_tempr)


# Calculaye indicators for each species

gall <- all_scnrs <- NULL


for (irow in 1:n){
  ## Add climate change impacts on Linf, Winf and K
  scnr_clim <- data.frame(multiple_tempr = multiple_tempr,
                          size_indicator_linf = NA,
                          size_indicator_Winf = NA,
                          size_indicator_K = NA)
  
  
  scnr_clim$size_indicator_linf <- numeric(scnr_tempr)
  scnr_clim$size_indicator_Winf <- numeric(scnr_tempr)
  scnr_clim$size_indicator_K <- numeric(scnr_tempr)

#Fish biology parameters

# Mortality and growth parameters for fish species from literature
Linf <- target_species$Linf..cm.[irow] 
Winf <- target_species$Winf..kg.[irow]
K <- target_species$K..year.[irow]
M <- target_species$M..year.[irow]
t0 <- target_species$t0..year.[irow]
max_age <- target_species$max_age.[irow] 
a <- 0.001 #growth performance index - this is scaling that determines units of weight. The unit of the weight is in grams, so a = 0.001
b <- 3 #growth performance index
Fmort_overall <- target_species$F.[irow] #fishing mortality

# logistic selectivity parameters for fishery
L50 <-  target_species$L50..cm. #length at 50% selectivity 
k_length <- 0.2 #slope of the logistic curve, i.e. how quickly it goes from 0 to 1

# Initial population of fish and age at recruitment into the fishery
N0 <- 42000 #initial population size of fish in tonnes
#recruit_age <- 20 #age at recruitment into the fishery

 


for (i in 1:scnr_tempr){
  Linf_new_clim <- Linf*tempr_coef*multiple_tempr[i] 
  Winf_new_clim <- Winf*tempr_coef*multiple_tempr[i]
  K_new_clim <- K*tempr_coef*multiple_tempr[i] 
  ## Calculate YPR model with new Linf growth parameters
  dat_clim <- abundance_catch_at_age(max_age, N0, Linf_new_clim, Winf, K, t0, a, b, M, L50, k_length, Fmort_overall)
  ind_tempr <- stock_indicators(dat_clim, Linf, Winf)
  scnr_clim$size_indicator_linf[i] <- ind_tempr$mlength_indicator
  
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

all_scnr <- c(all_scnr, list(scnr_clim))

g2 <- ggplot(scnr_clim, aes(x = multiple_tempr)) +
  geom_line(aes(y = size_indicator_linf), color = "black") +
  geom_line(aes(y = size_indicator_Winf), color = "blue") +
  geom_line(aes(y = size_indicator_K), color = "red") +
  labs(title = target_species, x = "Temperature change", y = "Size indicator")

gall <- c(gall, list(g2))

}

gall

wrap_plots(gall)

all_scnr_data <- bind_rows(all_scnrs)
