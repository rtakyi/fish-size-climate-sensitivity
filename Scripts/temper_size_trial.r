

## load library
library(ggplot2)
library(patchwork)
library(reshape2)

# Set theme for output plots
theme_set(theme_classic())
theme_update(axis.text.x = element_text(colour = "black", size = 24), 
              axis.text.y = element_text(colour = "black", size = 24), axis.title = element_text(size = 24),
              plot.title = element_text(size = 26, hjust = 0.5))



## simulate temperature effect on growth coeffecients: source: Kielbassa et al. 2010; Mallet et al. 1999 
k_temp_function <- function(tempr, tempr_max, tempr_min, tempr_opt, k_opt){
    growth_coef_tempr <- k_opt * ((tempr - tempr_min) * (tempr - tempr_max) /
                                    ((tempr - tempr_min) * (tempr - tempr_max) - (tempr - tempr_opt)^2)
                                  )
    return(growth_coef_tempr)
}


#  simulate temperature effect on growth performance and asymptotic length: source: Kielbassa et al. 2010; Mallet et al. 1999
Linf_temp_function <- function(k_baseline, Linf, 
    k_under_new_temperature, 
    temperature_deviation, 
    linf_temperature_sensitivity){

    ph_0 <- log10(k_baseline*Linf^2)
    ph_T <- ph_0 + linf_temperature_sensitivity*temperature_deviation
    asymp_L_tempr <- sqrt((10^(ph_T)) / (k_under_new_temperature))
    return(asymp_L_tempr)
}


## simulate temperature effect on growth performance and asymptotic weight: source: Kielbassa et al. 2010; Mallet et al. 1999
Winf_tempr <- function(Winf, Linf, Linf_tempr){
    Winf_tempr_1 <- Winf * (Linf_tempr / Linf)^b 
    return(Winf_tempr_1)
}

# # Parameters
case_study_parameters <- read.csv("Parameters/trial_parameters.csv")
target_species <- case_study_parameters[1,]

# Parameters for scenarios of changes in temperature, optimum temperature and growth performance due to climate change
tempr <- seq(12, 28, by = 0.1) # temperature range (Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea; Idike and Lupo (2024). Analysis of sea surface temperature patterns, vari...(29.34))
tempr_min_dev <- -10.3 # minimum temperature deviation from optimal temperature    
tempr_max_dev <- 7.7 # maximum temperature deviation from optimal temperature   
tempr_opt <- target_species$sp_opt_temp #27.13 #  source: Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea;


IPCC_scnrs <- (tempr_opt - tempr_opt) + c(1, 2, 4) # optimal temperature based on ipcc (CMIP6) projections (source: Sohou et al. 2020; https://www.ipcc.ch/report/ar6/wg1/downloads/factsheets/IPCC_AR6_WGI_Regional_Fact_Sheet_Australasia.pdf)


# Temperature difference between optimal and temperature sequence
# tempr_dev <- tempr - tempr_opt
# tempr_opt_dev <- tempr_opt - tempr_opt

sp_opt_temp_optk <- target_species$sp_opt_temp # species optimal temperature (source: fishbase.se/manual/key%20facts.htm)
sp_opt_temp_optk_dev <- target_species$sp_opt_temp - target_species$sp_opt_temp # optimal temperature (source: fishbase.se/manual/key%20facts.htm)
# IPCC_scnrs <- (sp_opt_temp_optk - sp_opt_temp_optk) + c(1, 2, 4) # optimal temperature based on ipcc (CMIP6) projections (source: Sohou et al. 2020; https://www.ipcc.ch/report/ar6/wg1/downloads/factsheets/IPCC_AR6_WGI_Regional_Fact_Sheet_Australasia.pdf)

# tempr_dev <- tempr - sp_opt_temp_optk # temperature deviation from optimal temperature
tempr_dev <- tempr - tempr_opt # temperature deviation from optimal temperature

k_opt <- target_species$growth_coef..yr.1. # optimal growth coefficient (source: fishbase.se/manual/key%20facts.htm)
a <- 0.001 
b <- 3.07 # source: fishbase.se/manual/key%20facts.htm
decline_gro_temp <- target_species$dec_gr # decline in growth at the extreme temperatures
# sln_opt <- target_species$sln_opt # slope of the growth performance curve at the optimal temperature
scnr_tempr <- length(tempr_dev)

Linf <- target_species$Linf..cm.

# Create a data frame to store the results
scnr_clim <- data.frame(tempr_dev = tempr_dev, 
    k_new_clim = rep(0, scnr_tempr), 
    Linf_new_clim = rep(0, scnr_tempr), 
    Linf_new_clim_not_sensitive = rep(0, scnr_tempr), 
    Winf_new_clim = rep(0, scnr_tempr))


for (i in 1:scnr_tempr){

# Calculate new growth parameters
    # k_new_clim <- k_temp_function(tempr_dev[i], tempr_max_dev, tempr_min_dev, tempr_opt_dev, k_opt)
    k_new_clim <- k_temp_function(tempr_dev[i], tempr_max_dev, tempr_min_dev, sp_opt_temp_optk_dev, k_opt)
    #make sure K doesn't go less than zero
    k_new_clim <- max(c(0, k_new_clim))

    # Store k value
    scnr_clim$k_new_clim[i] <- k_new_clim

    #update this with linf model... 
    Linf_new_clim <- Linf_temp_function(k_opt, Linf, k_new_clim, tempr_dev[i], decline_gro_temp)
    scnr_clim$Linf[i] <- Linf
    scnr_clim$Linf_new_clim[i] <- Linf_new_clim

  
    # #Comparison when the sensitivity is zero
    Linf_new_clim_no_sens <- Linf_temp_function(k_opt, Linf, k_new_clim, tempr_dev[i], 0)
    scnr_clim$Linf_new_clim_not_sensitive[i] <- Linf_new_clim_no_sens
    
   
    # Winf_new_clim <- a*Linf_new_clim^b

}
print(scnr_clim)

# make a ggplot to show Linf only
glinf <- ggplot(scnr_clim, aes(x = tempr_dev, y = Linf_new_clim)) +
  geom_line() +
  geom_line(aes(y = Linf_new_clim_not_sensitive), col = "red") +
  geom_hline(yintercept = Linf, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  annotate("text", x = sp_opt_temp_optk_dev, y = scnr_clim$Linf, label = "sp_opt_temp_opt", angle = 90, vjust = 0, hjust = -0.8, size = 8) +
  labs(title = "L∞ with and without temperature sensitivity", 
       x = "Deviation in temperature (°C)",
       y = "Asymptotic length (L∞)") +
    geom_text(aes(x = tempr_min_dev, y = Linf, label = "Tmin"), vjust = 1.2, hjust = 0.2, size = 8) +
    geom_text(aes(x = tempr_max_dev, y = Linf, label = "Tmax"), vjust = 1.2, hjust = 1, size = 8)

glinf 

#  Save gk plot to a file
# ggsave(glinf, filename = paste0("Shared/Outputs/Linf_new_clim", target_species$species, ".png"), width = 9, height = 9, units = "in", dpi = 600)



# # make a ggplot to show Linf only
gk <- ggplot(scnr_clim, aes(x = tempr_dev, y = k_new_clim)) +
  geom_line() +
  geom_hline(yintercept = k_opt, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  annotate("text", x = sp_opt_temp_optk_dev, y = k_opt, label = "sp_opt_temp_opt", angle = 90, vjust = 0, hjust = 1.5, size = 8) +
  labs(title = "K under different temperature variations",
       x = "Deviation in temperature (°C)",
       y = "Growth coefficient (K)") +
    geom_text(aes(x = tempr_min_dev, y = k_opt, label = "Tmin"), vjust = 1.5, hjust = 0.2, size = 8) +
    geom_text(aes(x = tempr_max_dev, y = k_opt, label = "Tmax"), vjust = 1.5, hjust = 1, size = 8)

# # # # #  gk + glinf
gk 
# # # save the only gk plot to a file

ggsave(gk + glinf, filename = paste0("Shared/Outputs/k_new_clim", target_species$species, ".png"), width = 16, height = 9, units = "in", dpi = 600)
