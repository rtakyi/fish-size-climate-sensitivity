

## load library
library(ggplot2)
library(patchwork)

# Set theme for output plots
theme_set(theme_classic())
theme_update(axis.text.x = element_text(colour = "black", size = 18), 
              axis.text.y = element_text(colour = "black", size = 18), axis.title = element_text(size = 18))


## simulate temperature effect on growth coeffecients: source: Kielbassa et al. 2010; Mallet et al. 1999 
k_temp_function <- function(tempr, tempr_max, tempr_min, tempr_opt, k_opt){
    growth_coef_tempr <- k_opt * ((tempr - tempr_min) * (tempr - tempr_max) /
                                    ((tempr - tempr_min) * (tempr - tempr_max) - (tempr - tempr_opt)^2)
                                  )
    return(growth_coef_tempr)
}


# #  simulate temperature effect on growth performance and asymptotic length: source: Kielbassa et al. 2010; Mallet et al. 1999
Linf_temp_function <- function(k_baseline, Linf, 
    k_under_new_temperature, 
    temperature_deviation,
    factor_growthrate_change_10C_temp,
    reference_temperature){ 

    ph_0 <- log10(k_baseline * Linf^2)
    ph_T <- ph_0 + log10(factor_growthrate_change_10C_temp^((temperature_deviation - reference_temperature) / 10))
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
tempr <- seq(15, 34, by = 0.1) # temperature range (Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea; Idike and Lupo (2024). Analysis of sea surface temperature patterns, vari...(29.34))
tempr_min_dev <- -17.3 # minimum temperature deviation from optimal temperature    
tempr_max_dev <- 10.7 # maximum temperature deviation from optimal temperature   
tempr_opt <- 27.13 #target_species$tempr_opt #  source: Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea;


IPCC_scnrs <- (tempr_opt - tempr_opt) + c(1.5, 2, 4) # optimal temperature based on ipcc (CMIP6) projections (source: Sohou et al. 2020; https://www.ipcc.ch/report/ar6/wg1/downloads/factsheets/IPCC_AR6_WGI_Regional_Fact_Sheet_Australasia.pdf)

# Temperature difference between optimal and temperature sequence
tempr_dev <- tempr - tempr_opt
tempr_opt_dev <- tempr_opt - tempr_opt
 
k <- target_species$growth_coef..yr.1. # growth coefficient
k_opt <- k # optimal growth coefficient (source: fishbase.se/manual/key%20facts.htm)
a <- 0.002 
b <- 3.07 # source: fishbase.se/manual/key%20facts.htm

scnr_tempr <- length(tempr_dev)

# Reference temperature
sp_ref_temp <- target_species$sp_opt_temp # reference temperature for the species (source: fishbase.se/manual/key%20facts.htm)
growth_rate_change_factor <- 2.5 # factor by which growth rate changes with a 10C temperature change

Linf <- target_species$Linf..cm.

# Create a data frame to store the results
scnr_clim <- data.frame(tempr_dev = tempr_dev, 
    k_new_clim = rep(0, scnr_tempr), 
    Linf_new_clim = rep(0, scnr_tempr), 
    Linf_new_clim_not_sensitive = rep(0, scnr_tempr), 
    Winf_new_clim = rep(0, scnr_tempr))


for (i in 1:scnr_tempr){

# Calculate new growth parameters
    k_new_clim <- k_temp_function(tempr_dev[i], tempr_max_dev, tempr_min_dev, tempr_opt_dev, k_opt)
    #make sure K doesn't go less than zero
    k_new_clim <- max(c(0, k_new_clim))

    # Store k value
    scnr_clim$k_new_clim[i] <- k_new_clim

    #update this with linf model... 
    # #Comparison when there is sensitivity to temperature and IPCC scenarios
    Linf_new_clim <- Linf_temp_function(k, Linf, k_new_clim, tempr_dev[i], growth_rate_change_factor, sp_ref_temp)
    scnr_clim$Linf[i] <- Linf
    scnr_clim$Linf_new_clim[i] <- Linf_new_clim

    # # #Comparison when the sensitivity is zero
    Linf_new_clim_not_sensitive <- Linf_temp_function(k, Linf, k_new_clim, tempr_dev[i], 2.5, 0)
    scnr_clim$Linf_new_clim_not_sensitive[i] <- Linf_new_clim_not_sensitive
    
    
    # Winf_new_clim <- a*Linf_new_clim^b

}
print(scnr_clim)
# make a ggplot to show Linf only
glinf <- ggplot(scnr_clim, aes(x = tempr_dev, y = Linf_new_clim)) +
  geom_line() +
  geom_line(aes(y = Linf_new_clim_not_sensitive), col = "red") +
  geom_hline(yintercept = Linf, linetype = "dashed") + 
  geom_vline(xintercept = 0, linetype = "dashed") + 
  labs(title = "Asymptotic length under different climate scenarios",
       x = "Temperature deviation from optimal temperature",
       y = "Asymptotic length") +
    geom_text(aes(x = tempr_min_dev, y = Linf, label = "Tmin"), vjust = 1.5, size = 6) +
    geom_text(aes(x = tempr_max_dev, y = Linf, label = "Tmax"), vjust = 1.5, size = 6)

glinf

# # make a ggplot to show Linf only
# gk <- ggplot(scnr_clim, aes(x = tempr_dev, y = k_new_clim)) +
#   geom_line() +
#   geom_hline(yintercept = k, linetype = "dashed") + 
#   geom_vline(xintercept = 0, linetype = "dashed") + 
#   labs(title = "k under different climate scenarios",
#        x = "Temperature deviation from optimal temperature",
#        y = "Asymptotic length") +
#   theme_minimal()

# gk + glinf


#