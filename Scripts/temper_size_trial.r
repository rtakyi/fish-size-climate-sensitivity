
## load library
library(ggplot2)



## simulate temperature effect on growth coeffecients: source: Kielbassa et al. 2010; Mallet et al. 1999 
k_tempr <- function(tempr, tempr_max, tempr_min, tempr_opt, k_opt){
    growth_coef_tempr <- k_opt * ((tempr - tempr_min) * (tempr - tempr_max) /
                                    ((tempr - tempr_min) * (tempr - tempr_max) - (tempr - tempr_opt)^2)
                                  )
    return(growth_coef_tempr)
}


##  simulate temperature effect on growth performance and asymptotic length: source: Kielbassa et al. 2010; Mallet et al. 1999
Linf_tempr <- function(k, Linf, k_tempr, tempr, tempr_opt, dec_gr, sln_opt){
    ph_t <- (dec_gr * 2) *(tempr - tempr_opt) + sln_opt
    growth_per_tempr <- (log10(k) + (2 * log10(Linf))) + ph_t
    asymp_L_tempr <- sqrt((10^(growth_per_tempr)) / (k_tempr))
    return(asymp_L_tempr)
}

## simulate temperature effect on growth performance and asymptotic weight: source: Kielbassa et al. 2010; Mallet et al. 1999
Winf_tempr <- function(Winf, Linf, Linf_tempr){
    Winf_tempr_1 <- Winf * (Linf_tempr / Linf)^b 
    return(Winf_tempr_1)
}

# # Parameters
case_study_parameters <- read.csv("Parameters/trial_parameters.csv")
target_species <- case_study_parameters

# Parameters for scenarios of changes in temperature, optimum temperature and growth performance due to climate change
tempr <- seq(7.1, 34, by = 0.1) # temperature range (Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea; Idike and Lupo (2024). Analysis of sea surface temperature patterns, vari...(29.34))
tempr_min_dev <- -17.3 # minimum temperature deviation from optimal temperature    
tempr_max_dev <- 10.7 # maximum temperature deviation from optimal temperature   
tempr_opt <- 27.13 #  source: Dovlo (2016). Seasonal variation in temperature in the Gulf of Guinea;


IPCC_scnrs <- (tempr_opt - tempr_opt) + c(1.5, 2, 4) # optimal temperature based on ipcc (CMIP6) projections (source: Sohou et al. 2020; https://www.ipcc.ch/report/ar6/wg1/downloads/factsheets/IPCC_AR6_WGI_Regional_Fact_Sheet_Australasia.pdf)

# Temperature difference between optimal and temperature sequence
tempr_dev <- tempr - tempr_opt
tempr_opt_dev <- tempr_opt - tempr_opt
 
k <- 0.24
k_opt <- 0.3 # optimal growth coefficient (source: fishbase.se/manual/key%20facts.htm)
a <- 0.002 
b <- 3.07 # source: fishbase.se/manual/key%20facts.htm
dec_gr <- target_species$dec_gr # decline in growth at the extreme temperatures
sln_opt <- target_species$sln_opt # slope of the growth performance curve at the optimal temperature
scnr_tempr <- length(tempr_dev)

Linf <- target_species$Linf

# Create a data frame to store the results
scnr_clim <- data.frame(tempr_dev = tempr_dev, k_new_clim = rep(0, scnr_tempr), Linf_new_clim = rep(0, scnr_tempr), Winf_new_clim = rep(0, scnr_tempr))


for (i in 1:scnr_tempr){

# Calculate new growth parameters
    k_new_clim <- k_tempr(tempr_dev[i], tempr_max_dev, tempr_min_dev, tempr_opt_dev, k_opt)
    #make sure K doesn't go less than zero
    k_new_clim <- max(c(0, k_new_clim))

    # Store k value
    scnr_clim$k_new_clim[i] <- k_new_clim

    #update this with linf model... 
    Linf_new_clim <- Linf_tempr(k_new_clim, Linf, k, tempr_dev[i], tempr_opt_dev, dec_gr, sln_opt)
    scnr_clim$Linf[i] <- Linf
    scnr_clim$Linf_new_clim[i] <- Linf_new_clim
    
    # Winf_new_clim <- a*Linf_new_clim^b

}
print()
# make a ggplot to show Linf only
ggplot(scnr_clim, aes(x = tempr_dev, y = Linf_new_clim)) +
  geom_line() +
  labs(title = "Asymptotic length under different climate scenarios",
       x = "Temperature deviation from optimal temperature",
       y = "Asymptotic length") +
  theme_minimal()

    
