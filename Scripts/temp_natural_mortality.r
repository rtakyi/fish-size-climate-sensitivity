# testing the effect of temperature on natural mortality using the Pauly 1980; Froese and Pauly 2000 equation for natural mortality and the temperature effect on growth performance and asymptotic length and weight using the Kielbassa et al. 2010; Mallet et al. 1999 equations for growth performance and asymptotic length and weight.

## load library
library(ggplot2)
library(patchwork)
library(reshape2)

# Set theme for output plots
theme_set(theme_classic())
theme_update(axis.text.x = element_text(colour = "black", size = 24), 
              axis.text.y = element_text(colour = "black", size = 24), axis.title = element_text(size = 24),
              plot.title = element_text(size = 26, hjust = 0.5))


# target_species <- read.csv("Parameters/cs_sp_parameters_fast.csv")
case_study_parameters <- read.csv("Parameters/cs_sp_parameters_slow.csv")

fish_data <- case_study_parameters[case_study_parameters$species == "Sphyraena sphyraena" & case_study_parameters$id == "ss",]

# Set the parameters for the functions
temp <- seq(18, 35, by = 0.1) # temperature range (made-up numbers)

# Extract base parameters for the species
Linf_base <- fish_data$Linf[1]
k_base <- fish_data$growth_coef[1]
temp_base <- fish_data$sp_opt_temp[1] 
M_base <- fish_data$M[1]

# assuming a 1% increase in K per degree increase in temperature
k_under_new_temp <- k_base * (1 + 0.01 * (temp - temp_base))

# assuming a 1% decrease in Linf per degree increase in temperature
Linf_new_clim <- Linf_base * (1 - 0.01 * (temp - temp_base))


## simulation of natural mortality from temperature: source: Pauly 1980; Froese and Pauly 2000
M_temp_function <- function(temp, Linf_new_clim, Linf_base, k_under_new_temp, k_base, temp_base, M_base){
  mult <- (10^(-0.066 - 0.279*log10(Linf_new_clim) + 0.6543*log10(k_under_new_temp) + 0.4634*log10(temp)))/
      (10^(-0.066 - 0.279*log10(Linf_base) + 0.6543*log10(k_base) + 0.4634*log10(temp_base)))
    # mult <- ((k_under_new_temp / k_base)^0.6543) * ((temp / temp_base)^0.4634) * ((Linf / Linf_base)^(-0.279))
  M_temp = M_base * mult
    # M_temp <- 10^(-0.066 - 0.279*log10(Linf) + 0.6543*log10(k_under_new_temp) + 0.4634*log10(temp))
    return(M_temp)
}

# M_temp_function(temp, Linf_new_clim, Linf_base, k_under_new_temp, k_base, temp_base, M_base)

# M_temp_function()

# plot M_temp_function across temperature range with ggplot
M_temp_values <- M_temp_function(temp, Linf_new_clim, Linf_base, k_under_new_temp, k_base, temp_base, M_base)
M_temp_df <- data.frame(temp = temp, M_temp = M_temp_values)
ggplot(M_temp_df, aes(x = temp, y = M_temp)) +
  geom_line() +
  labs(x = "Temperature (°C)", y = "Natural mortality (M)") +
  ggtitle(paste("Effect of Temperature on Natural Mortality -", fish_data$species[1])) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "italic"))


# save plot to a file
ggsave(filename = paste0("Shared/Outputs/temperature_effect_on_natural_mortality_", fish_data$species[1], ".png"), width = 10, height = 6, units = "in", dpi = 600)    
