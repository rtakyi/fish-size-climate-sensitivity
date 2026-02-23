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

fish_data <- read.csv("Parameter/cs_sp_parameters_slow.csv")


# Set the parameters for the functions
temp <- seq(18, 35, by = 0.1) # temperature range (made-up numbers)
Linf_base <- 100
Linf_new_clim <- 90
k_under_new_temp <- 0.5
k_base <-fish_data$growth_
temp_base <- fish_data$sp_opt_temp[which(fish_data$species == "Pseudotolithus senegalensis" & fish_data$id == "ps")] # optimal temperature for the species (source: fishbase.se/manual/key%20facts.htm)
M_base <- fish_data$M.[which(fish_data$species == "Pseudotolithus senegalensis" & fish_data$id == "ps")]


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
g1 <- ggplot(M_temp_df, aes(x = temp, y = M_temp)) +
  geom_line() +
  labs(x = "Temperature (°C)", y = "Natural mortality (M)") +
  ggtitle("Effect of Temperature on Natural Mortality") +
  theme(plot.title = element_text(hjust = 0.5))

  g1

# save plot to a file
ggsave(g1, filename = "Shared/Outputs/temperature_effect_on_natural_mortality.png", width = 10, height = 6, units = "in", dpi = 600)    

