# estimation of fishing and total mortality after the effct of temperature on growth parameters and natural mortality

# load libraries
library(tidyverse)
library(patchwork)

# made-up data
dat <- data.frame(age = 1:10, catch = rnorm(10, mean = c(1000, 800, 600, 400, 200, 100, 50, 25, 10, 5), sd = 50))
age_range <- c(2, 10) # fully recruited ages
temp <- seq(18, 35, by = 0.1) # temperature range (made-up numbers)
M_temp <- seq(0.1, 0.5, length.out = length(temp)) # made-up temperature-dependent natural mortality values
M_base <- 0.3 # made-up constant natural mortality value for comparison

## estimate total mortality (Z) from catch curve analysis using catch data at age
# Z can be estimated from the slope of the log-linear relationship between catch-at-age and age
Z_mort_catch <- function(dat, age_range){
    # dat: data frame with age and catch columns
    # age_range: vector of two values (min_age, max_age) for the regression
    
    # Filter data for the specified age range (fully recruited ages)
    filtered_dat <- dat[dat$age >= age_range[1] & dat$age <= age_range[2], ]
    
    # Remove zero catches to avoid log(0)
    filtered_dat <- filtered_dat[filtered_dat$catch > 0, ]
    
    # Perform catch curve regression: log(catch) ~ age
    catch_curve <- lm(log(catch) ~ age, data = filtered_dat)
    
    # Total mortality is the negative slope
    Z <- -coef(catch_curve)[2]
    
    return(Z)
}

## simulate estimation of fishing mortality from total and natural mortality depending on temperature: source: Beverton and Holt 1957 
F_temp_function <- function(Z, M_temp){
    # Calculate fishing mortality from total mortality and temperature-dependent natural mortality
    # F = Z - M (Beverton and Holt 1957)
    fmort <- Z - M_temp
    
    # Ensure fishing mortality is not negative
    fmort <- pmax(fmort, 0)
    
    return(fmort)
}


## simulate fishing mortality without temperature effect on natural mortality (i.e., using a constant M value)
F_constant_M <- function(Z, M_base){
    fmort <- Z - M_base
    fmort <- pmax(fmort, 0)
    return(fmort)
}


# use ggplot to plot fishing mortality against temperature-dependent natural mortality
Z_estimated <- Z_mort_catch(dat, age_range)
F_estimated <- F_temp_function(Z_estimated, M_temp)
F_constant <- F_constant_M(Z_estimated, M_base)
F_mort_df <- data.frame(M_temp = M_temp, F_estimated = F_estimated, temp = temp, F_constant = F_constant)
ggplot(F_mort_df, aes(x = temp, y = F_constant)) +
  geom_line() +
  labs(x = "Temperature (°C)", y = "Estimated Fishing Mortality (F)") +
  ggtitle("Effect of Temperature on Estimated Fishing Mortality") +
  theme(plot.title = element_text(hjust = 0.5, size = 20))

#   save plot to a file
ggsave(filename = "Shared/Outputs/temperature_effect_on_fishing_mortality.png", width = 10, height = 6, units = "in", dpi = 600)
