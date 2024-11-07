#Script to explore how Ktemp function works
# CJ Brown 2024-11-08

source("Scripts/fish_size_functions v2.R")

tempr <- seq(2, 32, by = 0.1) # temperature range (made-up numbers)
tempr_min <- 4 # minimum temperature (made-up number)
tempr_max <- 30 # maximum temperature (made-up number)
tempr_opt <- 20 # optimal temperature (made-up number)
K_opt <- 0.3 # optimal growth coefficient (made-up number)

Kvals <- K_tempr(tempr, tempr_max, tempr_min, tempr_opt, K_opt)

plot(tempr, Kvals, type = 'l')
abline(h = K_opt, col = "red")
abline(h = 0)
abline(v = tempr_min, lty = 2)
abline(v = tempr_opt, lty = 2)
abline(v = tempr_max, lty = 2)
