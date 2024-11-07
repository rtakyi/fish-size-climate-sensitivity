



source("scripts/library_parameters.R")
source("scripts/fish_size_functions 1.R") #load the functions

## Add climate change impacts on Linf and K
scnr_clim <- data.frame(multiple_tempr = multiple_tempr,
                         size_indicator_linf = NA,
                         size_indicator_Winf = NA,
                         size_indicator_K = NA)


scnr_clim$size_indicator_linf <- numeric(scnr_tempr)
scnr_clim$size_indicator_Winf <- numeric(scnr_tempr)
scnr_clim$size_indicator_K <- numeric(scnr_tempr)

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



scnr_clim
g2 <- ggplot(scnr_clim, aes(x = multiple_tempr)) +
  geom_line(aes(y = size_indicator_linf), color = "black") +
  geom_line(aes(y = size_indicator_Winf), color = "blue") +
  geom_line(aes(y = size_indicator_K), color = "red") +
  labs(title = "Sensitivity of size indicators to changes in Linf, Winf, and K due to climate change", x = "Temperature change", y = "Size indicator")

ggsave(g2, filename = "Outputs/Trachichthyidae/2024-10-30_Linf-Winf-K-sensitivity-climate-change_Hoplostethus_atlanticus-(0.04_4).tiff", width = 6, height = 4)



