
source("scripts/library_parameters.R")
source("scripts/fish_size_functions 1.R") #load the functions


# SIMULATIONS
#

scnr_out <- data.frame(multiple = multiple, 
                       size_indicator_linf = NA,
                       size_indicator_Winf = NA,
                       size_indicator_K = NA)

scnr_out$size_indicator_linf <- numeric(nscnar)
scnr_out$size_indicator_Winf <- numeric(nscnar)
scnr_out$size_indicator_K <- numeric(nscnar)


for (i in 1:nscnar){
  Linf_new <- Linf*multiple[i] 
  Winf_new <- Winf*multiple[i]
  K_new <- K*multiple[i]
  ## Calculate YPR model with new Linf growth parameters
  dat <- abundance_catch_at_age(max_age, N0, Linf_new, Winf, K, t0, a, b, M, L50, k_length, Fmort_overall)
  ind_temp <- stock_indicators(dat, Linf, Winf)
  scnr_out$size_indicator_linf[i] <- ind_temp$mlength_indicator
  
  ## Calculate YPR model with new Winf growth parameters
  dat <- abundance_catch_at_age(max_age, N0, Linf, Winf_new, K, t0, a, b, M, L50, k_length, Fmort_overall)
  ind_temp <- stock_indicators(dat, Linf, Winf)
  scnr_out$size_indicator_Winf[i] <- ind_temp$mweight_indicator

  ## Calculate YPR model with new K growth parameters
  dat <- abundance_catch_at_age(max_age, N0, Linf, Winf, K_new, t0, a, b, M, L50, k_length, Fmort_overall)
  ind_temp <- stock_indicators(dat, Linf, Winf)
  scnr_out$size_indicator_K[i] <- ind_temp$mlength_indicator
    
}


scnr_out
g1 <- ggplot(scnr_out, aes(x = multiple)) +
  geom_line(aes(y = size_indicator_linf), color = "black") +
  geom_line(aes(y = size_indicator_Winf), color = "blue") +
  geom_line(aes(y = size_indicator_K), color = "red") +
  labs(title = "Sensitivity of size indicators to changes in Linf, Winf, and K", x = "Multiple of Linf, Winf and K", y = "Size indicator")

ggsave(g1, filename = "Outputs/Trachichthyidae/2024-10-30_Linf-Winf-K-sensitivity_Hoplostethus_atlanticus-(0.04).tiff", width = 6, height = 4)
 
