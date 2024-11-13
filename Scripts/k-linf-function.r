#test the k to linf function

Linf <- 100
k <- 0.2

phi_0 <- log10(k) + 2*log10(Linf)
phi_1 <- 0
linf_eqn <- function(phi_0, phi_1, kt, k){
  lin <- phi_0 + phi_1*(kt-k)
  Linf <- sqrt((10^lin)/ kt)
  return(Linf)
}

kvals <- seq(0.1, 0.3, length.out = 100)

linfvals <- linf_eqn(phi_0, phi_1=0, kvals, k)
linfvals1 <- linf_eqn(phi_0, phi_1= 0.5, kvals, k)
linfvals2 <- linf_eqn(phi_0, phi_1= -0.5, kvals, k)


plot(kvals, linfvals, type = 'l')
lines(kvals, linfvals1, col = 'red')
lines(kvals, linfvals2, col = 'blue')
abline(h = Linf, col = 'green')

#
#now do the plot for a range of temperatures
#

k_tempr <- function(tempr, tempr_max, tempr_min, tempr_opt, k_opt){
  growth_coef_tempr <- k_opt * ((tempr - tempr_min) * (tempr- tempr_max) / 
                                  ((tempr - tempr_min) * (tempr - tempr_max) - (tempr - tempr_opt)^2)
  )
  return(growth_coef_tempr)
}

tempr <- seq(20, 30, by = 0.1) # temperature range (made-up numbers)

tempr_min <- 4 # minimum temperature (made-up number)
tempr_max <- 30 # maximum temperature (made-up number)
tempr_opt <- 24 # optimal temperature (made-up number)
k_opt <- k

k_tempr <- k_tempr(tempr, tempr_max, tempr_min, tempr_opt, k_opt)
linf_tempr_base <- linf_eqn(phi_0, phi_1=0, k_tempr, k)
linf_tempr_plus <- linf_eqn(phi_0, phi_1=0.5, k_tempr, k)
linf_tempr_minus <- linf_eqn(phi_0, phi_1=-0.5, k_tempr, k)

plot(tempr, k_tempr, type = 'l')

plot(tempr, linf_tempr_base, type = 'l', ylim = c(90, 150))
lines(tempr, linf_tempr_plus, col = 'red')
lines(tempr, linf_tempr_minus, col = 'blue')
abline(h = Linf, col = 'green')





