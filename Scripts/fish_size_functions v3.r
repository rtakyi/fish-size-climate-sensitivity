


## simulate logisitic selectivity
logistic_selectivity <- function(length, L50, k_length, Fmort_fully_selected){
    selectivity <- 1 / (1 + exp(-k_length * (length - L50)))
    adjust_selectivity <- selectivity * Fmort_fully_selected
    return(adjust_selectivity)
}  


## simulate length at age using the von Bertalanffy growth function
VBGF_length <- function(age, Linf, k, t0) {
    length_at_age <- Linf * (1 - exp(-k * (age - t0)))
    return(length_at_age)
}

## simulate weight at age using the von Bertalanffy growth function : source: Geromont and Butterworth 2015
VBGF_weight <- function(age, Linf, k, t0, a, b) {
    weight_at_age <- a * (Linf * (1 - exp(-k * (age - t0))))^b
    return(weight_at_age)
}

## simulate fish caught at age with Baranov catch equation
fish_caught_at_age <- function(abundance_at_age, M, fmort, max_age){
  #BARANOV catch equation
  t <- 1 #usually set to 1, so we calculate individually for each year. 
  fish_caught_at_age <- (fmort/(fmort + M)) * (1- exp((-fmort-M)*t))*abundance_at_age
  return(fish_caught_at_age)
}

## simulate fish abundance and catch at age
# max_age: maximum age of fish
# N0: initial abundance of fish
# Linf: asymptotic length
# Winf: asymptotic weight
# K: growth constant
# t0: age at which length is zero
# a: growth performance index
# b: growth performance index
# M: natural mortality
# L50: size at which the probability of selection is 0.5
# k_length: slope of the logistic curve
# Fmort_fully_selected: fishing mortality at fully selected length
abundance_catch_at_age <- function(max_age, N0, Linf, Winf, k, t0, a, b, M, L50, k_length, Fmort_fully_selected){
 datout <- data.frame(age = 0:max_age, abundance = rep(0, max_age+1), catch = rep(0, max_age+1), length = rep(0, max_age+1), weight = rep(0, max_age+1), fmort_lengths = rep(0, max_age+1))
  datout$abundance[1] <- N0 #initial abundance at age 0
  age <- 0 :max_age
  datout$length <- VBGF_length(age, Linf, k, t0) #length at age
  datout$weight <- VBGF_weight(age, Linf, k, t0, a, b) #weight at age
  datout$fmort_lengths <- logistic_selectivity(datout$length, L50, k_length, Fmort_fully_selected) #fishing mort at length
    for (i in 2:(max_age+1)){
    datout$abundance[i] <- datout$abundance[i-1]*exp((-M-datout$fmort_lengths[i]))

    datout$catch[i] <- fish_caught_at_age(datout$abundance[i-1], M, datout$fmort_lengths[i], max_age)
      }
 datout$catch_weight <- datout$catch * datout$weight
 
  return(datout)
}


#function to calculate indicators from output of abundance_catch_at_age
stock_indicators <- function(dat, Linf, Winf){
indicators <- with(dat, {
  data.frame(mweight = sum((abundance * weight) / sum(abundance)),
                           mlength = sum((abundance * length) / sum(abundance)),
                           total_catch = sum(catch),
                           total_catch_weight = sum(catch_weight),
                           total_abundance = sum(abundance),
                           total_biomass = sum(abundance * weight)
                           )
    })
    indicators$mlength_indicator <- indicators$mlength/Linf
    indicators$mweight_indicator <- indicators$mweight/Winf
    indicators$length_weight_ratio <- indicators$mlength_indicator/indicators$mweight_indicator
    indicators$biomass_per_recruit <- indicators$total_biomass/indicators$total_abundance
  return(indicators)
}


## simulate temperature effect on growth coeffecients: source: Kielbassa et al. 2010; Mallet et al. 1999 
k_tempr <- function(tempr, tempr_max, tempr_min, tempr_opt, k_opt){
    growth_coef_tempr <- k_opt * ((tempr - tempr_min) * (tempr - tempr_max) /
                                    ((tempr - tempr_min) * (tempr - tempr_max) - (tempr - tempr_opt)^2)
                                  )
    return(growth_coef_tempr)
}


##  simulate temperature effect on growth performance and asymptotic length: source: Kielbassa et al. 2010; Mallet et al. 1999
Linf_tempr <- function(k, Linf, k_tempr, phi_1){
    growth_per_tempr <- (log10(k) + (2 * log10(Linf))) + phi_1
    asymp_L_tempr <- sqrt((10^(growth_per_tempr)) / (k_tempr))
    return(asymp_L_tempr)
}

## simulate temperature effect on growth performance and asymptotic weight: source: Kielbassa et al. 2010; Mallet et al. 1999
Winf_tempr <- function(Winf, Linf, Linf_tempr){
    Winf_tempr_1 <- Winf * (Linf_tempr / Linf)^b 
    return(Winf_tempr_1)
}

