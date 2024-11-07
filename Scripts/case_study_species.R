

## Case study with species from Australian waters


## Species 1: Lutjanus fulgens (Golden snapper)
## Data source: 
### Fish biological parameters
Linf_golden_snapper <- 51.09 # asymptotic length in cm
Winf_golden_snapper <- 8 # asymptotic weight in kg (8000 g)
K_golden_snapper <- 0.47 # growth constant
M_golden_snapper <- 0.78 # natural mortality per year
t0_golden_snapper <- -0.3 # age at which length is zero
max_age_golden_snapper <- 6.41 # maximum age in years
min_age_golden_snapper <- 0 # minimum age in years
L50_golden_snapper <- 31.5 # the size at which the probability of selection is 0.5

Fmort_overall_golden_snapper <- 1.91 # fishing mortality per year
recruit_age_golden_snapper <- 3 # age (years) at recruitment into the fishery


## Species 2: L. sebae (Red snapper)
## Data source: https://doi.org/10.1093/icesjms/fsn064; Martinez-Andrade (2003)
### Fish biological parameters
Linf_red_snapper <- 116 # asymptotic length in cm
Winf_red_snapper <- 17.63 # asymptotic weight in kg (17630 g)
K_red_snapper <- 0.14 # growth constant
M_red_snapper <- 0.12 # natural mortality per year
t0_red_snapper <- -0.3 # age at which length is zero
max_age_red_snapper <- 34 # maximum age in years
min_age_red_snapper <- 0 # minimum age in years
L50_red_snapper <- 39.8 # the size at which the probability of selection is 0.5

Fmort_overall_red_snapper <- 0.09 # fishing mortality per year
recruit_age_red_snapper <- 1 # age (years) at recruitment into the fishery


## Species 3: L. johnii (John's snapper)
## Data source: Barua et al. 2023 (https://doi.org/10.1016/j.rsma.2023.102983); Martinez-Andrade (2003)
### Fish biological parameters
Linf_john_snapper <- 99.33 # asymptotic length in cm
Winf_john_snapper <- 10.92 # asymptotic weight in kg (10920 g)
K_john_snapper <- 0.16 # growth constant
M_john_snapper <- 0.24 # natural mortality per year
t0_john_snapper <- -0.3 # age at which length is zero
max_age_john_snapper <- 11 # maximum age in years
min_age_john_snapper <- 0 # minimum age in years
L50_john_snapper <- 27.08 # the size at which the probability of selection is 0.5

Fmort_overall_john_snapper <- 0.35 # fishing mortality per year
recruit_age_john_snapper <- 3 # age (years) at recruitment into the fishery



## Species 4: Sardinops sagax (Australian sardine)
## Data source: ;https://tasfisheriesresearch.org/sardine/biology; https://fish.gov.au/Archived-Reports/2023/Australian%20Sardine%20(2023).pdf; https://www.afma.gov.au/sites/default/files/2023-02/2022%20SPF%20Species%20Summaries.pdf; https://dx.doi.org/10.22161/ijfaf.3.4.1
### Fish biological parameters
Linf_sard <- 25 # asymptotic length in cm
Winf_sard <- 0.21 # asymptotic weight in kg (210 g)
K_sard <- 0.37 # growth constant
M_sard <- 0.48 # natural mortality per year
t0_s <- -0.28 # age at which length is zero
max_age <- 9 # maximum age in years
min_age <- 0 # minimum age in years
L50_sard <- 14.5 # the size at which the probability of selection is 0.5

Fmort_overall_sard_scnr_1 <- 0.24 # fishing mortality
Fmort_overall_sard_scnr_2 <- 0.35 # fishing mortality (source: https://doi.org/10.1016/j.rsma.2023.102972)
recruit_age_sard <- 1 # age at recruitment into the fishery


## Species 5: Sardinella aurita (Round sardinella)
## Data source: https://doi.org/10.31396/Biodiv.Jour.2019.10.4.353.358  and fishbase.sehttps://www.fishbase.se/summary/Sardinella-aurita.html and Ofori-Danson et al. (2018); 
Linf_round_sard <- 41 # asymptotic length in cm
Winf_round_sard <- 0.8 # asymptotic weight in kg
K_round_sard <- 0.47 # growth constant
M_round_sard <- 0.74 # natural mortality per year
t0_round_sard <- -0.3 # age at which length is zero
max_age_round_sard <- 5 # maximum age in years
min_age_round_sard <- 0 # minimum age in years
L50_round_sard <- 13 # the size at which the probability of selection is 0.5

Fmort_overall_round_sard <- 1.62 # fishing mortality per year
recruit_age_round_sard <- 1 # age (years) at recruitment into the fishery


## Species 6: Sardinella aurita (Round sardinella)
## Data source: Amponsah et al. 2017; Akel (2009)
### Fish biological parameters
Linf_round_sard_1 <- 21.5 # asymptotic length in cm
Winf_round_sard_1 <- 0.104 # asymptotic weight in kg (104 g)
K_round_sard_1 <- 0.25 # growth constant
M_round_sard_1 <- 0.76 # natural mortality per year
t0_round_sard_1 <- -0.74 # age at which length is zero
max_age_round_sard_1 <- 12 # maximum age in years
min_age_round_sard_1 <- 0 # minimum age in years
L50_round_sard_1 <- 5.99 # the size at which the probability of selection is 0.5

Fmort_overall_round_sard_1 <- 2.41 # fishing mortality per year
recruit_age_round_sard_1 <- 0.2 # age (years) at recruitment into the fishery



## Species 7: S. aurita (Round sardinella) 
## Data source: https://doi.org/10.1016/j.rsma.2019.100801; 
### Fish biological parameters
Linf_round_sard_2 <- 37.5 # asymptotic length in cm
Winf_round_sard_2 <- 1.055 # asymptotic weight in kg (1055 g)
K_round_sard_2 <- 1.02 # growth constant
M_round_sard_2 <- 0.7 # natural mortality per year
t0_round_sard_2 <- -0.6 # age at which length is zero
max_age_round_sard_2 <- 5 # maximum age in years
min_age_round_sard_2 <- 0 # minimum age in years
L50_round_sard_2 <- 27 # the size at which the probability of selection is 0.5

Fmort_overall_round_sard_2 <- 1.8 # fishing mortality per year
recruit_age_round_sard_2 <- 1 # age (years) at recruitment into the fishery



## Species 8: S. maderensis (Madeira sardinella)
## Data source: Ofori-Danson et al. (2018)  and   10.4172/2150-3508.1000189; https://doi.org/10.1016/0165-7836(95)00371-7; https://www.fishbase.se/summary/1047
### Fish biological parameters
### Fish biological parameters
Linf_madeira_sard <- 44.63 # asymptotic length in cm
Winf_madeira_sard <- 0.96 # asymptotic weight in kg (960 g)
K_madeira_sard <- 0.38 # growth constant
M_madeira_sard <- 0.81 # natural mortality per year
t0_madeira_sard <- -0.39 # age at which length is zero
max_age_madeira_sard <- 7.51 # maximum age in years
min_age_madeira_sard <- 0 # minimum age in years
L50_madeira_sard <- 13.99 # the size at which the probability of selection is 0.5

Fmort_overall_madeira_sard <- 0.43 # fishing mortality per year
recruit_age_madeira_sard <- 1 # age (years) at recruitment into the fishery


## Species 9: S. maderensis (Madeira sardinella)
## Data source: https://doi.org/10.1371/journal.pone.0156143  and  https://doi.org/10.1016/j.rsma.2019.100801
### Fish biological parameters
Linf_madeira_sard_1 <- 37.5 # asymptotic length in cm
Winf_madeira_sard_1 <- 0.300 # asymptotic weight in kg (300 g) 
K_madeira_sard_1 <- 1.01 # growth constant
M_madeira_sard_1 <- 0.5 # natural mortality per year
t0_madeira_sard_1 <- -0.4 # age at which length is zero
max_age_madeira_sard_1 <- 7.51 # maximum age in years
min_age_madeira_sard_1 <- 0 # minimum age in years
L50_madeira_sard_1 <- 24.5 # the size at which the probability of selection is 0.5

Fmort_overall_madeira_sard_1 <- 1.8 # fishing mortality per year
recruit_age_madeira_sard_1 <- 1 # age (years) at recruitment into the fishery

 
## Species 10: S. maderensis (Madeira sardinella)
## Data source: Sossoukpe et al. (2016)
### Fish biological parameters
Linf_madeira_sard_2 <- 33.6 # asymptotic length in cm
Winf_madeira_sard_2 <- 0.296 # asymptotic weight in kg (296 g)
K_madeira_sard_2 <- 0.65 # growth constant
M_madeira_sard_2 <- 1.3 # natural mortality per year
t0_madeira_sard_2 <- -0.24 # age at which length is zero
max_age_madeira_sard_2 <- 4.61 # maximum age in years
min_age_madeira_sard_2 <- 0 # minimum age in years
L50_madeira_sard_2 <- 23.8 # the size at which the probability of selection is 0.5

Fmort_overall_madeira_sard_2 <- 2.62 # fishing mortality per year
recruit_age_madeira_sard_2 <- 1 # age (years) at recruitment into the fishery



## Species 11: Engraulis encrasicolus (European anchovy)
## Data source: http://www.bioflux.com.ro/docs/2018.730-743.pdf
### Fish biological parameters
Linf_anchovy <- 17.89 # asymptotic length in cm
Winf_anchovy <- 0.0126 # asymptotic weight in kg (12.6 g)
K_anchovy <- 0.6 # growth constant
M_anchovy <- 0.56 # natural mortality per year
t0_anchovy <- -0.008 # age at which length is zero
max_age_anchovy <- 4.95 # maximum age in years
min_age_anchovy <- 0 # minimum age in years
L50_anchovy <- 3.71 # the size at which the probability of selection is 0.5

Fmort_overall_anchovy <- 1.75 # fishing mortality per year
#recruit_age_anchovy <- 1 # age (years) at recruitment into the fishery


## Species 12: E. encrasicolus (European anchovy)
## Data source: https://www.fisheriesjournal.com/archives/2016/vol4issue5/PartD/4-4-90-573.pdf
### Fish biological parameters
Linf_anchovy_1 <- 11.03 # asymptotic length in cm
Winf_anchovy_1 <- 0.02352 # asymptotic weight in kg (23.516 g)
K_anchovy_1 <- 0.58 # growth constant
M_anchovy_1 <- 1.59 # natural mortality per year
t0_anchovy_1 <- - 0.37 # age at which length is zero
max_age_anchovy_1 <- 5 # maximum age in years
min_age_anchovy_1 <- 0 # minimum age in years
L50_anchovy_1 <- 3.71 # the size at which the probability of selection is 0.5

Fmort_overall_anchovy_1 <- 1.81 # fishing mortality per year
#recruit_age_anchovy_1 <- 1 # age (years) at recruitment into the fishery


## Species 13: E. encrasicolus (European anchovy)
## Data source: https://doi.org/10.1017/S0025315413000611
### Fish biological parameters
Linf_anchovy_2 <- 16.368 # asymptotic length in cm
Winf_anchovy_2 <- 0.02352 # asymptotic weight in kg (23.516 g)
K_anchovy_2 <- 0.425 # growth constant
M_anchovy_2 <- 0.66  # natural mortality per year
t0_anchovy_2 <- -1.35 # age at which length is zero
max_age_anchovy_2 <- 5 # maximum age in years
min_age_anchovy_2 <- 0 # minimum age in years
L50_anchovy_2 <- 3.71 # the size at which the probability of selection is 0.5

Fmort_overall_anchovy_2 <- 2.18  # fishing mortality per year
#recruit_age_anchovy_2 <- 1 # age (years) at recruitment into the fishery



## Species 14: Orange roughy (Hoplostethus atlanticus)
## Data source: https://doi.org/10.1016/j.fishres.2022.106534; 10.1080/00288330.1990.9516406
### Fish biological parameters
Linf_orange_roughy <- 42.5 # asymptotic length in cm
Winf_orange_roughy <- 2.500 # asymptotic weight in kg (2500 g)
K_orange_roughy <- 0.06 # growth constant
M_orange_roughy <- 0.04 # natural mortality per year
t0_orange_roughy <- -0.35 # age at which length is zero
max_age_orange_roughy <- 80 # maximum age in years
min_age_orange_roughy <- 0 # minimum age in years
L50_orange_roughy <- 35.8 # the size at which the probability of selection is 0.5

Fmort_overall_orange_roughy <- 0.04 # fishing mortality per year
recruit_age_orange_roughy <- 20 # age (years) at recruitment into the fishery


## Species 16: Galeocerdo cuvier (Tiger shark)
## Data source: https://www.int-res.com/articles/ab2008/2/b002p161.pdf
### Fish biological parameters
Linf_tiger_shark_1 <- 366 # asymptotic length in cm
Winf_tiger_shark_1 <- 804.600 # asymptotic weight in kg (80600 g)
K_tiger_shark_1 <- 0.25 # growth constant
M_tiger_shark_1 <- 0.98 # natural mortality per year
t0_tiger_shark_1 <- -0.5 # age at which length is zero
max_age_tiger_shark_1 <- 32 # maximum age in years
min_age_tiger_shark_1 <- 0 # minimum age in years
L50_tiger_shark_1 <- 200 # the size at which the probability of selection is 0.5

Fmort_overall_tiger_shark_1 <- 1.24 # fishing mortality per year
recruit_age_tiger_shark <- 5 # age (years) at recruitment into the fishery

## Species 17: Galeocerdo cuvier (Tiger shark)
## Data source: https://www.fishbase.se/summary/galeocerdo-cuvier.html
### Fish biological parameters
Linf_tiger_shark_2 <- 750 # asymptotic length in cm
Winf_tiger_shark_2 <- 804.600 # asymptotic weight in kg (804600 g)
K_tiger_shark_2 <- 0.25 # growth constant
M_tiger_shark_2 <- 0.98 # natural mortality per year
t0_tiger_shark_2 <- -0.5 # age at which length is zero
max_age_tiger_shark_2 <- 50 # maximum age in years
min_age_tiger_shark_2 <- 0 # minimum age in years
L50_tiger_shark_2 <- 200 # the size at which the probability of selection is 0.5

Fmort_overall_tiger_shark_2 <- 1.24 # fishing mortality per year
recruit_age_tiger_shark <- 5 # age (years) at recruitment into the fishery




## Species 17: Scorpis aequipinnis (southern pigfish)
## Data source: https://doi.org/10.1016/j.fishres.2012.02.031
### Fish biological parameters
#Linf_pigfish <- 47.7 # asymptotic length in cm
#Winf_pigfish <- 
#K_pigfish <- 0.18 # growth constant
#M_pigfish <- 0.3 # natural mortality per year
#t0_pigfish <- -0.77 # age at which length is zero
#max_age_pigfish <- 68 # maximum age in years
#min_age_pigfish <- 0 # minimum age in years
#L50_pigfish <- 30 # the size at which the probability of selection is 0.5


#Fmort_overall_pigfish <- 0.09  # fishing mortality per year
#recruit_age_pigfish <- 21 # age (years) at recruitment into the fishery

















## Species 14: Achoerodus gouldii (Blue groper) is a slow-growing, long-lived species
## Data source: https://www.fish.wa.gov.au/documents/research_reports/frr242.pdf; https://researchportal.murdoch.edu.au/esploro/outputs/journalArticle/The-western-blue-groper-Achoerodus-gouldii/991005544982207891; https://www.fishbase.se/summary/Achoerodus-gouldii.html; https://fishesofaustralia.net.au/home/species/206
### Fish biological parameters
#Linf_blue_groper <- 175 # asymptotic length in cm
#Winf_blue_groper <-
#K_blue_groper <- 0.08 # growth constant
#M_blue_groper <- 0.072 # natural mortality per year
#t0_blue_groper <- -0.65 # age at which length is zero
#max_age_blue_groper <- 70 # maximum age in years
#min_age_blue_groper <- 0 # minimum age in years
#L50_blue_groper <- 65.3 # the size at which the probability of selection is 0.5

#Fmort_overall_blue_groper <- 0.039 # fishing mortality per year
#recruit_age_blue_groper <- 17 # age (years) at recruitment into the fishery...This has not been well documented in the literature 


## Species 15: Galeocerdo cuvier (Tiger shark) 
## Data source: https://doi.org/10.1071/MF20291; https://doi.org/10.1016/j.rsma.2024.103526
### Fish biological parameters
#Linf_tiger_shark <- 372 # asymptotic length in cm
#Winf <- 2.5 # asymptotic weight in kg
#K_tiger_shark <- 0.07 # growth constant
#M_tiger_shark <- 0.17 # natural mortality per year
#t0_tiger_shark <- -2.30 # age at which length is zero
#max_age_tiger_shark <- 32 # maximum age in years
#min_age_tiger_shark <- 0 # minimum age in years
#L50_tiger_shark <- 200 # the size at which the probability of selection is 0.5

#Fmort_overall_tiger_shark <- 0.18 # fishing mortality per year
#recruit_age_tiger_shark <- 5 # age (years) at recruitment into the fishery












