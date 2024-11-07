
library(patchwork)

source("scripts/library_parameters.R")
source("scripts/simulations_plot.R")
source("scripts/fish_size_functions 1.R") #load the functions

# arrange the plot with pachwork
combo_arrange <- (g1 | g2)
