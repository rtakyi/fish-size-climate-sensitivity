# Comparision of size indicators between static and dynamic conditions
# Created by : Richard Takyi (PhD Student, UTAS)

rm(list = ls())

# Load libraries
library(ggplot2)
library(tidyverse)
library(reshape2)
library(patchwork)
library(scales)

# Define reusable palette
# palette_name <- "RdBu"


# Set theme for output plots
theme_set(theme_classic())
theme_update(axis.text.x = element_text(colour = "black", size = 19, angle = 49, hjust = 1, vjust = 1, face = "italic"),
              axis.text.y = element_text(colour = "black", size = 19),
              axis.title.y.right = element_text(colour = "black", size = 19),
              axis.text.y.right = element_text(colour = "black", size = 19), 
              axis.title = element_text(size = 19),
              strip.text = element_text(size = 19),
                legend.text = element_text(size = 19),
                legend.title = element_text(size = 19),
              plot.title = element_text(size = 19, hjust = 0.5))

# Read in the data
# comparison <- read.csv("Parameters/comparison.csv")
comparison <- read.csv("Parameters/comparison_M_nonstationary.csv") 


# Ensure growth_strategy and species are treated as factors
comparison$growth_strategy <- as.factor(comparison$growth_strategy)
comparison$species <- as.factor(comparison$species)

# print(head(comparison))

n <- nrow(comparison)

#  Loop over comparison data
for (irow in 1:n) {
    # Extract the row
    row <- comparison[irow, ]
    
    # Extract the values
    growth_strategy <- row$growth_strategy
    species <- row$species
    
    # Print the values
    # print(paste("Growth Strategy:", growth_strategy))
    # print(paste("Species:", species))
    
    # Create a data frame for comparison
    comparison_df <- data.frame(species = species, 
                                 growth_strategy = growth_strategy,
                                 stationary_highest = row$stationary_highest,
                                 stationary_lowest = row$stationary_lowest,
                                 stationary_percentage_decline = row$stationary_percentage_decline,
                                 nonstationary_highest = row$nonstationary_highest,
                                 nonstationary_lowest = row$nonstationary_lowest,
                                 nonstationary_percentage_decline = row$nonstationary_percentage_decline,
                                 low_posit5 = row$low_posit5,
                                 low_negt5 = row$low_negt5,
                                 medium_posit5 = row$medium_posit5,
                                 medium_negt5 = row$medium_negt5,
                                 high_posit5 = row$high_posit5,
                                 high_negt5 = row$high_negt5,
                                 factor_chnge_stat_over_nonstat_hst_1 = row$factor_chnge_stat_over_nonstat_hst_1,
                                 factor_chnge_stat_over_nonstat_lst_1 = row$factor_chnge_stat_over_nonstat_lst_1,
                                 pfactor_chnge_nonstat_minus_stat_allover_stat_hst = row$pfactor_chnge_nonstat_minus_stat_allover_stat_hst,
                                 pfactor_chnge_nonstat_minus_stat_allover_stat_lst = row$pfactor_chnge_nonstat_minus_stat_allover_stat_lst,
                                 sp_opt_temp = row$sp_opt_temp)

    # Baseline values
    stationary_highest <- comparison$stationary_highest[irow]
    stationary_lowest <- comparison$stationary_lowest[irow]
    stationary_percentage_decline <- comparison$stationary_percentage_decline[irow]
    nonstationary_highest <- comparison$nonstationary_highest[irow]
    nonstationary_lowest <- comparison$nonstationary_lowest[irow]
    nonstationary_percentage_decline <- comparison$nonstationary_percentage_decline[irow]
    factor_chnge_stat_over_nonstat_hst_1 <- comparison$factor_chnge_stat_over_nonstat_hst_1[irow]
    factor_chnge_stat_over_nonstat_lst_1 <- comparison$factor_chnge_stat_over_nonstat_lst_1[irow]
    pfactor_chnge_nonstat_minus_stat_allover_stat_hst <- comparison$pfactor_chnge_nonstat_minus_stat_allover_stat_hst[irow]
    pfactor_chnge_nonstat_minus_stat_allover_stat_lst <- comparison$pfactor_chnge_nonstat_minus_stat_allover_stat_lst[irow]
    sp_opt_temp <- comparison$sp_opt_temp[irow]
    
}


# Plot a bar chart for factor changes in stationary vs non-stationary conditions with ggplot2
comparison_long <- comparison %>%
    mutate(
        growth_strategy = factor(growth_strategy, levels = c("Slow", "Fast"))
    ) %>%
    pivot_longer(
        cols = c(pfactor_chnge_nonstat_minus_stat_allover_stat_hst, pfactor_chnge_nonstat_minus_stat_allover_stat_lst),
        names_to = "metric",
        values_to = "value"
    ) %>%
    mutate(
        metric = recode(
            metric,
            # factor_chnge_stat_over_nonstat_hst_1 = "Lowest fishing mortality rate",
            # factor_chnge_stat_over_nonstat_lst_1 = "Highest fishing mortality rate"
            pfactor_chnge_nonstat_minus_stat_allover_stat_hst = "Lowest fishing mortality rate",
            pfactor_chnge_nonstat_minus_stat_allover_stat_lst = "Highest fishing mortality rate"
        ),
        metric = factor(metric, levels = c("Lowest fishing mortality rate", "Highest fishing mortality rate"))
    )

g10 <- ggplot() +
    # Bar plot
    geom_bar(
        data = comparison_long,
        aes(x = species, y = value, fill = growth_strategy),
        stat = "identity",
        position = "dodge"
    ) +
    facet_wrap(~ metric, scales = "free_y") +
    scale_fill_manual(
        name = "Growth strategy",
        values = c("Slow" = "lightblue", "Fast" = "orange")
    ) +
    scale_y_continuous(
        name = "Difference in percentage estimation bias \n (between stationary and non-stationary assessment)",
    ) +
    labs(
        title = "Estimation factor difference between stationary and non-stationary conditions for each species under changing M",
        x = "Species"
    ) +
    facet_wrap(
        ~ metric, 
        scales = "free_y",
        labeller = as_labeller(function(x) x)
    ) +
    # Set y-axis limit for "Lowest fishing mortality rate" facet
    ggplot2::geom_blank(data = comparison_long %>% filter(metric == "Lowest fishing mortality rate") %>% mutate(value = 10), 
                        aes(x = species, y = value))

# Set y-axis limit for the "Lowest fishing mortality rate" facet to 10
g10 <- g10 + 
    ggplot2::facet_wrap(
        ~ metric, 
        scales = "free_y",
        labeller = as_labeller(function(x) x)
    ) +
    scale_y_continuous(
                name = "Difference in percentage estimation bias \n (between stationary and non-stationary assessment)",
                limits = c(-5, 60),
                expand = expansion(mult = c(0, 0.05))
    ) +
    ggplot2::geom_blank(
        data = comparison_long %>% filter(metric == "Lowest fishing mortality rate") %>% mutate(value = 10),
        aes(x = species, y = value)
    )

g10


# # Save g10 plot to a file
ggsave(g10, filename = paste0("Shared/Outputs/comparison_factor_change_under_changing_M_", comparison, ".png"), width = 18, height = 12, units = "in", dpi = 300)



# # Plot a bar chart for the comparison with ggplot2
# # Reshape the data to long format for plotting multiple variables on y-axis
# # Combine stationary_highest/stationary_lowest and nonstationary_highest/nonstationary_lowest into two groups for plotting

# # # # Prepare data for bar plot (as before)
# comparison_long <- comparison %>%
#     mutate(
#         growth_strategy = factor(growth_strategy, levels = c("Slow", "Fast"))
#     ) %>%
#     pivot_longer(
#         cols = c(stationary_highest, stationary_lowest, nonstationary_highest, nonstationary_lowest),
#         names_to = "metric",
#         values_to = "value"
#     ) %>%
#     mutate(
#         group = case_when(
#             metric %in% c("stationary_highest", "stationary_lowest") ~ "Stationary conditions",
#             metric %in% c("nonstationary_highest", "nonstationary_lowest") ~ "Non-stationary conditions"
#         ),
#         metric = recode(
#             metric,
#             stationary_highest = "Highest",
#             stationary_lowest = "Lowest",
#             nonstationary_highest = "Highest",
#             nonstationary_lowest = "Lowest"
#         ),
#         group = factor(group, levels = c("Stationary conditions", "Non-stationary conditions")),
#         metric = factor(metric, levels = c("Highest", "Lowest"))
#     )

# g11 <- ggplot() +
#     # Bar plot
#     geom_bar(
#         data = comparison_long,
#         aes(x = species, y = value, fill = metric),
#         stat = "identity",
#         position = position_dodge(width = 0.8)
#     ) +
#     facet_wrap(~ group, scales = "free_y") +
#     scale_fill_manual(
#         name = "Indicator value",
#         values = c("Highest" = "brown", "Lowest" = "yellow")
#     ) +
#     scale_y_continuous(
#         name = "Length indicator"
#     ) +
#     labs(
#         title = "Highest and lowest length indicator values under lowest and highest fishing mortality rates \n, respectively, in both static and dynamic conditions at optimal temperature under changing M",
#         x = "Species"
#     )

# g11

# # # Save g11 plot to a file
# # ggsave(g11, filename = paste0("Shared/Outputs/comparison_", comparison, ".png"), width = 17, height = 9, units = "in", dpi = 600)


# # Plot a bar chart for the percentages of the comparison with ggplot2
# comparison_long <- comparison %>%
#     mutate(
#         growth_strategy = recode(
#             growth_strategy,
#             "Slow" = "Slow",
#             "Fast" = "Fast"
#         ),
#         # Set factor levels to ensure Slow comes before Fast
#         growth_strategy = factor(growth_strategy, levels = c("Slow", "Fast"))
#     ) %>%
#     pivot_longer(
#         cols = c(
#             stationary_percentage_decline, nonstationary_percentage_decline
#         ),
#         names_to = "metric",
#         values_to = "value"
#     ) %>%
#     mutate(
#         metric = recode(
#             metric,
#             stationary_percentage_decline = "Stationary conditions",
#             nonstationary_percentage_decline = "Non-stationary conditions"
#         ),
#         # Set factor levels to ensure stationary comes before non-stationary
#         metric = factor(
#             metric,
#             levels = c("Stationary conditions", "Non-stationary conditions")
#         )
#     )
# g12 <- ggplot(comparison_long,
#     aes(x = species, y = value, fill = growth_strategy)
# ) +
#     geom_bar(stat = "identity", position = "dodge") +
#     facet_wrap(~ metric, scales = "free_y") +
#     scale_fill_manual(
#         name = "Growth strategy",
#         values = c("Slow" = "lightblue", "Fast" = "orange")
#     ) +
#     labs(
#         title = "Percentage decline in length indicator values between lowest and highest \n fishing mortality rates under stationary and non-stationary conditions under changing M",
#         x = "Species",
#         y = "Percentage (%) decline in length indicator"
#     )

# g12

# Save g12 plot to a file
# ggsave(g12, filename = paste0("Shared/Outputs/comparison_percentage_", comparison, ".png"), width = 18, height = 9, units = "in", dpi = 600)


# # Save g11 and g12 plots to a file  
# # Combine g10 and g11 using patchwork and save as a single image
# ggsave(g11 / g12, filename = "Shared/Outputs/comparison_combined.png", width = 22, height = 22, units = "in", dpi = 600)



# Reshape data for plotting: combine posit5 and negt5 for each Fmort level, distinguish by temperature direction
comparison_long <- comparison %>%
    mutate(
        growth_strategy = factor(growth_strategy, levels = c("Slow", "Fast"))
    ) %>%
    pivot_longer(
        cols = c(
            low_posit5, low_negt5,
            medium_posit5, medium_negt5,
            high_posit5, high_negt5
        ),
        names_to = "metric",
        values_to = "value"
    ) %>%
    mutate(
        Fmort = case_when(
            grepl("^low_", metric) ~ "0.01",
            grepl("^medium_", metric) ~ "0.40",
            grepl("^high_", metric) ~ "1.77"
        ),
        Temp = case_when(
            grepl("posit5$", metric) ~ "+5°C",
            grepl("negt5$", metric) ~ "-5°C"
        ),
        Fmort = factor(Fmort, levels = c("0.01", "0.40", "1.77")),
        Temp = factor(Temp, levels = c("+5°C", "-5°C"))
    )

g13 <- ggplot(comparison_long,
    aes(x = species, y = value, fill = Temp, group = Temp)
) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    facet_wrap(~ Fmort, scales = "free_y") +
    scale_fill_manual(
        name = "Temperature",
        values = c("+5°C" ="red", "-5°C" = "skyblue")
    ) +
    scale_y_continuous(
        limits = c(0, 0.35),
        expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
        title = "Stationary vs non-stationary length-based indicator differences for each species at +5°C and -5°C",
        x = "Species",
        y = "Estimation bias \n (Stationary vs non-stationary length indicator)"
    ) 

g13

# # Save g13 plot to a file
ggsave(g13, filename = paste0("Shared/Outputs/comparison_low_medium_high_", comparison, ".png"), width = 18, height = 9, units = "in", dpi = 300)

# Prepare data for bar plot (as before)
# Prepare data for plotting highest and lowest values together for shared legend

# Combine highest and lowest into one long dataframe
comparison_hilo_long <- comparison %>%
    mutate(
        growth_strategy = factor(growth_strategy, levels = c("Slow", "Fast"))
    ) %>%
    pivot_longer(
        cols = c(stationary_highest, nonstationary_highest, stationary_lowest, nonstationary_lowest),
        names_to = "metric",
        values_to = "value"
    ) %>%
    mutate(
        condition = case_when(
            metric %in% c("stationary_highest", "stationary_lowest") ~ "Stationary",
            metric %in% c("nonstationary_highest", "nonstationary_lowest") ~ "Non-stationary"
        ),
        indicator = case_when(
            metric %in% c("stationary_highest", "nonstationary_highest") ~ "Highest values per species",
            metric %in% c("stationary_lowest", "nonstationary_lowest") ~ "Lowest values per species"
        ),
        condition = factor(condition, levels = c("Stationary", "Non-stationary")),
        indicator = factor(indicator, levels = c("Highest values per species", "Lowest values per species"))
    )

g14_combined <- ggplot(comparison_hilo_long,
    aes(x = species, y = value, fill = condition)
) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    scale_fill_manual(
        name = "Condition",
        values = c("Stationary" = "brown", "Non-stationary" = "orange")
    ) +
    facet_wrap(~ indicator, scales = "free_y") +
    labs(
        title = "Highest and lowest length indicator values under stationary \n and non-stationary conditions at optimal temperature",
        x = "Species",
        y = "Length indicator"
    ) +
    facet_wrap(~ indicator, scales = "free_y") +
    scale_y_continuous(
        limits = c(0, 1.6),
        expand = expansion(mult = c(0, 0.05))
    )

g14_combined


# # Plot a grouped bar chart showing both stationary and non-stationary percentage declines side by side for each species

comparison_long <- comparison %>%
    mutate(
        growth_strategy = factor(growth_strategy, levels = c("Slow", "Fast"))
    ) %>%
    pivot_longer(
        cols = c(stationary_percentage_decline, nonstationary_percentage_decline),
        names_to = "Condition",
        values_to = "Percentage_Decline"
    ) %>%
    mutate(
        Condition = recode(
            Condition,
            stationary_percentage_decline = "Stationary",
            nonstationary_percentage_decline = "Non-stationary"
        ),
        Condition = factor(Condition, levels = c("Stationary", "Non-stationary"))
    )

g15 <- ggplot(comparison_long,
    aes(x = species, y = Percentage_Decline, fill = Condition)
) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    scale_fill_manual(
        name = "Condition",
        values = c("Stationary" = "brown", "Non-stationary" = "orange")
    ) +
        scale_y_continuous(
            limits = c(0, 80),
            expand = expansion(mult = c(0, 0.05))
        ) +
        labs(
        title = "Percentage decline in length indicator values between lowest \n and highest fishing mortality rates under changing M",
        x = "Species",
        y = "Percentage (%) decline in length indicator"
    )

g15

# # # Save g15 plot to a file
ggsave(g14_combined + g15, filename = paste0("Shared/Outputs/comparison_percentage_under_changing_M_", comparison, ".png"), width = 26, height = 12, units = "in", dpi = 300)

# # ## Changes in indicators with temperature and fishing mortality

