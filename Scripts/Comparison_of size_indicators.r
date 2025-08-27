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
theme_update(axis.text.x = element_text(colour = "black", size = 18, angle = 45, hjust = 1, vjust = 1, face = "italic"),
              axis.text.y = element_text(colour = "black", size = 18),
              axis.title.y.right = element_text(colour = "black", size = 18),
              axis.text.y.right = element_text(colour = "black", size = 18), 
              axis.title = element_text(size = 18),
              strip.text = element_text(size = 18),
                legend.text = element_text(size = 18),
                legend.title = element_text(size = 18),
              plot.title = element_text(size = 18, hjust = 0.5))

# Read in the data
comparison <- read.csv("Parameters/comparison.csv")

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
                                 static_highest = row$static_highest,
                                 static_lowest = row$static_lowest,
                                 static_percentage_decline = row$static_percentage_decline,
                                 dynamic_highest = row$dynamic_highest,
                                 dynamic_lowest = row$dynamic_lowest,
                                 dynamic_percentage_decline = row$dynamic_percentage_decline,
                                 low_posit5 = row$low_posit5,
                                 low_negt5 = row$low_negt5,
                                 medium_posit5 = row$medium_posit5,
                                 medium_posit5 = row$medium_negt5,
                                 high_posit5 = row$high_posit5,
                                 high_negt5 = row$high_negt5,
                                 factor_change_static_dynamic_hst = row$factor_change_static_dynamic_hst,
                                 factor_change_static_dynamic_lst = row$factor_change_static_dynamic_lst,
                                 sp_opt_temp = row$sp_opt_temp)

    # Baseline values
    static_highest <- comparison$static_highest[irow]
    static_lowest <- comparison$static_lowest[irow]
    static_percentage_decline <- comparison$static_percentage_decline[irow]
    dynamic_highest <- comparison$dynamic_highest[irow]
    dynamic_lowest <- comparison$dynamic_lowest[irow]
    dynamic_percentage_decline <- comparison$dynamic_percentage_decline[irow]
    factor_change_static_dynamic_hst <- comparison$factor_change_static_dynamic_hst[irow]
    factor_change_static_dynamic_lst <- comparison$factor_change_static_dynamic_lst[irow]
    sp_opt_temp <- comparison$sp_opt_temp[irow]
    
}


# Plot a bar chart for factor changes in static vs dynamic conditions with ggplot2
comparison_long <- comparison %>%
    mutate(
        growth_strategy = factor(growth_strategy, levels = c("Slow", "Fast"))
    ) %>%
    pivot_longer(
        cols = c(factor_change_static_dynamic_hst, factor_change_static_dynamic_lst),
        names_to = "metric",
        values_to = "value"
    ) %>%
    mutate(
        metric = recode(
            metric,
            factor_change_static_dynamic_hst = "Lowest fishing mortality rate",
            factor_change_static_dynamic_lst = "Highest fishing mortality rate"
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
        name = "Difference in percentage estimation bias \n (between static and dynamic assessment)",
    ) +
    labs(
        title = "Estimation factor difference between static and dynamic conditions for each species",
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
    ggplot2::geom_blank(
        data = comparison_long %>% filter(metric == "Lowest fishing mortality rate") %>% mutate(value = 10),
        aes(x = species, y = value)
    )

g10


# # # Save g10 plot to a file
# ggsave(g10, filename = paste0("Shared/Outputs/comparison_factor_change_", comparison, ".png"), width = 18, height = 12, units = "in", dpi = 600)



# Plot a bar chart for the comparison with ggplot2
# Reshape the data to long format for plotting multiple variables on y-axis
# Combine static_highest/static_lowest and dynamic_highest/dynamic_lowest into two groups for plotting

# # Prepare data for bar plot (as before)
# comparison_long <- comparison %>%
#     mutate(
#         growth_strategy = factor(growth_strategy, levels = c("Slow", "Fast"))
#     ) %>%
#     pivot_longer(
#         cols = c(static_highest, static_lowest, dynamic_highest, dynamic_lowest),
#         names_to = "metric",
#         values_to = "value"
#     ) %>%
#     mutate(
#         group = case_when(
#             metric %in% c("static_highest", "static_lowest") ~ "Static conditions",
#             metric %in% c("dynamic_highest", "dynamic_lowest") ~ "Dynamic conditions"
#         ),
#         metric = recode(
#             metric,
#             static_highest = "Highest",
#             static_lowest = "Lowest",
#             dynamic_highest = "Highest",
#             dynamic_lowest = "Lowest"
#         ),
#         group = factor(group, levels = c("Static conditions", "Dynamic conditions")),
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
#         title = "Highest and lowest length indicator values under lowest and highest fishing mortality rates, respectively, in both static and dynamic conditions at optimal temperature",
#         x = "Species"
#     )

# g11

# # # # Save g11 plot to a file
# # # ggsave(g11, filename = paste0("Shared/Outputs/comparison_", comparison, ".png"), width = 17, height = 9, units = "in", dpi = 600)


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
#             static_percentage_decline, dynamic_percentage_decline
#         ),
#         names_to = "metric",
#         values_to = "value"
#     ) %>%
#     mutate(
#         metric = recode(
#             metric,
#             static_percentage_decline = "Static conditions",
#             dynamic_percentage_decline = "Dynamic conditions"
#         ),
#         # Set factor levels to ensure static comes before dynamic
#         metric = factor(
#             metric,
#             levels = c("Static conditions", "Dynamic conditions")
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
#         title = "Percentage decline in length indicator values between lowest and highest fishing mortality rates under static and dynamic conditions",
#         x = "Species",
#         y = "Percentage (%) decline in length indicator"
#     )

# g12

# # Save g12 plot to a file
# # ggsave(g12, filename = paste0("Shared/Outputs/comparison_percentage_", comparison, ".png"), width = 18, height = 9, units = "in", dpi = 600)


# # Save g11 and g12 plots to a file  
# # Combine g10 and g11 using patchwork and save as a single image
# ggsave(g11 / g12, filename = "Shared/Outputs/comparison_combined.png", width = 22, height = 22, units = "in", dpi = 600)



# # Reshape data for plotting: combine posit5 and negt5 for each Fmort level, distinguish by temperature direction
# comparison_long <- comparison %>%
#     mutate(
#         growth_strategy = factor(growth_strategy, levels = c("Slow", "Fast"))
#     ) %>%
#     pivot_longer(
#         cols = c(
#             low_posit5, low_negt5,
#             medium_posit5, medium_negt5,
#             high_posit5, high_negt5
#         ),
#         names_to = "metric",
#         values_to = "value"
#     ) %>%
#     mutate(
#         Fmort = case_when(
#             grepl("^low_", metric) ~ "0.01",
#             grepl("^medium_", metric) ~ "0.40",
#             grepl("^high_", metric) ~ "1.77"
#         ),
#         Temp = case_when(
#             grepl("posit5$", metric) ~ "+5°C",
#             grepl("negt5$", metric) ~ "-5°C"
#         ),
#         Fmort = factor(Fmort, levels = c("0.01", "0.40", "1.77")),
#         Temp = factor(Temp, levels = c("+5°C", "-5°C"))
#     )

# g13 <- ggplot(comparison_long,
#     aes(x = species, y = value, fill = Temp, group = Temp)
# ) +
#     geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
#     facet_wrap(~ Fmort, scales = "free_y") +
#     scale_fill_manual(
#         name = "Temperature",
#         values = c("+5°C" ="red", "-5°C" = "skyblue")
#     ) +
#     scale_y_continuous(
#         limits = c(0, 0.25),
#         expand = expansion(mult = c(0, 0.05))
#     ) +
#     labs(
#         title = "Static vs dynamic length-based indicator differences for each species at +5°C and -5°C",
#         x = "Species",
#         y = "Estimation bias \n (Static vs dynamic length indicator)"
#     ) 

# g13

# # # Save g13 plot to a file
# ggsave(g13, filename = paste0("Shared/Outputs/comparison_low_medium_high_", comparison, ".png"), width = 18, height = 9, units = "in", dpi = 600)

# Prepare data for bar plot (as before)
# Prepare data for plotting highest and lowest values together for shared legend

# # # Combine highest and lowest into one long dataframe
# comparison_hilo_long <- comparison %>%
#     mutate(
#         growth_strategy = factor(growth_strategy, levels = c("Slow", "Fast"))
#     ) %>%
#     pivot_longer(
#         cols = c(static_highest, dynamic_highest, static_lowest, dynamic_lowest),
#         names_to = "metric",
#         values_to = "value"
#     ) %>%
#     mutate(
#         condition = case_when(
#             metric %in% c("static_highest", "static_lowest") ~ "Static",
#             metric %in% c("dynamic_highest", "dynamic_lowest") ~ "Dynamic"
#         ),
#         indicator = case_when(
#             metric %in% c("static_highest", "dynamic_highest") ~ "Highest values per species",
#             metric %in% c("static_lowest", "dynamic_lowest") ~ "Lowest values per species"
#         ),
#         condition = factor(condition, levels = c("Static", "Dynamic")),
#         indicator = factor(indicator, levels = c("Highest values per species", "Lowest values per species"))
#     )

# g14_combined <- ggplot(comparison_hilo_long,
#     aes(x = species, y = value, fill = condition)
# ) +
#     geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
#     scale_fill_manual(
#         name = "Condition",
#         values = c("Static" = "brown", "Dynamic" = "orange")
#     ) +
#     facet_wrap(~ indicator, scales = "free_y") +
#     labs(
#         title = "Highest and lowest length indicator values under static and dynamic conditions at optimal temperature",
#         x = "Species",
#         y = "Length indicator"
#     ) +
#     facet_wrap(~ indicator, scales = "free_y") +
#     scale_y_continuous(
#         limits = c(0, 0.75),
#         expand = expansion(mult = c(0, 0.05))
#     )

# g14_combined


# # # Plot a grouped bar chart showing both static and dynamic percentage declines side by side for each species

# comparison_long <- comparison %>%
#     mutate(
#         growth_strategy = factor(growth_strategy, levels = c("Slow", "Fast"))
#     ) %>%
#     pivot_longer(
#         cols = c(static_percentage_decline, dynamic_percentage_decline),
#         names_to = "Condition",
#         values_to = "Percentage_Decline"
#     ) %>%
#     mutate(
#         Condition = recode(
#             Condition,
#             static_percentage_decline = "Static",
#             dynamic_percentage_decline = "Dynamic"
#         ),
#         Condition = factor(Condition, levels = c("Static", "Dynamic"))
#     )

# g15 <- ggplot(comparison_long,
#     aes(x = species, y = Percentage_Decline, fill = Condition)
# ) +
#     geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
#     scale_fill_manual(
#         name = "Condition",
#         values = c("Static" = "brown", "Dynamic" = "orange")
#     ) +
#     labs(
#         title = "Percentage decline in length indicator values between lowest and highest fishing mortality rates",
#         x = "Species",
#         y = "Percentage (%) decline in length indicator"
#     )

# g15

# # # Save g15 plot to a file
# ggsave(g14_combined + g15, filename = paste0("Shared/Outputs/comparison_percentage_", comparison, ".png"), width = 26, height = 12, units = "in", dpi = 600)

# # ## Changes in indicators with temperature and fishing mortality
