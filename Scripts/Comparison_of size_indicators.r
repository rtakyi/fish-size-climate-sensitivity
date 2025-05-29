
rm(list = ls())

# Load libraries
library(ggplot2)
library(tidyverse)
library(reshape2)
library(patchwork)

# Define reusable palette
# palette_name <- "RdBu"


# Set theme for output plots
theme_set(theme_classic())
theme_update(axis.text.x = element_text(colour = "black", size = 17, angle = 45, hjust = 1, face = "italic"),
              axis.text.y = element_text(colour = "black", size = 17), 
              axis.title = element_text(size = 17),
              strip.text = element_text(size = 17),
                legend.text = element_text(size = 17),
                legend.title = element_text(size = 17),
              plot.title = element_text(size = 17, hjust = 0.5))

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
                                 high_negt5 = row$high_negt5, )

    # Baseline values
    static_highest <- comparison$static_highest[irow]
    static_lowest <- comparison$static_lowest[irow]
    static_percentage_decline <- comparison$static_percentage_decline[irow]
    dynamic_highest <- comparison$dynamic_highest[irow]
    dynamic_lowest <- comparison$dynamic_lowest[irow]
    dynamic_percentage_decline <- comparison$dynamic_percentage_decline[irow]
    
   
    # Print the data frame
    # print(comparison_df)
    
}

# Plot a bar chart for the comparison with ggplot2
# Reshape the data to long format for plotting multiple variables on y-axis
# Combine static_highest/static_lowest and dynamic_highest/dynamic_lowest into two groups for plotting
comparison_long <- comparison %>%
    mutate(
        growth_strategy = factor(growth_strategy, levels = c("Slow", "Fast"))
    ) %>%
    pivot_longer(
        cols = c(static_highest, static_lowest, dynamic_highest, dynamic_lowest),
        names_to = "metric",
        values_to = "value"
    ) %>%
    mutate(
        group = case_when(
            metric %in% c("static_highest", "static_lowest") ~ "Static",
            metric %in% c("dynamic_highest", "dynamic_lowest") ~ "Dynamic"
        ),
        metric = recode(
            metric,
            static_highest = "Highest",
            static_lowest = "Lowest",
            dynamic_highest = "Highest",
            dynamic_lowest = "Lowest"
        ),
        group = factor(group, levels = c("Static", "Dynamic")),
        metric = factor(metric, levels = c("Highest", "Lowest"))
    )

g10 <- ggplot(
    comparison_long,
    aes(x = species, y = value, fill = metric)
) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    facet_wrap(~ group, scales = "free_y") +
    scale_fill_manual(
        name = "Indicator",
        values = c("Highest" = "darkgreen", "Lowest" = "darkorange")
    ) +
    labs(
        title = "Highest and lowest length-based indicators under static and dynamic conditions for each species",
        x = "Species",
        y = "Length-based indicator"
    )

g10

# # Save g10 plot to a file
# ggsave(g10, filename = paste0("Shared/Outputs/comparison_", comparison, ".png"), width = 17, height = 9, units = "in", dpi = 600)


# Plot a bar chart for the percentages of the comparison with ggplot2
comparison_long <- comparison %>%
    mutate(
        growth_strategy = recode(
            growth_strategy,
            "Slow" = "Slow",
            "Fast" = "Fast"
        ),
        # Set factor levels to ensure Slow comes before Fast
        growth_strategy = factor(growth_strategy, levels = c("Slow", "Fast"))
    ) %>%
    pivot_longer(
        cols = c(
            static_percentage_decline, dynamic_percentage_decline
        ),
        names_to = "metric",
        values_to = "value"
    ) %>%
    mutate(
        metric = recode(
            metric,
            static_percentage_decline = "Static",
            dynamic_percentage_decline = "Dynamic"
        ),
        # Set factor levels to ensure static comes before dynamic
        metric = factor(
            metric,
            levels = c("Static", "Dynamic")
        )
    )
g11 <- ggplot(comparison_long,
    aes(x = species, y = value, fill = growth_strategy)
) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ metric, scales = "free_y") +
    scale_fill_manual(
        name = "Growth Strategy",
        values = c("Slow" = "red", "Fast" = "blue")
    ) +
    labs(
        title = "Percentage decline in length-based indicators under static and dynamic conditions for each species",
        x = "Species",
        y = "Percentage (%) decline in length-based indicator"
    )

g11

# # Save g11 plot to a file
# # ggsave(g11, filename = paste0("Shared/Outputs/comparison_percentage_", comparison, ".png"), width = 18, height = 9, units = "in", dpi = 600)


# # Save g10 and g11 plots to a file  
# # Combine g10 and g11 using patchwork and save as a single image
ggsave(g10 / g11, filename = "Shared/Outputs/comparison_combined.png", width = 18, height = 18, units = "in", dpi = 600)


# Plot a bar chart for low, medium, and high values of the comparison with ggplot2
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

g12 <- ggplot(comparison_long,
    aes(x = species, y = value, fill = Temp, group = Temp)
) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
    facet_wrap(~ Fmort, scales = "free_y") +
    scale_fill_manual(
        name = "Temperature",
        values = c("+5°C" ="red", "-5°C" = "skyblue")
    ) +
    labs(
        title = "Static vs dynamic length-based indicator differences for each species at +5°C and -5°C",
        x = "Species",
        y = "Static vs dynamic length-based indicator differences"
    ) 

g12

# # Save g12 plot to a file
ggsave(g12, filename = paste0("Shared/Outputs/comparison_low_medium_high_", comparison, ".png"), width = 18, height = 9, units = "in", dpi = 600)