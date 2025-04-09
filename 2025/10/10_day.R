

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(ggridges)


# load data --------

olympics <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-07-27/olympics.csv')

# data cleaning -----------

df <- olympics[!is.na(height), ]

# df <- df[, sport_count := .N, by = sport]


# Define your selected sports
popular_sports <- c("Basketball", "Football", "Handball", 
                    "Cycling", "Volleyball", "Tennis", 
                    "Diving", "Gymnastics", "Swimming",
                    "Taekwondo", "Weightlifting","Wrestling" )



df <- df[sport %in% popular_sports]

# df[, sport := factor(sport, levels = sort(unique(sport), decreasing = TRUE))]

# Modify the sex column to replace "M" with "Male" and "F" with "Female"
df$sex <- factor(df$sex, levels = c("F", "M"), labels = c("Female", "Male"))


# Calculate the average height for each sport
avg_height <- tapply(df$height, df$sport, mean)

# Order the sports based on the average height (descending order)
ordered_sports <- names(sort(avg_height, decreasing = FALSE))


df$sport <- factor(df$sport, levels = ordered_sports)


col = c('#565781', '#6f6e9a', '#8887b4', '#a2a0cf', '#bcbaea', '#d8d7f5', '#f9d2c6', '#fdad94', '#f38b6f', '#e06c53', '#cd4b35', '#af3324')

# Apply transparency (alpha = 0.7) to the color palette
col_alpha <- adjustcolor(col, alpha.f = 0.9)


# plot --------


gr = ggplot(df, aes(x = height, y = sport, fill = sport)) +
    
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 0.05, lwd = 0.05) +
    
    scale_fill_manual(values = col_alpha) +
    
    
    labs(
        title = "Distribution of Athlete Heights Across Popular Olympic Sports",
        subtitle = "Separate distributions for female and male athletes reveal body type diversity",
        caption = "Source: <b> Olympic Games Data</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Height (cm)",
        y = "Sport"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        legend.position = "none", 
        
        plot.title = element_markdown(size = 17, face = "bold", color = "grey20", hjust = 0.5, family = "Candara", margin = margin(t = 2, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, family = "Candara", color = "grey40", margin = margin(t = 5, b = 20)),
        plot.caption = element_markdown(margin = margin(t = 10), size = 8.5, family = "Candara", hjust = 1),

        panel.grid.major = element_line(linewidth = .25, color = "grey75", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_line(linewidth = .25, color = "grey75", linetype = "dashed", lineend = "round"),

        
        axis.title.y = element_blank(),

        plot.margin = margin(20, 20, 20, 20),

        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        
        panel.spacing = unit(1.5, "cm"),
        
        strip.text = element_text(size = 12, face = "bold", color = "grey30", hjust = 0.5, margin = margin(b = 10))
        
    ) +
    
    facet_wrap(~sex) 
    


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 9.5, units = "in", dpi = 600
)

