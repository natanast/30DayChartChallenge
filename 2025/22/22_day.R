

rm(list = ls())
gc()


# libraries ---------

library(ggplot2)
library(dplyr)
library(stringr)
library(data.table)
library(ggtext)
library(extrafont)
library(ggstream)


# Load data -------

outer_space_objects <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-04-23/outer_space_objects.csv')


# Clean data ------

df <- outer_space_objects[
    , .(total_objects = sum(num_objects, na.rm = TRUE)), 
    by = Entity
]


df = df[order(-total_objects)]


index <- df[1:10, Entity]

df1 <- outer_space_objects[Entity %in% index]

df1$Entity <- df1$Entity |> factor(levels = rev(index))


# plot -------

col = c('#476670', '#62828c', '#7f9faa', '#9cbcc8', '#c2dae2', '#badaf4', '#8ebde2', '#70a0c3', '#5383a5', '#366788')

col = c('#405f69', '#587782', '#70909b', '#8aaab5', '#a3c4cf', '#96c5ea', '#7babcf', '#6191b4', '#48789a', '#2f6181')

col = c('#565781', '#6f6e9a', '#8887b4', '#a2a0cf', '#bcbaea', '#fdad94', '#f38b6f', '#e06c53', '#cd4b35', '#af3324')

ggplot(df1, aes(x = Year, y = Entity, fill = Entity, size = num_objects)) +
    
    geom_point(
        shape = 21,
        stroke = 0.15,
        alpha = 0.9,
        color = "white"
    ) +
    
    coord_radial(inner.radius = .3) +
    
    scale_size_continuous(range = c(3.5, 9)) +
    
    # scale_fill_brewer(palette = "Dark2") +
    
    
    scale_fill_manual(values = rev(col)) +
    
    # labs(
    #     title = "Fueling the Footprint: Major Emitters Since 1930",
    #     subtitle = "Ten entities that shaped the planet’s carbon footprint, measured in million tonnes of CO₂-equivalent (MtCO₂e)",
    #     x = "",
    #     y = "MtCO₂e",
    #     caption = "30DayChartChallenge 2025: <b> Day 21</b> | Source: <b> Carbon Majors Emissions Data (TidyTuesday) </b> | Graphic: <b>Natasa Anastasiadou</b>",
    # ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        axis.text.x = element_text(size = 10, vjust = 5, color = "grey80"),
        axis.text.y = element_blank(),
        
        axis.title = element_blank(),
        
        plot.title = element_markdown(size = 14, face = "bold", color = "grey20", hjust = 0.5, family = "Candara", margin = margin(t = 2, b = 5)),
        plot.subtitle = element_markdown(size = 11, hjust = 0.5, color = "grey40", margin = margin(t = 5, b = 20)),
        plot.caption = element_markdown(margin = margin(t = 10), size = 7, hjust = 1),
        
        panel.grid.major = element_line(linewidth = .15, color = "grey80", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        plot.background = element_rect(fill = "grey20", color = NA),
        
        plot.margin = margin(20, 20, 20, 20)

    )
        

g

# Save the plot with custom size and resolution
ggsave("21_day.png", plot = g, width = 10, height = 6, dpi = 600)


