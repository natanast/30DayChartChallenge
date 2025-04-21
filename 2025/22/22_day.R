

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


df1$label <- ifelse(df1$Year == 2023, df1$Entity, "")


df1$Entity <- df1$Entity |> factor(levels = rev(index))


# plot -------



col = c('#405f69', '#587782', '#70909b', '#8aaab5', '#a3c4cf', '#fdad94', '#f38b6f', '#e06c53', '#cd4b35', '#af3324')



g = ggplot(df1, aes(x = Year, y = Entity, fill = Entity, size = num_objects)) +
    
    geom_point(
        shape = 21,
        stroke = 0.1,
        color = "white"
    ) +
    
    coord_radial(inner.radius = .3) +
    
    scale_size_continuous(range = c(3, 7), name = "No. of objects") +
    
    scale_fill_manual(values = rev(col), guide = "none") +
    
    # Adjust x-axis limits to start from 1
    scale_x_continuous(limits = c(1960, 2030), expand = c(0, 0)) +
    
    geom_text(aes(label = label),
              na.rm = TRUE,
              color = "grey90",
              size = 2.5,
              fontface = "bold") +
    
    # labs(
    #     title = "Fueling the Footprint: Major Emitters Since 1930",
    #     subtitle = "Ten entities that shaped the planet’s carbon footprint, measured in million tonnes of CO₂-equivalent (MtCO₂e)",
    #     x = "",
    #     y = "MtCO₂e",
    #     caption = "30DayChartChallenge 2025: <b> Day 21</b> | Source: <b> Carbon Majors Emissions Data (TidyTuesday) </b> | Graphic: <b>Natasa Anastasiadou</b>",
    # ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "right",
        legend.title.position = "left",
        
        legend.title = element_text(size = 10, face = "bold", color = "grey80", angle = 90, hjust = .5),
        legend.text = element_text(size = 8, color = "grey90"),
     
        axis.text.x = element_text(size = 10, vjust = 5, color = "grey80"),
        axis.text.y = element_blank(),
        
        axis.title = element_blank(),
        
        plot.title = element_markdown(size = 14, face = "bold", color = "grey20", hjust = 0.5, family = "Candara", margin = margin(t = 2, b = 5)),
        plot.subtitle = element_markdown(size = 11, hjust = 0.5, color = "grey40", margin = margin(t = 5, b = 20)),
        plot.caption = element_markdown(margin = margin(t = 10), size = 7, hjust = 1),
        
        panel.grid.major = element_line(linewidth = .15, color = "grey80", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        plot.background = element_rect(fill = "grey50", color = NA),
        
        plot.margin = margin(20, 20, 20, 20)

    )
        

g

# Save the plot with custom size and resolution
ggsave("21_day.png", plot = g, width = 10, height = 10, dpi = 600)


