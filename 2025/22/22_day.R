

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

ggplot(df1, aes(x = Year, y = Entity, fill = Entity, size = num_objects)) +
    
    geom_point(
        shape = 21,
        stroke = 0.15,
        alpha = 0.9,
        color = "white"
    ) +
    
    
    coord_radial(inner.radius = .3) +
    
    scale_size_continuous(range = c(3.5, 9)) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        axis.title = element_blank(),
        
        
    )
        
    
    

    
    scale_fill_manual(values = col) +
    
    scale_x_continuous(breaks = c(1930, 1950, 1975, 2000, 2022), limits = c(1930, 2027)) +
    

    labs(
        title = "Fueling the Footprint: Major Emitters Since 1930",
        subtitle = "Ten entities that shaped the planet’s carbon footprint, measured in million tonnes of CO₂-equivalent (MtCO₂e)",
        x = "",
        y = "MtCO₂e",
        caption = "30DayChartChallenge 2025: <b> Day 21</b> | Source: <b> Carbon Majors Emissions Data (TidyTuesday) </b> | Graphic: <b>Natasa Anastasiadou</b>",
    ) +
    

    
    theme(
        legend.position = "none",
        
        axis.title.y = element_text(size = 10, vjust = 5),
        axis.text = element_text(size = 9),
        
        plot.title = element_markdown(size = 14, face = "bold", color = "grey20", hjust = 0.5, family = "Candara", margin = margin(t = 2, b = 5)),
        plot.subtitle = element_markdown(size = 11, hjust = 0.5, color = "grey40", margin = margin(t = 5, b = 20)),
        plot.caption = element_markdown(margin = margin(t = 10), size = 7, hjust = 1),

        panel.grid.major = element_line(linewidth = .25, color = "grey80", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
         
        plot.margin = margin(20, 20, 20, 20)
    )



g

# Save the plot with custom size and resolution
ggsave("21_day.png", plot = g, width = 10, height = 6, dpi = 600)




ggplot(df1, aes(x = Year, y = Entity, size = num_objects, fill = Entity)) +
    geom_point(shape = 21, color = "white", alpha = 0.9, stroke = 0.2) +
    scale_size_continuous(range = c(1, 6)) +
    coord_polar() +
    scale_fill_brewer(palette = "Dark2") +
    theme_minimal(base_family = "Candara") +
    theme(
        axis.title = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 6, angle = 90),
        panel.grid = element_line(color = "grey80", linewidth = 0.2),
        legend.position = "bottom",
        plot.background = element_rect(fill = "#000010", color = NA),
        panel.background = element_rect(fill = "#000010", color = NA),
        plot.title = element_text(color = "white", size = 14, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "grey70", size = 6, hjust = 1)
    ) +
    labs(
        title = "Starburst of Space Launches",
        subtitle = "Each point shows the number of objects launched into space by major entities across years",
        caption = "Data: TidyTuesday | Viz: Natasa Anastasiadou"
    )

