

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
library(colorspace)

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

col = c('#405f69', '#587782', '#70909b', '#8aaab5', '#a3c4cf', '#fdad94', '#f38b6f', '#e06c53', '#cd4b35', '#af3324')


g = ggplot(df1, aes(x = Year, y = Entity, fill = Entity, size = num_objects)) +
    
    geom_point(
        shape = 21,
        stroke = 0.1,
        color = "white"
    ) +
    
    coord_radial(inner.radius = .3) +
    
    scale_size_continuous(
        range = c(2.5, 7.5),   
        # breaks = c(500, 1500, 2500), 
        # labels = c("500", "1,500", "2,500"), 
        name = "No. of objects"
    ) +
    
    scale_fill_manual(values = rev(col), guide = "none") +
    
    # Adjust x-axis limits to start from 1
    scale_x_continuous(limits = c(1960, 2029), expand = c(0, 0)) +

    # World
    annotate(
        "text", 
        x = 2026.95, y = 10.2,
        label = "World",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = darken(col[1], 0.25)
    ) +
    
    # United States
    annotate(
        "text", 
        x = 2025.55, y = 9.8,
        label = "United States",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = darken(col[2], 0.25)
    ) +
    
    # Russia 
    annotate(
        "text", 
        x = 2026.55, y = 8.55,
        label = "Russia",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = darken(col[3], 0.25)
    ) +
    
    # China  
    annotate(
        "text", 
        x = 2026.65, y = 7.5,
        label = "China",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = darken(col[4], 0.25)
    ) +
    
    # United Kingdom  
    annotate(
        "text", 
        x = 2024.95, y = 7,
        label = "United Kingdom",
        hjust = 0,
        size = 3.25,
        lineheight = .7,
        fontface = "bold",
        color = darken(col[5], 0.35)
    ) +
    
    # Japan 
    annotate(
        "text", 
        x = 2026.15, y = 5.55,
        label = "Japan",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = darken(col[6], 0.25)
    ) +
    
    # France   
    annotate(
        "text", 
        x = 2025.65, y = 4.55,
        label = "France",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = darken(col[7], 0.25)
    ) +
    
    # India    
    annotate(
        "text", 
        x = 2025.85, y = 3.5,
        label = "India",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = darken(col[8], 0.25)
    ) +
    
    # Germany     
    annotate(
        "text", 
        x = 2024.15, y = 2.75,
        label = "Germany",
        hjust = 0,
        size = 3.25,
        lineheight = .7,
        fontface = "bold",
        color = darken(col[9], 0.25)
    ) +
    
    # European Space Agency    
    annotate(
        "text", 
        x = 2023.8, y = 1.55,
        label = "European Space Agency",
        hjust = 0,
        size = 2.25,
        lineheight = .7,
        fontface = "bold",
        color = darken(col[10], 0.25)
    ) +
    

    labs(
        title = "Racing for the Stars: Who’s Launching Objects into Space?",
        subtitle = "A visual chronicle of satellite and space object launches since 1960, highlighting the major players in the orbital age",
        caption = "30DayChartChallenge 2025: <b> Day 22</b> | Source: <b> Objects Launched into Space (TidyTuesday) </b> | Graphic: <b>Natasa Anastasiadou</b>",
    ) +

    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "right",
        legend.title.position = "left",
        
        legend.title = element_text(size = 10, face = "bold", color = "grey20", angle = 90, hjust = .5),
        legend.text = element_text(size = 9, color = "grey20"),
     
        axis.text.x = element_text(size = 11, vjust = 5, color = "grey20"),
        axis.text.y = element_blank(),
        
        axis.title = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", color = "grey10", hjust = 0.75, margin = margin(t = 25, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.25, color = "grey30", margin = margin(t = 10, b = 10)),
        plot.caption = element_markdown(margin = margin(t = 10), size = 8, hjust = 1.35),
        
        panel.grid.major = element_line(linewidth = .15, color = "grey75", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        plot.background = element_rect(fill = "grey60", color = NA),
        
        plot.margin = margin(20, 20, 20, 20)

    ) +
    
    guides(
        size = guide_legend(override.aes = list(shape = 21, color = "grey30", stroke = 0.5))
    )
        


g

# Save the plot with custom size and resolution
ggsave("22_day.png", plot = g, width = 10, height = 10, dpi = 600)

