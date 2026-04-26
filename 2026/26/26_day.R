

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(colorspace)


# load data ------

emissions <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-05-21/emissions.csv')


# clean data ------

dt_viz <- emissions[
    year >= 1950, 
    .(total_MtCO2e = sum(total_emissions_MtCO2e, na.rm = TRUE)), 
    by = .(year, parent_entity)
]

dt_total <- dt_viz[, .(global_total = sum(total_MtCO2e, na.rm = TRUE)), by = year]


# Plot -------

 
gr <- ggplot() +
    
    geom_line(
        data = dt_viz,
        aes(x = year, y = total_MtCO2e, group = parent_entity), 
        color = "grey50", 
        alpha = 0.40,       
        linewidth = 0.3
    ) +
    
   geom_line(
        data = dt_total,
        aes(x = year, y = global_total),
        color = "#a33a3a",
        linewidth = 1.5
    ) +
    
    geom_area(
        data = dt_total,
        aes(x = year, y = global_total),
        fill = '#d92525',
        alpha = 0.08
    ) +
    
    # Scales
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(1950, 2024, 10)) +
    
    labs(
        title = "Global emission trends (1950–2022)",
        subtitle = "Tracking carbon emissions since 1950. The actions of single companies are messy and uncertain, <br> but their total combined pollution creates a massive global trend.",
        caption = "30DayChartChallenge 2026: <b> Day 26 (Uncertainties) </b> | Prompt: <b>Trend</b> | Source: <b>Carbon Majors</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "",
        y = "Total Emissions (MtCO2e)"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        plot.background = element_rect(fill = "grey93", color = NA),
        
        panel.grid.major.y = element_line(color = "grey75", linewidth = 0.4, linetype = "dashed"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        
        
        axis.text = element_text(color = "grey20", size = 10, face = "bold"),
        axis.title.y = element_text(color = "grey20", size = 11, margin = margin(r = 15), face = "bold"),
        
        
        plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey35", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20)
    )

gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 9, units = "in", dpi = 600
)



