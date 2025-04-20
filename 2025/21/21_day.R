
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

emissions <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-05-21/emissions.csv')


# Clean data ------

top_emitters_dt <- emissions[
    , .(total_emissions = sum(total_emissions_MtCO2e, na.rm = TRUE)),
    by = parent_entity
]


top_emitters_dt = top_emitters_dt[order(-total_emissions)]

top_emitters <- top_emitters_dt[1:10, parent_entity]

df <- emissions[parent_entity %in% top_emitters]


df_plot <- df[, .(total_emissions = sum(total_emissions_MtCO2e, na.rm = TRUE)),
    by = .(year, parent_entity)
]


custom_order <- c("China (Coal)",  "BP", "Chevron", "Coal India", 
                  "Former Soviet Union", "ExxonMobil",
                  "National Iranian Oil Co.", "Saudi Aramco",
                  "Shell", "Gazprom")


df_plot$parent_entity <- df_plot$parent_entity |> factor(levels = custom_order)


# plot -------

library(paletteer)

col = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 10)


g = ggplot(df_plot, aes(x = year, y = total_emissions, fill = parent_entity)) +
    
    geom_stream(type = "ridge", color = "white", lwd = 0.1) +
    
    scale_fill_manual(values = col) +
    
    scale_x_continuous(breaks = c(1930, 1950, 1975, 2000, 2022), limits = c(1930, 2022)) +
    
    labs(
        title = "Top 10 Emitting Entities Over Time",
        subtitle = "Emissions measured in million tonnes of CO₂ equivalent (MtCO₂e)",
        x = "",
        y = "Emissions (MtCO₂e)",
        caption = "30DayChartChallenge 2025: <b> Day 19</b> | Source: <b> Big Tech Stock Prices (Kaggle) </b> | Graphic: <b>Natasa Anastasiadou</b>",
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        axis.title.y = element_text(size = 10, vjust = 5),
        axis.text = element_text(size = 9),
        
        plot.title = element_markdown(size = 14, face = "bold", color = "grey20", hjust = 0.5, family = "Candara", margin = margin(t = 2, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey40", margin = margin(t = 5, b = 20)),
        plot.caption = element_markdown(margin = margin(t = 10), size = 7, hjust = 1),

        panel.grid.major = element_line(linewidth = .25, color = "grey80", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
         
        plot.margin = margin(20, 20, 20, 20)
    )

    # labs(
    #     title = "Stock Trends of Big Tech Companies Over the Years",
    #     subtitle = "Tracking the Annual Adjusted Closing Prices of Leading Tech Giants from 2010 to 2022",
    #     caption = "30DayChartChallenge 2025: <b> Day 19</b> | Source: <b> Big Tech Stock Prices (Kaggle) </b> | Graphic: <b>Natasa Anastasiadou</b>",
    #     x = "",
    #     y = "Avg Adj Close Price"
    # ) +
    # 
    # theme(
    #     legend.position = "none",
    #     
    #     plot.title = element_markdown(size = 11, face = "bold", color = "grey20", hjust = 0.5, family = "Candara", margin = margin(t = 2, b = 5)),
    #     plot.subtitle = element_markdown(size = 9, hjust = 0.5, color = "grey40", margin = margin(t = 5, b = 20)),
    #     plot.caption = element_markdown(margin = margin(t = 5), size = 5.5, hjust = 1),
    #     
    #     panel.grid.major = element_line(linewidth = .25, color = "grey80", linetype = "dashed", lineend = "round"),
    #     panel.grid.minor = element_line(linewidth = .25, color = "grey80", linetype = "dashed", lineend = "round"),
    #     
    #     axis.title.y = element_text(size = 8, vjust = 5),
    #     
    #     axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5),
    #     axis.text.y = element_text(size = 6),
    #     
    #     strip.text.x.top = element_text(size = 7),
    #     
    #     plot.background = element_rect(fill = "#e4e4e3", color = NA),
    #     
    #     plot.margin = margin(20, 20, 20, 20)
    # ) 

g

# Save the plot with custom size and resolution
ggsave("21_day.png", plot = g, width = 10, height = 6, dpi = 600)


