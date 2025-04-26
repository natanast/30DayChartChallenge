

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(dplyr)


# load data --------

tornados <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-05-16/tornados.csv')

    
# data cleaning -----------

df = tornados[, .(yr, st, slat, slon, elat, elon, fat, loss, inj)]


# Plotting -------

col =  c('#2c5769', '#6F99AD', '#ffb39a', '#df7775', '#ab403f')



p = df_avg_country |>
    
    ggplot(aes(x = avg_Exposure, y = avg_Coping, size = avg_WRI, fill = RiskCategory)) +
    
    geom_point(
        shape = 21, 
        alpha = 0.85, 
        stroke = 0.2, 
        color = "white"
    ) +
    
    scale_size(range = c(1.5, 9)) +
    
    geom_text(
        aes(label = label),
        hjust = 0.5, 
        vjust = -1.75, 
        size = 3, 
        color = "black"
    ) +

    scale_fill_manual(
        values = rev(col)
    ) +
    
    labs(
        title = "Disaster Risk Across Countries: How Exposure and Coping Capacity Shape Vulnerability (2011-2021)",
        subtitle = "Exploring the relationship between countries' exposure to hazards and their coping capacity,<br> with insights into the World Risk Index (WRI).</br>",
        caption = "30DayChartChallenge 2025: <b> Day 25</b> 
                   | Source: <b> World Disaster Risk Dataset (Kaggle) </b> 
                   | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Average Exposure to Hazards",
        y = "Average Lack of Coping Capabilities",
        size = "Average WRI",
        fill = "Risk Category"
    ) +
    
    
    theme_minimal(base_family = "Candara") +
    
    theme(

        legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.box = "vertical",
        
        axis.title.y = element_text(size = 11, vjust = 5),
        axis.title.x = element_text(size = 11, vjust = -2),
        
        axis.text = element_text(size = 10),
        
        plot.title = element_markdown(size = 15, face = "bold", hjust = 0.25, margin = margin(t = 2, b = 2)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5,  color = "grey30", margin = margin(t = 5, b = 20)),
        plot.caption  = element_markdown(margin = margin(t = 35), size = 8, hjust = 1.45),
        
        panel.grid.major = element_line(color = "grey65", linewidth = 0.25, linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA),
    ) +
    
    guides(
        size = guide_legend(override.aes = list(shape = 21, color = "grey30", stroke = 0.5)),
        fill = guide_legend(override.aes = list(size = 5))
    )


p 

ggsave(
    plot = p, filename = "25_day.png",
    width = 10, height = 10, units = "in", dpi = 600
)    

