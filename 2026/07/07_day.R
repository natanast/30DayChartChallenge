

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(ggridges) 
library(scales)   


# load data ------

dt <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-07-14/astronauts.csv")


# clean data ------

dt <- dt[!is.na(hours_mission) & hours_mission > 0]

top_nations <- dt[, .N, by = nationality][order(-N)][1:4, nationality]

dt_plot <- dt[nationality %in% top_nations]

dt_plot[, nationality := factor(nationality, levels = c("France", "Japan", "U.S.", "U.S.S.R/Russia"))]


# plot -----

col <- c(
    "France" = "#5a8192",         # Slate Blue
    "Japan" = "#e9c46a",          # Sun Orange
    "U.S." = "#db9044",           # Deep Steel Blue
    "U.S.S.R/Russia" = "#b24745"  # Terracotta Red
)


gr <- ggplot(dt_plot, aes(x = hours_mission, y = nationality, fill = nationality)) +
    
    geom_density_ridges(
        jittered_points = TRUE,
        position = position_points_jitter(width = 0.05, height = 0),
        point_shape = 21,
        point_size = 1.5,
        point_color = "white",
        point_fill = "grey30",
        alpha = 0.8,
        color = "white",
        scale = 1
    ) +
    
    scale_fill_manual(values = col) +
    
    scale_x_log10(
        breaks = c(1, 10, 100, 1000, 10000),
        labels = c("1 Hour", "10 Hrs", "100 Hrs", "1,000 Hrs\n(~40 Days)", "10,000 Hrs\n(~1 Year)"),
        expand = expansion(mult = c(0.05, 0.1))
    ) +
    
    labs(
        title = "The Multiscale Duration of Human Spaceflight",
        subtitle = "A logarithmic distribution of space mission durations.",
        caption = "30DayChartChallenge 2026: <b> Day 7 </b> | Source: <b> The Astronaut Database (TidyTuesday) </b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "", 
        x = "Mission Duration (Logarithmic Scale)"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none", 
        
        axis.title.x = element_text(size = 11, face = "bold", color = "grey30", margin = margin(t = 15)),
        axis.text.y = element_text(size = 12, face = "bold", color = "black", vjust = 0),
        axis.text.x = element_text(size = 10, face = "bold", color = "grey40"),
        
        panel.grid.major.x = element_line(linewidth = 0.4, color = "grey85", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), 
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


gr



# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 9, units = "in", dpi = 600
)

