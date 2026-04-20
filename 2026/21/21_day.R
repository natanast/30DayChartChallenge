

rm(list = ls())
gc()


# load libraries -----

library(ggplot2)
library(stringr)
library(data.table)
library(ggtext)
library(extrafont)
library(colorspace)


# load data -------

worlds_fairs <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-08-13/worlds_fairs.csv')


# clean data -----



# plot -------
# 
# cols <- c("Above 0" = "#b24745", "Below 0" = "#00429d")
# 
# 
# gr <- ggplot(dt_plot, aes(x = Year, y = Anomaly, color = Temp_Type, fill = Temp_Type)) +
#     
#     geom_hline(yintercept = 0, color = "grey40", linewidth = 0.7) +
#     
#     geom_segment(
#         aes(x = Year, xend = Year, y = 0, yend = Anomaly), 
#         linewidth = 0.65, 
#         alpha = 0.85
#     ) +
#     
#     geom_point(
#         size = 3.5, 
#         shape = 21,
#         stroke = 0.25
#     ) +
#     
#     scale_color_manual(
#         values = cols |> darken(.15), 
#         guide = "none"
#     ) +
#     
#     scale_fill_manual(
#         values = cols |> lighten(.15), 
#         guide = "none"
#     ) +
#     
#     scale_x_continuous(breaks = seq(1880, 2020, by = 20)) +
#     
#     labs(
#         title = "The Escalation of Global Temperatures",
#         subtitle = "Annual global temperature anomalies (°C) compared to the 1951-1980 average",
#         caption = "30DayChartChallenge 2024: <b>Day 20</b> | Source: <b>NASA GISS (Kaggle)</b> | Graphic: <b>Natasa Anastasiadou</b>",
#         x = "",
#         y = "Temperature Anomaly (°C)"
#     ) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#        
#         legend.position = "none",
#         
#         plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 16, hjust = 0.5, color = "grey30", margin = margin(t = 5, b = 15)),
#         plot.caption = element_markdown(margin = margin(t = 35), size = 10, hjust = 1),
#         
#         panel.grid.major.y = element_line(linewidth = .25, color = "grey70", linetype = "dashed"),
#         panel.grid.minor.y = element_blank(),
#         
#         panel.grid.major.x = element_line(linewidth = .25, color = "grey70", linetype = "dashed"), 
#         panel.grid.minor.x = element_blank(),
#         
#         axis.title.y = element_text(size = 13, vjust = 5, face = "bold", color = "grey30"),
#         axis.text = element_text(size = 11, color = "grey40"),
#         
#         plot.background = element_rect(fill = "grey95", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )
# 
# gr

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 9, units = "in", dpi = 600
)


rm(list = ls())
gc()

# 1. Load Libraries -------
library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(ggridges)

# 2. Load Fresh 2024 Data -------
olympics <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-08-06/olympics.csv')

# 3. Clean and Prep -------
# Select sports with fascinating historical timelines
target_sports <- c("Tug-Of-War", "Polo", "Art Competitions", "Archery", "Tennis", "Athletics")

dt_oly <- olympics[sport %in% target_sports]

# Order them conceptually from the "Extinct" sports down to the "Constant" sports
# This makes the ridgeline flow beautifully from top to bottom
dt_oly[, sport := factor(sport, levels = c("Tug-Of-War", "Polo", "Art Competitions", "Archery", "Tennis", "Athletics"))]

# 4. Custom Elegant Color Palette -------
vintage_colors <- c(
    "Tug-Of-War" = "#8c6243",       # Rope brown
    "Polo" = "#c49a6c",             # Equestrian gold
    "Art Competitions" = "#7a9cb8", # Faded blue
    "Archery" = "#659685",          # Forest teal
    "Tennis" = "#c26d63",           # Clay red
    "Athletics" = "#6e6e6e"         # Track charcoal
)

# 5. Plot -------
gr <- ggplot(dt_oly, aes(x = year, y = sport, fill = sport)) +
    
    # The Ridgeline Geometry
    # scale = 1.3 provides elegant overlap without covering up the historical gaps
    geom_density_ridges(color = "#fdfbf7", alpha = 0.85, linewidth = 0.8, scale = 1.3) +
    
    scale_fill_manual(values = vintage_colors) +
    scale_x_continuous(breaks = seq(1896, 2024, by = 20), expand = c(0.02, 0)) +
    
    labs(
        title = "Extinct and Resurrected",
        subtitle = "The historical lifespan of selected Olympic events (1896 - 2024)",
        caption = "30DayChartChallenge 2026: <b>Day 21 (Historical)</b> | Source: <b>Olympics (TidyTuesday)</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Year of Olympic Games",
        y = ""
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        plot.title = element_markdown(size = 20, face = "bold", color = "#2b2b2b", hjust = 0.5, margin = margin(b = 8)),
        plot.subtitle = element_text(size = 13, hjust = 0.5, color = "#5a5a5a", margin = margin(b = 30)),
        plot.caption = element_markdown(size = 9, color = "#8a8a8a", margin = margin(t = 20)),
        
        # Soft vertical timeline lines
        panel.grid.major.x = element_line(linewidth = 0.3, color = "#dcd6cc", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        axis.title.x = element_text(size = 11, face = "bold", color = "#5a5a5a", margin = margin(t = 15)),
        axis.text.x = element_text(size = 10, color = "#5a5a5a", face = "bold"),
        
        axis.text.y = element_text(size = 11, face = "bold", color = "#2b2b2b", vjust = 0),
        
        # Soft "canvas" background
        plot.background = element_rect(fill = "#fdfbf7", color = NA),
        plot.margin = margin(30, 40, 30, 30)
    )

gr

# 6. Save -------
ggsave("21_historical_olympics_ridges.png", plot = gr, width = 11, height = 7, dpi = 600)
