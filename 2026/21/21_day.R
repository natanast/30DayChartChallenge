

rm(list = ls())
gc()


# load libraries -----

library(ggplot2)
library(stringr)
library(data.table)
library(ggtext)
library(extrafont)
library(colorspace)


rm(list = ls())
gc()

# 1. Load Libraries -------
library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)

# 2. Load Data -------
# Pulling the Himalayan Expeditions dataset directly from the TidyTuesday archives
expeditions <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')

# 3. Clean and Prep -------
# Count the number of expeditions per year starting from 1920
dt_hist <- expeditions[year >= 1920, .(n_expeditions = .N), by = year][order(year)]

# 4. Create Historical Annotations -------
# We will use segments and text to mark major eras in Himalayan climbing
annotations <- data.table(
    year = c(1924, 1953, 1978, 1996),
    y_pos = c(150, 250, 350, 420),
    label = c(
        "1924\nMallory & Irvine\nEverest Attempt",
        "1953\nHillary & Norgay\nFirst Everest Ascent",
        "1978\nReinhold Messner\nAscent w/o Oxygen",
        "1996\nThe Everest Disaster\n(Commercialization Era)"
    ),
    # We right-align the last label (1) so it doesn't get cut off the edge of the screen
    h_align = c(0, 0, 0, 1) 
)

# 5. Plot -------
gr <- ggplot(dt_hist, aes(x = year, y = n_expeditions)) +
    
    # The historical "Mountain" shape (Area chart)
    geom_area(fill = "#7393a7", alpha = 0.5) +
    geom_line(color = "#2c424d", linewidth = 0.8) +
    
    # Add vertical dotted lines for the historical milestones
    geom_segment(data = annotations, aes(x = year, xend = year, y = 0, yend = y_pos), 
                 color = "#b24745", linetype = "dotted", linewidth = 0.7) +
    
    # Add historical milestone text
    geom_text(data = annotations, aes(x = year, y = y_pos, label = label, hjust = h_align),
              family = "Candara", size = 3.5, fontface = "bold", color = "#b24745",
              vjust = -0.2, lineheight = 0.9) +
    
    # Add points at the base of the timeline like pins on a map
    geom_point(data = annotations, aes(x = year, y = 0), color = "#b24745", size = 2.5) +
    
    scale_x_continuous(breaks = seq(1920, 2020, by = 10), expand = c(0.02, 0)) +
    
    # Expand the Y axis slightly so our text doesn't hit the ceiling
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) + 
    
    labs(
        title = "The History of Himalayan Mountaineering",
        subtitle = "Annual number of recorded expeditions (1920 - 2019)",
        caption = "30DayChartChallenge 2026: <b>Day 21 (Historical)</b> | Source: <b>The Himalayan Database</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Year of Expedition",
        y = "Number of Expeditions"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        plot.title = element_markdown(size = 18, face = "bold", color = "#2c424d", hjust = 0.5, margin = margin(b = 5)),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#4a5a63", margin = margin(b = 25)),
        plot.caption = element_markdown(size = 8, color = "#6c7a83", margin = margin(t = 20)),
        
        # Vintage styling: soft dashed horizontal lines, no vertical lines
        panel.grid.major.y = element_line(linewidth = .3, color = "#d1ccc3", linetype = "dashed"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        
        axis.title = element_text(size = 10, face = "bold", color = "#4a5a63"),
        axis.text = element_text(size = 9, color = "#4a5a63"),
        
        # Vintage parchment/old map background
        plot.background = element_rect(fill = "#f4f0e8", color = NA),
        plot.margin = margin(25, 30, 25, 25)
    )

gr

# 6. Save -------
ggsave("21_historical_himalayas.png", plot = gr, width = 11, height = 7, dpi = 600)

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

