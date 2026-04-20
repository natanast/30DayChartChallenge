

rm(list = ls())
gc()


# load libraries -----

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(ggstream)


# load data -------

dt <- "olympics_dataset.csv" |> fread()
rm(list = ls())
gc()

# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(ggstream)

# load data ------

# Using the exact filename from your console
dt_olympics <- "olympics_dataset.csv" |>
    fread()

# clean data -----

# 1. Filter for Summer Olympics and ONLY Gold Medals (Updated to capitalized column names)
dt_gold <- dt_olympics[Season == "Summer" & Medal == "Gold"]

# 2. Find the top 10 National Olympic Committees (NOCs) by total historical GOLD medals
top_gold_nocs <- dt_gold[, .N, by = NOC][order(-N)][1:10, NOC]

# 3. Filter the dataset to ONLY include these top 10 NOCs
dt_top10 <- dt_gold[NOC %in% top_gold_nocs]

# 4. Count total gold medals awarded per year, per NOC
dt_count <- dt_top10[, .(gold_count = .N), by = .(Year, NOC)]

# 5. Complete the grid ONLY for years the Summer Olympics actually happened.
olympic_years <- unique(dt_olympics[Season == "Summer", Year])
grid <- CJ(Year = olympic_years, NOC = unique(dt_count$NOC))
dt_plot <- dt_count[grid, on = .(Year, NOC)]
dt_plot[is.na(gold_count), gold_count := 0]

# 6. Reorder factor levels from 1st to 10th
dt_plot[, NOC := factor(NOC, levels = top_gold_nocs)]

# plot --------

# 10-color vintage palette
cols <- c(
    "#b25c56",  # 1
    "#d97a73",  # 2
    "#e8998f",  # 3
    "#fcd4be",  # 4
    "#e3d2b8",  # 5
    "#aebfbc",  # 6
    "#8aa39b",  # 7
    "#85aebc",  # 8
    "#628b9c",  # 9
    "#4a6b7c"   # 10
)
names(cols) <- levels(dt_plot$NOC)

gr <- ggplot(dt_plot, aes(x = Year, y = gold_count, fill = NOC)) +
    
    # The stream plot
    geom_stream(
        bw = 0.85, 
        color = "#f2f2f2", 
        linewidth = 0.25,
        alpha = 0.9
    ) +
    
    scale_fill_manual(values = cols, name = "Top 10 National Committees") +
    
    guides(fill = guide_legend(nrow = 2, override.aes = list(size = 3, color = NA))) +
    
    labs(
        title = "The Golden Eras of the Summer Games",
        subtitle = "Tracking the Top 10 all-time gold-winning nations reveals the shifting eras of global sporting superpowers",
        caption = "30DayChartChallenge 2026: <b> Day 21 </b> | Source: <b> Kaggle (1896-2024) </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Year",
        y = "Athlete Gold Medals Awarded"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "grey85", linetype = "dashed", linewidth = 0.4),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#f2f2f2", color = NA),
        panel.background = element_rect(fill = "#f2f2f2", color = NA),
        
        axis.text.x = element_text(size = 10, color = "grey40", face = "bold"),
        axis.text.y = element_blank(), 
        axis.title.x = element_text(size = 12, face = "bold", color = "grey30", margin = margin(t = 15)),
        axis.title.y = element_text(size = 12, face = "bold", color = "grey30", margin = margin(r = 15)),
        
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(size = 10),
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20)
    )

gr


gr <- ggplot(dt_plot, aes(x = Year, y = gold_count, fill = NOC)) +
    
    # LAYER 1: The solid main body (clipped to actual Olympic years)
    geom_stream(
        bw = 0.85, 
        extra_span = 0.1,       # Tells the math to create the taper
        color = "#f2f2f2",      # Keeps your white separation lines
        linewidth = 0.3,
        alpha = 0.9
    ) +
    

    # LAYER 2: The translucent faded tails (extends into the extra_span padding)
    geom_stream(
        bw = 0.85, 
        type = "mirror", 
        extra_span = 0.1, 
        true_range = "none",    # Forces ggplot to draw the imaginary taper
        alpha = 0.3,            # Makes it translucent
        color = NA              # Removes white borders on the tails for a smoother fade
    ) +
    
    scale_fill_manual(values = cols, name = "Top 10 National Committees") +
    
    guides(fill = guide_legend(nrow = 2, override.aes = list(size = 3, color = NA))) +
    
    labs(
        title = "The Golden Eras of the Summer Games",
        subtitle = "Tracking the Top 10 all-time gold-winning nations.",
        caption = "30DayChartChallenge 2026: <b> Day 21 </b> | Source: <b> Kaggle (1896-2024) </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Year",
        y = "Athlete Gold Medals Awarded"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_line(color = "grey85", linetype = "dashed", linewidth = 0.4),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#f2f2f2", color = NA),
        panel.background = element_rect(fill = "#f2f2f2", color = NA),
        
        axis.text.x = element_text(size = 10, color = "grey40", face = "bold"),
        axis.text.y = element_blank(), 
        axis.title.x = element_text(size = 12, face = "bold", color = "grey30", margin = margin(t = 15)),
        axis.title.y = element_text(size = 12, face = "bold", color = "grey30", margin = margin(r = 15)),
        
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(size = 10),
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20)
    )

gr

# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)


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


