

rm(list = ls())
gc()


# load libraries -----

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(ggstream)


# load data -------

dt_olympics <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-08-06/olympics.csv" |>
    fread()

# clean data -----

# 1. Filter for Summer Olympics and ONLY Gold Medals
dt_gold <- dt_olympics[season == "Summer" & medal == "Gold"]

# 2. Find the top 10 National Olympic Committees (NOCs) by total historical GOLD medals
top_gold_nocs <- dt_gold[, .N, by = noc][order(-N)][1:10, noc]

# 3. CRITICAL FIX: Actually filter the dataset to ONLY include these top 10 NOCs!
dt_top10 <- dt_gold[noc %in% top_gold_nocs]

# 4. Count total gold medals awarded per year, per NOC
dt_count <- dt_top10[, .(gold_count = .N), by = .(year, noc)]

# 5. Complete the grid ONLY for years the Summer Olympics actually happened.
olympic_years <- unique(dt_olympics[season == "Summer", year])
grid <- CJ(year = olympic_years, noc = unique(dt_count$noc))
dt_plot <- dt_count[grid, on = .(year, noc)]
dt_plot[is.na(gold_count), gold_count := 0]

# 6. Reorder factor levels from 1st to 10th
dt_plot[, noc := factor(noc, levels = top_gold_nocs)]

# plot --------

# Expanded 10-color vintage palette
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
names(cols) <- levels(dt_plot$noc)

gr <- ggplot(dt_plot, aes(x = year, y = gold_count, fill = noc)) +
    
    
    geom_stream(
        bw = .85, 
        type = "mirror", 
        color = "#f2f2f2", 
        linewidth = 0.3,
        alpha = 0.9
    ) +
    
    scale_fill_manual(values = cols, name = "Top 10 National Committees") +
    
    # Changed nrow to 2 so the 10 items fit perfectly at the bottom
    guides(fill = guide_legend(nrow = 2, override.aes = list(size = 3, color = NA))) +
    
    labs(
        title = "The Golden Eras of the Summer Games",
        subtitle = "Tracking the Top 10 all-time gold-winning nations reveals the rise and fall<br>of global sporting superpowers, including the stark impact of the Cold War.",
        caption = "30DayChartChallenge 2026: <b> Day 21 </b> | Source: <b> 120 Years of Olympic History (TidyTuesday) </b> | Graphic: <b>Natasa Anastasiadou</b>",
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


