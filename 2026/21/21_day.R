

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
