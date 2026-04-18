
rm(list = ls())
gc()


# load libraries -----

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(stringr)
library(colorspace)


# load data ------

dt <- 


# clean data -----



#  plot -----



# gr <- ggplot(dt_counts, aes(x = type_clean, y = elevation_binned, color = type_clean, fill = type_clean)) +
#     
#     geom_point(
#         aes(size = N), 
#         alpha = 0.85,
#         shape = 21,
#         stroke = 0.65
#     ) +
#     
# 
#     scale_size_continuous(
#         range = c(3, 13), 
#         name = "Number of\nVolcanoes", 
#         breaks = c(10, 25, 50, 75)
#     ) +
#     
#     guides(
#         size = guide_legend(
#             override.aes = list(
#                 fill = "grey80", 
#                 color = "grey40", 
#                 stroke = 0.5,
#                 size = c(4, 6, 8, 10) 
#             )
#         )
#     ) +
#     
#     scale_color_manual(
#         values = col |> darken(.15), 
#         guide = "none"
#     ) +
#     
#     scale_fill_manual(
#         values = col , 
#         guide = "none"
#     ) +
#     
#     labs(
#         title = "How Tall Are Different Types of Volcanoes",
#         subtitle = "Comparing the elevation distributions of five common volcanic structures.<br>Bubbles represent 500-meter intervals, with larger sizes indicating more volcanoes.",
#         caption = "30DayChartChallenge 2026: <b> Day 11 </b> | Source: <b> Volcano Eruptions (TidyTuesday) </b> | Graphic: <b>Natasa Anastasiadou</b>",
#         x = "", 
#         y = "Elevation (Meters above sea level)"
#     ) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#         legend.key.size = unit(0.05, "cm"),
#         legend.position = "right",
#         legend.title = element_text(size = 10, face = "bold", color = "grey40"),
#         legend.text = element_text(size = 9, color = "grey30"),
#         
#         axis.text.x = element_text(size = 12, face = "bold", color = "black", margin = margin(t = 10)),
#         axis.text.y = element_text(size = 10, face = "bold", color = "grey50"),
#         axis.title.y = element_text(size = 11, face = "bold", color = "grey30", margin = margin(r = 15)),
#         
# 
#         panel.grid.major = element_line(linewidth = 0.35, color = "grey85", linetype = "dashed"),
#         panel.grid.minor = element_blank(),
#         
#         plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 14, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
#         plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
#         
#         plot.background = element_rect(fill = "grey95", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )
# 
# gr



# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 9, height = 9, units = "in", dpi = 600
)
