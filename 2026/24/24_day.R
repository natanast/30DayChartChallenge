

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(friends)


# load data ------

# This is the direct raw link from their official repository
url <- "https://raw.githubusercontent.com/dmulvihill/transit-costs/master/data/data.csv"

# Use data.table to grab it
dt_latest <- fread(url)

# clean data ------


# plot --------
# 
# 
# gr <- ggplot(dt_long, aes(x = Year, y = Spending, group = Gift)) +
#     
#     geom_line(aes(color = Gift), linewidth = 1) +
#     
# 
#     geom_point(
#         aes(color = Gift),
#         shape = 21, 
#         stroke = 1, 
#         size = 4.5,
#         fill = "grey95"
#     ) +
#     
#     geom_text_repel(
#         data = dt_long[Year == "2010"],
#         aes(label = paste0(Gift, " ($", round(Spending), ")"), color = Gift),
#         hjust = 1.1, 
#         direction = "y", 
#         size = 4.5, 
#         fontface = "bold", 
#         segment.color = NA
#     ) +
#     
#     
#     geom_text_repel(
#         data = dt_long[Year == "2022"],
#         aes(label = paste0("($", round(Spending), ") ", Gift), color = Gift),
#         hjust = -0.1, 
#         direction = "y", 
#         size = 4.5, 
#         fontface = "bold", 
#         segment.color = NA
#     ) +
#     
#     scale_color_manual(values = col) +
#     
#     
#     scale_x_discrete(expand = expansion(mult = 0.5)) +
#     
#     labs(
#         title = "The Rising Cost of Romance",
#         subtitle = "Average per-person Valentine's Day spending in the US: 2010 vs 2022.",
#         caption = "30DayChartChallenge 2026: <b> Day 4 (Slope)</b> | Source: <b> NRF (TidyTuesday)</b> | Graphic: <b>Natasa Anastasiadou</b>"
#     ) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#         legend.position = "none", 
#         
#         axis.title = element_blank(),
#         axis.text.y = element_blank(),
#         axis.text.x = element_text(size = 14, face = "bold", color = "black"),
#         
# 
#         panel.grid.major = element_line(linewidth = 0.35, color = "grey85"),
#         panel.grid.minor = element_blank(),
#         
#         plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
#         plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1, lineheight = 1.2),
#         
#         plot.background = element_rect(fill = "#e4e4e3", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )

# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 9, units = "in", dpi = 600
)

