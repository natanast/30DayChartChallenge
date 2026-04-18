
rm(list = ls())
gc()


# load libraries -----

library(ggplot2)
library(dplyr)
library(stringr)
library(data.table)
library(gghighlight)
library(ggtext)
library(extrafont)


# Load data -------



# Clean data ------

# plot -------
# 
# col <- c("Alphabet" = "#1c499e", 
#          "Amazon" = "#964b22", 
#          "Apple" = "#b03030", 
#          "Microsoft" = "#cf8f3a", 
#          "NVIDIA" = "#6ca1c9")
# 
# 
# gr <- ggplot(df, aes(x = as.numeric(year), y = avg_price, color = company_name)) +
#     
#     
#     geom_line(linewidth = 1) +
#     
#    
#     geom_point(
#         data = df_2024, aes(x = as.numeric(year), y = avg_price, fill = company_name), 
#         size = 5, 
#         shape = 21, 
#         stroke = 0.5, 
#         color = "white"
#     ) +  
#     
#     
#     gghighlight(
#         use_direct_label = FALSE,
#         unhighlighted_params = list(colour = alpha("grey83", 1), linewidth = 0.6)
#     ) +
#     
#     
#     geom_text(
#         data = df_2024,
#         aes(
#             x = as.numeric(year),
#             y = avg_price,
#             label = paste0("$", round(avg_price)),
#             color = company_name
#         ),
#         size = 3.75,
#         vjust = 2.5, 
#         hjust = 0.3,
#         family = "Candara",
#         fontface = "bold"
#     ) +
#     
#     facet_wrap('~company_name', ncol = 2) +
#     
#     scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
#     
#     scale_color_manual(values = col) +
#     scale_fill_manual(values = col) +
#     
#     labs(
#         title = "Stock Trends of Big Tech Companies Over the Years",
#         subtitle = "Tracking the Annual Average High Prices of Leading Tech Giants from 2010 to 2024",
#         caption = "30DayChartChallenge 2026: <b> Day 19 </b> | Source: Stock Data <b> (Kaggle) </b> | Graphic: <b>Natasa Anastasiadou</b>",
#         x = "",
#         y = "Avg High Price (USD)"
#     ) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#         legend.position = "none",
#         
#         plot.title = element_markdown(size = 18, face = "bold", color = "grey20", hjust = 0.5, margin = margin(t = 2, b = 5)),
#         plot.subtitle = element_markdown(size = 15, hjust = 0.5, color = "grey40", margin = margin(t = 5, b = 20)),
#         plot.caption = element_markdown(margin = margin(t = 10), size = 10, hjust = 1),
#         
#         panel.grid.major = element_line(linewidth = .25, color = "grey85", linetype = "dashed", lineend = "round"),
#         panel.grid.minor = element_line(linewidth = .25, color = "grey85", linetype = "dashed", lineend = "round"),
#         
#         axis.title.y = element_text(size = 12, vjust = 5, face = "bold", color = "grey30"),
#         
#         axis.text.x = element_text(size = 10, angle = 45, hjust = 1, color = "grey40"), 
#         axis.text.y = element_text(size = 10, color = "grey40"),
#         
#         strip.text.x.top = element_text(size = 12, face = "bold", margin = margin(b = 10), color = "grey10"),
#         
#         plot.background = element_rect(fill = "grey95", color = NA),
#         
#         plot.margin = margin(20, 20, 20, 20)
#     ) 
# 
# gr


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 10, units = "in", dpi = 600
)

