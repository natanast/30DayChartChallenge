

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

olympics <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2021/2021-07-27/olympics.csv')

# data cleaning -----------
library(tidyverse)
library(ggridges)
library(tidytuesdayR)

# Load the dataset
tt <- tt_load('2021-07-27')
olympics <- tt$athletes

# Filter: remove NAs and select top sports with enough weight data
top_sports <- olympics %>%
    filter(!is.na(height)) %>%
    count(sport, sort = TRUE) %>%
    top_n(10, n) %>%
    pull(sport)

# Filter main data
olympics_filtered <- olympics %>%
    filter(!is.na(weight), sport %in% top_sports)

# Ridgeline plot
ggplot(olympics_filtered, aes(x = weight, y = fct_rev(fct_infreq(sport)), fill = stat(x))) +
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 0.4) +
    scale_fill_viridis_c(option = "C") +
    labs(
        title = "Distribution of Athlete Weights Across Sports",
        subtitle = "Multimodal distributions reflect different body types across Olympic disciplines",
        x = "Weight (kg)",
        y = "Sport"
    ) +
    theme_minimal(base_family = "Candara") +
    theme(
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.title.y = element_blank()
    )






# # plot ---------
# 
# 
# gr = df |>
#     ggplot(aes(x = reorder(name, rating_dev), y = rating_dev, fill = rating_dev)) +
#     
#     geom_segment(aes(xend = name, yend = 0, color = rating_dev), size = 0.7) +
#     
#     geom_point(
#         shape = 21, size = 4, stroke = 0.15, color = "white"
#     ) +
#     
#     scale_color_gradientn(
#         colors = col,
#         limits = c(min(df$rating_dev), max(df$rating_dev)),
#         values = scales::rescale(c(min(df$rating_dev), 0, max(df$rating_dev))), # Rescale to ensure zero is the midpoint
#         breaks = c(min(df$rating_dev), 0, max(df$rating_dev))
#     ) +
#     
#     scale_fill_gradientn(
#         colors = col,
#         limits = c(min(df$rating_dev), max(df$rating_dev)),
#         values = scales::rescale(c(min(df$rating_dev), 0, max(df$rating_dev))), # Rescale to ensure zero is the midpoint
#         breaks = c(min(df$rating_dev), 0, max(df$rating_dev))
#     ) +
# 
#     labs(
#         title = "Ratings of Dungeons & Dragons Board Games",
#         subtitle = "A chart showing how each D&D-themed board game's average rating deviates from the overall D&D average.",
#         caption = "Source: <b> Board Games Data</b> | Graphic: <b>Natasa Anastasiadou</b>",
#         x = "Board Game",
#         y = "Rating Deviation"
#     ) +
#     
#     theme_minimal() +
#     
#     theme(
#         
#         legend.position = "none",
#         
#         axis.title.x = element_blank(),
#         axis.title.y = element_text(size = 11, family = "Candara"),
#         
#         axis.text.y = element_text(size = 10, family = "Candara"),
#         axis.text.x = element_text(size = 9, angle = 90, hjust = 1, vjust = 0.5, family = "Candara"),
#         
#         plot.title = element_markdown(size = 17, face = "bold", color = "grey20", hjust = 0.5, family = "Candara", margin = margin(t = 2, b = 5)),
#         plot.subtitle = element_markdown(size = 13, hjust = 0.5, family = "Candara", color = "grey40", margin = margin(t = 5, b = 20)),
#         plot.caption = element_markdown(margin = margin(t = 10), size = 8.5, family = "Candara", hjust = 1),
# 
#         panel.grid.major = element_line(linewidth = .25, color = "grey75", linetype = "dashed", lineend = "round"),
#         panel.grid.minor = element_line(linewidth = .25, color = "grey75", linetype = "dashed", lineend = "round"),
#         
#         plot.margin = margin(20, 20, 20, 20),
#         
#         plot.background = element_rect(fill = "#e4e4e3", color = NA)
#         
#     )
# 


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 9.5, units = "in", dpi = 600
)

