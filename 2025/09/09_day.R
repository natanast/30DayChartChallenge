

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

tornados <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-05-16/tornados.csv')


# Plot 1 -----------
# Create the histogram
ggplot(tornados, aes(x = yr)) +
    geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
    labs(title = "Tornado Frequency by Year",
         x = "Year",
         y = "Number of Tornadoes") +
    theme_minimal() +
    scale_x_continuous(breaks = seq(1950, 2022, by = 5)) # Adjust x-axis to show ev


# Create the histogram
ggplot(tornados, aes(x = mo)) +
    geom_histogram(stat = "count", fill = "steelblue", color = "black", alpha = 0.7) +
    labs(title = "Tornado Frequency by Month",
         x = "Month",
         y = "Number of Tornadoes") +
    scale_x_continuous(breaks = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    theme_minimal()


# plot -----------

# ggplot(avg_risk_dt, aes(x = factor(Group), y = Proportion, fill = Group)) +
#     
#     geom_bar(
#         position = "stack", 
#         stat = "identity", 
#         width = 1, 
#         alpha = 0.9, 
#         color = "black",
#         linewidth = 0.25,
#         fill = "#ACD4EC"
#     ) +
#     
#     coord_polar(start = 0) + 
#     
#     geom_text(data = label_data, aes(x = id, y = Proportion, label = Group, angle = angle),
#               color = "black", fontface = "bold", alpha = 0.6, size = 2.3, inherit.aes = FALSE,
#               family = "Candara") +
# 
# 
#     theme_minimal() +
#     
#     # labs(title = "Circular Barplot of Proportions of Conditions",
#     #      subtitle = "Proportion of individuals with each condition (binary 0/1)",
#     #      x = "Group",
#     #      y = "Proportion of 1's") +
#     
# 
#     theme(
# 
#         legend.position = "none",
#         legend.title.position = "left",
# 
#         legend.title = element_blank(),
# 
#         axis.title.x = element_blank(),
# 
#         axis.title.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
# 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_line(linewidth = .35, color = "grey80"),
# 
#         plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, family = "Candara", margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 11, hjust = 0.5, family = "Candara", color = "grey30", margin = margin(t = 5, b = 25)),
#         plot.caption = element_markdown(margin = margin(t = 5), size = 8, family = "Candara", hjust = 1.25),
# 
#         plot.margin = margin(6, 6, 6, 6),
# 
#         plot.background = element_rect(fill = "#e4e4e3", color = NA)
#     )
# 
# gr
# 

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 8, units = "in", dpi = 600
)

