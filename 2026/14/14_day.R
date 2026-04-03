

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(stringr)
library(ggalluvial)

# load data ------



# clean data -----


# plot --------

rm(list = ls())
gc()

# load libraries -------
library(data.table)
library(tidyverse) # for fct_reorder
library(stringr)
library(ggplot2)
library(ggtext)
library(extrafont)
library(colorspace)
library(viridis)

# load data ------
erasmus_raw <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-03-08/erasmus.csv')

# clean data ------
# Focus on total participants between countries
df <- erasmus_raw[, .(n = sum(participants)), by = .(sending_country_code, receiving_country_code)]

# To keep the heatmap readable, we'll take the top 22 "trading" countries by volume
top_countries <- df[, .(total = sum(n)), by = sending_country_code][order(-total)][1:22, sending_country_code]

df_plot <- df[sending_country_code %in% top_countries & receiving_country_code %in% top_countries]

# Reorder factors so the heatmap has a logical flow (by volume)
df_plot[, sending_country_code := fct_reorder(sending_country_code, n, sum)]
df_plot[, receiving_country_code := fct_reorder(receiving_country_code, n, sum)]

# plot --------

# Define a base color for the scale to use with colorspace if needed
base_col <- "#b24745" 

gr <- ggplot(df_plot, aes(x = sending_country_code, y = receiving_country_code)) +
    
    # Heatmap tiles with a subtle border
    geom_tile(aes(fill = n), color = "grey95", linewidth = 0.5) +
    
    coord_equal() +
    
    # Color scale using Viridis (Magma) - works beautifully for "intensity"
    scale_fill_viridis_c(
        option = "magma",
        trans = "log10", # Log scale helps visualize smaller relationships
        name = "Participants",
        labels = scales::comma_format(),
        begin = 0.1, end = 0.9
    ) +
    
    labs(
        title = "The Global **Trade** of Knowledge",
        subtitle = "<b>Relationships</b> between Erasmus+ sending and receiving countries. <br>
    The color intensity represents the <b>total number of participants</b> (log10 scale).",
        caption = "30DayChartChallenge 2026: <b> Day 14 | Relationships (Trade)</b> 
               | Source: <b>TidyTuesday (Erasmus Student Mobility)</b> 
               | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Sending Country",
        y = "Receiving Country"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "right",
        legend.title = element_text(size = 9, face = "bold"),
        legend.key.height = unit(1.2, "cm"),
        
        # Grid and axis setup
        panel.grid = element_blank(),
        axis.title = element_text(size = 10, face = "bold", color = "grey30"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        
        # Natasa's Signature Styling
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 25), size = 8, hjust = 1, color = "grey40"),
        
        plot.background = element_rect(fill = "grey95", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

# Display plot
gr

# save ---------
ggsave(
    plot = gr, filename = "Day14_Trade_Erasmus.png",
    width = 9, height = 9, units = "in", dpi = 600
)


# gr <- ggplot(df_picto, aes(x = x, y = season_label)) +
#     
#     
#     geom_text(label = "🍔", size = 8, family = "Segoe UI Emoji") +
#     
#     scale_x_continuous(limits = c(0, 13), breaks = seq(0, 12, by = 2)) +
#     
#     labs(
#         title = "Bob's Burgers: The Short & Long Seasons",
#         subtitle = "Total unique words spoken per season. Season 2 was cut to just 9 episodes. <br><b>Each 🍔 represents 2,000 words.</b>",
#         caption = "30DayChartChallenge 2026: <b> Day 2</b>
#                    | Source: <b> bobsburgers (TidyTuesday | Nov 2024)</b>
#                    | Graphic: <b>Natasa Anastasiadou</b>",
#         
#     ) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#         
#         axis.title = element_blank(),
#         
#         axis.text.x = element_text(size = 10, color = "grey30"),
#         axis.text.y = element_text(size = 12, face = "bold", color = "black", margin = margin(r = 10)),
#         
#         panel.grid.major = element_line(linewidth = 0.35, color = "grey85"),
#         panel.grid.minor = element_blank(),
#         
        # plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        # plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        # plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
#         
#         plot.background = element_rect(fill = "grey95", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )
# 
# gr
# 


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)


