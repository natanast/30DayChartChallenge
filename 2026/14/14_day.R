

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(colorspace)
library(shadowtext)
library(tidyr)
library(forcats)
library(scales)

# load data ------

erasmus_raw <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-03-08/erasmus.csv')

# clean data ------

df_heatmap <- erasmus_raw[, .(N = sum(participants, na.rm = TRUE)), 
                          by = .(sending_country_code, receiving_country_code)]


top_list <- df_heatmap[, .(total = sum(N)), by = sending_country_code][order(-total)][1:15, sending_country_code]
df_heatmap <- df_heatmap[sending_country_code %in% top_list & receiving_country_code %in% top_list]


df_heatmap <- df_heatmap |>
    complete(sending_country_code, receiving_country_code, fill = list(N = 0)) |>
    as.data.table()


df_heatmap[sending_country_code == receiving_country_code, N := NA]


df_heatmap[, sending_country_code := fct_reorder(sending_country_code, N, function(x) sum(x, na.rm = TRUE))]
df_heatmap[, receiving_country_code := fct_reorder(receiving_country_code, N, function(x) sum(x, na.rm = TRUE))]


# plot --------

gr = ggplot(df_heatmap, aes(x = sending_country_code, y = receiving_country_code, fill = N)) +
    
    geom_tile(color = "grey20", linewidth = .25) +
    
    geom_shadowtext(
        aes(label = N), 
        color = "black",
        family = "Candara",
        bg.color = "grey95", 
        bg.r = .1, 
        size = 2.2 
    ) +
    
    scale_fill_stepsn(
        colors = c('#2c5769', '#6F99AD', 'grey96', '#ffb5ac', '#a33a3a'),
        breaks = c(1, 20, 50, 70, 100),  
        # Use pseudo_log_trans to handle zeros gracefully without warnings
        transform = pseudo_log_trans(base = 10),
        name = "Total Participants",
        na.value = "grey96",
        guide = guide_colorsteps(
            barheight = unit(10, "lines"), 
            barwidth = unit(0.4, "lines")
        )
    ) +
    # scale_fill_stepsn(
    #     colors = c('#2c5769', '#6F99AD', 'grey96', '#ffb5ac', '#a33a3a'),
    #     breaks = c(1, 20, 50, 70, 100),  
    #     transform = "log10",
    #     # labels = scales::comma,
    #     name = "Total Participants",
    #     na.value = "grey96",
    #     guide = guide_colorsteps(
    #         barheight = unit(10, "lines"), 
    #         barwidth = unit(0.4, "lines")
    #     )
    # ) +
    
    theme_minimal(base_family = "Candara") +
    
    labs(
        title = "The **Trade** of Cultural Capital: Erasmus Mobility",
        subtitle = "This heatmap illustrates the **relationships** and student exchange volumes between the top 15 participating countries.",
        caption = "30DayChartChallenge 2026: <b> Day 14</b>
                   | Source: <b> Erasmus Data (TidyTuesday)</b>
                   | Graphic: <b>Natasa Anastasiadou</b>",
         x = "Sending Country",
         y = "Receiving Country"
    ) +
    
    theme(
        legend.position = "right",
        legend.title.position = "left",
        
        legend.title = element_text(size = 9, angle = 90, hjust = .5, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 8, family = "Candara", color = "grey30"),
        
        axis.title.x = element_text(size = 9, family = "Candara", face = "bold"),
        axis.title.y = element_text(size = 9, family = "Candara", face = "bold"),
        
        axis.text.x = element_text(size = 8, family = "Candara"),
        axis.text.y = element_text(size = 8, family = "Candara"),
        
        panel.grid = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "grey93", color = NA)
    )  

gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 8, units = "in", dpi = 600
)
