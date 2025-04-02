

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(ggthemes)
library(paletteer)
library(colorspace)
library(ggrepel)


# load data --------

taylor_album_songs <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-10-17/taylor_album_songs.csv')


# data cleaning ------

d = taylor_album_songs[, .(album_name, track_number, track_name, danceability, energy)]

# Plot -----------

ggplot(d, aes(x = track_number, y = album_name, group = album_name)) +
    
    geom_line(color = "grey65", size = 0.6, alpha = 0.7) +
    
    geom_point(
        aes(fill = energy, size = danceability),
        shape = 21,
        stroke = 0.15,
        alpha = 0.9
    ) +
    
    scale_size(range = c(1, 6), guide = guide_legend(title = "Danceability")) +  # Adjust the point size range
    
    
    scale_fill_stepsn(
        # colors = c("#7d7ca9","#abaad9","#ffffe0","#ff9a92","#b24745"),
        colors =  c('#2c5769', '#6F99AD', 'grey96', '#ffb5ac', '#b1532a'),
        # breaks = c(1000, 1200, 1400),
        guide = guide_colorsteps(
            title = "Bubble Color",
            barheight = unit(7, "lines"),
            barwidth = unit(0.5, "lines")
            
        )
    ) +
    
    theme_minimal() +
    
    labs(
        title = " Danceability & Energy Levels in Taylor Swift’s Albums.",
        subtitle = "Tracks are ranked by their order in each album, with <b>bubble size</b> representing <b>danceability</b> and <b>color</b> indicating <b>energy levels</b>.",
        caption = "Source: <b>  {taylor} R Package</b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +
    
    theme(

        legend.position = "right",
        legend.title.position = "left",
        
        legend.title = element_text(size = 7, face = "bold", family = "Candara", color = "grey30", angle = 90, hjust = .5),
        legend.text = element_text(size = 7, family = "Candara", color = "grey30"),

        axis.title.y = element_blank(),
        # axis.title.x = element_blank(),
        # axis.text.x = element_blank(), 
        # axis.text.y = element_blank(), 
        
        panel.grid.major = element_line(linewidth = .35, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 15, face = "bold", hjust = 0.5, family = "Candara", margin = margin(t = 15)),
        plot.subtitle = element_markdown(size = 11, hjust = 0.5, family = "Candara", color = "grey30", margin = margin(t = 5, b = 10)),
        plot.caption = element_markdown(margin = margin(t = 12), size = 8, family = "Candara", hjust = 1.3),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
    )

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 8, height = 8, units = "in", dpi = 600
)

