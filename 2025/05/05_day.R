

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


# Plot -----------

ggplot(d, aes(x = episode, y = season, group = season)) +
    
    geom_line(color = "grey65", size = 0.6, alpha = 0.7) +
    
    geom_point(
        aes(fill = unique_words),
        size = 3.5,
        shape = 21,
        stroke = 0.15,
        alpha = 0.9
    ) +
    
    scale_fill_stepsn(
        colors = c("#7d7ca9","#abaad9","#ffffe0","#ff9a92","#b24745"),
        breaks = c(1000, 1200, 1400),
        guide = guide_colorsteps(
            title = "Bubble Color",
            barheight = unit(7, "lines"),
            barwidth = unit(0.5, "lines")
            
        )
    ) +

    coord_radial(inner.radius = .2) +
    
    theme_minimal() +
    
    # Adjust x-axis limits to start from 1
    scale_x_continuous(limits = c(1, max(d$episode) + 2), expand = c(0, 0)) +
    
    geom_text(aes(label = season_text),
              na.rm = TRUE,
              color = "grey50",
              size = 2.5,
              fontface = "bold",
              hjust = 1.75,
              vjust = 0.35) +
    
    labs(
        title = "Radial plot of Unique Words per episode across 14 seasons of <span style='color: #b24745;'>Bob's Burgers</span>.",
        subtitle = "Each bubble represents an <b>Episode</b>. Each circle represents a <b>Season</b>. <br> Episodes with unique words ranging from <b><span style='color: #7d7ca9; font-weight: bold;'>1000 to 1200</span></b> appear more often.",
        caption = "Source: <b>  {bobsburgersR} R Package</b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +
    
    theme(

        legend.position = "right",
        legend.title.position = "left",
        
        legend.title = element_text(size = 7, face = "bold", family = "Candara", color = "grey30", angle = 90, hjust = .5),
        legend.text = element_text(size = 7, family = "Candara", color = "grey30"),

        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        
        panel.grid.major = element_line(linewidth = .35, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 15, face = "bold", hjust = 0.5, family = "Candara", margin = margin(t = 25, l = 50)),
        plot.subtitle = element_markdown(size = 11, hjust = 0.5, family = "Candara", color = "grey30", margin = margin(t = 10, l = 50)),
        plot.caption = element_markdown(margin = margin(t = 12), size = 8, family = "Candara", hjust = 1.25),
        
        plot.margin = margin(6, 6, 6, 6),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
    )

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 8, height = 8, units = "in", dpi = 600
)


