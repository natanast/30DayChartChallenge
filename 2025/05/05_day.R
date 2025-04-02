

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
# library(ggtext)
library(paletteer)


# load data --------

taylor_album_songs <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-10-17/taylor_album_songs.csv')

# data cleaning ------


# Plot -----------

gr = ggplot(df_long, aes(x = factor(Year), y = Runners, fill = Category)) +
    
    geom_bar(width = 0.7, stat = "identity", position = "dodge") +
    
    coord_radial(
        start = 0,
        inner.radius = 0.15
    ) +

    labs(
        title = "London Marathon",
        subtitle = "A year-by-year comparison of starters and finishers in the London Marathon.",
        caption = "Source: <b>  {LondonMarathon} R package</b> | Graphic: <b>Natasa Anastasiadou</b>",
        fill = ""
    ) +
    
    
    scale_fill_manual(values = c("Starters" = "#f09a8c", "Finishers" = "#5f899d")) +
    
    geom_text(data = label_data, aes(x = id, y = Starters, label = Year, hjust = hjust, angle = angle, vjust = 0.5), 
              color = "black", fontface = "bold", alpha = 0.6, size = 2.5, , inherit.aes = FALSE) +
    

    theme_minimal() +
    
    theme(
        
        # legend.position = c(0.5, 0.10),
        # legend.position = "bottom",
        legend.position = c(0.5, 0.1),  # Centered horizontally (0.5), moved up (0.1)
        legend.direction = "horizontal",
        # legend.title = element_text(size = 10, hjust = 0.5, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 8, family = "Candara", color = "grey30"),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, family = "Candara", margin = margin(t = 10)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.25, family = "Candara", color = "grey30", margin = margin(t = 15)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, family = "Candara", hjust = 1.2),
        
        plot.margin = margin(20, 20, 20, 20),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),  
        axis.title = element_text(size = 14),  
        
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        
    )
    

gr


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 8, height = 8, units = "in", dpi = 600
)


