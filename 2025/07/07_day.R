

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(paletteer)
library(tidyverse)


# load data --------

office_ratings <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-03-17/office_ratings.csv')


office_ratings$season <- office_ratings$season |> as.character()



# outliers 

outliers <- office_ratings[imdb_rating > 9.5 | imdb_rating <= 7]


office_ratings_clean <- office_ratings[!(imdb_rating > 9.5 | imdb_rating <= 7)]



colors = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 9)

# Plot 1 -----------

gr <- ggplot() +
    
    geom_boxplot(
        data = office_ratings_clean,
        aes(x = season, y = imdb_rating, fill = season),
        position = position_dodge(width = .5),
        width = .35, outlier.shape = NA, alpha = 0.45, linewidth = 0.2
    ) +
    
    geom_point(
        data = office_ratings_clean,
        aes(x = season, y = imdb_rating, fill = season),
        position = position_jitterdodge(jitter.width = .25, dodge.width = .5),
        shape = 21, size = 2.5, stroke = .15, color = "white"
    ) +
    
    geom_point(
        data = outliers,
        aes(x = season, y = imdb_rating, fill = season),
        shape = 21, size = 4.5, stroke = 0.25, color = "white"
    ) +
    # 
    # geom_text(
    #     data = outliers,
    #     aes(x = season, y = imdb_rating, label = paste0("Ep. ", episode)),
    #     # position = position_jitter(width = 0.25, height = 0),
    #     family = "Candara",
    #     size = 3.5,
    #     hjust = -0.4,
    #     vjust = 0.4,
    #     check_overlap = TRUE
    # ) +
    # 
    
    geom_label(
        data = outliers,
        aes(x = season, y = imdb_rating, label = paste0("Ep. ", episode)),
        # position = position_jitter(width = 0.25, height = 0),
        family = "Candara",
        size = 3.5,
        label.size = 0.2,     # border thickness
        label.padding = unit(0.15, "lines"),
        label.r = unit(0.15, "lines"),  # border radius
        color = "grey35",
        hjust = -0.4,
        alpha = 0.15
    ) +
    
    scale_fill_manual(values = colors) +
    
    scale_color_manual(values = colors) +
    
    theme_minimal() +
    
    labs(
        title = "The Office: IMDb Ratings Across Seasons",
        subtitle = "Rating distribution per season. Outliers (above 9.5 or below and equal 7) are labeled.",
        caption = "Source: <b>The Office ratings</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Season",
        y = "IMDb Rating"
    ) +
    
    theme(
        legend.position = "none",
        
        axis.title.x = element_text(size = 10, family = "Candara"),
        axis.title.y = element_text(size = 10, family = "Candara"),
        axis.text.x = element_text(size = 10, family = "Candara"),
        axis.text.y = element_text(size = 10, family = "Candara"),
        
        panel.grid.major = element_line(linewidth = .35, color = "grey80", linetype = "dashed"),
        panel.grid.minor = element_line(linewidth = .35, color = "grey80", linetype = "dashed"),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, family = "Candara", margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 11, hjust = 0.5, family = "Candara", color = "grey30", margin = margin(t = 5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 20), size = 8, family = "Candara", hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
    )

gr

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 8, units = "in", dpi = 600
)

