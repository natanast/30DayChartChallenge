

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

ratings <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-01-25/ratings.csv')


# data cleaning -----------

index <- str_detect(ratings$name, "D&D|Dungeons & Dragons")

df <- ratings[index, ]


df$name <- df$name |> 
    str_remove_all("D&D|Dungeons & Dragons|:") |>  
    str_squish()



# Calculate the rating deviation
avg_rating <- mean(df$average, na.rm = TRUE)

df[ , rating_dev := average - avg_rating]


df_long <- melt(
    df,
    id.vars = "name",
    measure.vars = "rating_dev",
    variable.name = "game_name",
    value.name = "rating_dev"
)


# plot ---------

library(paletteer)


col = c('#003344', '#0e4051', '#1d4d5e', '#2c5a6c', '#3b677a', '#4a7588', '#598396', '#6791a5', '#76a0b3', '#84aec3', '#93bdd2', '#a2cde2', '#bddae9', '#d9e8ef', '#f5f5f5', '#f8e0de', '#facbc7', '#fdb5ae', '#faa098', '#ed8f89', '#e17e79', '#d46e69', '#c85d59', '#b94d4b', '#aa3e40', '#9a2f34', '#8a2028', '#79101b', '#67000e')


col = c('#1d4d5e', '#4D6291', '#9A68A4', '#DD708E', '#E56946', '#e17e79', '#b94d4b', '#8a2028')


col = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 30)

gr = df |>
    ggplot(aes(x = reorder(name, rating_dev), y = rating_dev, fill = rating_dev)) +
    
    geom_segment(aes(xend = name, yend = 0, color = rating_dev), size = 0.7) +
    
    geom_point(
        shape = 21, size = 4, stroke = 0.15, color = "white"
    ) +
    
    scale_color_gradientn(
        colors = col,
        limits = c(min(df$rating_dev), max(df$rating_dev)),
        values = scales::rescale(c(min(df$rating_dev), 0, max(df$rating_dev))), # Rescale to ensure zero is the midpoint
        breaks = c(min(df$rating_dev), 0, max(df$rating_dev))
    ) +
    
    scale_fill_gradientn(
        colors = col,
        limits = c(min(df$rating_dev), max(df$rating_dev)),
        values = scales::rescale(c(min(df$rating_dev), 0, max(df$rating_dev))), # Rescale to ensure zero is the midpoint
        breaks = c(min(df$rating_dev), 0, max(df$rating_dev))
    ) +

    labs(
        title = "Ratings of Dungeons & Dragons Board Games",
        subtitle = "A chart showing how each D&D-themed board game's average rating deviates from the overall D&D average.",
        caption = "Source: <b> Board Games Data</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Board Game",
        y = "Rating Deviation"
    ) +
    
    theme_minimal() +
    
    theme(
        
        legend.position = "none",
        
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 11, family = "Candara"),
        
        axis.text.y = element_text(size = 10, family = "Candara"),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 1, vjust = 0.5, family = "Candara"),
        
        plot.title = element_markdown(size = 17, face = "bold", color = "grey20", hjust = 0.5, family = "Candara", margin = margin(t = 2, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, family = "Candara", color = "grey40", margin = margin(t = 5, b = 20)),
        plot.caption = element_markdown(margin = margin(t = 10), size = 8.5, family = "Candara", hjust = 1),

        panel.grid.major = element_line(linewidth = .25, color = "grey75", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_line(linewidth = .25, color = "grey75", linetype = "dashed", lineend = "round"),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
        
    )



# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 9.5, units = "in", dpi = 600
)

