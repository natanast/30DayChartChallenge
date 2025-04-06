

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(paletteer)


# load data --------

office_ratings <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-03-17/office_ratings.csv')


office_ratings$season <- office_ratings$season |> as.character()

# Plot 1 -----------

colors =  c('#2c5769', '#6F99AD', 'grey96', '#ffb5ac', '#a33a3a')
colors = c('#2c5769', '#507a8d', '#76a0b2', '#9cc6d8', '#b2dded', '#d08078', '#c26962', '#b3524e', '#a33a3a')

colors = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 9)

# Season-wise IMDb ratings
gr = office_ratings |>
    
    ggplot(aes(x = factor(season), y = imdb_rating)) +
    
    geom_boxplot(
        aes(fill = season),
        position = position_dodge(width = .5),
        width = .35, outlier.shape = NA, alpha = 0.45, linewidth = 0.2
    ) +
    
    geom_point(
        aes(fill = season),
        position = position_jitterdodge(jitter.width = .25, dodge.width = .5),
        shape = 21, size = 3, stroke = .15, color = "white"
    ) +
    
    scale_fill_manual(values = colors) +

    scale_color_manual(values = colors) +
    
    theme_minimal() +
    
    labs(
        title = "IMDb Ratings by Season for 'The Office",
        subtitle = "Proportion of individuals with each condition (binary 0/1)",
        caption = "Source: <b>  Long Beach Animal Shelter Data</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Season",
        y = "IMDb Rating"
         ) +


        theme(

            legend.position = "none",
            legend.title.position = "left",

            legend.title = element_blank(),

            # axis.title.x = element_blank(),

            # axis.title.y = element_blank(),
            # axis.text.x = element_blank(),
            # axis.text.y = element_blank(),

            panel.grid.major = element_line(linewidth = .35, color = "grey80", linetype = "dashed"),
            panel.grid.minor = element_line(linewidth = .35, color = "grey80", linetype = "dashed"),

            plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, family = "Candara", margin = margin(t = 15, b = 5)),
            plot.subtitle = element_markdown(size = 11, hjust = 0.5, family = "Candara", color = "grey30", margin = margin(t = 5, b = 25)),
            plot.caption = element_markdown(margin = margin(t = 5), size = 8, family = "Candara", hjust = 1),

            plot.margin = margin(20, 20, 20, 20),

            plot.background = element_rect(fill = "#e4e4e3", color = NA)
        )




gr

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 8, units = "in", dpi = 600
)

