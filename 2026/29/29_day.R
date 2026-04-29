

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(friends)
library(ggdist)
library(colorspace)
library(ggtext)


# load data ------

dt_raw <- friends_info 


# clean data ------

dt_raw |> setDT()


dt_raw[, season := factor(season)]



# plot -----

friends_blue <- "#2c5769"

# bg_light <- "grey93"


# bg_light   <- "grey93"
dot_light  <- "#bdbdbd" 
line_dark  <- "#2b2b2b" 


gr <- ggplot(dt_raw, aes(x = season, y = imdb_rating)) +
    

    stat_halfeye(
        fill = friends_blue |> lighten(0.6),
        color = NA,
        width = .6, 
        .width = 0, 
        justification = -.3,
        alpha = 0.8
    ) +
    
    geom_jitter(
        # data = dt_raw,
        # aes(color = imdb_rating, fill = imdb_rating),
        color = "white", 
        fill = friends_blue,
        width = .1, 
        alpha = 0.65, 
        size = 2.5,
        shape = 21
    ) +
    
    stat_summary(
        fun = median, 
        geom = "line", 
        aes(group = 1), 
        color = friends_blue |> darken(0.10), 
        linewidth = .5
    ) +
    
    stat_summary(
        fun = median, 
        geom = "point", 
        color = friends_blue |> darken(0.20),
        fill = friends_blue |> darken(0.15), 
        size = 4, 
        shape = 21
    ) +
    
    scale_y_continuous(limits = c(6.5, 10), breaks = seq(7, 10, 0.5)) +
    
    labs(
        title = "IMDb User Ratings and Distribution for Friends",
        subtitle = "A monochrome analysis of viewer consensus across ten seasons. The density shapes highlight the<br>**uncertainty** and variance in episode quality compared to the steady seasonal medians.",
        caption = "30DayChartChallenge 2026: <b> Day 29 </b>
                   | Source: <b> Friends {R package}</b>
                   | Graphic:<b> Natasa Anastasiadou</b>",
        x = "Season",
        y = "IMDb Rating"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        plot.background = element_rect(fill = "grey95", color = NA),
        
        
        plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.4, color = "#e0e0e0"),
        panel.grid.minor = element_blank(),
        
        axis.text = element_text(color = "grey40", size = 11, face = "bold"),
        axis.title = element_text(color = "grey30", size = 10, face = "bold"),
        
        plot.margin = margin(20, 20, 20, 20)
    )

gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 8, units = "in", dpi = 600
)

