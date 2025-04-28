

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

ufo_sightings <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-06-20/ufo_sightings.csv')
places <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-06-20/places.csv')
day_parts_map <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-06-20/day_parts_map.csv')


# data cleaning -----------

df = ufo_sightings[, .(reported_date_time, country_code)]

df$reported_date_time <- df$reported_date_time |> str_sub(1, 7)

# Count utterances per character per episode
df1 = df[, .N, by = .(reported_date_time, country_code)]


library(ggplot2)
library(data.table)
library(stringr)

# Get counts by year
df1[, year := str_sub(reported_date_time, 1, 4)]
df1[, year := as.integer(year)]

year_counts <- df1[, .N, by = year]

year_counts = year_counts[year > 1960,]

year_counts[, decade := paste0(floor(year / 10) * 10, "s")]



ggplot(year_counts, aes(x = factor(year), y = N, color = decade)) +
    geom_segment(aes(xend = factor(year), yend = 0), linewidth = 0.5) +
    geom_point(size = 2) +
    coord_radial(start = 0, inner.radius = 0.15) +  # <-- circular!
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(color = "grey80"),
          plot.margin = margin(20, 20, 20, 20),
          
          plot.background = element_rect(fill = "grey90", color = NA)
          # panel.grid = element_blank(),
          # plot.background = element_rect(fill = "black", color = NA),
          # axis.text.x = element_text(color = "white", size = 6, angle = 90, vjust = 0.5)
          ) +
    labs(title = "UFO Sightings per Year",
         subtitle = "Each lollipop represents a year and number of sightings",
         x = "", y = "")






# plot --------

# col = c('#155F83', '#B07AA1', '#FFB900', '#d0615d', '#a33a3a')

col = c('#155F83', '#9c95cd', '#FFB900', '#d0615d', '#a33a3a')


p = final_data|> 
    
    ggplot(aes(x = season, y = imdb_rating, fill = inclusion_sd)) +
    
    geom_jitter(
        shape = 21, 
        size = 4.5, 
        alpha = 0.95, 
        width = 0.1,
        stroke = 0.1,
        color = "white"
    ) +
    
    scale_fill_gradientn(
        colors = col,
        guide = guide_colorbar(
            title = "Inclusion sd",
            barheight = unit(6, "lines"),
            barwidth = unit(.55, "lines")
        )
    ) +
    
    theme_minimal(base_family = "Candara") +

    labs(
        title = "The One with Character Inclusion: IMDb Rating vs. Dialogue Balance",
        subtitle = "The chart shows how character's dialogue is distributed in each episode (Inclusion sd) and how this balance relates to IMDb ratings.",
        x = "Season",
        y = "IMDb Rating",
        caption = "30DayChartChallenge 2025: <b> Day 28</b>
                   | Source: <b> F·R·I·E·N·D·S (TidyTuesday)</b>
                   | Graphic: <b>Natasa Anastasiadou</b>",
    ) +
    
    theme(
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 12, vjust = 5),
        axis.title.x = element_text(size = 12, vjust = -2.5),
        
        legend.title.position = "left",
        legend.title = element_text(size = 10, angle = 90, hjust = .5, face = "bold", color = "gray20"),
        legend.text = element_text(size = 9, color = "gray20"),
        
        panel.grid.major = element_line(color = "grey75", linewidth = 0.25, linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_line(color = "grey75", linewidth = 0.25, linetype = "dashed", lineend = "round"),
        
        plot.title = element_markdown(size = 15, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.35, color = "grey30", margin = margin(t = 5, b = 15)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1.3),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey90", color = NA)
    )


p 

ggsave(
    plot = p, filename = "28_day.png",
    width = 10, height = 10, units = "in", dpi = 600
)    

