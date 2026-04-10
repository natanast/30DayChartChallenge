#fix labs

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)


# load data ------


dt <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-05-07/rolling_stone.csv")


# clean data -----

dt_clean <- dt[!is.na(spotify_popularity) & release_year >= 1960]

dt_clean[, Decade := paste0(floor(release_year / 10) * 10, "s")]

dt_clean$Decade <- dt_clean$Decade |> factor(levels = c("1960s", "1970s", "1980s", "1990s", "2000s", "2010s"))


#  plot -----

decade_colors <- c(
    "1960s" = "#5a8192", "1970s" = "#7f9faa", "1980s" = "#a8b5b2",
    "1990s" = "#d4a373", "2000s" = "#db9044", "2010s" = "#b24745"
)



gr <- ggplot(dt_clean, aes(x = Decade, y = spotify_popularity, fill = Decade)) +
    
    geom_violin(
        trim = FALSE, alpha = 0.6, adjust = 0.8,
        color = "black", linewidth = 0.3
    ) +
    
    # Raw Data Points (The "Rain"): Placed behind the violin
    geom_jitter(
        size = 2.5, width = 0.15, shape = 21, 
        stroke = 0.2, alpha = 0.9, color = "white"
    ) +
    
    # Applying your discrete scale instead of the continuous one
    scale_fill_manual(values = decade_colors) +
    
    scale_y_continuous(
        limits = c(0, 105),
        breaks = seq(0, 100, by = 25),
        labels = c("0", "25", "50", "75", "100")
    ) +
    
    labs(
        title = "THE SURVIVAL OF THE CLASSICS",
        subtitle = "Distribution of modern **Spotify Popularity** for the Rolling Stone Top 500 Albums.<br>Each dot represents an album; the 'clouds' show where most albums in that decade sit.",
        caption = "30DayChartChallenge 2026: <b> Day 10 </b> | Source: <b> Rolling Stone (TidyTuesday) </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "", 
        y = "POPULARITY SCORE"
    ) +
    
    theme_minimal(base_family = "Candara") +
    theme(
        legend.position = "none",
        
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        axis.text.y = element_text(size = 10, color = "grey40"),
        axis.title.y = element_text(size = 10, face = "bold", color = "grey30", margin = margin(r = 10)),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
        
        plot.background = element_rect(fill = "grey95", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


gr


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 9, height = 9, units = "in", dpi = 600
)

