

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

friends <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-09-08/friends.csv')
friends_emotions <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-09-08/friends_info.csv')


# data cleaning -----------

main_characters = c("Rachel Green", "Ross Geller", "Joey Tribbiani",
                    "Monica Geller", "Chandler Bing", "Phoebe Buffay")

friends_main = friends[speaker %in% main_characters]


# Count utterances per character per episode
dialogue_counts = friends_main[, .N, by = .(season, episode, speaker)]


# Total utterances per episode
dialogue_counts[, total := sum(N), by = .(season, episode)]

# Percent of dialogue
dialogue_counts[, percent := N / total]


# Calculate standard deviation of percent per episode
inclusion = dialogue_counts[, .(inclusion_sd = sd(percent)), by = .(season, episode)]


final_data = merge(inclusion, friends_info, by = c("season", "episode"))

final_data = final_data[, .(season, episode, inclusion_sd, us_views_millions, imdb_rating)]

final_data$season <- final_data$season |> as.character() |> factor(levels = 1:10)



# plot --------

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
        caption = "30DayChartChallenge 2025: Day 28 | Source: F·R·I·E·N·D·S (TidyTuesday) | Graphic: Natasa Anastasiadou",
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
        
        plot.title = element_text(size = 15, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_text(size = 12, hjust = 0.35, color = "grey30", margin = margin(t = 5, b = 15)),
        plot.caption = element_text(margin = margin(t = 35), size = 8, hjust = 1.2),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey90", color = NA)
    )


p 


# logo -------

library(cowplot)
library(grid)

img <- png::readPNG("Friends-logo.png")
logo <- grid::rasterGrob(img, interpolate = TRUE)

# Combine your plot and the image
final_plot_with_logo <- ggdraw(p) +
    draw_grob(logo, x = .99, y = 1.05, width = 0.18, height = 0.18, hjust = 1, vjust = 1)


final_plot_with_logo

# save ------

ggsave(
    plot = final_plot_with_logo, filename = "28_day.png",
    width = 10, height = 10, units = "in", dpi = 600
)    

