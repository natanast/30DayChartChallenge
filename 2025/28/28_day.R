

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

col = c("#4575b4","#7876B1", "#FFB900", "#d73027")


p = final_data|> 
    
    ggplot(aes(x = season, y = imdb_rating, fill = inclusion_sd)) +
    
    geom_jitter(
        shape = 21, 
        size = 4.5, 
        alpha = 0.9, 
        width = 0.1,
        stroke = 0.15,
        color = "white"
    ) +
    
    scale_fill_gradientn(
        colors = col,
        guide = guide_colorbar(
            title = "Inclusion sd",
            barheight = unit(6, "lines"),
            barwidth = unit(.75, "lines")
        )
    ) +
    
    theme_minimal(base_family = "Candara") +

    labs(
        title = "Character Inclusion vs IMDb Rating by Season",
        subtitle = "Each point is an episode; color shows how balanced the dialogue was.",
        x = "Season",
        y = "IMDb Rating",
        caption = "30DayChartChallenge 2025: <b> Day 28</b>
                   | Source: <b> FRIENDS </b>
                   | Graphic: <b>Natasa Anastasiadou</b>",
    ) +
    
    theme(
        
        panel.grid.major = element_line(color = "grey75", linewidth = 0.25, linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_line(color = "grey75", linewidth = 0.25, linetype = "dashed", lineend = "round"),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.5, color = "grey30", margin = margin(t = 5, b = 15)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1.03),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey90", color = NA)
    )


p 

ggsave(
    plot = p, filename = "28_day.png",
    width = 9, height = 9, units = "in", dpi = 600
)    

