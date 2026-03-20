

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)


# load data ------

dt <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-11-19/episode_metrics.csv")


# clean data -----

df_counts <- dt[, .(total_words = sum(unique_words, na.rm = TRUE)), by = season]


df_counts[, icons_needed := round(total_words / 2000)]


# Create one row per burger 
df_picto <- df_counts[rep(1:.N, icons_needed)]

# Assign an "x" coordinate
df_picto[, x := 1:.N, by = season]

# Format the y-axis labels
df_picto[, season_label := paste("Season", season)]
df_picto[, season_label := factor(season_label, levels = paste("Season", 14:1))] 



# plot --------

gr <- ggplot(df_picto, aes(x = x, y = season_label)) +
    
    
    geom_text(label = "🍔", size = 8, family = "Segoe UI Emoji") +
    
    scale_x_continuous(limits = c(0, 13), breaks = seq(0, 12, by = 2)) +
    
    labs(
        title = "Bob's Burgers: The Short & Long Seasons",
        subtitle = "Total unique words spoken per season. Season 2 was cut to just 9 episodes. <br><b>Each 🍔 represents 2,000 words.</b>",
        caption = "30DayChartChallenge 2026: <b> Day 2</b>
                   | Source: <b> bobsburgers (TidyTuesday | Nov 2024)</b>
                   | Graphic: <b>Natasa Anastasiadou</b>",
        
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        axis.title = element_blank(),
        
        axis.text.x = element_text(size = 10, color = "grey30"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black", margin = margin(r = 10)),
        
        panel.grid.major = element_line(linewidth = 0.35, color = "grey85"),
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
    width = 9, height = 9, units = "in", dpi = 600,
    device = ragg::agg_png # Ensures beautiful full-color emojis
)
