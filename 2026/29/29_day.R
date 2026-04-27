

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(colorspace)
library(shadowtext)
library(tidyr)
library(forcats)
library(scales)

# load data ------

erasmus_raw <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-03-08/erasmus.csv')


# clean data ------

df_heatmap <- erasmus_raw[, .(N = sum(participants, na.rm = TRUE)), 
                          by = .(sending_country_code, receiving_country_code)]


top_list <- df_heatmap[, .(total = sum(N)), by = sending_country_code][order(-total)][1:15, sending_country_code]
df_heatmap <- df_heatmap[sending_country_code %in% top_list & receiving_country_code %in% top_list]


df_heatmap <- df_heatmap |>
    complete(sending_country_code, receiving_country_code, fill = list(N = 0)) |>
    as.data.table()


df_heatmap[sending_country_code == receiving_country_code, N := NA]


df_heatmap[, sending_country_code := fct_reorder(sending_country_code, N, function(x) sum(x, na.rm = TRUE))]
df_heatmap[, receiving_country_code := fct_reorder(receiving_country_code, N, function(x) sum(x, na.rm = TRUE))]


# plot --------

gr = ggplot(df_heatmap, aes(x = sending_country_code, y = receiving_country_code, fill = N)) +
    
    geom_tile(color = "grey20", linewidth = .25) +
    
    geom_shadowtext(
        aes(label = N), 
        color = "black",
        family = "Candara",
        bg.color = "grey95", 
        bg.r = .1, 
        size = 3.5 
    ) +
    
    scale_fill_stepsn(
        colors = c('#2c5769', '#6F99AD', 'grey96', '#ffb5ac', '#a33a3a'),
        breaks = c(1, 20, 50, 70, 100),  
        transform = pseudo_log_trans(base = 10),
        name = "Total Participants",
        na.value = "grey96",
        guide = guide_colorsteps(
            barheight = unit(10, "lines"), 
            barwidth = unit(0.4, "lines")
        )
    ) +

    theme_minimal(base_family = "Candara") +
    
    labs(
        title = "The Student Trade: Erasmus Mobility",
        subtitle = "Mapping the **relationships** and student **trade** between Europe's top 15 academic destinations.",
        caption = "30DayChartChallenge 2026: <b> Day 14</b>
                   | Source: <b> Erasmus Data (TidyTuesday)</b>
                   | Graphic: <b>Natasa Anastasiadou</b>",
         x = "Sending Country",
         y = "Receiving Country"
    ) +
    
    theme(
        legend.position = "right",
        legend.title.position = "left",
        
        legend.title = element_text(size = 9, angle = 90, hjust = .5, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 8, color = "grey30"),
        
        axis.title.x = element_text(size = 10, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 10, face = "bold", margin = margin(r = 10)),
        
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        
        panel.grid = element_blank(),
        
        plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.2),
        
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "grey93", color = NA)
    )  

gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 8, units = "in", dpi = 600
)


# --- DAY 29: MONOCHROME & UNCERTAINTIES (FRIENDS) ---

rm(list = ls())
gc()

library(data.table)
library(ggplot2)
library(ggtext)

# 1. LOAD DATA
url <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv'
dt_raw <- fread(url)

# 2. CLEAN DATA
# Ensure season is a factor for the X-axis
dt_raw[, season := factor(season)]

# 3. PLOT (Clean, Centered, Monochrome)
bg_light   <- "grey93"
dot_light  <- "#bdbdbd" # Light grey for uncertainty cloud
line_dark  <- "#2b2b2b" # Charcoal for the trend


col = c('#155F83', '#9c95cd', '#FFB900', '#d0615d', '#a33a3a')



gr <- ggplot(dt_raw, aes(x = season, y = imdb_rating)) +
    
    # The Uncertainty: The "cloud" of individual episode ratings
    geom_jitter(
        color = "#d0615d", 
        alpha = 0.6, 
        width = 0.2, 
        size = 2, 
        shape = 16
    ) +
    
    # The Central Trend: Median rating per season
    stat_summary(
        fun = median, 
        geom = "line", 
        aes(group = 1), 
        color = "#a33a3a", 
        linewidth = 1
    ) +
    
    # Highlighting the Median points
    stat_summary(
        fun = median, 
        geom = "point", 
        color = "#a33a3a", 
        fill = "#a33a3a",
        size = 3.5, 
        shape = 21
    ) +
    
    scale_y_continuous(limits = c(6.5, 10), breaks = seq(7, 10, 0.5)) +
    
    labs(
        title = "IMDB User Ratings for All Episodes of Friends",
        subtitle = "A monochrome distribution of episode quality across ten seasons. The vertical spread of grey dots <br>represents the **uncertainty** in viewer reception, peaking during the series finale in Season 10.",
        caption = "30DayChartChallenge 2026: **Day 29 (Monochrome)** | Source: **TidyTuesday** | Graphic: **Natasa Anastasiadou**",
        x = "Season",
        y = "IMDB Rating"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        plot.background = element_rect(fill = bg_light, color = NA),
        panel.background = element_rect(fill = bg_light, color = NA),
        
        # Centered Title and Subtitle
        plot.title = element_markdown(size = 20, face = "bold", color = "#1a1a1c", hjust = 0.5, margin = margin(t = 20)),
        plot.subtitle = element_markdown(size = 11, color = "grey30", lineheight = 1.3, hjust = 0.5, margin = margin(t = 10, b = 30)),
        plot.caption = element_markdown(size = 8, color = "grey50", hjust = 0.5, margin = margin(t = 30)),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.4, color = "#e0e0e0"),
        panel.grid.minor = element_blank(),
        
        axis.text = element_text(color = "grey40", size = 10, face = "bold"),
        axis.title = element_text(color = "grey30", size = 10, face = "bold"),
        
        plot.margin = margin(20, 50, 20, 50)
    )

gr







# 2. DEFINE YOUR MONOCHROME HERO COLOR (From your previous chart)
friends_blue <- "#2c5769"

# 3. PLOT
bg_light <- "grey93"

gr <- ggplot(dt_raw, aes(x = season, y = imdb_rating)) +
    
    # THE UNCERTAINTY: Half-eye density shapes
    # Using a light tint of your blue
    stat_halfeye(
        fill = friends_blue |> lighten(0.6),
        color = NA,
        width = .6, 
        .width = 0, 
        justification = -.3,
        alpha = 0.8
    ) +
    
    # THE RAW DATA: Jittered points
    # Using a sequential gradient of your blue
    geom_jitter(
        aes(color = imdb_rating),
        width = .1, 
        alpha = 0.6, 
        size = 2,
        shape = 16
    ) +
    
    # THE CENTRAL TREND: Median line and points
    # Using a dark shade of your blue
    stat_summary(
        fun = median, geom = "line", aes(group = 1), 
        color = friends_blue |> darken(0.2), linewidth = 1
    ) +
    
    stat_summary(
        fun = median, geom = "point", 
        color = friends_blue |> darken(0.2), 
        fill = friends_blue |> darken(0.25), 
        size = 3, shape = 21
    ) +
    
    # Sequential scale: Light Blue to Deep Blue
    scale_color_gradient(
        low = friends_blue |> lighten(0.4), 
        high = friends_blue |> darken(0.3)
    ) +
    
    scale_y_continuous(limits = c(6.5, 10), breaks = seq(7, 10, 0.5)) +
    
    labs(
        title = "IMDb User Ratings and Distribution for Friends",
        subtitle = "A monochrome analysis of viewer consensus across ten seasons. The density shapes highlight the<br>**uncertainty** and variance in episode quality compared to the steady seasonal medians.",
        caption = "30DayChartChallenge 2026: **Day 29 (Monochrome)** | Source: **TidyTuesday** | Graphic: **Natasa Anastasiadou**",
        x = "Season",
        y = "IMDb Rating"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        plot.background = element_rect(fill = bg_light, color = NA),
        panel.background = element_rect(fill = bg_light, color = NA),
        
        # Centered Cédric Scherer-style layout
        plot.title = element_markdown(size = 22, face = "bold", color = "#1a1a1c", hjust = 0.5, margin = margin(t = 20)),
        plot.subtitle = element_markdown(size = 11, color = "grey30", lineheight = 1.3, hjust = 0.5, margin = margin(t = 10, b = 35)),
        plot.caption = element_markdown(size = 8, color = "grey50", hjust = 0.5, margin = margin(t = 40)),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.4, color = "#e0e0e0"),
        panel.grid.minor = element_blank(),
        
        axis.text = element_text(color = "grey40", size = 11, face = "bold"),
        axis.title = element_text(color = "grey30", size = 10, face = "bold"),
        
        plot.margin = margin(20, 20, 20, 20)
    )

gr
