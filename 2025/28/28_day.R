

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

df <- global_temps[1:145, 1:13]


df_long <- melt(df, id.vars = "Year", variable.name = "Month", value.name = "Anomaly")

# make sure Anomaly is numeric
df_long[, Anomaly := as.numeric(Anomaly)]

df_long[, Anomaly_Sign := ifelse(Anomaly < 0, "Below 0", "Above 0")]



# plot -------

p = df_long |>
    ggplot(aes(x = Year, y = Anomaly)) +
    
    geom_point(
        aes(fill = Anomaly_Sign), 
        shape = 21, 
        size = 2, 
        alpha = 0.6, 
        color = "white", 
        stroke = 0.1
    ) +
    
    geom_smooth(
        method = "gam",
        formula = y ~ s(x, bs = "cs"),
        color = "#0d3d4d", 
        fill = "#0d3d4d",
        linewidth = 1.25, 
        lineend = "round"
    ) +
    
    scale_fill_manual(
        values = c("Below 0" = "#4575b4", "Above 0" = "#d73027")
    ) + 
    
    scale_x_continuous(
        breaks = c(1880, 1920, 1960, 2000, 2024)
    ) +
    
    labs(
        title = "Warming and Cooling Patterns Through Time",
        subtitle = "Monthly deviations of the Global Surface Temperature (°C) from the 1951–1980 average, 
                    <br> with <span style='color:#d73027;'><b>red</b></span> for <span style='color:#d73027;'><b>warmer</b></span> and <span style='color:#4575b4;'><b>blue</b></span> for <span style='color:#4575b4;'><b>cooler</b></span> deviations.",
        caption = "30DayChartChallenge 2025: <b> Day 27</b>
                       | Source: <b> NASA </b>
                       | Graphic: <b>Natasa Anastasiadou</b>",
        y = "Temperature Anomaly (°C)"
    ) +

    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12, vjust = 3),
        
        axis.text = element_text(size = 11),
        
        panel.grid.major = element_line(color = "grey75", linewidth = 0.25, linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.5, color = "grey30", margin = margin(t = 5, b = 15)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1.03),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey90", color = NA)
    )


p 

ggsave(
    plot = p, filename = "27_day.png",
    width = 9, height = 9, units = "in", dpi = 600
)    

