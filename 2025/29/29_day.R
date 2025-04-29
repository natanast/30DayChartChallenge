

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
# places <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-06-20/places.csv')
# day_parts_map <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-06-20/day_parts_map.csv')


# data cleaning -----------

df = ufo_sightings[, .(reported_date_time, country_code)]

df$year <- df$reported_date_time |> str_sub(1, 4) 

df1 = df[, .N, by = .(country_code)]


df1 = df1[N > 80 & N < 7000]

# Reorder the country codes based on N (descending)
df1$country_code <- reorder(df1$country_code, df1$N)


library(paletteer)

col = paletteer_c("ggthemes::Sunset-Sunrise Diverging", 10)


# plot ------

p = ggplot(df1, aes(x = N , y = country_code, fill = country_code)) +
    
    geom_bar(stat = "identity", width = 0.5, alpha = 0.85) +
    
    coord_radial(start = 0, inner.radius = 0.2) +
    
    scale_fill_manual(values = rev(col)) +
    
    scale_x_continuous(
        limits = c(0, 3900), 
        expand = c(0, 0)
    ) +
    
    # Canada
    annotate(
        "text",
        x = 3780, y = 10.3,
        label = "Canada",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = col[1]
    ) +
    
    
    # United Kingdom
    annotate(
        "text",
        x = 3650, y = 10,
        label = "United Kingdom",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = col[2]
    ) +
    
    # Australia
    annotate(
        "text",
        x = 3740, y = 8.5,
        label = "Australia",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = col[3]
    ) +
    
    # India
    annotate(
        "text",
        x = 3780, y = 7.25,
        label = "India",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = col[4]
    ) +
    
    # Mexico
    annotate(
        "text",
        x = 3730, y = 6.35,
        label = "Mexico",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = col[5]
    ) +
    
    # New Zealand
    annotate(
        "text",
        x = 3600, y = 5.9,
        label = "New Zealand",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = col[6]
    ) +
    
    # South Africa
    annotate(
        "text",
        x = 3550, y = 4.9,
        label = "South Africa",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = col[7]
    ) +
    
    # Netherlands
    annotate(
        "text",
        x = 3520, y = 4.2,
        label = "Netherlands",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = col[8]
    ) +
    
    # Germany
    annotate(
        "text",
        x = 3530, y = 3,
        label = "Germany",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = col[9]
    ) +
    
    # Ireland
    annotate(
        "text",
        x = 3530, y = 1.8,
        label = "Ireland",
        hjust = 0,
        size = 3.5,
        lineheight = .7,
        fontface = "bold",
        color = col[10]
    ) +

    labs(
        title = "Global UFO Sightings: A Closer Look at the Frequency by Country",
        subtitle = "This visualization highlights the number of UFO sightings across various countries, <br>with a focus on countries with notable sighting frequencies. <br>
                    The <b>United States</b> leads with over <b>85,000</b> reports (not shown).",
        x = "",
        y = "",
        caption = "30DayChartChallenge 2025: <b> Day 29</b>
                   | Source: <b> UFO Sightings Redux (TidyTuesday)</b>
                   | Graphic: <b>Natasa Anastasiadou</b>",
    ) +
    

    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        
        axis.text.y = element_blank(),

        panel.grid.major.x = element_line(color = "grey78", linewidth = 0.25, linetype = "dashed", lineend = "round"),
        panel.grid.major.y = element_line(color = "grey78", linewidth = 0.25),
        
        panel.grid.minor.x = element_line(color = "grey78", linewidth = 0.25, linetype = "dashed", lineend = "round"),
        panel.grid.minor.y = element_blank(),
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 15, hjust = 0.5, color = "grey30", margin = margin(t = 5, b = 10)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 10, hjust = 1.75),
        
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey90", color = NA)
    ) 


p 

ggsave(
    plot = p, filename = "29_day.png",
    width = 10, height = 10, units = "in", dpi = 600
)    

