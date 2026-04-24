

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(stringr)
library(colorspace)


# Load data -------

dt_ufo <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-06-20/ufo_sightings.csv')
places <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-06-20/places.csv')
day_parts_map <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-06-20/day_parts_map.csv')

rm(list = ls())
gc()

# load libraries -------
library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)


# Clean data ------
# 1. Remove NAs and group the shapes
dt_ufo <- dt_ufo[!is.na(shape)]

dt_ufo[, certainty := fcase(
    shape %in% c("light", "flash", "unknown", "other", "changing", "star", "fireball", "orb"), "Uncertain Phenomena",
    shape %in% c("disk", "triangle", "cigar", "cylinder", "sphere", "circle", "oval", "rectangle", "chevron", "cube", "diamond", "egg", "teardrop", "cone", "cross"), "Geometric / Concrete",
    default = "Other"
)]

# 2. Count the occurrences of each shape and sort them
dt_viz <- dt_ufo[certainty != "Other", .(count = .N), by = .(shape, certainty)]
setorder(dt_viz, count)

# 3. Factor the shape column so ggplot orders it correctly from biggest to smallest
dt_viz[, shape := factor(shape, levels = dt_viz$shape)]

# Let's just take the Top 15 most common shapes to keep the chart clean
dt_viz <- tail(dt_viz, 15)




# clean data ------

setnames(dt_raw, make.names(names(dt_raw)))


dt_china <- dt_raw[Country == "CN"]


dt_china[, length_km := as.numeric(str_replace_all(Length, ",", ""))]
dt_china[, start_yr := as.numeric(str_extract(Start.year, "\\d{4}"))]



dt_viz <- dt_china[!is.na(start_yr) & start_yr >= 2000 & start_yr <= 2024, 
                   .(km_started = sum(length_km, na.rm = TRUE)), 
                   by = .(year = start_yr)]


setorder(dt_viz, year)

dt_viz[, total_network := cumsum(km_started)]



# plot -------

my_col <- "#4a6b7c"


gr <- ggplot(dt_viz, aes(x = year, y = total_network)) +
    
  
    geom_step(color = my_col, linewidth = .85, direction = "hv") +
    
    geom_point(
        shape = 21, 
        fill = my_col |> lighten(.15),
        color = my_col |> darken(.15), 
        size = 3.5
    ) +
    
        annotate(
        "curve", x = 2006, y = 2500, xend = 2008, yend = 1200,
        curvature = -0.2, arrow = arrow(length = unit(0.2, "cm")), color = "grey40"
    ) +
    
    annotate(
        "text", x = 2000, y = 3000, 
        label = "2008 Beijing Olympics\nsparks urban rail boom",
        family = "Candara", fontface = "italic", size = 3.5, color = "grey20", hjust = 0
    ) +
    
    
    annotate(
        "text", x = 2024, y = 11500, 
        label = "10,938 km",
        family = "Candara", fontface = "bold", size = 5, color = scmp_red, hjust = 1
    ) +
    
    
    scale_x_continuous(breaks = seq(2000, 2024, 4)) +
    
    
    scale_y_continuous(
        labels = scales::comma, 
        position = "right", 
        expand = expansion(mult = c(0, 0.15)) 
    ) +
    
    labs(
        title = "China's Urban Rail Staircase",
        subtitle = "The cumulative length of urban rail projects started each year. <br> Construction accelerated rapidly following the 2008 Beijing Olympics",
        caption = "30DayChartChallenge 2026: <b> Day 24 </b> | Source: <b> Transit Costs Project </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "",
        y = "Total Kilometers"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.3, linetype = "dashed"),
        
        
        axis.text = element_text(size = 10, color = "grey40", face = "bold"),
        axis.text.y.right = element_text(margin = margin(r = 10)),
        
        axis.title.y = element_text(size = 14, color = "grey40"),
        
        # Titles
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 15, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1, lineheight = 1.2),
        
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 9, units = "in", dpi = 600
)

