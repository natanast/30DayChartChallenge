#fix labs

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)


# load data ------

rm(list = ls())
gc()

# load libraries -------
library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)

# 1. Load data ------
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-05-12/volcano.csv"
dt <- fread(url)

# 2. Clean and prep ------
# FIX 1: Clean the text FIRST to merge "Stratovolcano" and "Stratovolcano(es)" into one category
dt[, type_clean := gsub("\\(es\\)|\\(s\\)", "", primary_volcano_type)]

# Now get the actual 5 most common volcano types
top_types <- dt[, .N, by = type_clean][order(-N)][1:5, type_clean]
dt_phys <- dt[type_clean %in% top_types]

# FIX 2: Change the binning from 250 to 500 meters. 
# This means fewer vertical dots, and the bubbles will represent larger groups!
dt_phys[, elevation_binned := round(elevation / 500) * 500]

# 3. Plot --------
gr <- ggplot(dt_phys, aes(x = type_clean, y = elevation_binned, color = type_clean)) +
    
    # The thin vertical stems
    geom_segment(
        aes(x = type_clean, xend = type_clean, y = min(elevation_binned), yend = max(elevation_binned)), 
        color = "grey85", linewidth = 0.5, alpha = 0.5
    ) +
    
    # geom_count scales the size based on how many volcanoes are in that 500m bin
    geom_count(alpha = 0.85) +
    
    # Control the max size of the bubbles so they don't overlap horizontally
    scale_size_area(max_size = 12, name = "Number of\nVolcanoes") +
    
    # A fiery palette (using unnamed values so it automatically maps to the new clean top 5)
    scale_color_manual(values = c("#b24745", "#db9044", "#5a8192", "#7f9faa", "#a8b5b2"), guide = "none") +
    
    labs(
        title = "THE PHYSICAL STATURE OF VOLCANOES",
        subtitle = "Distribution of global volcanoes by their physical structure and elevation.<br>Elevations are grouped into 500-meter intervals. **Larger bubbles indicate a higher concentration.**",
        caption = "30DayChartChallenge 2026: <b> Day 11 (Distributions: Physical)</b> | Source: <b> Smithsonian Institution </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "", 
        y = "Elevation (Meters above sea level)"
    ) +
    
    theme_minimal(base_family = "Candara") +
    theme(
        legend.position = "right",
        legend.title = element_text(size = 10, face = "bold", color = "grey40"),
        legend.text = element_text(size = 9, color = "grey30"),
        
        axis.text.x = element_text(size = 12, face = "bold", color = "black", margin = margin(t = 10)),
        axis.text.y = element_text(size = 10, face = "bold", color = "grey50"),
        axis.title.y = element_text(size = 11, face = "bold", color = "grey30", margin = margin(r = 15)),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.3, color = "grey90", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_text(size = 20, face = "bold", hjust = 0, margin = margin(t = 10, b = 5)),
        plot.subtitle = element_markdown(size = 11, hjust = 0, color = "grey30", lineheight = 1.2, margin = margin(b = 20)),
        plot.caption = element_markdown(margin = margin(t = 20), size = 8, hjust = 1, color = "grey40"),
        
        plot.background = element_rect(fill = "#fdfcf0", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

# 4. Save ---------


gr

# clean data -----




#  plot -----

gr

# plot --------

# 
# 
# gr = ggplot(df_dots, aes(x, y)) +
#     
#     geom_point(aes(fill = species), size = 4.5, shape = 21, color = "white", stroke = .25) +
#     
#     facet_wrap(~island, nrow = 1, strip.position = "bottom") +
#     
#     coord_equal() +
#     
#     scale_fill_manual(values = col) +
#     
#     scale_x_continuous(limits = c(0.5, 10.5)) +
#     scale_y_continuous(
#         limits = c(0.5, 10.5), 
#         breaks = c(0.5, 5.5, 10.5), 
#         labels = c("0%", "50%", "100%")
#     ) +
#     
#     labs(
#         title = "Penguin Demographics Across the Palmer Archipelago",
#         subtitle = "<b>Each dot</b> represents <b>1%</b> of the total penguin population on that island.",
#         caption = "30DayChartChallenge 2026: <b> Day 1</b>
#                    | Source: <b> palmerpenguins (R package)</b>
#                    | Graphic: <b>Natasa Anastasiadou</b>",
#         
#     ) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#         legend.position = "bottom",
#         
#         axis.title = element_blank(),
#         # axis.text = element_blank(),
#         
#         axis.text.x = element_blank(),
#         axis.text.y = element_text(size = 9.5),
#         
#         strip.text = element_text(size = 9.5),
#         
#         panel.grid.major = element_line(linewidth = 0.35, color = "grey85"),
#         panel.grid.minor = element_blank(),
#         
#         plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
#         plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
#         
#         plot.background = element_rect(fill = "grey95", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 9, height = 9, units = "in", dpi = 600
)

