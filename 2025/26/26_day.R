

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(shadowtext)

library(sf)
library(usmap)


# load data --------

tornados <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-05-16/tornados.csv')

    
# data cleaning -----------

df = tornados[, .(yr, st, slat, slon, elat, elon, fat, inj)]

df_fat <- df[ , .(total_fat = sum(fat, na.rm = TRUE)), by = st]

# map --------

map <- us_map() |> 
    select(state = abbr) |> 
    st_as_sf()


# Merge data with map
map_fat <- map |> 
    left_join(df_fat, by = c("state" = "st"))


# Calculate centroids for state labels
centroids <- st_centroid(map_fat)
centroids_coords <- st_coordinates(centroids)
map_fat$centroid_x <- centroids_coords[, 1]
map_fat$centroid_y <- centroids_coords[, 2]



# Plotting -------

p = ggplot(map_fat) +
    
    geom_sf(data = map, fill = "grey95", linewidth = 0.05) +
    
    geom_sf(aes(fill = total_fat), color = "gray20", linewidth = 0.3)  +
    

    geom_shadowtext(
        aes(x = centroid_x, y = centroid_y, label = state),
        size = 3.5,
        family = "Candara",
        color = "gray10",
        bg.color = "grey93",
        bg.r = 0.06
    ) +

    scale_fill_stepsn(
        colors = c("#e2e0ff","#a09fcf", "#62628e", "#35375f"),
        breaks = c(100, 300, 500, 700),
        labels = scales::comma,
        guide = guide_colorsteps(
            title = "Fatalities",
            barheight = unit(0.5, "lines"),
            barwidth = unit(10, "lines")
            
        )
    ) +
    
    labs(
        title = "Uncertain Paths: Tornado-Related Fatalities in the U.S.",
        subtitle = "Aggregated fatalities reported from tornado events between 2000 and 2022.",
        caption = "30DayChartChallenge 2025: <b> Day 26</b>
                       | Source: <b> Tornados (TidyTuesday) </b>
                       | Graphic: <b>Natasa Anastasiadou</b>",
    ) +
    
    theme_void(base_family = "Candara") +
    
    theme(
        
        legend.position = "bottom",
        legend.title.position = "top",
        
        legend.title = element_text(size = 10, face = "bold", color = "grey30", hjust = .5),
        legend.text = element_text(size = 8, color = "grey30"),
        
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.5, color = "grey30", margin = margin(t = 5, b = 15)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1.03),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey90", color = NA)
    )

p 

ggsave(
    plot = p, filename = "26_day.png",
    width = 9, height = 9, units = "in", dpi = 600
)    

