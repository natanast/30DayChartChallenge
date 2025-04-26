

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


# index = which(df$fat > 0 & df$inj > 0 & df$yr > 2000)

# df = df[index]


df_inj <- df[, .(total_inj = sum(inj, na.rm = TRUE)), by = st]
df_fat <- df[ , .(total_fat = sum(fat, na.rm = TRUE)), by = st]




# map --------

map <- us_map() |> 
    select(state = abbr) |> 
    st_as_sf()


# Merge data with map
map_inj <- map |> 
    left_join(df_inj, by = c("state" = "st"))

map_fat <- map |> 
    left_join(df_fat, by = c("state" = "st"))



# map_inj = map_inj[!is.na(map_inj$yr), ]
# map_fat = map_fat[!is.na(map_fat$yr), ]





# Calculate centroids for state labels
centroids <- st_centroid(map_inj)
centroids_coords <- st_coordinates(centroids)
map_inj$centroid_x <- centroids_coords[, 1]
map_inj$centroid_y <- centroids_coords[, 2]



# Calculate centroids for state labels
centroids <- st_centroid(map_fat)
centroids_coords <- st_coordinates(centroids)
map_fat$centroid_x <- centroids_coords[, 1]
map_fat$centroid_y <- centroids_coords[, 2]





# Plotting -------


ggplot(map_inj) +
    
    geom_sf(data = map, fill = "grey95", linewidth = 0.05) +
    
    geom_sf(aes(fill = total_inj), color = "gray25", linewidth = 0.2)  +
    

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
        labels = scales::comma,
        guide = guide_colorsteps(
            title = "Injuries",
            barheight = unit(0.5, "lines"),
            barwidth = unit(10, "lines")
            
        )
    ) +
    
    labs(
        title = "Disaster Risk Across Countries: How Exposure and Coping Capacity Shape Vulnerability (2011-2021)",
        subtitle = "Exploring the relationship between countries' exposure to hazards and their coping capacity,<br> with insights into the World Risk Index (WRI).</br>",
        caption = "30DayChartChallenge 2025: <b> Day 26</b>
                       | Source: <b> Tornados (TidyTuesday) </b>
                       | Graphic: <b>Natasa Anastasiadou</b>",
    ) +
    
    theme_void(base_family = "Candara") +
    
    theme(
        
        legend.position = "bottom",
        legend.title.position = "top",
        
        legend.title = element_text(size = 9, face = "bold", color = "grey30", hjust = .5),
        legend.text = element_text(size = 7, color = "grey30"),
        
        
        plot.title = element_markdown(size = 15, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 5, b = 15)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1.03),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA)
    )


ggplot(map_fat) +

    geom_sf(data = map, fill = "grey95", linewidth = 0.05) +

    geom_sf(aes(fill = total_fat), color = "gray25", linewidth = 0.2) +
    
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
        labels = scales::comma,
        guide = guide_colorsteps(
            title = "Fatalities",
            barheight = unit(0.5, "lines"),
            barwidth = unit(10, "lines")
            
        )
    ) +
    
        labs(
            title = "Disaster Risk Across Countries: How Exposure and Coping Capacity Shape Vulnerability (2011-2021)",
            subtitle = "Exploring the relationship between countries' exposure to hazards and their coping capacity,<br> with insights into the World Risk Index (WRI).</br>",
            caption = "30DayChartChallenge 2025: <b> Day 26</b>
                       | Source: <b> Tornados (TidyTuesday) </b>
                       | Graphic: <b>Natasa Anastasiadou</b>",
        ) +
    
    theme_void() +
    
    theme(
        
        legend.position = "bottom",
        legend.title.position = "top",
        
        legend.title = element_text(size = 9, face = "bold", family = "Candara", color = "grey30", hjust = .5),
        legend.text = element_text(size = 7, family = "Candara", color = "grey30"),
        
        
        plot.title = element_markdown(size = 20, face = "bold", hjust = 0.5, family = "Candara", margin = margin(t = 25, l = 50)),
        plot.subtitle = element_markdown(size = 15, hjust = 0.5, family = "Candara", color = "grey30", margin = margin(t = 10, l = 50)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, family = "Candara", hjust = 1.03),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA)
    )




ggsave(
    plot = p, filename = "25_day.png",
    width = 10, height = 10, units = "in", dpi = 600
)    

