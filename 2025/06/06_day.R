

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(tidyverse)

# load data --------

life_expectancy <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-12-05/life_expectancy.csv')
life_expectancy_different_ages <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-12-05/life_expectancy_different_ages.csv')
life_expectancy_female_male <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-12-05/life_expectancy_female_male.csv')


# data cleaning ------

# Filter dataset for selected country and year
df <- life_expectancy_different_ages[Entity == "Greece" & Year > 1950, ]



# Reshape the dataframe from wide to long format
df_tidy <- melt(df, 
                id.vars = c("Entity", "Year"), 
                measure.vars = c("LifeExpectancy0", "LifeExpectancy10", "LifeExpectancy25", 
                                 "LifeExpectancy45", "LifeExpectancy65", "LifeExpectancy80"),
                variable.name = "Number",
                value.name = "LifeExpectancy")


# Filter dataset to keep only years that are the start of a decade (e.g., 1951, 1961, etc.)
df_tidy <- df_tidy[Year %% 10 == 1, ]

df_tidy$Year <- df_tidy$Year |> as.character()

# Plot -----------

ggplot(df_tidy, aes(x = Year, y = LifeExpectancy, fill = Number)) +
    
    geom_bar(
        position = "stack", 
        stat = "identity", 
        width = .8, 
        alpha = 0.9, 
        color = "grey35",
        linewidth = 0.01
    ) +
    
    # coord_polar(start = 0) 
    coord_radial(expand = FALSE)

#     geom_point(
#         aes(fill = energy, size = danceability),
#         shape = 21,
#         stroke = 0.15,
#         alpha = 0.9
#     ) +
# 
#     scale_size(
#         range = c(1.5, 8),
#         guide = guide_legend(title = "Danceability"),
#         breaks = c(0.3, 0.5, 0.8)
#     ) +
# 
#     scale_fill_stepsn(
#         colors =  c('#2c5769', '#6F99AD', 'grey96', '#D78D50', '#BC3C29'),
#         guide = guide_colorsteps(
#             title = "Energy",
#             barheight = unit(7, "lines"),
#             barwidth = unit(0.5, "lines")
# 
#         )
#     ) +
# 
#     theme_minimal() +
# 
#     labs(
#         title = " Danceability & Energy Levels in Taylor Swift’s Albums.",
#         subtitle = "Albums are arranged chronologically from left to right. <br> <br>
#                     Tracks are ranked by their order in each album, with <b>bubble size</b> representing <b>danceability</b> and <b>color</b> indicating <b>energy levels</b>.",
# 
#         caption = "Source: <b>  {taylor} R Package</b> | Graphic: <b>Natasa Anastasiadou</b>",
#         y = "Track number"
#     ) +
# 
#     theme(
# 
#         legend.position = "right",
#         legend.title.position = "left",
# 
#         legend.title = element_text(size = 8, face = "bold", family = "Candara", color = "grey30", angle = 90, hjust = .5),
#         legend.text = element_text(size = 7, family = "Candara", color = "grey30"),
# 
#         axis.title.x = element_blank(),
# 
#         axis.title.y = element_text(size = 10, family = "Candara"),
#         axis.text.x = element_text(size = 10, family = "Candara", angle = 45, hjust = 1, vjust = 1),
#         axis.text.y = element_text(size = 10, family = "Candara"),
# 
#         panel.grid.major = element_line(linewidth = .35, color = "grey80"),
#         panel.grid.minor = element_line(linewidth = .35, color = "grey80"),
# 
#         plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, family = "Candara", margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 11, hjust = 0.5, family = "Candara", color = "grey30", margin = margin(t = 5, b = 25)),
#         plot.caption = element_markdown(margin = margin(t = 20), size = 8, family = "Candara", hjust = 1.2),
# 
#         plot.margin = margin(20, 20, 20, 20),
# 
#         plot.background = element_rect(fill = "#e4e4e3", color = NA)
#     )
# 
# gr

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 9, height = 11, units = "in", dpi = 600
)
