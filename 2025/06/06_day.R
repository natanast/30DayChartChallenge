

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
                measure.vars = c("LifeExpectancy0", "LifeExpectancy45", "LifeExpectancy80"),
                variable.name = "Number",
                value.name = "LifeExpectancy")


df_tidy$Number <- factor(df_tidy$Number, 
                         levels = c("LifeExpectancy80", "LifeExpectancy45", "LifeExpectancy0"))


# Filter dataset to keep only years that are the start of a decade (e.g., 1951, 1961, etc.)
df_tidy <- df_tidy[Year %% 10 == 1, ]

df_tidy$Year <- df_tidy$Year |> as.character()


custom_colors <- c("LifeExpectancy0" = "#A3C9F1",  # Light Blue
                   "LifeExpectancy45" = "#F4A300",  # Golden Yellow
                   "LifeExpectancy80" = "#E1A7D3")  # Light Purple

# Plot -----------

gr = ggplot(df_tidy, aes(x = Year, y = LifeExpectancy, fill = Number)) +
    
    geom_bar(
        position = "stack", 
        stat = "identity", 
        width = 1, 
        alpha = 0.9, 
        color = "black",
        linewidth = 0.25
    ) +
    
    coord_polar(start = 0) +
    
    scale_fill_manual(values = custom_colors) +
    

    theme_minimal() +

    labs(
        title = "Life Expectancy in Greece by Age Group",
        subtitle = "Data for Decade Start Years (e.g., 1951, 1961, etc.)",
        caption = "Source: <b>  {taylor} R Package</b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "Track number"
    ) +

    theme(

        legend.position = "right",
        legend.title.position = "left",

        legend.title = element_text(size = 8, face = "bold", family = "Candara", color = "grey30", angle = 90, hjust = .5),
        legend.text = element_text(size = 7, family = "Candara", color = "grey30"),

        axis.title.x = element_blank(),

        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 10, family = "Candara", hjust = 1, vjust = 1),
        axis.text.y = element_blank(),

        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(linewidth = .35, color = "grey80"),

        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, family = "Candara", margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 11, hjust = 0.5, family = "Candara", color = "grey30", margin = margin(t = 5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 5), size = 8, family = "Candara", hjust = 1.25),

        plot.margin = margin(6, 6, 6, 6),

        plot.background = element_rect(fill = "#e4e4e3", color = NA)
    )

gr

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 9, height = 9, units = "in", dpi = 600
)

