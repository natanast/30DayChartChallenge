

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
# library(colorspace)
library(paletteer)
library(dplyr) 

# load data --------

rolling_stone <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-05-07/rolling_stone.csv')

# data cleaning ------

df <- rolling_stone[ which( !is.na(rank_2003) & !is.na(rank_2012) & !is.na(rank_2020)), ]

df <- df[ which( release_year > 1980 & weeks_on_billboard > 100 ), ]

df <- df[, .(clean_name, album, rank_2003, rank_2012, rank_2020)]


# Melt the data from wide to long format
df_long <- melt(df, id.vars = c("clean_name", "album"), variable.name = "year", value.name = "rank")

# Convert year to numeric for proper ordering
df_long$year <- as.numeric(gsub("rank_", "", df_long$year))

# Combine artist name and album for the y-axis label
df_long$album_label <- paste(df_long$clean_name, df_long$album, sep = " - ")

# Order the data using data.table's setorder
setorder(df_long, clean_name, year)



# Plot bump chart
ggplot(df_long, aes(x = year, y = album_label, color = clean_name, group = clean_name)) +
    geom_bump(size = 2) +  # Bump lines
    geom_point(size = 3) +  # Points at each year
    scale_y_discrete() +  # Use discrete y-axis to display artist and album names
    labs(
        title = "Album Ranking Changes Over Time",
        x = "Year",
        y = "Artist - Album",
        caption = "Source: Rolling Stone Top 500 Albums"
    ) +
    theme_minimal() +
    theme(legend.position = "none") 


# save ---------

# ggsave(
#    plot = gr, filename = "Rplot.png",
#    width = 9, height = 7, units = "in", dpi = 600
#)


