

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
library(ggtext)
# library(colorspace)
library(paletteer)
library(dplyr) 
library(ggbump)

# load data --------

rolling_stone <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-05-07/rolling_stone.csv')

# data cleaning ------

df <- rolling_stone[ which( !is.na(rank_2003) & !is.na(rank_2012) & !is.na(rank_2020)), ]

df <- df[ which( weeks_on_billboard > 200 ), ]

df <- df[, .(clean_name, album, rank_2003, rank_2012, rank_2020)]


# Melt the data from wide to long format
df_long <- melt(df, id.vars = c("clean_name", "album"), variable.name = "year", value.name = "rank")

# Convert year to numeric for proper ordering
df_long$year <- as.numeric(gsub("rank_", "", df_long$year))


# Combine artist name and album for the y-axis label
df_long$album_label <- paste0(df_long$clean_name, df_long$album, sep = " - ")

df_long$point_label <- ifelse(df_long$year == 2003, paste(df_long$clean_name, df_long$album, sep = " - "), "" )


# Order the data using data.table's setorder
setorder(df_long, clean_name, year)




# Plot bump chart
gr = ggplot(df_long, aes(x = year, y = rank, color = album_label, group = album_label)) + 
    
    geom_bump(size = 0.5) +
    
    geom_point(size = 2) +
    
    geom_text(
        aes(label = point_label), 
        nudge_y = 3.3, 
        nudge_x = 2.2, 
        family = "Candara", 
        size = 3
    ) +
    
    scale_x_continuous(breaks = c(2003, 2012, 2020)) +
    
    labs(
        title = "Ranking Changes of Albums Over Time",
        subtitle = "",
        caption = "Source: <b>Rolling Stone Dataset</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "", 
        y = "Rank"
         ) +
    
    theme_minimal() +
    
    theme(
        legend.position = "none",
        # legend.position = c(1.0, 0.5),
        legend.title.position = "left",
        legend.title = element_text(size = 10, angle = 90, hjust = 0.5, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 8, family = "Candara", color = "grey30"),
        
        plot.background = element_rect(fill = "grey93", color = NA),
        
        axis.text.x = element_text(size = 10, face = "bold", family = "Candara"), 
        axis.text.y = element_markdown(size = 10, family = "Candara", face = "bold"),
        
        panel.grid.major = element_line(linewidth = .35, color = "grey85"),
        panel.grid.minor = element_line(linewidth = .35, color = "grey85", linetype = "dashed"),
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, family = "Candara", margin = margin(b = 10, t = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.25, family = "Candara", color = "grey30", margin = margin(b = 20, t = 2)),
        plot.caption = element_markdown(margin = margin(t = 20), size = 8, family = "Candara", hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20)
        )

gr

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 9, units = "in", dpi = 600
)


