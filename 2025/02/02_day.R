

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


df_long <- melt(df, id.vars = c("clean_name", "album"), variable.name = "year", value.name = "rank")

df_long$year <- as.numeric(gsub("rank_", "", df_long$year))

df_long$album_label <- paste(df_long$clean_name, df_long$album, sep = " - ")

df_long$point_label <- ifelse(df_long$year == 2003, paste(df_long$clean_name, df_long$album, sep = " - "), "" )

setorder(df_long, year, rank)


col = c(
     "Metallica - Metallica"                 = "#33608CFF", 
    "Pearl Jam - Ten"                        = "#9768A5FF", 
    "Led Zeppelin - Led Zeppelin IV"         = "#E7718AFF", 
    "Pink Floyd - The Dark Side of the Moon" = "#D78D50", 
    "Carole King - Tapestry"                 = "#ED7846FF", 
    "Nirvana - Nevermind"                    = "#D54C45FF", 
    "The Beatles - Sgt. Pepper's Lonely Hearts Club Band"= "#B81840FF"
) 




# Plot -----------

gr = ggplot(df_long, aes(x = year, y = rank, color = album_label, group = album_label)) + 
    
    geom_bump(size = 0.5) +
    
    geom_point(aes(fill = album_label), size = 2.5, shape = 21, stroke = 0.25, color = "white") +
    
    geom_text(
        aes(label = point_label), 
        nudge_y = 3.5, 
        nudge_x = 2.5, 
        family = "Candara", 
        size = 3.2
    ) +
    
    scale_color_manual(values = col) +
    
    scale_fill_manual(values = col) +
    
    scale_x_continuous(breaks = c(2003, 2012, 2020)) +
    
    scale_y_reverse() +
    
    labs(
        title = "Evolution of Rolling Stone's album rankings",
        subtitle = "Tracking changes in rankings from 2003 to 2020. Only albums with over <b>200</b> weeks on Billboard are included.",
        caption = "Source: <b>Rolling Stone Dataset</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "", 
        y = "Rank"
         ) +
    
    theme_minimal() +
    
    theme(
        legend.position = "none",
        legend.title.position = "left",
        legend.title = element_text(size = 10, angle = 90, hjust = 0.5, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 8, family = "Candara", color = "grey30"),
        
        plot.background = element_rect(fill = "grey93", color = NA),
        
        axis.title.y = element_text(size = 11, family = "Candara", face = "bold"),
        axis.text.x = element_text(size = 10, family = "Candara", face = "bold"),, 
        axis.text.y = element_text(size = 10, family = "Candara", face = "bold"),
        
        panel.grid.major = element_line(linewidth = .35, color = "grey85"),
        panel.grid.minor = element_line(linewidth = .35, color = "grey85", linetype = "dashed"),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, family = "Candara", margin = margin(b = 10, t = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.4, family = "Candara", color = "grey30", margin = margin(b = 20, t = 2)),
        plot.caption = element_markdown(margin = margin(t = 20), size = 8, family = "Candara", hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20)
        )

gr

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 9.5, height = 9, units = "in", dpi = 600
)


