

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

news <- fread('guardian_articles.csv')


# data cleaning -----------

df <- news[, .(sectionName, webPublicationDate)]


df$webPublicationDate <-  df$webPublicationDate |> str_extract("^\\d{4}")

# Remove rows where sectionName is NA or empty
df <- df[!is.na(sectionName) & sectionName != ""]

# Remove 2022
df <- df[webPublicationDate != "2022"]


count <- df[, .N, by = .(webPublicationDate, sectionName)]


# For each year, keep the top 5 sections based on the count
top_5_sections <- count[, head(.SD[order(-N)]), by = webPublicationDate]

top_5_sections <- top_5_sections[, .SD[1:5], by = webPublicationDate]


# plot -----------


col = c("#00429d", "#73a2c6", '#396375', '#5a8192', '#6f6e9a', '#a2a0cf', '#e37b78',"#A65628", '#b24745')


grid_colors = c(
    "American Crow"             = '#6f6e9a',
    "American Goldfinch"        = "#A65628",
    "American Robin"            = "#b24745",
    "Belted Kingfisher"         = "#00429d",
    "Blue Jay"                  = "#396375",
    "Downy Woodpecker"          = '#a2a0cf',
    "Green Heron"               = "#FDAE61",
    "Northern Flicker"          = "#e37b78",
    "Ruby-throated Hummingbird" = "#73a2c6", 
    "Scarlet Tanager"           = "#7f9faa"
)



# plot ---------

# Create the stacked bar plot
p = ggplot(top_5_sections, aes(x = webPublicationDate, y = N, fill = sectionName)) +
    
    geom_bar(stat = "identity", width = 0.5) +
    
    scale_fill_manual(values = col) +
    
    labs(
        title = "Media & Time: Tracking Section Popularity by Year",
        subtitle = "Top 5 Guardian news sections published each year from 2016 to 2021.",
        caption = "30DayChartChallenge: <b> Day 18</b> | Source: <b> Guardian News Articles (Kaggle) </b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "No. of Article", 
        fill = "Type"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        axis.title.x = element_blank(),
        
        axis.title.y = element_text(size = 9),
        
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        
        legend.text = element_text(size = 7),         # Smaller text
        legend.title = element_text(size = 8),        # Optional: smaller title
        legend.key.size = unit(0.8, "lines"),         # Smaller boxes
        legend.spacing.y = unit(0.5, "lines"),
        
        legend.position = "right",
        # legend.title = element_text(size = 8),
        # legend.text = element_text(size = 7),
        
        panel.grid.major = element_line(color = "grey75", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 14, face = "bold", hjust = 0.5, margin = margin(t = 2, b = 2)),
        plot.subtitle = element_markdown(size = 10, hjust = 0.5,  color = "grey30", margin = margin(t = 5, b = 10)),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 6, hjust = 1.38),
        
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
        
    )



p


ggsave(
    plot = p, filename = "Rplot.png",
    width = 8, height = 7, units = "in", dpi = 600
)    



