

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(treemapify)

# library(stringr)
library(extrafont)
# library(colorspace)
library(ggtext)
library(paletteer)
# library(shadowtext)

# load data --------

languages <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-03-21/languages.csv')


# data cleaning ------

df = languages[, .(pldb_id, number_of_users)]

df = df[number_of_users > 20000, ]

label = paste0(df$pldb_id, "\n", df$number_of_users)

# Label
df[, label := paste0(pldb_id, "\n", scales::comma(number_of_users))]


col = c('#60608b', '#9291be', '#b9b8e5', '#fcc1ad', '#e7877d', '#c15451')


# plot --------

gr = ggplot(df, aes(area = number_of_users, fill = number_of_users, label = label)) +
    
    
    geom_treemap(layout = "squarified", color = "#f8f2f9", start = "topleft", radius = unit(2, "pt")) +
    
    geom_treemap_text(
        colour = "white",
        # size = 15,
        layout = "squarified", 
        start = "topleft", 
        grow = FALSE, 
        reflow = TRUE,
        min.size = .5,
        family = "Candara") +
    
    # scale_fill_manual(values = col) +
    scale_fill_stepsn(
        colors = rev(col), 
        breaks = c(50000, 100000, 500000, 1000000, 5000000), 
        transform = "log2",
        labels = c("50K", "100K", "500K", "1M", "5M"),
        guide = guide_colorsteps(barwidth = unit(0.5, "lines"), 
                                 barheight = unit(10, "lines"))
    ) +
    
    labs(
        title = "The Most Used Programming Languages",
        subtitle = "Estimated number of users for programming languages based on PLDB data.",
        caption = "Source: <b> Programming Language DataBase </b> | Graphic: <b>Natasa Anastasiadou</b>",
        fill = "Number of users",
    ) +
    
    theme_void() + 
    
    theme(
        # legend.position = "bottom",
        legend.title.position = "left",
        legend.title = element_text(size = 10, angle = 90, hjust = 0.5, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 8, family = "Candara", color = "grey30"),
        
        plot.background = element_rect(fill = "grey93", color = NA),
        
        plot.title = element_markdown(size = 19, face = "bold", hjust = 0.5, family = "Candara", margin = margin(b = 10, t = 5)),
        plot.subtitle = element_markdown(size = 15, hjust = 0.25, family = "Candara", color = "grey30", margin = margin(b = 20, t = 2)),
        plot.caption = element_markdown(margin = margin(t = 20), size = 7, family = "Candara", hjust = 1.2),
        
        plot.margin = margin(20, 20, 20, 20)
    )


gr

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 9, height = 10, units = "in", dpi = 600
)


