

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(treemapify)

# library(stringr)
# library(extrafont)
# library(colorspace)
# library(ggtext)
# library(paletteer)
# library(shadowtext)

# load data --------

languages <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-03-21/languages.csv')


# data cleaning ------

df = languages[, .(pldb_id, number_of_users)]

df = df[number_of_users > 10000, ]

label = paste0(df$pldb_id, "\n", df$number_of_users)

# Label
df[, label := paste0(pldb_id, "\n", scales::comma(number_of_users))]

# plot --------

ggplot(df, aes(area = number_of_users, fill = pldb_id, label = label)) +
    geom_treemap(layout = "squarified", color = "#f8f2f9", start = "topleft", radius = unit(2, "pt")) +
    
    geom_treemap_text(
        colour = "white",
        # size = 15,
        layout = "squarified", 
        start = "topleft", 
        grow = FALSE, 
        reflow = TRUE,
        min.size = .5) +
    
    theme(
        legend.position = "none"
    )






# save ---------

# ggsave(
#    plot = gr, filename = "Rplot.png",
#    width = 9, height = 7, units = "in", dpi = 600
#)


