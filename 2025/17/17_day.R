


rm(list = ls())
gc()


# load libraries -----------

library(data.table)
# library(dplyr)
library(circlize)
library(stringr)
# library(ComplexHeatmap)
library(showtext)

# data cleaning ------------

# https://www.kaggle.com/datasets/anoopjohny/birdsoftheworld-unprocessed?resource=download

birds <- fread("Birdsoftheworld.csv", header = TRUE)

top10 <- birds[, .N, by = .(species)]

top10 <- top10[order(-N)]
    
top10 <- top10[1:10]


df2 = birds[which( species %in% top10$species )] 

df2 = df2[, .(species, location)]

df2 = df2[, .N, by = .(species, location)]



# df_plot --------------

t1 <- dcast(df2, species ~ location, value.var = "N", fill = 0) |>
    as.data.frame()


rownames(t1) <- t1$species
t1 <- t1[, -1]



df_plot <- as.matrix(t1)

# Reorder rows and columns alphabetically by sector names
df_plot <- df_plot[order(rev(rownames(df_plot))), order(colnames(df_plot))]




# colors ------------------


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


# Add gray30 color for locations
locations <- setdiff(colnames(df_plot), names(grid_colors))
for (loc in locations) {
    grid_colors[loc] <- "gray30"
}

# plot --------------

png("Rplot.png", width = 3000, height = 3000, res = 200) 



# Automatically use showtext in all plots
showtext_auto()

# Add Candara (make sure it’s installed on your system)
font_add("candara", regular = "Candara.ttf")

# Use the font
par(family = "candara", bg = "grey94")



circos.par(
    start.degree = 270,
    canvas.xlim = c(-1.1, 1.1),
    canvas.ylim = c(-1.1, 1.1)
    
)

# chord diagram
chordDiagram(
    df_plot, 
    grid.col = grid_colors, 
    annotationTrack = c("grid", "names"),
    annotationTrackHeight = c(0.01, 0.001),
    preAllocateTracks = list(track.height = 0.15)
)


# labs
title("Bird Connections Around the World",
      cex.main = 5.5,
      font.main = 1,
      line = -1.5)

mtext("Exploring the top 10 most frequently observed bird species and their distribution across locations.",
      side = 3, line = -4, cex = 3.5)

mtext("Source: Bird Sightings Dataset | Graphic: Natasa Anastasiadou",
      side = 3, line = -73, cex = 2.5, adj = 1)


# Customize labels
circos.track(
    track.index = 1, panel.fun = function(x, y) {
        circos.text(
            CELL_META$xcenter,
            CELL_META$ylim[1],
            CELL_META$sector.index,
            facing = "clockwise",
            niceFacing = TRUE,
            adj = c(0, 0.5),
            cex = 2.5
        )
    }, bg.border = NA
)


dev.off()


circos.clear()

