


rm(list = ls())
gc()


# load libraries -----------

library(data.table)
library(dplyr)
library(circlize)
library(stringr)
library(ComplexHeatmap)



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



# plot --------------

png("Rplot.png", width = 3000, height = 3000, res = 200) 

par(bg = "grey94")


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
      cex.main = 3,
      font.main = 1,
      line = -2)


mtext("Exploring the top 10 most frequently observed bird species and their global distribution across.",
      side = 3, line = -4, cex = 1.5)

mtext("Source: National Park Service | Graphic: Natasa Anastasiadou",
      side = 3, line = -73, cex = 1, adj = 1)


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
            cex = 1.2
        )
    }, bg.border = NA
)



# # legend
# lgd = Legend(
#     at = unique(df1$Legend), 
#     type = "points", 
#     legend_gp = gpar(col = grid_colors, cex = 1.5),  
#     title_gp = gpar(fontsize = 15, fontface = "bold"),
#     labels_gp = gpar(fontsize = 12),  
#     title = "National Park name",
#     nrow = 5
# )

# draw(
#     lgd, 
#     x = unit(4, "mm"), 
#     y = unit(4, "mm"), 
#     just = c("left", "bottom")
# )


dev.off()


circos.clear()

