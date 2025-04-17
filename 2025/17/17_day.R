


rm(list = ls())
gc()


# load libraries -----------

library(data.table)
library(dplyr)
library(circlize)
library(stringr)
library(ComplexHeatmap)



# data cleaning ------------
# tuesdata <- tidytuesdayR::tt_load('2024-10-08')
# 
# most_visited_nps_species_data <- tuesdata$most_visited_nps_species_data

birds <- fread("Birdsoftheworld.csv", header = TRUE)

top10 <- birds[, .N, by = .(species)]

top10 <- top10[order(-N)]
    
top10 <- top10[1:10]


df2 = birds[which( species %in% top10$species )] 

df2 = df2[, .(species, location)]

df2 = df2[, .N, by = .(species, location)]


# df <- most_visited_nps_species_data[, c("ParkName","ParkCode", "CategoryName")] |> 
#     as.data.table()
# 
# df1 <- df[, .(count = .N), by = .(ParkName, ParkCode, CategoryName)] 
# 
# df1$Legend <- paste0(df1$ParkCode, " - ", str_remove_all(df1$ParkName, "National|Park"))


# df_plot --------------

t1 <- dcast(df2, species ~ location, value.var = "N", fill = 0) |>
    as.data.frame()


rownames(t1) <- t1$species
t1 <- t1[, -1]


# t <- dcast(df1, ParkCode ~ CategoryName, value.var = "count", fill = 0) |>
#     as.data.frame()
# 
# rownames(t) <- t$ParkCode  
# t <- t[, -1]


df_plot <- as.matrix(t1)

# # Define new order 
# new_order <- c("ZION", "YOSE", "YELL", "ROMO", "OLYM", "JOTR", "INDU", "HOSP", 
#                "GRSM", "GRTE", "GRCA", "GLAC", "CUVA", "BRCA", "ACAD")


# df_plot <- df_plot[new_order, ]


# colors ------------------

grid_colors <- c(
    "ACAD" = "#FDAE61",
    "BRCA" = "#5E4FA2",
    "CUVA" = "#66C2A5",
    "GLAC" = "#BC3C29",
    "GRCA" = "#0072B5",
    "GRTE" = "#91D1C2",   
    "GRSM" = "#984EA3",
    "HOSP" = "#b24745",
    "INDU" = "#E18727",
    "JOTR" = "#ffdac4",
    "OLYM" = "#B09C85",
    "ROMO" = "#fb8a8c",
    "YELL" = "#A65628",
    "YOSE" = "#FF7F00",
    "ZION" = "#20854E"
)


grid_colors = c('#396375', '#5a8192', '#7f9faa', '#a7bec0', '#d2ded1', '#febaad', '#f49992', '#e37b78', '#cc5f5e', '#b24745')

c(IGLJ3 = "#D53E4F",IGLJ2 = "#5E4FA2", IGLJ1 ="#66C2A5")

col = c("#00429d", "#73a2c6", "#ffffe0", "#ff9a92", "#b24745")



grid_colors = c('#7f9faa', '#b24745', "#5E4FA2", "#66C2A5", "#73a2c6", "#A65628", "#FDAE61", "#a7bec0", '#396375', '#f49992')


grid_colors = c("#00429d", "#73a2c6", "#ff9a92", "#b24745", "#396375", "#7f9faa", "#A65628", "#FDAE61", '#6f6e9a', '#a2a0cf')


# Get all unique sectors (species + locations)
sectors <- unique(c(df2$species, df2$location))

# Assign colors to each sector
# Recycle the color palette if there are more sectors than colors
grid_colors <- setNames(rep(grid_colors, length.out = length(sectors)), sectors)

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
title("National Park Species",
      cex.main = 3,  
      font.main = 1,
      line = -2)


mtext("Species distribution across national parks",
      side = 3, line = -4, cex = 1.5)

mtext("Source: National Park Service | Graphic: Natasa Anastasiadou",
      side = 3, line = -73, cex = 1, adj = 1)


# # Customize labels
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
