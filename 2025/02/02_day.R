

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
library(colorspace)
library(ggtext)
library(paletteer)
library(shadowtext)

# load data --------

palmtrees <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-03-18/palmtrees.csv')


# data cleaning ------


# plot --------


# save ---------

# ggsave(
#    plot = gr, filename = "Rplot.png",
#    width = 9, height = 7, units = "in", dpi = 600
#)


