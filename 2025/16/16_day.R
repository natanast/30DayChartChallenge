

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(palmerpenguins)
library(extrafont)
library(ggtext)
library(patchwork)



# load data --------

penguins <- as.data.table(penguins)


# data cleaning -----------


penguins_clean <- penguins[!is.na(bill_length_mm) & !is.na(bill_depth_mm) & !is.na(species)]


# col = c("#73a2c6", "#b24745")
# 
# col = c("#00429d", "#b24745")
# 
# col <- c("Below" = "#73a2c6", "Above" = "#b24745")

font = "Candara"

# plot --------

library(colorspace)

p1 <- ggplot(penguins_clean, aes(x = bill_length_mm, y = bill_depth_mm)) +
    
    geom_smooth(
        method = "lm", 
        color = "#4B7F9C", 
        fill = "#4B7F9C",
        linewidth = 0.75, 
        lineend = "round"
    ) +
    
    geom_point(
        color = "#4B7F9C", 
        fill = "#4B7F9C",
        shape = 21, 
        size = 1.5, 
        stroke = 0.5,
        alpha = 0.8
    ) +
    
    # scale_x_continuous(expand = c(0, 0)) +
    # scale_y_continuous(expand = c(0, 0)) +
    # 
    labs(
        # title = "Negative Relationship: All Penguins Combined",
        # subtitle = "Bill length vs bill depth (ignoring species)",
        x = "Bill Length (mm)",
        y = "Bill Depth (mm)"
    ) +
    
    theme_minimal(base_family = font) +
    theme(

        legend.position = "none",

        axis.line = element_line(),
        axis.ticks = element_line(),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dashed", lineend = "round", color = "grey85")
    )

p1


# Colors inspired by your palette
penguin_colors <- c(
    "Adelie" = "#FF6F00", 
    "Chinstrap" = "#FF95A8", 
    "Gentoo" = "#008EA0"
)

# penguin_fills <- lighten(penguin_colors, amount = 0.75)

p2 <- ggplot(penguins_clean, aes(x = bill_length_mm, y = bill_depth_mm)) +
    
    geom_smooth(
        aes(color = species, fill = species), 
        method = "lm", 
        linewidth = 0.75, 
        lineend = "round"
    ) +
    
    geom_point(
        aes(color = species, fill = species), 
        shape = 21, 
        size = 1.5, 
        stroke = 0.5, 
        alpha = 0.8
    ) +
    
    scale_color_manual(values = penguin_colors) +
    
    scale_fill_manual(values = penguin_colors) +
    

    labs(
        x = "Bill Length (mm)",
        y = "Bill Depth (mm)",
    ) +
    
    theme_minimal(base_family = font) +
    theme(
        # legend.position = "right",
        legend.position = c(.9, .15),
        legend.title = element_blank(),
        

        axis.line = element_line(),
        axis.ticks = element_line(),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dashed", lineend = "round", color = "grey85")
    )

p2

# Combine plots using patchwork
final_plot <- (p1 | p2) +
    plot_annotation(
        # title = "Simpson’s Paradox in Palmer Penguins 🐧",
        # subtitle = "Ignoring species, bill length and depth appear negatively correlated.\nBut within each species, the relationship is positive!",
        caption = "Source: #TidyTuesday • Data: Palmer Station LTER • Viz: Natasa Anastasiadou",
        theme = theme(
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey30"),
            plot.caption = element_text(size = 8, hjust = 1)
        )
    )

# Print the final plot
final_plot



# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 8, units = "in", dpi = 600
)





