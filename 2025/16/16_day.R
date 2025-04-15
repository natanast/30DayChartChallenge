

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


font = "Candara"

# p1 --------

library(colorspace)

p1 <- ggplot(penguins_clean, aes(x = bill_length_mm, y = bill_depth_mm)) +
    
    geom_smooth(
        method = "lm", 
        color = "#396375", 
        fill = "#396375",
        linewidth = 0.75, 
        lineend = "round"
    ) +
    
    geom_point(
        color = "white", 
        fill = "#396375",
        shape = 21, 
        size = 2.5, 
        stroke = 0.25,
        alpha = 0.8
    ) +
    
 
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

# p2 ---------

colors = c('#00429d', '#73a2c6', '#ffffe0', '#f4777f', '#93003a')

# Colors inspired by your palette
penguin_colors <- c(
    "Adelie" = "#b24745", 
    "Chinstrap" = "#73a2c6", 
    "Gentoo" = "#00429d"
)


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
        size = 2.5, 
        stroke = 0.25, 
        color = "white",
        alpha = 0.8
    ) +
    
    scale_color_manual(values = penguin_colors) +
    
    scale_fill_manual(values = penguin_colors) +
    

    labs(
        x = "Bill Length (mm)",
        y = "",
    ) +
    
    theme_minimal(base_family = font) +
    theme(
        # legend.position = "right",
        legend.position = c(.85, .15),
        legend.title = element_blank(),
        

        axis.line = element_line(),
        axis.ticks = element_line(),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dashed", lineend = "round", color = "grey85")
    )

p2


# Combine plots 

final_plot <- (p1 | p2) +
    
    plot_annotation(
        title = "Simpson’s Paradox in Palmer Penguins",
        subtitle = "Ignoring species, bill length and depth appear negatively correlated.\nBut within each species, the relationship is positive.",
        caption = "Source: <b> {palmerpenguins} R package</b> | Graphic: <b>Natasa Anastasiadou</b>",
    ) &
    
    theme(
        plot.title = element_text(face = "bold", size = 16, family = font, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, family = font, color = "grey30"),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, hjust = 1),
        
    )


final_plot


# save ---------

ggsave(
   plot = final_plot, filename = "Rplot.png",
   width = 10, height = 8, units = "in", dpi = 600
)





