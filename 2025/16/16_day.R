

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

# library(colorspace)

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
        size = 2, 
        stroke = 0.15,
        alpha = 0.8
    ) +
    
 
    labs(
        # title = "Negative Relationship: All Penguins Combined",
        # subtitle = "Bill length vs bill depth (ignoring species)",
        x = "",
        y = "Bill Depth (mm)"
    ) +
    
    theme_minimal(base_family = font) +
    
    theme(

        legend.position = "none",

        axis.line = element_line(),
        axis.ticks = element_line(),
        
        axis.title.x = element_blank(),
        
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dashed", lineend = "round", color = "grey75")
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
        size = 2, 
        stroke = 0.15, 
        color = "white",
        alpha = 0.8
    ) +
    
    scale_color_manual(values = penguin_colors) +
    
    scale_fill_manual(values = penguin_colors) +
    

    labs(
        x = "",
        y = "",
    ) +
    
    theme_minimal(base_family = font) +
    
    theme(
        
        legend.position = c(.85, .15),
        legend.key.size = unit(0.4, "cm"),
        legend.title = element_blank(),
        legend.text = element_text(size = 5),
        
        axis.title.x = element_blank(),

        axis.line = element_line(),
        axis.ticks = element_line(),
        
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 7),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = "dashed", lineend = "round", color = "grey75")
    )

p2



x_axis_label <- ggplot() +
    theme_void(base_family = font) +
    annotate("text", x = 0.5, y = 0.5, label = "Bill Length (mm)", family = font, size = 2.5, hjust = 0.5, vjust = -0.85)


# Combine plots 

final_plot <- ((p1 | p2) / x_axis_label) +
    plot_layout(heights = c(1, 0.05)) +
    plot_annotation(
        title = "Simpson’s Paradox in Palmer Penguins",
        subtitle = "Ignoring species, bill length and depth appear negatively correlated.\nBut within each species, the relationship is positive.",
        caption = "Source: <b> {palmerpenguins} R package</b> | Graphic: <b>Natasa Anastasiadou</b>",
    ) &
    
    theme(
        plot.title = element_text(face = "bold", size = 14, family = font, hjust = 0.5),
        plot.subtitle = element_text(size = 10, hjust = 0.5, family = font, color = "grey30"),
        plot.caption  = element_markdown(margin = margin(t = -10), family = font, size = 6, hjust = 1.05),
        plot.margin = margin(15, 15, 15, 15),
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
        
    )


final_plot


# save ---------

ggsave(
   plot = final_plot, filename = "Rplot.png",
   width = 10, height = 7, units = "in", dpi = 600
)





