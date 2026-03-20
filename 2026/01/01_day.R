

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(palmerpenguins)
library(ggplot2)
library(ggtext)
library(extrafont)


# data cleaning ------

df <- as.data.table(penguins)

df <- df[!is.na(species) & !is.na(island), .(n = .N), by = .(island, species)]

df1 <- df[, percent := round((n / sum(n)) * 100), by = island]


# Expand the data so we have 1 row per percentage point (100 rows per island)
df_dots <- df[rep(1:.N, percent)]

# Assign 10x10 grid coordinates
df_dots[, `:=`(
    x = rep(1:10, length.out = .N),
    y = rep(1:10, each = 10, length.out = .N)
), by = island]


col <- c("Adelie" = "#678e9f", "Chinstrap" = "#81A88D", "Gentoo" = "#b24745")

# col = c('#5a8192', '#b24745','#a2a0cf', "#00429d", "#81A88D" )

# plot --------



gr = ggplot(df_dots, aes(x, y)) +
    
    geom_point(aes(fill = species), size = 4.5, shape = 21, color = "white", stroke = .25) +
    
    facet_wrap(~island, nrow = 1, strip.position = "bottom") +
    
    coord_equal() +
    
    scale_fill_manual(values = col) +
    
    scale_x_continuous(limits = c(0.5, 10.5)) +
    scale_y_continuous(
        limits = c(0.5, 10.5), 
        breaks = c(0.5, 5.5, 10.5), 
        labels = c("0%", "50%", "100%")
    ) +
    
    labs(
        title = "Penguin Demographics Across the Palmer Archipelago",
        subtitle = "<b>Each dot</b> represents <b>1%</b> of the total penguin population on that island.",
        caption = "30DayChartChallenge 2026: <b> Day 1</b>
                   | Source: <b> palmerpenguins (R package)</b>
                   | Graphic: <b>Natasa Anastasiadou</b>",
        
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "bottom",
        
        axis.title = element_blank(),
        # axis.text = element_blank(),
        
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 9.5),
        
        strip.text = element_text(size = 9.5),
        
        panel.grid.major = element_line(linewidth = 0.35, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
        
        plot.background = element_rect(fill = "grey95", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


gr

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 9, height = 8, units = "in", dpi = 600
)

