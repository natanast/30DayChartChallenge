

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------
#2023-07-11

global_temps <- fread("GLB.Ts+dSST.csv")


# data cleaning -----------

df <- global_temps[1:145, 1:13]

df <- df[Year >= 1990, ]


rownames(df) <- df$Year |> as.character()

df$Year <- NULL

# plot --------


gr = ggplot(df, aes(x = height, y = sport, fill = sport)) +
    
    geom_density_ridges_gradient(scale = 2, rel_min_height = 0.01, gradient_lwd = 0.05, lwd = 0.05) +
    
    scale_fill_manual(values = col_alpha) +
    
    
    labs(
        title = "Distribution of Athlete Heights Across Popular Olympic Sports",
        subtitle = "Separate distributions for female and male athletes reveal body type diversity",
        caption = "Source: <b> Olympic Games Data</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Height (cm)",
        y = "Sport"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        legend.position = "none", 
        
        plot.title = element_markdown(size = 17, face = "bold", color = "grey20", hjust = 0.5, family = "Candara", margin = margin(t = 2, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, family = "Candara", color = "grey40", margin = margin(t = 5, b = 20)),
        plot.caption = element_markdown(margin = margin(t = 10), size = 8.5, family = "Candara", hjust = 1),

        panel.grid.major = element_line(linewidth = .25, color = "grey75", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_line(linewidth = .25, color = "grey75", linetype = "dashed", lineend = "round"),

        
        axis.title.y = element_blank(),
        axis.text = element_text(size = 11),

        plot.margin = margin(20, 20, 20, 20),

        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        
        panel.spacing = unit(1, "cm"),
        
        strip.text = element_text(size = 12, face = "bold", color = "grey30", hjust = 0.5, margin = margin(b = 10))
        
    ) +
    
    facet_wrap(~sex) 
    

gr

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 10, units = "in", dpi = 600
)

