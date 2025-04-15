

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(packcircles)
library(tidyverse)
library(shadowtext)


# load data --------

team_results <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-03-26/team-results.csv')
public_picks <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-03-26/public-picks.csv')


# data cleaning -----------

top_10_teams <- team_results[order(-PASE)][1:10]

df <- top_10_teams[, .(TEAM, PASE, CHAMPPERCENT)]

df$CHAMPPERCENT <- df$CHAMPPERCENT |> str_remove_all("%") |> as.numeric()




# 🟣 CHAMPPERCENT = what people thought would happen (perception).
# 
# 🟢 PASE = what actually happened (performance).


# plot -----------

# Process the data 
df_plot <- df[, {
    
    dat.egg <- circleProgressiveLayout(CHAMPPERCENT)
    dat.egg <- circleLayoutVertices(dat.egg, npoints = 100)
    
    cbind(dat.egg, .SD[dat.egg$id])
}]


df_plot_l <- df_plot |>
    group_by(TEAM, CHAMPPERCENT, PASE, id) |>
    summarise(
        x = (min(x) + max(x)) / 2,
        y = (min(y) + max(y)) / 2
    )


df_plot_l$lbl = df_plot_l$PASE



col = c('#396375', '#5a8192', '#7f9faa', '#a7bec0', '#d2ded1', '#febaad', '#f49992', '#e37b78', '#cc5f5e', '#b24745')


# plot ---------



p <- df_plot |>
    
    ggplot(aes(x, y, group = id)) +
    
    geom_polygon(aes(fill = TEAM), color = "grey30", linewidth = .25) +
    
    geom_shadowtext(
        data = df_plot_l, 
        aes(x, y, label = lbl, size = CHAMPPERCENT),
        inherit.aes = FALSE,
        color = "grey1", 
        bg.color = "#d9e3f1", 
        bg.r = .05,
        family = "Candara"
    ) +
    
    
    scale_fill_manual(
        values = col,
        name = "Team"
        
    ) +

    scale_size_continuous(guide = "none", range = c(5, 9)) +
    
    
    # coord_equal() +
    
    labs(
        title = "Expectations vs Performance: Top NCAA Men's March Madness Teams in 2024",
        subtitle = "<b> Bubble size</b> represents each team's <b>championship likelihood</b> — bigger size indicated bigger likelihood. <br> <b>Label</b> shows their Performance Against Seed Expectations <b>(PASE)</b> — larger PASE indicates better performance.",
        caption = "Source: <b> NCAA Men's March Madness</b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        axis.title = element_blank(),
        axis.text = element_blank(),
        
        legend.position = "right",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 7),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 5, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5,  color = "grey30"),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, hjust = 1.25),
        
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
        
    )

p


ggsave(
    plot = p, filename = "Rplot.png",
    width = 10, height = 7, units = "in", dpi = 600
)    



