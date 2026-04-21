

rm(list = ls())
gc()


# load libraries -----

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(ggstream)


# load data -------

dt <- "olympics_dataset.csv" |> fread()


# clean data -----

dt_gold <- dt[Season == "Summer" & Medal == "Gold"]

top_gold_nocs <- dt_gold[, .N, by = NOC][order(-N)][1:10, NOC]

dt_top10 <- dt_gold[NOC %in% top_gold_nocs]

dt_count <- dt_top10[, .(gold_count = .N), by = .(Year, NOC)]


olympic_years <- unique(dt[Season == "Summer", Year])
grid <- CJ(Year = olympic_years, NOC = unique(dt_count$NOC))
dt_plot <- dt_count[grid, on = .(Year, NOC)]
dt_plot[is.na(gold_count), gold_count := 0]

dt_plot[, NOC := factor(NOC, levels = top_gold_nocs)]


# plot --------

cols <- c(
    "#b25c56",  
    "#d97a73",  
    "#e8998f",  
    "#fcd4be",  
    "#e3d2b8",  
    "#aebfbc",  
    "#8aa39b",  
    "#85aebc",  
    "#628b9c",  
    "#4a6b7c"   
)

names(cols) <- levels(dt_plot$NOC)


gr <- ggplot(dt_plot, aes(x = Year, y = gold_count, fill = NOC)) +
    
    geom_stream(
        bw = 0.85, 
        extra_span = 0.1,
        color = "#f2f2f2",
        linewidth = 0.3,
        alpha = 0.9
    ) +
    
    geom_stream(
        bw = 0.85, 
        type = "mirror", 
        extra_span = 0.1, 
        true_range = "none",
        alpha = 0.3,
        color = NA 
    ) +
    
    scale_fill_manual(values = cols, name = "Countries") +
    
    labs(
        title = "The Golden Eras of the Summer Games",
        subtitle = "Tracking the Top 10 all-time gold-winning nations.",
        caption = "30DayChartChallenge 2026: <b> Day 21 </b> | Source: <b> Kaggle </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Year",
        y = "Athlete Gold Medals Awarded"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "grey80", linetype = "dashed", linewidth = .25),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#f2f2f2", color = NA),
        panel.background = element_rect(fill = "#f2f2f2", color = NA),
        
        axis.text.x = element_text(size = 10, color = "grey40", face = "bold"),
        axis.text.y = element_blank(), 
        axis.title.x = element_text(size = 12, face = "bold", color = "grey30", margin = margin(t = 15)),
        axis.title.y = element_text(size = 12, face = "bold", color = "grey30", margin = margin(r = 15)),
        
        legend.position = "right",
        legend.title = element_text(face = "bold", size = 11),
        legend.text = element_text(size = 10),
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20)
    )


gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)
