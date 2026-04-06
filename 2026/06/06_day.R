

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)


# load data ------

dt <- data.table(
    country = c("Czech Republic", "Montenegro", "Moldova", "Lithuania", "Poland", 
                "Russia", "Belarus", "Georgia", "Cyprus", "Kosovo"),
    score_2020 = c(76.43, 66.17, 68.84, 78.81, 71.35, 
                   51.08, 50.25, 71.41, 79.55, 70.67),
    score_2025 = c(83.96, 72.83, 73.36, 82.27, 74.79, 
                   24.57, 25.73, 50.53, 59.04, 52.73)
)

# clean data ------

dt[, change := score_2025 - score_2020]
dt[, status := fifelse(change > 0, "Improved", "Declined")]

# Order by the actual change
dt[, country := factor(country, levels = dt[order(change)]$country)]

col <- c("Declined" = "#b24745", "Improved" = "#5a8192")


# plot -----

gr <- ggplot(dt) +
    
    geom_segment(aes(x = score_2020, xend = score_2025, y = country, yend = country), color = "grey75", linewidth = 2) +
    
    geom_point(aes(x = score_2020, y = country), color = "grey60", size = 3) +
    
    geom_point(aes(x = score_2025, y = country, fill = status), shape = 21, size = 4, stroke = 0.5) +
    
    geom_text(
        aes(x = score_2025, y = country, label = round(score_2025, 1), color = status),
        fontface = "bold", size = 3.5, 
        nudge_x = fifelse(dt$change > 0, 3.5, -3.5)
    ) +
    
    scale_fill_manual(values = col) +
    scale_color_manual(values = col) +
    
    # Expanded the X-axis to comfortably fit the severe drops of Russia and Belarus
    scale_x_continuous(limits = c(15, 90), breaks = seq(20, 100, 20)) +
    
    labs(
        title = "Press Freedom in Europe",
        subtitle = "Comparing the 5 largest improvements and declines in RSF scores: <span style='color:grey50;'><b>2020</b></span> vs <span style='color:black;'><b>2025</b></span>.",
        caption = "30DayChartChallenge 2026: <b> Day 6 (Data Day: RSF)</b> | Source: <b> Reporters Without Borders </b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "", x = "RSF Press Freedom Score (100 = Perfect Freedom)"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none", 
        axis.title.x = element_text(size = 11, face = "bold", color = "grey30", margin = margin(t = 10)),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.text.x = element_text(size = 11, color = "grey30"),
        panel.grid.major.x = element_line(linewidth = 0.35, color = "grey85", linetype = "dashed"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1, lineheight = 1.2),
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

gr

# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 9, units = "in", dpi = 600
)


