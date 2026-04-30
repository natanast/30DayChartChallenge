

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(colorspace)
library(ggbeeswarm)

# load data ------

dt <- "IHME-GBD_2023_DATA-4506560c-1.csv" |> fread()


# clean data ----

milestone_years <- c(2000, 2010, 2020, 2023)
dt_milestones <- dt[year %in% milestone_years]

# Separate regions and countries
regions <- c("Central Europe", "Eastern Europe", "Western Europe")
dt_countries <- dt_milestones[!(location_name %in% regions)]
dt_regions   <- dt_milestones[location_name %in% regions]


bg_light   <- "grey93"
low_col    <- "#ADC2C8" 
high_col   <- "#155F83" 
region_col <- "#1a1a1c" 


# plot ------

gr <- ggplot(dt_countries, aes(x = val, y = factor(year))) +
    
    geom_quasirandom(
        aes(color = val), 
        alpha = 0.9, size = 2, groupOnX = FALSE
    ) +
    
    
    stat_summary(
        data = dt_regions,
        aes(x = val, y = factor(year)),
        fun = mean, geom = "point", 
        color = "#a33a3a" |> darken(.15), 
        fill = "#a33a3a" |> lighten(.05), 
        stroke = .65, 
        size = 4., 
        shape = 21
    ) +
    
    
    facet_wrap(~cause_name, scales = "free_x") +
    
    scale_color_gradientn(
        colors = c("#2c5769", "#6F99AD", "#b25c56", "#8a2a2a"),
        values = scales::rescale(c(0, 10, 30, 50)),
        limits = c(0, 100),
        breaks = c(0, 10, 30, 50, 70, 100)
    ) +
    
    labs(
        title = "Evolution of Global Health Estimates & Reporting Certainty",
        subtitle = "Comparing mortality rates per 100,000. Points represent individual countries colored by intensity,<br>while bars show the **uncertainty intervals** and diamonds mark the regional averages.",
        caption = "30DayChartChallenge 2026: **Day 30** | Source: **IHME GHDx** | Graphic: **Natasa Anastasiadou**",
        x = "Death Rate per 100,000",
        y = NULL
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none", 
        plot.background = element_rect(fill = "grey95", color = NA),
        
        plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.2),
        
        strip.text = element_text(size = 13, face = "bold", color = "grey20", margin = margin(b = 15)),
        axis.text.y = element_text(size = 12, face = "bold", color = high_col),
        axis.text.x = element_text(color = "grey40"),
        
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3, color = "grey85"),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(3, "lines"),
        
        plot.margin = margin(20, 20, 20, 20)
    )

gr



# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 8, units = "in", dpi = 600
)
