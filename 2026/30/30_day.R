

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(colorspace)
library(shadowtext)
library(tidyr)
library(forcats)
library(scales)

# load data ------

dt <- "IHME-GBD_2023_DATA-4506560c-1.csv" |> fread()



# --- DAY 30: THE FINALE - TEMPORAL BEESWARM (COLOR BY VALUE) ---

library(data.table)
library(ggplot2)
library(ggtext)
library(ggbeeswarm)

# 1. DATA PREP
milestone_years <- c(2000, 2010, 2020, 2023)
dt_milestones <- dt[year %in% milestone_years]

# Separate regions and countries
regions <- c("Central Europe", "Eastern Europe", "Western Europe")
dt_countries <- dt_milestones[!(location_name %in% regions)]
dt_regions   <- dt_milestones[location_name %in% regions]

# 2. DESIGN PALETTE
bg_light   <- "grey93"
low_col    <- "#ADC2C8" # Light teal/grey
high_col   <- "#155F83" # Deep signature blue
region_col <- "#1a1a1c" # Dark charcoal for the regional anchor

# 3. PLOT
gr <- ggplot(dt_countries, aes(x = val, y = factor(year))) +
    
    # THE UNCERTAINTY: Subtle range bars for every country
    # geom_linerange(
    #     aes(xmin = lower, xmax = upper),
    #     color = "grey80", alpha = 0.4, linewidth = 0.4
    # ) +
    
    # THE POINTS: Beeswarm distribution colored by VALUE
    geom_quasirandom(
        aes(color = val), # Redundant encoding: position + color
        alpha = 0.9, size = 2, groupOnX = FALSE
    ) +
    
    # THE REGIONAL CONTEXT: Large diamond for the average
    # Using a solid dark color to stand out against the gradient
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
    
    # FACET BY CAUSE
    facet_wrap(~cause_name, scales = "free_x") +
    
    # SCALES
    # scale_color_gradient(low = "#4A6990", high = "#b25c56") +
    # 

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
        
        # Centered Header Hierarchy
        plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.2),
        
        # Facet and Axis Styling
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

#     
#     scale_fill_stepsn(
        # colors = c('#2c5769', '#6F99AD', 'grey96', '#ffb5ac', '#a33a3a'),
#         breaks = c(1, 20, 50, 70, 100),  
#         transform = pseudo_log_trans(base = 10),
#         name = "Total Participants",
#         na.value = "grey96",
#         guide = guide_colorsteps(
#             barheight = unit(10, "lines"), 
#             barwidth = unit(0.4, "lines")
#         )
#     ) +
# 
#     theme_minimal(base_family = "Candara") +
#     
#     labs(
#         title = "The Student Trade: Erasmus Mobility",
#         subtitle = "Mapping the **relationships** and student **trade** between Europe's top 15 academic destinations.",
#         caption = "30DayChartChallenge 2026: <b> Day 14</b>
#                    | Source: <b> Erasmus Data (TidyTuesday)</b>
#                    | Graphic: <b>Natasa Anastasiadou</b>",
#          x = "Sending Country",
#          y = "Receiving Country"
#     ) +
#     
#     theme(
#         legend.position = "right",
#         legend.title.position = "left",
#         
#         legend.title = element_text(size = 9, angle = 90, hjust = .5, face = "bold", family = "Candara", color = "grey30"),
#         legend.text = element_text(size = 8, color = "grey30"),
#         
#         axis.title.x = element_text(size = 10, face = "bold", margin = margin(t = 10)),
#         axis.title.y = element_text(size = 10, face = "bold", margin = margin(r = 10)),
#         
#         axis.text.x = element_text(size = 9),
#         axis.text.y = element_text(size = 9),
#         
#         panel.grid = element_blank(),
#         
        # plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        # plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        # plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.2),

#         plot.margin = margin(20, 20, 20, 20),
#         plot.background = element_rect(fill = "grey93", color = NA)
#     )  
# 
# gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 8, units = "in", dpi = 600
)
