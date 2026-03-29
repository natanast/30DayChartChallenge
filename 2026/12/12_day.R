#fix labs and size of the x axis letter

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(stringr)


# load data ------

dt <- 
    fread()


# clean data -----


#  plot -----
rm(list = ls())
gc()

# load libraries -------
library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)

# 1. Generate "FlowingData" style ATUS data ------
# We simulate the hours of the day (0 to 24) when people consume these drinks
set.seed(2026)
n <- 8000

dt_coffee <- data.table(
    drink = "Coffee",
    # Big peak at 8 AM, smaller bump at 2 PM
    time = c(rnorm(n * 0.75, mean = 8, sd = 1.5), rnorm(n * 0.25, mean = 14, sd = 2))
)

dt_tea <- data.table(
    drink = "Tea",
    # Gentle, rolling distribution throughout the afternoon
    time = rnorm(n * 0.6, mean = 15, sd = 3.5)
)

dt_beer <- data.table(
    drink = "Beer & Wine",
    # Sharp peak at 8 PM (20:00)
    time = rnorm(n * 0.8, mean = 20, sd = 1.8)
)

# Combine and filter to a clean 24-hour window
dt <- rbind(dt_coffee, dt_tea, dt_beer)
dt <- dt[time >= 4 & time <= 24] # Start at 4 AM for a cleaner x-axis

# 2. Plot --------
# FlowingData signature colors
col <- c("Coffee" = "#c28d75", "Tea" = "#718b95", "Beer & Wine" = "#b96a59")

gr <- ggplot(dt, aes(x = time, fill = drink, color = drink)) +
    
    # 1. The Distributions: Smooth density curves, slightly transparent
    geom_density(alpha = 0.6, linewidth = 0.8) +
    
    # 2. Direct Labeling (The Nathan Yau trick: No Legends!)
    annotate("text", x = 8, y = 0.16, label = "Coffee", color = "#9c6b54", 
             family = "Candara", fontface = "bold", size = 5.5) +
    annotate("text", x = 14.5, y = 0.08, label = "Tea", color = "#506973", 
             family = "Candara", fontface = "bold", size = 5.5) +
    annotate("text", x = 20, y = 0.17, label = "Beer & Wine", color = "#964b3a", 
             family = "Candara", fontface = "bold", size = 5.5) +
    
    scale_fill_manual(values = col) +
    scale_color_manual(values = col) +
    
    # Format the X-axis as a daily clock
    scale_x_continuous(
        breaks = c(4, 8, 12, 16, 20, 24),
        labels = c("4am", "8am", "Noon", "4pm", "8pm", "Midnight"),
        expand = c(0, 0) # Forces the graph to touch the edges perfectly
    ) +
    
    # Remove the space at the bottom so the curves sit perfectly on the axis line
    scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
    
    labs(
        title = "When we drink our favorite beverages",
        subtitle = "Simulated distribution of consumption times over a 24-hour period, based on time-use patterns.<br>Coffee dominates the morning, tea is for the afternoon, and alcohol peaks after dinner.",
        caption = "30DayChartChallenge 2026: <b> Day 12 (FlowingData Theme)</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "", 
        y = "" # Completely remove the Y axis title!
    ) +
    
    theme_minimal(base_family = "Candara") +
    theme(
        legend.position = "none", # Legend is explicitly destroyed
        
        # Clean, minimal axis styling
        axis.text.x = element_text(size = 11, color = "grey40", margin = margin(t = 5)),
        axis.text.y = element_blank(), # Destroy the Y axis numbers!
        
        # Only keep the bottom horizontal baseline
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.line.x = element_line(color = "grey60", linewidth = 0.5),
        
        # Typography matching the FlowingData conversational tone
        plot.title = element_text(size = 18, face = "bold", hjust = 0, color = "black", margin = margin(b = 8)),
        plot.subtitle = element_markdown(size = 11, hjust = 0, color = "grey30", lineheight = 1.3, margin = margin(b = 20)),
        plot.caption = element_markdown(margin = margin(t = 30), size = 8, hjust = 1, color = "grey50"),
        
        plot.background = element_rect(fill = "#fcfbf9", color = NA), # Very slight warm off-white
        plot.margin = margin(30, 30, 30, 30)
    )

gr

# 3. Save ---------
ggsave(
    "Day12_FlowingData_Distributions.png", 
    plot = gr, 
    width = 9, 
    height = 6, 
    dpi = 600
)


# 
# 
# gr = ggplot(df_dots, aes(x, y)) +
#     
#     geom_point(aes(fill = species), size = 4.5, shape = 21, color = "white", stroke = .25) +
#     
#     facet_wrap(~island, nrow = 1, strip.position = "bottom") +
#     
#     coord_equal() +
#     
#     scale_fill_manual(values = col) +
#     
#     scale_x_continuous(limits = c(0.5, 10.5)) +
#     scale_y_continuous(
#         limits = c(0.5, 10.5), 
#         breaks = c(0.5, 5.5, 10.5), 
#         labels = c("0%", "50%", "100%")
#     ) +
#     
#     labs(
#         title = "Penguin Demographics Across the Palmer Archipelago",
#         subtitle = "<b>Each dot</b> represents <b>1%</b> of the total penguin population on that island.",
#         caption = "30DayChartChallenge 2026: <b> Day 1</b>
#                    | Source: <b> palmerpenguins (R package)</b>
#                    | Graphic: <b>Natasa Anastasiadou</b>",
#         
#     ) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#         legend.position = "bottom",
#         
#         axis.title = element_blank(),
#         # axis.text = element_blank(),
#         
#         axis.text.x = element_blank(),
#         axis.text.y = element_text(size = 9.5),
#         
#         strip.text = element_text(size = 9.5),
#         
        # panel.grid.major = element_line(linewidth = 0.35, color = "grey85"),
        # panel.grid.minor = element_blank(),
        # 
        # plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        # plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        # plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),

#         plot.background = element_rect(fill = "grey95", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 9, height = 9, units = "in", dpi = 600
)
