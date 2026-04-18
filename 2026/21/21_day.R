
rm(list = ls())
gc()


# load libraries -----

library(ggplot2)
library(stringr)
library(data.table)
library(ggtext)
library(extrafont)
library(colorspace)


# Load data -------
rm(list = ls())
gc()

# 1. Load Libraries -------
library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)

# 2. Load Data -------
languages <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-03-21/languages.csv')

# 3. Clean and Prep -------
# Filtering for 'programming' using the correct column name found in your head()
dt_hist <- languages[github_language_type == "programming" & appeared >= 1950 & appeared <= 2022]

# Aggregate by year
dt_counts <- dt_hist[, .(n = .N), by = .(Year = appeared)][order(Year)]

# Identify "Milestone" languages for historical annotations
milestones <- dt_hist[title %in% c("Fortran", "C", "Python", "Java", "Rust", "Go")]
# Get the count for those specific years to place the label correctly
milestones <- merge(milestones, dt_counts, by.x = "appeared", by.y = "Year")

# 4. Plot -------
gr <- ggplot(dt_counts, aes(x = Year, y = n)) +
    
    # A Step Area gives it a more "structured/historical" look than a smooth curve
    geom_step(color = "#2a3d45", linewidth = 0.8) +
    geom_area(stat = "step", fill = "#2a3d45", alpha = 0.15) +
    
    # Add points for the historical milestones
    geom_point(data = milestones, aes(x = appeared, y = n), color = "#b24745", size = 2) +
    
    # Add labels for the milestones
    geom_text(data = milestones, aes(x = appeared, y = n, label = title), 
              vjust = -1, family = "Candara", size = 3, fontface = "bold", color = "#b24745") +
    
    scale_x_continuous(breaks = seq(1950, 2020, by = 10)) +
    
    labs(
        title = "The Historical Expansion of Programming",
        subtitle = "Annual count of new programming languages appearing in the digital ecosystem",
        caption = "30DayChartChallenge 2026: <b>Day 21 (Historical)</b> | Source: <b>PLDB (TidyTuesday)</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Year of Appearance",
        y = "Number of New Languages"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        plot.title = element_markdown(size = 18, face = "bold", color = "grey20", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40", margin = margin(b = 25)),
        plot.caption = element_markdown(size = 8, color = "grey40", margin = margin(t = 20)),
        
        # Vintage styling: remove vertical lines, keep soft horizontal ones
        panel.grid.major.y = element_line(linewidth = .3, color = "grey85", linetype = "dotted"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        
        axis.title = element_text(size = 10, face = "bold", color = "grey30"),
        # Warm background for a "historical paper" feel
        plot.background = element_rect(fill = "#f9f7f2", color = NA),
        plot.margin = margin(25, 25, 25, 25)
    )

gr

# 5. Save -------
ggsave("21_historical_pldb.png", plot = gr, width = 11, height = 7, dpi = 600)


# clean data -----



# plot -------
# 
# cols <- c("Above 0" = "#b24745", "Below 0" = "#00429d")
# 
# 
# gr <- ggplot(dt_plot, aes(x = Year, y = Anomaly, color = Temp_Type, fill = Temp_Type)) +
#     
#     geom_hline(yintercept = 0, color = "grey40", linewidth = 0.7) +
#     
#     geom_segment(
#         aes(x = Year, xend = Year, y = 0, yend = Anomaly), 
#         linewidth = 0.65, 
#         alpha = 0.85
#     ) +
#     
#     geom_point(
#         size = 3.5, 
#         shape = 21,
#         stroke = 0.25
#     ) +
#     
#     scale_color_manual(
#         values = cols |> darken(.15), 
#         guide = "none"
#     ) +
#     
#     scale_fill_manual(
#         values = cols |> lighten(.15), 
#         guide = "none"
#     ) +
#     
#     scale_x_continuous(breaks = seq(1880, 2020, by = 20)) +
#     
#     labs(
#         title = "The Escalation of Global Temperatures",
#         subtitle = "Annual global temperature anomalies (°C) compared to the 1951-1980 average",
#         caption = "30DayChartChallenge 2024: <b>Day 20</b> | Source: <b>NASA GISS (Kaggle)</b> | Graphic: <b>Natasa Anastasiadou</b>",
#         x = "",
#         y = "Temperature Anomaly (°C)"
#     ) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#        
#         legend.position = "none",
#         
#         plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 16, hjust = 0.5, color = "grey30", margin = margin(t = 5, b = 15)),
#         plot.caption = element_markdown(margin = margin(t = 35), size = 10, hjust = 1),
#         
#         panel.grid.major.y = element_line(linewidth = .25, color = "grey70", linetype = "dashed"),
#         panel.grid.minor.y = element_blank(),
#         
#         panel.grid.major.x = element_line(linewidth = .25, color = "grey70", linetype = "dashed"), 
#         panel.grid.minor.x = element_blank(),
#         
#         axis.title.y = element_text(size = 13, vjust = 5, face = "bold", color = "grey30"),
#         axis.text = element_text(size = 11, color = "grey40"),
#         
#         plot.background = element_rect(fill = "grey95", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )
# 
# gr

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 9, units = "in", dpi = 600
)

