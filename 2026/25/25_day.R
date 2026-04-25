

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(stringr)
library(colorspace)


# Load data -------

dt_ufo <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-06-20/ufo_sightings.csv')

# clean data --------

dt_ufo <- dt_ufo[!is.na(shape)]

dt_ufo[, certainty := fcase(
    shape %in% c("light", "flash", "unknown", "other", "changing", "star", "fireball", "orb"), "Uncertain Phenomena",
    shape %in% c("disk", "triangle", "cigar", "cylinder", "sphere", "circle", "oval", "rectangle", "chevron", "cube", "diamond", "egg", "teardrop", "cone", "cross"), "Geometric / Concrete",
    default = "Other"
)]

# Aggregate and Sort
dt_viz <- dt_ufo[certainty != "Other", .(count = .N), by = .(shape, certainty)]
setorder(dt_viz, -count) # Sorted by size for a spiral look

# --- NEW: Label Data Calculation for Radial Plot ---
dt_viz[, id := .I]
total_n <- nrow(dt_viz)
# Calculate angles for the labels (text should point outward)
dt_viz[, angle := 90 - 360 * (id - 0.5) / total_n]
dt_viz[, hjust := fcase(angle < -90, 1, default = 0)]
dt_viz[, angle := fcase(angle < -90, angle + 180, default = angle)]

# plot -------

# Space Palette
bg_color      <- "#0b0c10" 
text_col      <- "#c5c6c7" 
uncertain_col <- "#66fcf1" 
concrete_col  <- "#45a29e" 

gr <- ggplot(dt_viz, aes(x = factor(shape, levels = dt_viz$shape), y = count, fill = certainty)) +
    
    # Radial Bars
    geom_bar(width = 0.7, stat = "identity", alpha = 0.9) +
    
    # The Circular Engine
    coord_radial(
        start = 0,
        inner.radius = 0.1, # Creates the "hole" in the middle
        expand = FALSE
    ) +
    
    # Adding the labels manually using our calculated angles
    geom_text(
        aes(label = shape, x = id, y = count + 2000, angle = angle, hjust = hjust),
        color = text_col, family = "Candara", size = 2.8, fontface = "bold", alpha = 0.8
    ) +
    
    labs(
        title = "THE TAXONOMY OF THE UNKNOWN",
        subtitle = "A census of 80,000+ UFO sightings. The night sky is dominated by <span style='color:#66fcf1'><b>Uncertain Phenomena</b></span><br>(lights and flashes) rather than defined <span style='color:#45a29e'><b>Geometric Objects</b></span>.",
        caption = "30DayChartChallenge 2026: <b> Day 25 (Uncertainties) </b> | Source: <b> NUFORC </b> | Graphic: <b>Natasa Anastasiadou</b>",
        fill = ""
    ) +
    
    scale_fill_manual(values = c("Uncertain Phenomena" = uncertain_col, "Geometric / Concrete" = concrete_col)) +
    
    scale_y_continuous(limits = c(-5000, max(dt_viz$count) + 10000)) + # Adjusting limits to move bars away from center
    
    theme_minimal() +
    
    theme(
        # Center the Legend
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.text = element_text(size = 9, family = "Candara", color = text_col),
        
        # Dark Background
        plot.background = element_rect(fill = "grey40", color = NA),
        
        # Typography
        plot.title = element_markdown(size = 20, face = "bold", hjust = 0.5, family = "Candara", color = "white", margin = margin(t = 10)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5, family = "Candara", color = text_col, lineheight = 1.2, margin = margin(t = 10, b = 10)),
        plot.caption = element_markdown(margin = margin(t = 20), size = 8, family = "Candara", color = "grey40", hjust = 1),
        
        # Cleanup
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),  
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
    )

gr


# Save the plot
ggsave(
    plot = gr, filename = "Day25_Space_UFOs.png",
    width = 9, height = 7, units = "in", dpi = 600
)


# plot -------

my_col <- "#4a6b7c"


gr <- ggplot(dt_viz, aes(x = year, y = total_network)) +
    
  
    geom_step(color = my_col, linewidth = .85, direction = "hv") +
    
    geom_point(
        shape = 21, 
        fill = my_col |> lighten(.15),
        color = my_col |> darken(.15), 
        size = 3.5
    ) +
    
        annotate(
        "curve", x = 2006, y = 2500, xend = 2008, yend = 1200,
        curvature = -0.2, arrow = arrow(length = unit(0.2, "cm")), color = "grey40"
    ) +
    
    annotate(
        "text", x = 2000, y = 3000, 
        label = "2008 Beijing Olympics\nsparks urban rail boom",
        family = "Candara", fontface = "italic", size = 3.5, color = "grey20", hjust = 0
    ) +
    
    
    annotate(
        "text", x = 2024, y = 11500, 
        label = "10,938 km",
        family = "Candara", fontface = "bold", size = 5, color = scmp_red, hjust = 1
    ) +
    
    
    scale_x_continuous(breaks = seq(2000, 2024, 4)) +
    
    
    scale_y_continuous(
        labels = scales::comma, 
        position = "right", 
        expand = expansion(mult = c(0, 0.15)) 
    ) +
    
    labs(
        title = "China's Urban Rail Staircase",
        subtitle = "The cumulative length of urban rail projects started each year. <br> Construction accelerated rapidly following the 2008 Beijing Olympics",
        caption = "30DayChartChallenge 2026: <b> Day 24 </b> | Source: <b> Transit Costs Project </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "",
        y = "Total Kilometers"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.3, linetype = "dashed"),
        
        
        axis.text = element_text(size = 10, color = "grey40", face = "bold"),
        axis.text.y.right = element_text(margin = margin(r = 10)),
        
        axis.title.y = element_text(size = 14, color = "grey40"),
        
        # Titles
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 15, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1, lineheight = 1.2),
        
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 9, units = "in", dpi = 600
)



