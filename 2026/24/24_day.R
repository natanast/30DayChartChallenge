

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(friends)


# load data ------

rm(list = ls())
gc()

# load libraries -----
library(data.table)
library(stringr)
library(ggplot2)
library(ggtext)
library(extrafont)

# Load data -------

# This URL pulls the 'database' tab directly from the Google Sheet you found
url <- "https://docs.google.com/spreadsheets/d/16GoHcbW-eVzHUUP_XCWVXS1s_i3ZBnmZh4kvdSX7muU/export?format=csv&gid=1828904092"
# Load the data, skipping the first row which is a title row
dt_raw <- fread(url, skip = 1)

# Clean the column names to remove spaces (very important for data.table)
setnames(dt_raw, make.names(names(dt_raw)))
# Clean and filter data ------

# 1. Filter for China (Using the code 'CN' found in the sheet)
dt_china <- dt_raw[Country == "CN"]

# 2. Clean numeric columns
# We remove commas and non-numeric characters before converting to numeric
dt_china[, length_km := as.numeric(str_replace_all(Length, ",", ""))]
dt_china[, start_yr := as.numeric(str_extract(Start.year, "\\d{4}"))]

# 3. Aggregate by Year
# We group by start_yr to see the 'construction boom'
dt_viz <- dt_china[!is.na(start_yr) & start_yr >= 2000 & start_yr <= 2024, 
                   .(km_started = sum(length_km, na.rm = TRUE)), 
                   by = .(year = start_yr)]

# 4. Create Cumulative Timeseries
setorder(dt_viz, year)
dt_viz[, total_network := cumsum(km_started)]

# Verification
print(paste("Rows found for China:", nrow(dt_china)))
tail(dt_viz)
# clean data ------

# Define the SCMP Red
scmp_red <- "#b03030"

# plot -------

gr <- ggplot(dt_viz, aes(x = year, y = total_network)) +
    
    # The "Mountain" - Area chart with subtle border
    geom_area(fill = scmp_red, alpha = 0.85) +
    geom_line(color = scmp_red, linewidth = 1) +
    
    # SCMP Annotation 1: Pointing to the 2008 Olympics surge
    annotate(
        "curve", x = 2006, y = 2500, xend = 2008, yend = 1200,
        curvature = -0.2, arrow = arrow(length = unit(0.2, "cm")), color = "grey40"
    ) +
    annotate(
        "text", x = 2000, y = 3000, 
        label = "2008 Beijing Olympics\nsparks urban rail boom",
        family = "Candara", fontface = "italic", size = 3.5, color = "grey20", hjust = 0
    ) +
    
    # SCMP Annotation 2: The Peak Value
    annotate(
        "text", x = 2024, y = 11500, 
        label = "10,938 km",
        family = "Candara", fontface = "bold", size = 5, color = scmp_red, hjust = 1
    ) +
    
    # Formatting Axes
    scale_x_continuous(breaks = seq(2000, 2024, 4)) +
    scale_y_continuous(
        labels = scales::comma, 
        position = "right", # SCMP often puts Y-axis on the right
        expand = expansion(mult = c(0, 0.1))
    ) +
    
    labs(
        title = "China's Urban Rail Explosion",
        subtitle = "Cumulative kilometers of transit projects started per year.<br>Data includes subways, light rail, and urban rapid transit.",
        caption = "30DayChartChallenge 2026: <b> Day 24 (SCMP) </b> | Source: <b> Transit Costs Project (NYU) </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "",
        y = "Total Kilometers"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        # Remove grid lines for that clean newspaper look
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3),
        
        # Axis text styling
        axis.text = element_text(size = 10, color = "grey40", face = "bold"),
        axis.title.y = element_text(size = 10, color = "grey40", hjust = 1),
        
        # Titles
        plot.title = element_markdown(size = 22, face = "bold", color = "grey10", margin = margin(b = 5)),
        plot.subtitle = element_markdown(size = 13, color = "grey30", lineheight = 1.2, margin = margin(b = 25)),
        plot.caption = element_markdown(size = 9, color = "grey50", margin = margin(t = 20), hjust = 1),
        
        plot.background = element_rect(fill = "white", color = NA),
        plot.margin = margin(30, 30, 30, 30)
    )

gr

# save ---------
# IMPORTANT: We save this with a TALL aspect ratio for the SCMP look
ggsave(
    plot = gr, filename = "Day24_SCMP_ChinaRail.png",
    width = 7, height = 11, units = "in", dpi = 600
)
# plot --------
# 
# 
# gr <- ggplot(dt_long, aes(x = Year, y = Spending, group = Gift)) +
#     
#     geom_line(aes(color = Gift), linewidth = 1) +
#     
# 
#     geom_point(
#         aes(color = Gift),
#         shape = 21, 
#         stroke = 1, 
#         size = 4.5,
#         fill = "grey95"
#     ) +
#     
#     geom_text_repel(
#         data = dt_long[Year == "2010"],
#         aes(label = paste0(Gift, " ($", round(Spending), ")"), color = Gift),
#         hjust = 1.1, 
#         direction = "y", 
#         size = 4.5, 
#         fontface = "bold", 
#         segment.color = NA
#     ) +
#     
#     
#     geom_text_repel(
#         data = dt_long[Year == "2022"],
#         aes(label = paste0("($", round(Spending), ") ", Gift), color = Gift),
#         hjust = -0.1, 
#         direction = "y", 
#         size = 4.5, 
#         fontface = "bold", 
#         segment.color = NA
#     ) +
#     
#     scale_color_manual(values = col) +
#     
#     
#     scale_x_discrete(expand = expansion(mult = 0.5)) +
#     
#     labs(
#         title = "The Rising Cost of Romance",
#         subtitle = "Average per-person Valentine's Day spending in the US: 2010 vs 2022.",
#         caption = "30DayChartChallenge 2026: <b> Day 4 (Slope)</b> | Source: <b> NRF (TidyTuesday)</b> | Graphic: <b>Natasa Anastasiadou</b>"
#     ) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#         legend.position = "none", 
#         
#         axis.title = element_blank(),
#         axis.text.y = element_blank(),
#         axis.text.x = element_text(size = 14, face = "bold", color = "black"),
#         
# 
#         panel.grid.major = element_line(linewidth = 0.35, color = "grey85"),
#         panel.grid.minor = element_blank(),
#         
#         plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
#         plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1, lineheight = 1.2),
#         
#         plot.background = element_rect(fill = "#e4e4e3", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )

# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 9, units = "in", dpi = 600
)

