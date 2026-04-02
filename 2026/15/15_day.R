

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(stringr)
library(ggalluvial)

# load data ------


# clean data -----



# plot --------



# gr <- ggplot(df_picto, aes(x = x, y = season_label)) +
#     
#     
#     geom_text(label = "🍔", size = 8, family = "Segoe UI Emoji") +
#     
#     scale_x_continuous(limits = c(0, 13), breaks = seq(0, 12, by = 2)) +
#     
#     labs(
#         title = "Bob's Burgers: The Short & Long Seasons",
#         subtitle = "Total unique words spoken per season. Season 2 was cut to just 9 episodes. <br><b>Each 🍔 represents 2,000 words.</b>",
#         caption = "30DayChartChallenge 2026: <b> Day 2</b>
#                    | Source: <b> bobsburgers (TidyTuesday | Nov 2024)</b>
#                    | Graphic: <b>Natasa Anastasiadou</b>",
#         
#     ) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#         
#         axis.title = element_blank(),
#         
#         axis.text.x = element_text(size = 10, color = "grey30"),
#         axis.text.y = element_text(size = 12, face = "bold", color = "black", margin = margin(r = 10)),
#         
#         panel.grid.major = element_line(linewidth = 0.35, color = "grey85"),
#         panel.grid.minor = element_blank(),
#         
        # plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        # plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        # plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
#         
#         plot.background = element_rect(fill = "grey95", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )
# 
# gr
# 


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)


rm(list = ls())
gc()

# load libraries -------
library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)

# 1. Load data ------
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-03-03/tortoise_body_condition_cleaned.csv"
dt <- fread(url)

# 2. Clean and Filter ------
# Remove rows with missing correlation variables or unknown sex
dt_clean <- dt[!is.na(body_mass_grams) & !is.na(straight_carapace_length_mm) & sex %in% c("f", "m")]

# Create a clean grouping label for the legend
dt_clean[, group_label := fcase(
    locality == "Konjsko", "Mainland (Healthy Environment)",
    locality %in% c("Beach", "Plateau"), "Island (High Harassment)"
)]

# 3. Create the Palette -------
# Using your quiet, muted Mediterranean tones
cols <- c(
    "Mainland (Healthy Environment)" = "#8aa39b",  # Sage Green
    "Island (High Harassment)"       = "#b25c56"   # Muted Brick
)

# 4. Plot --------
gr <- ggplot(dt_clean, aes(x = straight_carapace_length_mm, y = body_mass_grams, color = group_label)) +
    
    # Draw the points with slight transparency
    geom_point(alpha = 0.6, size = 2.5, stroke = 0) +
    
    # Add a quiet regression line to show the correlation trend for both groups
    geom_smooth(method = "lm", aes(fill = group_label), alpha = 0.15, linetype = "dashed", linewidth = 0.5) +
    
    # Map the exact colors for points and regression ribbons
    scale_color_manual(values = cols, name = "Population") +
    scale_fill_manual(values = cols, guide = "none") +
    
    labs(
        title = "The physical cost of survival",
        subtitle = "Correlating carapace length with body mass in Hermann's tortoises.<br>Notice how the island population (brick) trends physically lighter for their size compared to the mainland.",
        caption = "30DayChartChallenge 2026: <b> Day 15 (Correlation) </b> | Source: <b> Golem Grad Tortoise Study </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Straight Carapace Length (mm)",
        y = "Body Mass (grams)"
    ) +
    
    theme_minimal(base_family = "Candara") +
    theme(
        # The reference dashed gridlines on a light grey background
        panel.grid.major = element_line(color = "grey80", linetype = "dashed", linewidth = 0.4),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#f2f2f2", color = NA),
        panel.background = element_rect(fill = "#f2f2f2", color = NA),
        
        # Axis styling
        axis.text = element_text(size = 9, color = "grey40", face = "bold"),
        axis.title.x = element_text(size = 11, face = "bold", color = "grey30", margin = margin(t = 15)),
        axis.title.y = element_text(size = 11, face = "bold", color = "grey30", margin = margin(r = 15)),
        
        # Legend styling
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 10, face = "bold", color = "grey30"),
        legend.background = element_rect(fill = "#f2f2f2", color = NA),
        legend.key = element_rect(fill = "#f2f2f2", color = NA),
        legend.guides = guide_legend(override.aes = list(size = 4, alpha = 1)), 
        
        # Titles
        plot.title = element_text(size = 22, face = "bold", color = "black", hjust = 0, margin = margin(b = 8)),
        plot.subtitle = element_markdown(size = 11, color = "grey40", hjust = 0, lineheight = 1.3, margin = margin(b = 25)),
        plot.caption = element_markdown(margin = margin(t = 20), size = 8, color = "grey50", hjust = 1),
        
        plot.margin = margin(30, 30, 30, 30)
    )

gr

# 5. Save ---------
ggsave(
    "Day15_Correlation_Tortoises.png", 
    plot = gr, 
    width = 10, height = 8, dpi = 600
)


