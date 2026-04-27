

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


# 2. DESIGN PALETTE (Monochrome Slate)
bg_light   <- "grey93"
col_east   <- "#2A5A6D" # Deep Slate Blue
col_cent   <- "#7A9BA8" # Lighter Slate Blue

# Perfect for Day 30
ggplot(dt, aes(x = year, y = val, color = location_name, fill = location_name)) +
    # The Uncertainty Cloud
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
    # The Estimated Truth
    geom_line(linewidth = 1) +
    # Compare the two causes
    facet_wrap(~cause_name, scales = "free_y") +
    # Clean editorial colors (Monochrome Blue/Slate)
    scale_fill_manual(values = c("#7A9BA8", "#2A5A6D", "grey60")) +
    scale_color_manual(values = c("#7A9BA8", "#2A5A6D", "grey60")) +
    theme_minimal() +
    labs(title = "The Uncertainty of Crisis") # Plus your other labs...




# Filter for start and end years
dt_slope <- dt[year %in% c(2000, 2023)]

ggplot(dt_slope, aes(x = factor(year), y = val, group = location_name, color = location_name)) +
    # The Uncertainty Bars
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, alpha = 0.5) +
    # The Slope Line
    geom_line(linewidth = 0.8, alpha = 0.7) +
    # The Estimates
    geom_point(size = 3) +
    facet_wrap(~cause_name, scales = "free_y") +
    scale_color_manual(values = c("#7A9BA8", "#2A5A6D", "grey60")) +
    labs(
        title = "Two Decades of Health Estimates",
        subtitle = "Comparing mortality rates between 2000 and 2023. Vertical bars illustrate the 95%<br>**uncertainty** intervals at each time point.",
        x = "Comparison Year",
        y = "Rate per 100,000"
    ) +
    theme_minimal(base_family = "Candara") # + your theme settings


# --- DAY 30: THE FINALE - LOLLIPOP RANKING ---

library(data.table)
library(ggplot2)
library(ggtext)

# 1. DATA PREP (Filtering for the latest year)
dt_2023 <- dt[year == 2023]

# Reordering locations so the highest mortality is at the top
dt_2023[, location_name := reorder(location_name, val)]

# 2. COLORS (Final Finale Teal)
bg_light   <- "grey93"
col_main   <- "#2A5A6D" # Deep Slate Teal
col_accent <- "#ADC2C8" # Muted Teal for the stick

# 3. PLOT
gr <- ggplot(dt_2023, aes(x = val, y = location_name)) +
    
    # THE UNCERTAINTY STICK (The 95% Interval)
    geom_segment(
        aes(x = lower, xend = upper, yend = location_name),
        color = col_accent, linewidth = 2, alpha = 0.5
    ) +
    
    # THE ESTIMATE CANDY (The Value)
    geom_point(color = col_main, size = 4) +
    
    # FACET BY CAUSE
    facet_wrap(~cause_name, scales = "free_x") +
    
    labs(
        title = "Ranking Mortality Rates with Estimation Uncertainty",
        subtitle = "2023 death rates per 100,000 population. The line segments illustrate the 95% **uncertainty** intervals,<br>showing the range of precision in GHDx estimates across Central and Eastern Europe.",
        caption = "30DayChartChallenge 2026: **Day 30 (GHDx Data Day)** | Theme: **Uncertainties** | Source: **IHME GHDx** | Graphic: **Natasa Anastasiadou**",
        x = "Rate per 100,000",
        y = NULL
    ) +
    
    theme_minimal(base_family = "Candara") +
    theme(
        plot.background = element_rect(fill = bg_light, color = NA),
        panel.background = element_rect(fill = bg_light, color = NA),
        
        # Centered Header
        plot.title = element_markdown(size = 22, face = "bold", color = "#1a1a1c", hjust = 0.5, margin = margin(t = 20)),
        plot.subtitle = element_markdown(size = 11, color = "grey30", lineheight = 1.3, hjust = 0.5, margin = margin(t = 10, b = 35)),
        plot.caption = element_markdown(size = 8, color = "grey50", hjust = 0.5, margin = margin(t = 40)),
        
        # Clean Facet Labels
        strip.text = element_text(size = 11, face = "bold", color = "grey20"),
        panel.spacing = unit(2, "lines"),
        
        axis.text = element_text(color = "grey40", size = 10, face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.margin = margin(20, 60, 20, 60)
    )

gr










# --- DAY 30: THE FINALE - TEMPORAL BEESWARM ---

library(data.table)
library(ggplot2)
library(ggtext)
library(ggbeeswarm)

# 1. DATA PREP
# Select specific milestone years to show the evolution
milestone_years <- c(2000, 2010, 2020, 2023)
dt_milestones <- dt[year %in% milestone_years]

# Separate regions and countries
regions <- c("Central Europe", "Eastern Europe", "Western Europe")
dt_countries <- dt_milestones[!(location_name %in% regions)]
dt_regions   <- dt_milestones[location_name %in% regions]

# 2. COLORS (Monochrome Slate/Teal)
bg_light    <- "grey93"
col_points  <- "#7A9BA8" # Muted Slate
col_region  <- "#155F83" # Deep Blue

# 3. PLOT
gr <- ggplot(dt_countries, aes(x = val, y = factor(year))) +
    
    # THE UNCERTAINTY: Light range bars for every country
    geom_linerange(
        aes(xmin = lower, xmax = upper),
        color = "grey80", alpha = 0.4, linewidth = 0.4
    ) +
    
    # THE POINTS: Beeswarm distribution
    geom_quasirandom(
        color = col_points, alpha = 0.6, size = 1.8, groupOnX = FALSE
    ) +
    
    # THE REGIONAL CONTEXT: Large diamond for the average
    stat_summary(
        data = dt_regions,
        aes(x = val, y = factor(year)),
        fun = mean, geom = "point", 
        color = col_region, size = 4, shape = 18
    ) +
    
    # FACET BY CAUSE: Side-by-side comparison of the two health issues
    facet_wrap(~cause_name, scales = "free_x") +
    
    labs(
        title = "Evolution of Mortality Estimates and Reporting Uncertainty",
        subtitle = "Comparing country-level death rates per 100,000 across four milestone years. Individual points represent<br>countries, while the shaded bars illustrate the **uncertainty intervals** in GHDx modeling.",
        caption = "30DayChartChallenge 2026: **Day 30 (GHDx)** | Theme: **Uncertainties** | Source: **IHME GHDx** | Graphic: **Natasa Anastasiadou**",
        x = "Death Rate per 100,000",
        y = NULL
    ) +
    
    theme_minimal(base_family = "Candara") +
    theme(
        plot.background = element_rect(fill = bg_light, color = NA),
        panel.background = element_rect(fill = bg_light, color = NA),
        
        # Centered Header
        plot.title = element_markdown(size = 22, face = "bold", color = "#1a1a1c", hjust = 0.5, margin = margin(t = 20)),
        plot.subtitle = element_markdown(size = 11, color = "grey30", lineheight = 1.3, hjust = 0.5, margin = margin(t = 10, b = 40)),
        plot.caption = element_markdown(size = 8, color = "grey50", hjust = 0.5, margin = margin(t = 40)),
        
        # Facet and Axis Styling
        strip.text = element_text(size = 12, face = "bold", color = "grey20", margin = margin(b = 15)),
        axis.text.y = element_text(size = 12, face = "bold", color = col_region),
        axis.text.x = element_text(color = "grey40"),
        
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3, color = "grey85"),
        panel.grid.minor = element_blank(),
        panel.spacing = unit(3, "lines"),
        
        plot.margin = margin(20, 20, 20, 20)
    )

gr


# --- DAY 30: THE GRAND FINALE (COLOR EDITION) ---

library(data.table)
library(ggplot2)
library(ggtext)
library(ggbeeswarm)

# 1. MAPPING REGIONS
# We need to tell R which country belongs to which region for the coloring
dt[, region := fcase(
  location_name %in% c("Eastern Europe", "Russian Federation", "Ukraine", "Belarus", "Serbia"), "Eastern",
  location_name %in% c("Central Europe", "Poland", "Hungary", "Czechia", "Slovakia"), "Central",
  location_name %in% c("Western Europe", "Sweden", "Ireland", "Germany", "France"), "Western",
  default = "Other"
)]

# 2. DATA PREP
milestone_years <- c(2000, 2010, 2020, 2023)
dt_viz <- dt[year %in% milestone_years & region != "Other"]

# Separate for layers
dt_countries <- dt_viz[!(location_name %in% c("Eastern Europe", "Central Europe", "Western Europe"))]
dt_regions   <- dt_viz[location_name %in% c("Eastern Europe", "Central Europe", "Western Europe")]

# 3. PALETTE (Using your Friends-inspired colors)
reg_cols <- c("Eastern" = "#d0615d", "Central" = "#155F83", "Western" = "#FFB900")

# 4. PLOT
gr <- ggplot(dt_countries, aes(x = val, y = factor(year), color = region)) +
  
  # THE UNCERTAINTY: Vertical intervals for every country
  # We make them very thin and light to avoid clutter
  geom_linerange(
    aes(xmin = lower, xmax = upper),
    alpha = 0.3, linewidth = 0.5
  ) +
  
  # THE DOTS: Beeswarm of countries colored by region
  geom_quasirandom(
    alpha = 0.6, size = 2, groupOnX = FALSE, stroke = 0.2
  ) +
  
  # THE REGIONAL DIAMONDS: Large anchors for the averages
  geom_point(
    data = dt_regions,
    aes(x = val, y = factor(year), fill = region),
    size = 4.5, shape = 23, color = "white", stroke = 1
  ) +
  
  # FACET BY CAUSE
  facet_wrap(~cause_name, scales = "free_x", ncol = 1) +
  
  # SCALES
  scale_color_manual(values = reg_cols) +
  scale_fill_manual(values = reg_cols) +
  
  labs(
    title = "Regional Divergence in Health Estimates and Certainty",
    subtitle = "Analysis of death rates per 100,000. Points represent individual countries, while **diamonds** mark the regional<br>averages. Shaded bars indicate the 95% **uncertainty intervals** for each specific measurement.",
    caption = "30DayChartChallenge 2026: **Day 30 (GHDx)** | Theme: **Uncertainties** | Source: **IHME GHDx** | Graphic: **Natasa Anastasiadou**",
    x = "Death Rate per 100,000",
    y = NULL
  ) +
  
  theme_minimal(base_family = "Candara") +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.background = element_rect(fill = "grey93", color = NA),
    panel.background = element_rect(fill = "grey93", color = NA),
    
    # Text Hierarchy
    plot.title = element_markdown(size = 24, face = "bold", hjust = 0.5, margin = margin(t = 20)),
    plot.subtitle = element_markdown(size = 12, color = "grey30", hjust = 0.5, lineheight = 1.3, margin = margin(t = 10, b = 40)),
    plot.caption = element_markdown(size = 9, color = "grey50", hjust = 0.5, margin = margin(t = 40)),
    
    # Facet and Axis
    strip.text = element_text(size = 13, face = "bold", color = "grey20", margin = margin(b = 20)),
    axis.text.y = element_text(size = 12, face = "bold", color = "grey30"),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.4, color = "grey85"),
    panel.grid.minor = element_blank(),
    
    plot.margin = margin(20, 70, 20, 70)
  )

gr


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
    geom_linerange(
        aes(xmin = lower, xmax = upper),
        color = "grey80", alpha = 0.4, linewidth = 0.4
    ) +
    
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
        color = region_col, fill = "white", stroke = 1.5, size = 4, shape = 23
    ) +
    
    # FACET BY CAUSE
    facet_wrap(~cause_name, scales = "free_x") +
    
    # SCALES
    scale_color_gradient(low = "grey", high = "#f30000") +
    
    # scale_fill_gradient(
    #     low = "grey", high = "#f30000",
    #     guide = guide_colorbar(
    #         title = "-log10(p.adj)",
    #         barheight = unit(10, "lines"),
    #         barwidth = unit(.75, "lines")
    #     )
    # ) +
    
    labs(
        title = "Evolution of Global Health Estimates & Reporting Certainty",
        subtitle = "Comparing mortality rates per 100,000. Points represent individual countries colored by intensity,<br>while bars show the **uncertainty intervals** and diamonds mark the regional averages.",
        caption = "30DayChartChallenge 2026: **Day 30 (GHDx Data Day)** | Theme: **Uncertainties** | Source: **IHME GHDx** | Graphic: **Natasa Anastasiadou**",
        x = "Death Rate per 100,000",
        y = NULL
    ) +
    
    theme_minimal(base_family = "Candara") +
    theme(
        legend.position = "none", # Color is redundant, so we can hide the legend
        plot.background = element_rect(fill = bg_light, color = NA),
        panel.background = element_rect(fill = bg_light, color = NA),
        
        # Centered Header Hierarchy
        plot.title = element_markdown(size = 24, face = "bold", color = "#1a1a1c", hjust = 0.5, margin = margin(t = 20)),
        plot.subtitle = element_markdown(size = 11, color = "grey30", lineheight = 1.3, hjust = 0.5, margin = margin(t = 10, b = 40)),
        plot.caption = element_markdown(size = 8, color = "grey50", hjust = 0.5, margin = margin(t = 40)),
        
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






# clean data ------

df_heatmap <- erasmus_raw[, .(N = sum(participants, na.rm = TRUE)), 
                          by = .(sending_country_code, receiving_country_code)]


top_list <- df_heatmap[, .(total = sum(N)), by = sending_country_code][order(-total)][1:15, sending_country_code]
df_heatmap <- df_heatmap[sending_country_code %in% top_list & receiving_country_code %in% top_list]


df_heatmap <- df_heatmap |>
    complete(sending_country_code, receiving_country_code, fill = list(N = 0)) |>
    as.data.table()


df_heatmap[sending_country_code == receiving_country_code, N := NA]


df_heatmap[, sending_country_code := fct_reorder(sending_country_code, N, function(x) sum(x, na.rm = TRUE))]
df_heatmap[, receiving_country_code := fct_reorder(receiving_country_code, N, function(x) sum(x, na.rm = TRUE))]


# plot --------

gr = ggplot(df_heatmap, aes(x = sending_country_code, y = receiving_country_code, fill = N)) +
    
    geom_tile(color = "grey20", linewidth = .25) +
    
    geom_shadowtext(
        aes(label = N), 
        color = "black",
        family = "Candara",
        bg.color = "grey95", 
        bg.r = .1, 
        size = 3.5 
    ) +
    
    scale_fill_stepsn(
        colors = c('#2c5769', '#6F99AD', 'grey96', '#ffb5ac', '#a33a3a'),
        breaks = c(1, 20, 50, 70, 100),  
        transform = pseudo_log_trans(base = 10),
        name = "Total Participants",
        na.value = "grey96",
        guide = guide_colorsteps(
            barheight = unit(10, "lines"), 
            barwidth = unit(0.4, "lines")
        )
    ) +

    theme_minimal(base_family = "Candara") +
    
    labs(
        title = "The Student Trade: Erasmus Mobility",
        subtitle = "Mapping the **relationships** and student **trade** between Europe's top 15 academic destinations.",
        caption = "30DayChartChallenge 2026: <b> Day 14</b>
                   | Source: <b> Erasmus Data (TidyTuesday)</b>
                   | Graphic: <b>Natasa Anastasiadou</b>",
         x = "Sending Country",
         y = "Receiving Country"
    ) +
    
    theme(
        legend.position = "right",
        legend.title.position = "left",
        
        legend.title = element_text(size = 9, angle = 90, hjust = .5, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 8, color = "grey30"),
        
        axis.title.x = element_text(size = 10, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 10, face = "bold", margin = margin(r = 10)),
        
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        
        panel.grid = element_blank(),
        
        plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.2),
        
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "grey93", color = NA)
    )  

gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 8, units = "in", dpi = 600
)
