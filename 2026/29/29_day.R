

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

erasmus_raw <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-03-08/erasmus.csv')


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




# --- DAY 29: MONOCHROME & UNCERTAINTIES (PASSWORDS) ---

rm(list = ls())
gc()

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)

# 1. LOAD THE REAL TIDYTUESDAY DATA (Jan 14, 2020)
dt_raw <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# 2. PREPARE THE DATA
# Drop any empty rows
dt_viz <- dt_raw[!is.na(password) & !is.na(offline_crack_sec)]

# Clean up the category names (e.g., "sport" -> "Sport")
dt_viz[, category := tools::toTitleCase(category)]

# Reorder categories by their median crack time so the plot looks organized
dt_viz[, category := reorder(category, offline_crack_sec, FUN = median)]

# 3. PLOTTING (The Brutalist Monochrome Aesthetic)
bg_dark    <- "#0f0f0f" # Deep, almost-black background
text_light <- "#e2e2e2" # Bright white/grey for primary text
text_dim   <- "#737373" # Muted grey for secondary text and gridlines

gr <- ggplot(dt_viz, aes(x = offline_crack_sec, y = category)) +
    
    # THE DATA: White, semi-transparent points heavily jittered
    # This creates a "static/noise" look representing thousands of passwords
    geom_jitter(
        color = "#ffffff", 
        alpha = 0.25, 
        size = 1.5, 
        height = 0.25, 
        shape = 16
    ) +
    
    # A vertical threshold line marking 1 Day to crack (86,400 seconds)
    geom_vline(xintercept = 86400, color = "#ffffff", linetype = "dashed", linewidth = 0.5, alpha = 0.5) +
    
    # Annotation for the threshold
    annotate(
        "text", x = 86400 * 1.5, y = 1.5, 
        label = "Threshold: 1 Day to Crack", 
        color = text_light, angle = 90, size = 3, family = "Candara", fontface = "bold"
    ) +
    
    # Log10 scale because crack times range from 0.001 seconds to centuries
    scale_x_log10(
        labels = scales::trans_format("log10", scales::math_format(10^.x)),
        breaks = scales::trans_breaks("log10", function(x) 10^x)
    ) +
    
    labs(
        title = "The Illusion of Security",
        subtitle = "Visualizing the time required to crack common leaked passwords. The dense clusters on the left<br>expose the massive **uncertainty** and vulnerability of human-generated security.",
        caption = "30DayChartChallenge 2026: **Day 29 (Monochrome)** | Prompt: **Uncertainties** | Source: **TidyTuesday** | Graphic: **Natasa Anastasiadou**",
        x = "Offline Crack Time in Seconds (Log10 Scale)",
        y = ""
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        plot.background = element_rect(fill = bg_dark, color = NA),
        panel.background = element_rect(fill = bg_dark, color = NA),
        
        # Monochrome grid lines (Dark Grey)
        panel.grid.major.y = element_line(linewidth = 0.2, color = "#333333"),
        panel.grid.major.x = element_line(linewidth = 0.2, color = "#333333"),
        panel.grid.minor = element_blank(),
        
        axis.text = element_text(color = text_dim, size = 10, face = "bold"),
        axis.title.x = element_text(color = text_light, size = 11, face = "bold", margin = margin(t = 15)),
        
        plot.title = element_markdown(size = 22, face = "bold", color = "#ffffff", hjust = 0),
        plot.subtitle = element_markdown(size = 12, color = text_dim, lineheight = 1.3, margin = margin(b = 20)),
        plot.caption = element_markdown(size = 8, color = text_dim, margin = margin(t = 25)),
        
        plot.margin = margin(30, 30, 30, 30)
    )

gr
