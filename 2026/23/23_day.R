

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(friends)


# load data ------

dt <- friends_info

# clean data ------
rm(list = ls())
gc()

# load libraries -----
library(friends)
library(ggplot2)
library(dplyr)
library(extrafont)
library(ggtext)

# Load data -------
dt_tv <- friends_info

# Clean data -----
dt_tv$air_date <- as.Date(dt_tv$air_date)
dt_tv$season <- factor(dt_tv$season)

# Calculate the average rating for each season, and find where each season starts and ends
dt_avg <- dt_tv |> 
    group_by(season) |> 
    summarize(
        avg_rating = mean(imdb_rating, na.rm = TRUE),
        start_date = min(air_date),
        end_date = max(air_date)
    )

# THE FIX: Join the average back to the main dataset so the lollipops know where to start!
dt_tv <- dt_tv |> 
    left_join(dt_avg |> select(season, avg_rating), by = "season")

# Identify standout episodes for labels
best_ep <- dt_tv |> filter(imdb_rating == max(imdb_rating, na.rm = TRUE))
worst_ep <- dt_tv |> filter(imdb_rating == min(imdb_rating, na.rm = TRUE))


# plot --------

# Expanded vintage palette to cover 10 seasons smoothly
cols <- c(
    "#b25c56", "#c28d75", "#e8998f", "#e6bcae", "#aebfbc", 
    "#8aa39b", "#628b9c", "#7AA6DC", "#6785ae", "#4A6990"
)

gr <- ggplot() +
    
    # 1. The Season Average Lines (Drawn first so they sit behind the points)
    geom_segment(
        data = dt_avg,
        aes(x = start_date, xend = end_date, y = avg_rating, yend = avg_rating, color = season),
        linewidth = 1.5
    ) +
    
    # 2. The Lollipop Stems (Now starting at the avg_rating!)
    geom_segment(
        data = dt_tv,
        aes(x = air_date, xend = air_date, y = avg_rating, yend = imdb_rating, color = season),
        linewidth = 0.4, alpha = 0.8
    ) +
    
    # 3. The Lollipop Heads 
    geom_point(
        data = dt_tv,
        aes(x = air_date, y = imdb_rating, color = season),
        size = 1, alpha = 0.9
    ) +
    
    # # Annotate Best Episode
    # annotate(
    #     "text", x = as.Date("2003-05-01"), y = 9.75, 
    #     label = "Series Finale (9.7)", 
    #     family = "Candara", fontface = "bold", size = 3, color = "grey30", hjust = 1
    # ) +
    # 
    # # Annotate Worst Episode
    # annotate(
    #     "text", x = as.Date("2000-05-01"), y = 7.15, 
    #     label = "The Mac and C.H.E.E.S.E (7.2)", 
    #     family = "Candara", fontface = "bold", size = 3, color = "grey30", hjust = 0
    # ) +
    
    scale_y_continuous(
        limits = c(7, 10), 
        breaks = seq(7, 10, by = 0.5),
        expand = c(0, 0.1)
    ) +
    
    scale_x_date(
        date_breaks = "1 year", 
        date_labels = "%Y",
        expand = expansion(mult = c(0.02, 0.02))
    ) +
    
    scale_color_manual(values = cols) +
    
    labs(
        title = "The One With All The Seasons",
        subtitle = "IMDb ratings for every episode of <b>Friends</b> (1994–2004). Thick horizontal lines<br>represent the average rating for each season.",
        caption = "30DayChartChallenge 2026: <b> Day 23 (Seasons) </b> | Source: <b> {friends} R Package </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Original Air Date",
        y = "IMDb Episode Rating"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "grey80", linetype = "dotted", linewidth = 0.4),
        panel.grid.minor.y = element_blank(),
        
        axis.text.x = element_text(size = 10, color = "grey40", face = "bold", margin = margin(t = 10)),
        axis.text.y = element_text(size = 10, color = "grey40", face = "bold", margin = margin(r = 10)), 
        
        axis.title.x = element_text(size = 12, face = "bold", color = "grey30", margin = margin(t = 15)),
        axis.title.y = element_text(size = 12, face = "bold", color = "grey30", margin = margin(r = 15)),
        
        legend.position = "none",
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1),
        
        plot.background = element_rect(fill = "grey95", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

gr

# save ---------
ggsave(
    plot = gr, filename = "Day23_Seasons_Friends.png",
    width = 10, height = 7, units = "in", dpi = 600
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


rm(list = ls())
gc()

# load libraries -----
library(friends)
library(ggplot2)
library(dplyr)
library(stringr)
library(data.table)
library(extrafont)
library(ggtext)

# Load data -------
dt_tv <- as.data.table(friends_info)

# Clean data ------
dt_tv$air_date <- as.Date(dt_tv$air_date)

# Calculate season average
dt_tv[, season_avg := mean(imdb_rating, na.rm = TRUE), by = season]

# Determine Win/Loss (TRUE/FALSE)
dt_tv[, is_win := imdb_rating >= season_avg]

# Set Y position (+1 for Win, -1 for Loss)
dt_tv[, y_pos := fifelse(is_win, 1, -1)]

# Get episode number for the X-axis (this pulls the blocks tight together!)
dt_tv[, ep_num := seq_len(.N), by = season]

# Get the starting year for each season to prove it's a timeseries
dt_tv[, start_year := format(min(air_date), "%Y"), by = season]

# Create a clean label using stringr: e.g., "Season 1 (1994)"
dt_tv[, season_label := str_c("Season ", season, " (", start_year, ")")]

# Lock the factor levels so they display in chronological order from top to bottom
label_order <- unique(dt_tv[order(as.numeric(season)), season_label])
dt_tv$season_label <- factor(dt_tv$season_label, levels = label_order)


# plot -------

cols <- c("TRUE" = "#b25c56", "FALSE" = "#4A6990")

gr <- ggplot(dt_tv, aes(x = ep_num, y = y_pos, fill = is_win)) +
    
    # Baseline for the sparkline
    geom_hline(yintercept = 0, color = "grey80", linewidth = 0.5) +
    
    # width = 0.8 leaves just a tiny, clean gap between the episodes
    geom_col(width = 0.65, show.legend = FALSE) +
    
    # Facet by our new custom label, placing it on the left
    facet_grid(season_label ~ ., switch = "y") +
    
    scale_fill_manual(values = cols) +
    
    labs(
        title = "The One With The Hits and Misses",
        subtitle = "A Win-Loss Sparkline of every <b>Friends</b> episode.<br>Blocks facing <span style='color:#4A6990'><b>UP</b></span> scored above their season's average IMDb rating. Blocks facing <span style='color:#b25c56'><b>DOWN</b></span> scored below it.",
        caption = "30DayChartChallenge 2026: <b> Day 23 (Seasons) </b> | Source: <b> {friends} R Package </b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +
    
    # theme_void removes all the distracting axes and grids
    theme_void(base_family = "Candara") +
    
    theme(
        # Format the Season (Year) labels on the left
        strip.text.y.left = element_text(size = 11, face = "bold", color = "grey30", angle = 0, hjust = 1, margin = margin(r = 15)),
        
        # Titles and Caption
        plot.title = element_markdown(size = 18, face = "bold", color = "grey20", hjust = 0.5, margin = margin(t = 2, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey40", margin = margin(t = 5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, color = "grey50", hjust = 1),
        
        plot.background = element_rect(fill = "grey95", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

gr

