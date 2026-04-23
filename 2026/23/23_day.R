

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


# clean data ------

dt_tv$air_date <- as.Date(dt_tv$air_date)

dt_tv[, season_avg := mean(imdb_rating, na.rm = TRUE), by = season]

dt_tv[, is_win := imdb_rating >= season_avg]

dt_tv[, y_pos := fifelse(is_win, 1, -1)]


dt_tv[, ep_num := seq_len(.N), by = season]


dt_tv[, start_year := format(min(air_date), "%Y"), by = season]
dt_tv[, season_label := str_c("Season ", season, " (", start_year, ")")]


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
        subtitle = "A Win-Loss Sparkline of every <b>Friends</b> episode.<br>Blocks facing <span style='color:#b25c56'><b>UP</b></span> scored above their season's average IMDb rating. Blocks facing <span style='color:#4A6990'><b>DOWN</b></span> scored below it.",
        caption = "30DayChartChallenge 2026: <b> Day 23 (Seasons) </b> | Source: <b> {friends} R Package </b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +
    
    
    theme_void(base_family = "Candara") +
    
    theme(
        # Format the Season (Year) labels on the left
        strip.text.y.left = element_text(size = 11, face = "bold", color = "grey30", angle = 0, hjust = 1, margin = margin(r = 15)),
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1, lineheight = 1.2),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

gr

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
        # plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        # plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        # plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1, lineheight = 1.2),

#         plot.background = element_rect(fill = "#e4e4e3", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )

# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 9, units = "in", dpi = 600
)

