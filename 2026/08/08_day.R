
# change the size of the letters inside the cirle and the legend

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(stringr) 


# load data ------

dt <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-01-21/spotify_songs.csv")


# clean data ------

data <- dt[!is.na(mode)]

# Format columns
data[, condition := fifelse(mode == 1, "Major Key", "Minor Key")]
data[, group := str_to_title(playlist_genre)]
data[, name := str_to_title(playlist_subgenre)]

# Aggregate counts
data <- data[, .(value = .N), by = .(group, name, condition)]


grid <- data[, CJ(condition = c("Major Key", "Minor Key")), by = .(group, name)]
data <- data[grid, on = .(group, name, condition)]
data[is.na(value), value := 0] 


empty_bar <- 2
groups <- unique(data$group)


to_add <- data.table(
    group = rep(groups, each = empty_bar * 2),
    name = rep(paste0("spacer_", seq_len(empty_bar * length(groups))), each = 2),
    condition = rep(c("Major Key", "Minor Key"), times = empty_bar * length(groups)),
    value = NA_real_
)

data <- rbind(data, to_add)


data[, is_spacer := grepl("spacer_", name)]
setorder(data, group, is_spacer, name)


data[, id := .GRP, by = .(group, name)]


label_data <- data[is_spacer == FALSE, .(tot = sum(value, na.rm = TRUE)), by = .(id, name)]
number_of_bar <- max(data$id)


label_data[, angle := 90 - 360 * (id - 0.5) / number_of_bar]
label_data[, hjust := fifelse(angle < -90, 1, 0)]
label_data[, angle := fifelse(angle < -90, angle + 180, angle)]


base_data <- data[!is.na(value), .(start = min(id), end = max(id)), by = group]
base_data[, title := (start + end) / 2]

data[, is_spacer := NULL]


col <- c(
    "Major Key" = "#5a8192", 
    "Minor Key" = "#db9044"  
)



# plot --------

gr <- ggplot(data) +      
    
    geom_bar(
        aes(x = as.factor(id), y = value, fill = condition), 
        stat = "identity", alpha = 0.9
    ) +
    
    scale_fill_manual(values = col) +
    
    geom_segment(
        data = base_data, 
        aes(x = start, y = -100, xend = end, yend = -100), 
        colour = "grey30", 
        linewidth = 0.8
    ) +

    geom_text(
        data = base_data, aes(x = title, y = -400, label = group), 
        hjust = 0.5, colour = "black", alpha = 0.9, size = 4.5, fontface = "bold"
    ) +
    
    geom_text(
        data = label_data, aes(x = id, y = tot + 100, label = name, hjust = hjust), 
        color = "grey20", fontface = "bold", alpha = 0.8, size = 4, angle = label_data$angle, inherit.aes = FALSE
    ) +
    
    ylim(-1500, max(label_data$tot, na.rm = TRUE) + 800) +
    
    coord_polar() +
    
    labs(
        title = "The Distribution of Musical Keys",
        subtitle = "Comparing Major vs. Minor keys across 30,000 Spotify tracks and 24 subgenres.",
        caption = "30DayChartChallenge 2026: <b> Day 8 </b> | Source: <b> Spotify (TidyTuesday) </b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 11, face = "bold", color = "grey30"),
        legend.margin = margin(b = -35),
        
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 16, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 10, hjust = 1.25),

        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        plot.margin = margin(20, 20, 20, 20)
        
    )

gr


#         plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
#         plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
#         
#         plot.background = element_rect(fill = "grey95", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )
# 
# gr



# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)

