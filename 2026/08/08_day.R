

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)



# load data ------

dt <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-02-13/historical_spending.csv")


# clean data ------




# plot --------

# 
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
    width = 9, height = 9, units = "in", dpi = 600
)


rm(list = ls())
gc()

# load libraries -------
library(tidyverse) 
library(ggtext)
library(extrafont)

# 1. Load data ------
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-01-21/spotify_songs.csv"
dt <- fread(url)

# 2. Clean and count the distributions ------
data <- dt %>%
    filter(!is.na(mode)) %>% # Ensure no raw missing data
    mutate(
        condition = ifelse(mode == 1, "Major Key", "Minor Key"),
        group = str_to_title(playlist_genre),
        name = str_to_title(playlist_subgenre)
    ) %>%
    group_by(group, name, condition) %>%
    summarise(value = n(), .groups = "drop") %>%
    arrange(group, name, condition)

# 3. Trigonometry & Formatting (The NA Bug Fix!) ------
empty_bar <- 2

# Create precise spacer rows. Notice condition is strictly Major/Minor, NOT NA!
to_add <- data.frame(
    group = rep(unique(data$group), each = empty_bar * 2),
    name = NA,
    condition = rep(c("Major Key", "Minor Key"), times = empty_bar * length(unique(data$group))),
    value = NA
)

# Bind the spacers and re-sort so the invisible bars sit at the end of each group
data <- bind_rows(data, to_add) %>% 
    arrange(group, is.na(name), name) 

# Assign the numerical IDs for the circular X-axis
data$id <- rep(seq(1, nrow(data) / 2), each = 2)

# Get the name and the y position of each label
label_data <- data %>%
    group_by(id, name) %>%
    summarize(tot = sum(value, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(name))

# Calculate the exact angle for the text labels
number_of_bar <- max(data$id)
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle + 180, angle)

# Prepare base lines for the 6 Genre group labels
base_data <- data %>%
    filter(!is.na(value)) %>%
    group_by(group) %>%
    summarize(start = min(id), end = max(id), .groups = "drop") %>%
    rowwise() %>%
    mutate(title = mean(c(start, end)))

# Your editorial palette
col <- c(
    "Major Key" = "#5a8192", 
    "Minor Key" = "#db9044"  
)

# 4. Plot --------
gr <- ggplot(data) +      
    
    geom_bar(aes(x = as.factor(id), y = value, fill = condition), stat = "identity", alpha = 0.9) +
    scale_fill_manual(values = col) +
    
    geom_segment(data = base_data, aes(x = start, y = -100, xend = end, yend = -100), colour = "grey50", alpha = 0.8, linewidth = 0.8) +
    
    geom_text(data = base_data, aes(x = title, y = -400, label = group), hjust = 0.5, colour = "black", alpha = 0.9, size = 4.5, fontface = "bold", family = "Candara") +
    
    geom_text(data = label_data, aes(x = id, y = tot + 100, label = name, hjust = hjust), color = "grey20", fontface = "bold", alpha = 0.8, size = 3, angle = label_data$angle, family = "Candara", inherit.aes = FALSE) +
    
    ylim(-1500, max(label_data$tot, na.rm = TRUE) + 800) +
    
    coord_polar() +
    
    labs(
        title = "The Circular Distribution of Musical Keys",
        subtitle = "Comparing Major vs. Minor keys across 30,000 Spotify tracks and 24 subgenres.<br>Notice how EDM heavily favors minor keys, while classic rock is dominated by major keys.",
        caption = "30DayChartChallenge 2026: <b> Day 8 (Distributions: Circular)</b> | Source: <b> Spotify / TidyTuesday </b> | Graphic: <b>Natasa Anastasiadou</b>"
    ) +
    
    theme_minimal(base_family = "Candara") +
    theme(
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 11, face = "bold", color = "grey30"),
        legend.margin = margin(b = 20),
        
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 10, b = 10), size = 8, hjust = 1, lineheight = 1.2),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

gr

