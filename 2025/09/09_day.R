

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

ratings  <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-01-25/ratings.csv')
# details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-01-25/details.csv')



# Plot 1 -----------


# Filter for D&D-themed games (Assuming you have a 'category' column or similar to filter D&D games)
df_dnd <- ratings %>% 
    filter(grepl("D&D|Dungeons & Dragons", name, ignore.case = TRUE))  # adjust according to the actual column name

# Calculate the rating deviation
avg_rating <- mean(df_dnd$average, na.rm = TRUE)
df_dnd <- df_dnd %>% 
    mutate(rating_dev = average - avg_rating)

# Reshape the data back to long format
df_long <- df_dnd %>%
    select(name, rating_dev) %>%
    pivot_longer(cols = -name, names_to = "game_name", values_to = "rating_dev")

# Check the reshaped data
head(df_long)



# lollipop plot
ggplot(df_dnd, aes(x = reorder(name, rating_dev), y = rating_dev, color = rating_dev)) +
    geom_segment(aes(xend = name, yend = 0), size = 1) +  # Draw line from zero to rating
    geom_point(size = 4) +  # Dot at the end of the line
    scale_color_gradient2(midpoint = 0, low = "red", high = "green", mid = "white") +
    labs(
        title = "Diverging Lollipop Chart of D&D Board Game Rating Deviations",
        x = "Board Game",
        y = "Rating Deviation"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, family="Candara", size = 10),
        plot.title = element_text(size = 14, color = 'black', hjust = 0.5, family="Candara"),
        plot.background = element_rect(fill='white'),
        panel.background = element_rect(fill='white')
    )



# plot -----------

# ggplot(avg_risk_dt, aes(x = factor(Group), y = Proportion, fill = Group)) +
#     
#     geom_bar(
#         position = "stack", 
#         stat = "identity", 
#         width = 1, 
#         alpha = 0.9, 
#         color = "black",
#         linewidth = 0.25,
#         fill = "#ACD4EC"
#     ) +
#     
#     coord_polar(start = 0) + 
#     
#     geom_text(data = label_data, aes(x = id, y = Proportion, label = Group, angle = angle),
#               color = "black", fontface = "bold", alpha = 0.6, size = 2.3, inherit.aes = FALSE,
#               family = "Candara") +
# 
# 
#     theme_minimal() +
#     
#     # labs(title = "Circular Barplot of Proportions of Conditions",
#     #      subtitle = "Proportion of individuals with each condition (binary 0/1)",
#     #      x = "Group",
#     #      y = "Proportion of 1's") +
#     
# 
#     theme(
# 
#         legend.position = "none",
#         legend.title.position = "left",
# 
#         legend.title = element_blank(),
# 
#         axis.title.x = element_blank(),
# 
#         axis.title.y = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
# 
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_line(linewidth = .35, color = "grey80"),
# 
#         plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, family = "Candara", margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 11, hjust = 0.5, family = "Candara", color = "grey30", margin = margin(t = 5, b = 25)),
#         plot.caption = element_markdown(margin = margin(t = 5), size = 8, family = "Candara", hjust = 1.25),
# 
#         plot.margin = margin(6, 6, 6, 6),
# 
#         plot.background = element_rect(fill = "#e4e4e3", color = NA)
#     )
# 
# gr
# 

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 8, units = "in", dpi = 600
)

