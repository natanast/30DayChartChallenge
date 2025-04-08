

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

ratings <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2022/2022-01-25/ratings.csv')


# data cleaning -----------

index <- str_detect(ratings$name, "D&D|Dungeons & Dragons")

df <- ratings[index, ]


df$name <- df$name |> 
    str_remove_all("D&D|Dungeons & Dragons|:") |>  
    str_squish()



# Calculate the rating deviation
avg_rating <- mean(df$average, na.rm = TRUE)

df[ , rating_dev := average - avg_rating]


df_long <- melt(
    df,
    id.vars = "name",
    measure.vars = "rating_dev",
    variable.name = "game_name",
    value.name = "rating_dev"
)


# plot ---------

library(paletteer)

col = c('#60608b', '#9291be', '#b9b8e5', '#fcc1ad', '#e7877d', '#c15451')

col = c('#0e4051', '#2c5769', '#598396', '#93bdd2', '#facbc7', '#fdb5ae', '#c15451', '#9a2f34')

col = c('#003344', '#0e4051', '#1d4d5e', '#2c5a6c', '#3b677a', '#4a7588', '#598396', '#6791a5', '#76a0b3', '#84aec3', '#93bdd2', '#a2cde2', '#bddae9', '#d9e8ef', '#f5f5f5', '#f8e0de', '#facbc7', '#fdb5ae', '#faa098', '#ed8f89', '#e17e79', '#d46e69', '#c85d59', '#b94d4b', '#aa3e40', '#9a2f34', '#8a2028', '#79101b', '#67000e')

col = c('#0e4051', '#598396', '#facbc7', '#9a2f34')

gr = df |>
    ggplot(aes(x = reorder(name, rating_dev), y = rating_dev, color = rating_dev)) +
    
    geom_segment(aes(xend = name, yend = 0), size = 0.5) +
    
    geom_point(size = 4) +
    
    # Adjust color palette for diverging colors
    scale_color_gradientn(
        colors = col, 
        limits = c(min(df$rating_dev), max(df$rating_dev)), 
        values = scales::rescale(c(min(df$rating_dev), 0, max(df$rating_dev))), # Rescale to ensure zero is the midpoint
        breaks = c(min(df$rating_dev), 0, max(df$rating_dev))
    ) +
    
    labs(
        title = "Diverging Lollipop Chart of D&D Board Game Rating Deviations",
        x = "Board Game",
        y = "Rating Deviation"
    ) +
    
    theme_minimal() +
    
    theme(
        
        legend.position = "none",
        
        axis.title.x = element_blank(),
        
        axis.text.y = element_text(size = 12, family = "Candara", color = "#e4e4e3"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = 8, family = "Candara", color = "#e4e4e3"),
        
        plot.title = element_markdown(size = 16, face = "bold", color = "#e4e4e3", hjust = 0.5, family = "Candara", margin = margin(t = 5, b = 30)),
        plot.subtitle = element_markdown(size = 11, hjust = 0.5, family = "Candara", color = "#e4e4e3", margin = margin(t = 5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 5), size = 8, family = "Candara", hjust = 1.25),

        panel.grid.major = element_line(linewidth = .15, color = "grey70"),
        panel.grid.minor = element_line(linewidth = .15, color = "grey70"),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey60", color = NA)
        
    )

gr



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

