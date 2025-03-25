

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(extrafont)
library(ggtext)
library(paletteer)


# load data --------

london_marathon <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-04-25/london_marathon.csv')


# data cleaning ------

df <- london_marathon[, .(Year, Starters, Finishers)]

df <- df[!is.na(Starters), ]

df_long <- melt(df, id.vars = "Year", measure.vars = c("Starters", "Finishers"), 
                variable.name = "Category", value.name = "Runners")

# labels -------

# Labels Preparation
label_data <- df[, .(Year, Finishers)]
label_data[, id := .I]  # Assign an ID from 1 to N

number_of_bar <- nrow(label_data)

# Calculate angles
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
label_data[, angle := ifelse(angle < -90, angle + 180, angle)]
label_data[, hjust := ifelse(angle < -90, 1, 0)]


# Plot -----------

gr = ggplot(df_long, aes(x = factor(Year), y = Runners, fill = Category)) +
    
    geom_bar(width = 0.6, stat = "identity", position = "dodge") +
    
    coord_radial(
        start = 0,
        inner.radius = 0.15
    ) +

    labs(
        title = "Comparison of Starters and Finishers in the London Marathon",
        fill = "Category"
    ) +
    
    
    scale_fill_manual(values = c("Starters" = "#F39B7F", "Finishers" = "#3a5cbc")) +
    
    geom_text(data = label_data, aes(x = id, y = Finishers, label = Year, hjust = hjust, angle = angle, vjust = 0.5), 
              color = "black", fontface = "bold", alpha = 0.6, size = 2.5, , inherit.aes = FALSE) +
    
    # # Adjusted text labels
    # geom_text(data = label_data, aes(x = factor(Year), y = max(df_long$Runners) + 5000, 
    #                                  label = Year, angle = angle, hjust = hjust), 
    #           color = "black", fontface = "bold", alpha = 0.8, size = 3) +
    # 
    # 
    theme_minimal() +
    
    theme(
        plot.margin = margin(20, 20, 20, 20),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),  
        axis.title = element_text(size = 14),  
        
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        legend.text = element_text(size = 7),  
        legend.title = element_text(size = 10),  
        plot.title = element_text(size = 16)
        
    )
    




gr


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 9.5, height = 9, units = "in", dpi = 600
)


