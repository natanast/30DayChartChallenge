

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
label_data <- df[, .(Year, Starters)]
label_data[, id := .I]  # Assign an ID from 1 to N

number_of_bar <- nrow(label_data)

# Calculate angles
angle <- 90 - 360 * (label_data$id - 0.5) / number_of_bar
label_data[, angle := ifelse(angle < -90, angle + 180, angle)]
label_data[, hjust := ifelse(angle < -90, 1, 0)]

label_data$Starters <- ifelse(label_data$Year < 2001, label_data$Starters + 1000, label_data$Starters + 6000)



# Plot -----------

gr = ggplot(df_long, aes(x = factor(Year), y = Runners, fill = Category)) +
    
    geom_bar(width = 0.7, stat = "identity", position = "dodge") +
    
    coord_radial(
        start = 0,
        inner.radius = 0.15
    ) +

    labs(
        title = "Comparison of Starters and Finishers in the London Marathon",
        fill = "Category"
    ) +
    
    
    scale_fill_manual(values = c("Starters" = "#f09a8c", "Finishers" = "#5f899d")) +
    
    geom_text(data = label_data, aes(x = id, y = Starters, label = Year, hjust = hjust, angle = angle, vjust = 0.5), 
              color = "black", fontface = "bold", alpha = 0.6, size = 2.5, , inherit.aes = FALSE) +
    

    theme_minimal() +
    
    theme(
        
        legend.position = "bottom",
        legend.title = element_text(size = 10, hjust = 0.5, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 8, family = "Candara", color = "grey30"),
        
        plot.margin = margin(20, 20, 20, 20),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),  
        axis.title = element_text(size = 14),  
        
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 16)
        
    )
    

gr


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 8, height = 8, units = "in", dpi = 600
)


