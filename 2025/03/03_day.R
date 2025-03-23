

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

# winners <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-04-25/winners.csv')
london_marathon <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-04-25/london_marathon.csv')


# data cleaning ------

df <- london_marathon[, .(Year, Starters, Finishers)]

df <- df[!is.na(Starters), ]

df_long <- melt(df, id.vars = "Year", measure.vars = c("Starters", "Finishers"), 
                variable.name = "Category", value.name = "Runners")


# Plot -----------
# Plot the data
gr = ggplot(df_long, aes(x = factor(Year), y = Runners, fill = Category)) +
    geom_bar(stat = "identity", position = "dodge") +
    
    coord_radial(
        start = 0,
        inner.radius = 0.35
    ) +

    labs(title = "Comparison of Starters and Finishers in the London Marathon",
         x = "Year", y = "Count", fill = "Category") +
    

    # theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    
    theme_minimal() +
    
    theme(
        plot.margin = margin(20, 20, 20, 20),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank(),  
        axis.title = element_text(size = 14),  
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        legend.text = element_text(size = 7),  
        legend.title = element_text(size = 10),  
        plot.title = element_text(size = 16)
        
    ) +
    
    scale_fill_manual(values = c("Starters" = "#F39B7F", "Finishers" = "#3a5cbc"))





# df |>
#     ggplot(aes(x = Year, y = percent, fill = Background)) +
#     
#     geom_bar(position = "stack", stat = "identity", width = .8, alpha = 0.9) +
#     
#     labs(
#         y = "",
#         x = "", 
#         title = "IG expanded clonality"
#     ) +
#     
#     scale_fill_manual(
#         values = c("Expanded" = "#F39B7F", "Polyclonal" = "#3a5cbc"),
#         na.value = "transparent",  # Keeps NA but makes it invisible
#         na.translate = FALSE       # Prevents "NA" from appearing in the legend
#     ) +
#     
#     coord_radial(
#         start = 0,
#         inner.radius = 0.5
#     ) +
#     
#     # Add lines at specific x-axis positions (using factor of 'id' for the x-axis)
#     geom_segment(aes(x = 1, y = -4, xend = 19, yend = -4), color = "grey30", size = 1) +
#     geom_segment(aes(x = 22, y = -4, xend = 40, yend = -4), color = "grey30", size = 1) +
#     geom_segment(aes(x = 43, y = -4, xend = 61, yend = -4), color = "grey30", size = 1) +
#     
#     # Add text on top of these lines
#     geom_text(aes(x = 10, y = -15, label = "His"), size = 3, hjust = 0.5) +
#     geom_text(aes(x = 32, y = -15, label = "KTR-T1"), size = 3, hjust = 0.5) +
#     geom_text(aes(x = 53, y = -30, label = "KTR-T2"), size = 3, hjust = 0.5, vjust = 0.5) +
#     
#     
#     
#     # Add numeric labels at positions 0, 25, 50, and 100 on the radial axis
#     annotate("text", x = 0, y = 5, label = "0%", size = 2, hjust = 0.5, color = "black") +
#     annotate("text", x = 0, y = 25, label = "25%", size = 2, hjust = 0.5, color = "black") +
#     annotate("text", x = 0, y = 50, label = "50%", size = 2, hjust = 0.5, color = "black") +
#     annotate("text", x = 0, y = 75, label = "75%", size = 2, hjust = 0.5, color = "black") +
#     annotate("text", x = 0, y = 100, label = "100%", size = 2, hjust = 0.5, color = "black") +
#     
#     theme_minimal() +
#     
#     theme(
#         plot.margin = margin(20, 20, 20, 20),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),  
#         axis.title = element_text(size = 14),  
#         
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         
#         legend.text = element_text(size = 7),  
#         legend.title = element_text(size = 10),  
#         plot.title = element_text(size = 16)
#         
#     ) +
#     
#     geom_text(data = label_data, aes(x = id, y = 110, label = Patient, hjust = 0.2, vjust = 0.3), 
#               color = "black", fontface = "bold", alpha = 0.6, size = 2.5, angle = angle1, inherit.aes = FALSE) +
#     
#     
#     geom_text(data = label_data, aes(x = id + 21, y = 110, label = Patient, hjust = 0.6, vjust = 0.3), 
#               color = "black", fontface = "bold", alpha = 0.6, size = 2.5, angle = angle2, inherit.aes = FALSE) +
#     
#     geom_text(data = label_data, aes(x = id + 42, y = 110, label = Patient, hjust = 0.7, vjust = 0.3), 
#               color = "black", fontface = "bold", alpha = 0.6, size = 2.5, angle = angle3, inherit.aes = FALSE)
# 



# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 9.5, height = 9, units = "in", dpi = 600
)


