

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(ggrepel)
library(extrafont)
library(ggrepel)

# load data ------

dt <- "https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/plate31/data.csv" |>
    fread()


# clean data -----

dt[, grouping := c("POOR", "FAIR", "FAIR", "COMFORTABLE", "COMFORTABLE", "COMFORTABLE", "WELL-TO-DO")]


spending_cols <- c("Rent", "Food", "Clothes", "Tax", "Other")

dt_plot <- melt(dt, 
                id.vars = c("Class", "grouping"), 
                measure.vars = spending_cols,
                variable.name = "category", 
                value.name = "pct")


dt_plot[, Class := factor(Class, levels = rev(unique(dt$Class)))]
dt_plot[, category := factor(category, levels = c("Other", "Tax", "Clothes", "Food", "Rent"))]

# plot --------

cols <- c(
    "Rent"    = "#000000", # Black
    "Food"    = "#7876B1", # Red
    "Clothes" = "#F39B7F", # Pink
    "Tax"     = "#7AA6DC", # Blue
    "Other"   = "#ADB6B6"  # Gold
)


gr <- ggplot(dt_plot, aes(x = pct, y = Class, fill = category)) +
    
    geom_col(width = 0.7, color = "white", linewidth = 0.1) +
    
    
    geom_text(aes(label = ifelse(pct > 0, paste0(pct, "%"), ""),
                  color = category), 
              position = position_stack(vjust = 0.5),
              size = 3, 
              fontface = "bold", 
              family = "mono") +
    
    
    geom_text(data = unique(dt_plot[, .(Class, grouping)]), 
              aes(x = 102, y = Class, label = grouping, fill = NULL), 
              hjust = 0, 
              size = 3.5, 
              fontface = "bold", 
              family = "mono",
              color = "grey20") +
    
    
    scale_fill_manual(values = cols, guide = guide_legend(nrow = 1, reverse = TRUE)) +
    
    
    scale_color_manual(values = c(
        "Rent"    = "white", 
        "Food"    = "white", 
        "Clothes" = "black", 
        "Tax"     = "white", 
        "Other"   = "black"
    ), guide = "none") +
    
    scale_x_continuous(limits = c(0, 125), breaks = seq(0, 100, 10), expand = c(0,0)) +
    
    labs(
        title = "Income and expenditure of 150 negro families in Atlanta, GA., U.S.A.",
        subtitle = "The relationship between income and expenditure categories.",
        caption = "30DayChartChallenge 2026: <b> Day 17</b>
                   | Source: <b> </b>
                   | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Percent",
        y = NULL
        
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        plot.background = element_rect(fill = "#e1d8c9", color = NA),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(family = "mono", size = 9, face = "bold"),
        
        axis.text.y = element_text(face = "bold", size = 9, color = "black"),
        axis.text.x = element_text(family = "mono", size = 9),
        axis.line.x = element_line(color = "black"),
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 15, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20)
    ) 

gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 8, units = "in", dpi = 600
)


