

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(ggrepel)


# load data ------


# load data ------
dt <- "https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/plate31/data.csv" |>
    fread()

# Add grouping column based on your output
dt[, grouping := c("POOR", "FAIR", "FAIR", "COMFORTABLE", "COMFORTABLE", "COMFORTABLE", "WELL-TO-DO")]

# clean data -----

# We only want to melt the spending categories
# Based on your output, these are: Rent, Food, Clothes, Tax, Other
spending_cols <- c("Rent", "Food", "Clothes", "Tax", "Other")

dt_plot <- melt(dt, 
                id.vars = c("Class", "grouping"), 
                measure.vars = spending_cols,
                variable.name = "category", 
                value.name = "pct")

# Match the stacking order: Other (left) -> Tax -> Clothes -> Food -> Rent (right)
dt_plot[, Class := factor(Class, levels = rev(unique(dt$Class)))]
dt_plot[, category := factor(category, levels = c("Other", "Tax", "Clothes", "Food", "Rent"))]

# plot --------

# Du Bois Colors
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
        
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5, family = "serif"),
        plot.subtitle = element_text(size = 11, hjust = 0.5, family = "mono", margin = margin(b=30)),
        
        plot.margin = margin(20, 20, 20, 20)
    ) +
    coord_cartesian(clip = "off")

gr



# clean data -----


# plot --------


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
        # plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        # plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        # plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
#         
#         plot.background = element_rect(fill = "grey95", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )



# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)


