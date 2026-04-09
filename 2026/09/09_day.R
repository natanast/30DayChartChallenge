

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(palmerpenguins)
library(ggplot2)
library(ggtext)
library(extrafont)
library(colorspace)


# load data ------
 
dt <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-08-05/income_inequality_processed.csv")


# clean data -----

dt_clean <- dt[!is.na(gini_mi_eq) & !is.na(gini_dhi_eq)]

dt_recent <- dt_clean[, .SD[which.max(Year)], by = Entity]

dt_melt <- melt(
    dt_recent, 
    id.vars = c("Entity", "Code", "Year"),
    measure.vars = c("gini_mi_eq", "gini_dhi_eq"),
    variable.name = "income_type",
    value.name = "gini_index"
)


dt_melt[, income_type := fifelse(income_type == "gini_mi_eq", 
                                 "Pre-Tax (Market Income)", 
                                 "Post-Tax (Disposable Income)")]


dt_melt[, income_type := factor(income_type, levels = c("Pre-Tax (Market Income)", "Post-Tax (Disposable Income)"))]


#  plot -----

col <- c(
    "Pre-Tax (Market Income)" = "#b24745",       
    "Post-Tax (Disposable Income)" = "#5a8192"   
)

gr <- ggplot(dt_melt, aes(x = gini_index, fill = income_type, color = income_type)) +
    
    geom_density(alpha = 0.75, linewidth = 0.6) +
    
    scale_fill_manual(values = col |> lighten(.15)) +
    scale_color_manual(values = col |> darken(.15)) +
    
    
    scale_x_continuous(
        limits = c(0.15, 0.75),
        breaks = seq(0.2, 0.7, by = 0.1),
        labels = c("0.20\n(More Equal)", "0.30", "0.40", "0.50", "0.60", "0.70\n(More Unequal)")
    ) +
    
    labs(
        title = "The Impact of Taxation on Global Wealth",
        subtitle = "The Gini coefficient measures inequality (0 = perfect equality, 1 = maximum inequality).<br>Notice how taxation shifts the distribution across 124 countries toward equality.",
        caption = "30DayChartChallenge 2026: <b> Day 9 </b> | Source: <b> Our World in Data (TidyTuesday)</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Gini Coefficient", 
        y = "Density of Countries"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 9, face = "bold", color = "grey30"),
        legend.key.size = unit(0.5, "cm"),
        legend.margin = margin(b = 10),
        
        axis.title.x = element_text(size = 12, face = "bold", color = "grey30", margin = margin(t = 15)),
        axis.title.y = element_text(size = 12, face = "bold", color = "grey30", margin = margin(r = 15)),
        
        axis.text.x = element_text(size = 11, face = "bold", color = "black"),
        axis.text.y = element_blank(), 
        
        panel.grid.major.x = element_line(linewidth = 0.4, color = "grey80", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 10, hjust = 1),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

gr

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 10, units = "in", dpi = 600
)

