

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

df = "WHO_data.csv" |> fread()
    
    
# data cleaning -----------

df <- df[, .(`World Bank income group`, Year, 
             `Life expectancy at birth (years)  Both sexes`,
             `Life expectancy at birth (years)  Male`, 
             `Life expectancy at birth (years)  Female`)]


colnames(df) <- c("Income_group", "Year", "LE_birth_both_sexes", "LE_birth_male", "LE_birth_female")


# Filter out 'Global' and keep relevant years only if needed
df_plot <- df[Income_group != "Global"]

# Ensure Year is treated as a factor (optional, but helpful for spacing)
df_plot[, Year := as.factor(Year)]

df_plot[, Income_group := factor(Income_group, levels = c("Low-income", "High-income", "Lower-middle-income", "Upper-middle-income"))]



df_plot[, size_group := ifelse(LE_birth_both_sexes < 65, "small",
                                ifelse(LE_birth_both_sexes <= 75, "medium", "large"))]



# plot -----------

col = c('#5a8192', '#b24745','#a2a0cf', "#00429d" )


p = ggplot(df_plot, aes(x = Year, y = LE_birth_both_sexes, group = 1)) +
    
    geom_segment(
        aes(x = Year, xend = Year, y = 0, yend = LE_birth_both_sexes, col = Income_group), 
        size = 0.75
    ) +
    
    geom_point(
        aes(fill = Income_group, col = Income_group, size = size_group),
        shape = 21, 
        stroke = .2, 
        color = "white"
    ) +
    
    scale_fill_manual(values = col, guide = "none") +
    
    scale_color_manual(values = col, guide = "none") +
    
    scale_size_manual(
        values = c(small = 4, medium = 5.5, large = 7),
        labels = c("> 75", "65–75", "< 65")
    ) +
    
    facet_wrap(~Income_group, ncol = 2) +
    
    labs(
        title = "Life Expectancy Trends at Birth by Income Group from 2000 to 2019",
        subtitle = "Comparing life expectancy changes across different income groups over two decades, based on WHO data.",
        caption = "30DayChartChallenge 2025: <b> Day 24</b> 
                   | Source: <b> Life Expectancy WHO data (Kaggle) </b> 
                   | Graphic: <b>Natasa Anastasiadou</b>",
        y = "Life Expectancy (Years)",
        x = "Year",
        size = "Type"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        strip.text = element_text(face = "bold", size = 11),
        
        legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),

        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12.5, vjust = 3),
        
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 11),
        

        panel.grid.major = element_line(color = "grey65", linewidth = 0.25, linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 2, b = 2)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5,  color = "grey30", margin = margin(t = 5, b = 20)),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8.5, hjust = 1.45),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA),
        
    ) +
    
    guides(
        size = guide_legend(override.aes = list(shape = 21, color = "grey30", stroke = 0.5))
    )



p 

ggsave(
    plot = p, filename = "24_day.png",
    width = 10, height = 10, units = "in", dpi = 600
)    

