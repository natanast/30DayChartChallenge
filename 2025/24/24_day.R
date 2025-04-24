

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




# plot -----------

col = c('#5a8192', '#b24745','#a2a0cf', "#00429d" )


p = ggplot(df_plot, aes(x = Year, y = LE_birth_both_sexes, group = 1)) +
    
    geom_segment(
        aes(x = Year, xend = Year, y = 0, yend = LE_birth_both_sexes), 
        color = "grey65", 
        size = .75
    ) +
    
    geom_point(
        aes(fill = Income_group),
        shape = 21, 
        stroke = .85, 
        size = 5,
        color = "white"
    ) +
    
    scale_fill_manual(values = col) +
    
    facet_wrap(~Income_group, ncol = 2) +
    
    labs(
        title = "Life Expectancy at Birth by Income Group (2000–2019)",
        subtitle = "Lollipop timeseries by WHO",
        caption = "30DayChartChallenge 2025: <b> Day 24</b> 
                   | Source: <b> Life Expectancy WHO data (Kaggle) </b> 
                   | Graphic: <b>Natasa Anastasiadou</b>",
        y = "Life Expectancy (Years)",
        x = "Year"
        # fill = "Type"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        # strip.text = element_text(face = "bold", size = 14),
        
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        

        panel.grid.major = element_line(color = "grey65", linewidth = 0.25, linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 2, b = 2)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5,  color = "grey30", margin = margin(t = 5, b = 10)),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA)
        
    )



p 

ggsave(
    plot = p, filename = "24_day.png",
    width = 9, height = 9, units = "in", dpi = 600
)    

