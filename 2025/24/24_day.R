

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


ggplot(df_plot, aes(x = Year, y = LE_birth_both_sexes, group = 1)) +
    
    geom_segment(aes(x = Year, xend = Year, y = 0, yend = LE_birth_both_sexes), color = "gray70", size = 1) +
    
    geom_point(
        shape = 21, 
        stroke = .85, 
        size = 4,
       fill = "grey95"
    ) +
    
    facet_wrap(~Income_group, ncol = 2) +
    
    labs(
        title = "Life Expectancy at Birth by Income Group (2000–2019)",
        subtitle = "Lollipop timeseries by WHO",
        caption = "30DayChartChallenge 2025: <b> Day 23</b> | Source: <b> Big Tech Stock Prices (TidyTuesday) </b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "Life Expectancy (Years)",
        x = "Year"
        # fill = "Type"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        # strip.text = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 2, b = 2)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5,  color = "grey30", margin = margin(t = 5, b = 10)),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, hjust = 1)
        
    )




# plot -----------




p = ggplot(df_plot, aes(x = year, y = avg_close)) +
    
    geom_line(
        linewidth = .75, aes(color = company)
    ) +
    
    geom_point(
        shape = 21, stroke = .85, size = 4,
        aes(color = company), fill = "grey95"
    ) +
    
    scale_y_log10() +
    
    scale_x_continuous(
        breaks = c(2010, 2012, 2014, 2016, 2018, 2020, 2022)
    ) +
    
    scale_color_manual(
        values = col,
        name = "Companies"
    ) +
    
    labs(
        title = "Annual Closing Price Trends of Big Tech Giants",
        subtitle = "Year-over-year growth of Apple, Amazon, Netflix & Tesla from 2010 to 2022",
        caption = "30DayChartChallenge 2025: <b> Day 23</b> | Source: <b> Big Tech Stock Prices (TidyTuesday) </b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "Avg Closing Price (log scale)",
        fill = "Type"
    ) +

    theme_minimal(base_family = "Candara") +
    
    theme(
        
        axis.title.x = element_blank(),
        
        axis.title.y = element_text(size = 12),
        
        axis.text = element_text(size = 10),
        
        legend.position = c(.87, .25),
        legend.text = element_text(size = 10),         
        legend.title = element_text(size = 11),        
        legend.key.size = unit(0.8, "lines"),
        legend.spacing.y = unit(0.5, "lines"),
        

        panel.grid.major = element_line(color = "grey75", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 2, b = 2)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5,  color = "grey30", margin = margin(t = 5, b = 10)),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, hjust = 1),
        
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
        
    )



p


ggsave(
    plot = p, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)    

