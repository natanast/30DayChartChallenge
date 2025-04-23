

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

big_tech_stock_prices <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-02-07/big_tech_companies.csv')


# data cleaning -----------


big_tech_stock_prices[, year := year(date)]

selected <- c("AAPL","AMZN","NFLX","TSLA")

big_tech_filtered <- big_tech_stock_prices[stock_symbol %in% selected]


stock_yearly_avg <- big_tech_filtered[, .(avg_close = mean(close, na.rm = TRUE)), by = .(stock_symbol, year)]







# plot -----------

col = c('#5a8192', '#b24745','#a2a0cf', "#00429d" )


p = ggplot(stock_yearly_avg, aes(x = year, y = avg_close)) +
    
    geom_line(
        linewidth = .75, aes(color = stock_symbol)
    ) +
    
    geom_point(
        shape = 21, stroke = .85, size = 4,
        aes(color = stock_symbol), fill = "grey95"
    ) +
    
    scale_y_log10() +
    
    scale_x_continuous(
        breaks = c(2010, 2012, 2014, 2016, 2018, 2020, 2022)
    ) +
    
    scale_color_manual(
        values = col,
        name = "Companies"
    ) +
    
    # labs(
    #     title    = "Netflix Monthly Avg Closing Price",
    #     subtitle = "#30DayChartChallenge – TimeSeries + Log Scale",
    #     x        = "Month",
    #     y        = "Avg Closing Price (log scale)"
    # ) +
    # 
    # 
    labs(
        title = "Yearly Average Closing Prices of Big Tech Stocks",
        subtitle = "Top 5 Guardian news sections published each year from 2016 to 2021.",
        caption = "30DayChartChallenge 2025: <b> Day 23</b> | Source: <b> Big Tech Stock Prices (TidyTuesday) </b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "Avg Closing Price (log scale)",
        fill = "Type"
    ) +

    theme_minimal(base_family = "Candara") +
    
    theme(
        
        axis.title.x = element_blank(),
        
        axis.title.y = element_text(size = 12),
        
        axis.text = element_text(size = 10),
        
        legend.position = c(.85, .28),
        legend.text = element_text(size = 9),         
        legend.title = element_text(size = 10),        
        legend.key.size = unit(0.8, "lines"),
        legend.spacing.y = unit(0.5, "lines"),
        

        panel.grid.major = element_line(color = "grey75", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 14, face = "bold", hjust = 0.5, margin = margin(t = 2, b = 2)),
        plot.subtitle = element_markdown(size = 10, hjust = 0.5,  color = "grey30", margin = margin(t = 5, b = 10)),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 6, hjust = 1),
        
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
        
    )



p


ggsave(
    plot = p, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)    

