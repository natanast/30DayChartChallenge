

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

# Step 2: Create 'year' column from the 'date' column
big_tech_stock_prices[, year := year(date)]

selected <- c("AAPL","AMZN","NFLX","TSLA")

big_tech_filtered <- big_tech_stock_prices[stock_symbol %in% selected]

# Step 4: Calculate yearly average closing prices for each selected stock
stock_yearly_avg <- big_tech_filtered[, .(avg_close = mean(close, na.rm = TRUE)), by = .(stock_symbol, year)]






# plot -----------

ggplot(stock_yearly_avg, aes(x = year, y = avg_close, color = stock_symbol )) +
    
    geom_line() +
    # geom_segment(aes(xend = year, y = min(avg_close, na.rm=TRUE)/10, yend = avg_close),
    #              color = "gray70") +
    
    geom_point(size = 2.5) +
    
    scale_y_log10() +
    
    labs(
        title    = "Netflix Monthly Avg Closing Price",
        subtitle = "#30DayChartChallenge – TimeSeries + Log Scale",
        x        = "Month",
        y        = "Avg Closing Price (log scale)"
    ) +
    
    theme_minimal(base_size = 14) +
    
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1)
    )







col = c("#00429d", "#73a2c6", '#396375', '#5a8192', '#6f6e9a', '#a2a0cf', '#e37b78',"#A65628", '#b24745')


grid_colors = c(
    "American Crow"             = '#6f6e9a',
    "American Goldfinch"        = "#A65628",
    "American Robin"            = "#b24745",
    "Belted Kingfisher"         = "#00429d",
    "Blue Jay"                  = "#396375",
    "Downy Woodpecker"          = '#a2a0cf',
    "Green Heron"               = "#FDAE61",
    "Northern Flicker"          = "#e37b78",
    "Ruby-throated Hummingbird" = "#73a2c6", 
    "Scarlet Tanager"           = "#7f9faa"
)



# plot ---------

# Create the stacked bar plot
p = ggplot(top_5_sections, aes(x = webPublicationDate, y = N, fill = sectionName)) +
    
    geom_bar(stat = "identity", width = 0.5) +
    
    scale_fill_manual(values = col) +
    
    labs(
        title = "Media & Time: Tracking Section Popularity by Year",
        subtitle = "Top 5 Guardian news sections published each year from 2016 to 2021.",
        caption = "30DayChartChallenge: <b> Day 18</b> | Source: <b> Guardian News Articles (Kaggle) </b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "No. of Article", 
        fill = "Type"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        axis.title.x = element_blank(),
        
        axis.title.y = element_text(size = 9),
        
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        
        legend.text = element_text(size = 7),         # Smaller text
        legend.title = element_text(size = 8),        # Optional: smaller title
        legend.key.size = unit(0.8, "lines"),         # Smaller boxes
        legend.spacing.y = unit(0.5, "lines"),
        
        legend.position = "right",
        # legend.title = element_text(size = 8),
        # legend.text = element_text(size = 7),
        
        panel.grid.major = element_line(color = "grey75", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 14, face = "bold", hjust = 0.5, margin = margin(t = 2, b = 2)),
        plot.subtitle = element_markdown(size = 10, hjust = 0.5,  color = "grey30", margin = margin(t = 5, b = 10)),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 6, hjust = 1.38),
        
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
        
    )



p


ggsave(
    plot = g, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)    






library(data.table)
library(lubridate)
library(ggplot2)

# 1. Prepare the data
setDT(big_tech_stock_prices)

# Filter to the four symbols
selected <- c("AAPL","AMZN","NFLX","TSLA")
dt4 <- big_tech_stock_prices[stock_symbol %in% selected]

# Compute monthly averages
dt4_monthly <- dt4[,
                   .(avg_close = mean(close, na.rm=TRUE)),
                   by = .(stock_symbol,
                          year  = year(date),
                          month = month(date))
][
    , year_month := as.Date(sprintf("%04d-%02d-01", year, month))
][
    order(stock_symbol, year_month)
]

# 2. Plot faceted lollipop with log scale
ggplot(dt4_monthly, aes(x = year_month, y = avg_close)) +
    geom_line() +
    # geom_segment(aes(xend = year_month,
    #                  y    = min(avg_close, na.rm=TRUE)/10,
    #                  yend = avg_close),
    #              color = "gray70",
    #              linewidth = 0.05) +
    geom_point(aes(color = stock_symbol), size = 1) +
    scale_y_log10() +
    # facet_wrap(~ stock_symbol, ncol = 2, scales = "free_y") +
    labs(
        title    = "Monthly Avg Closing Price for Four Big Tech Stocks",
        subtitle = "#30DayChartChallenge – TimeSeries + Log Scale",
        x        = "Month",
        y        = "Avg Closing Price (log10)",
        color    = "Company"
    ) +
    theme_minimal(base_size = 14) +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
    )
