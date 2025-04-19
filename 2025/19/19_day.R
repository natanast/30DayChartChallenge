
rm(list = ls())
gc()


# libraries ---------

library(ggplot2)
library(dplyr)
# library(readr)
library(data.table)
library(gghighlight)
library(ggtext)
library(extrafont)

# Load data ---------
big_tech_stock_prices <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-02-07/big_tech_companies.csv')


# Clean data
big_tech_stock_prices$date <- as.Date(big_tech_stock_prices$date)

# Extract year
big_tech_stock_prices$year <- format(big_tech_stock_prices$date, "%Y")

big_tech_stock_prices <- big_tech_stock_prices[year != "2023"]

# Calculate yearly average adjusted close price
yearly_avg <- big_tech_stock_prices %>%
    group_by(stock_symbol, year) %>%
    summarise(adj_close = mean(adj_close, na.rm = TRUE)) %>%
    ungroup()


# Plot
g <- ggplot(yearly_avg, aes(x = as.numeric(year), y = adj_close, color = stock_symbol)) +
    
    geom_line(size = 0.85) +
    
    gghighlight(use_direct_label = FALSE,
                unhighlighted_params = list(colour = alpha("grey85", 1))) +
    
    facet_wrap('~stock_symbol', ncol = 7) +
    
    scale_x_continuous( breaks = c(2010, 2016, 2022)) +
    
    labs(
        title = "Big Tech Stock Trends Over Time",
        subtitle = "Each facet highlights one company",
        x = "",
        y = "Avg Adjusted Close Price"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5),
        
        axis.text.x = element_text(size = 10, angle = 90, vjust = 0.5),
        
        plot.margin = margin(20, 20, 20, 20)
    )

g

# Save the plot with custom size and resolution
ggsave("19_day.png", plot = g, width = 10, height = 6, dpi = 600)
