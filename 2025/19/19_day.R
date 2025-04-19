
rm(list = ls())
gc()


# libraries ---------

library(ggplot2)
library(dplyr)
library(stringr)
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

# Convert data to a data.table
setDT(big_tech_stock_prices)

# Calculate yearly average adjusted close price
yearly_avg <- big_tech_stock_prices[, .(adj_close = mean(adj_close, na.rm = TRUE)), by = .(stock_symbol, year)]


df <- merge(yearly_avg, big_tech_companies, by = "stock_symbol", all.x = TRUE)


df$company <- str_replace_all(df$company, " Inc\\.|,", "")

df$company <- ifelse(df$company == "International Business Machines Corporation", df$stock_symbol, df$company)



# plot -------

col = c('#6f6e9a',"#A65628","#b24745", "#00429d", "#396375", "#D54C45FF","#FDAE61", 
        "#e37b78","#73a2c6", "#7f9faa", "#33608CFF","#9768A5FF","#E7718AFF", "#ED7846FF")




# Plot
g <- ggplot(df, aes(x = as.numeric(year), y = adj_close, color = company)) +
    
    geom_smooth(size = 0.5, se = FALSE, span = .4) +
    # geom_smooth(method = "loess", size = 1, span = 0.5, se = FALSE) +  # Smooth the lines
    # geom_line(size = 0.5) +
    
    gghighlight(use_direct_label = FALSE,
                unhighlighted_params = list(colour = alpha("grey80", 1))) +
    
    facet_wrap('~company', ncol = 7) +
    
    scale_x_continuous( breaks = c(2010, 2016, 2022)) +
    
    scale_color_manual(values = col) +
    
    labs(
        title = "Big Tech Stock Trends Over Time",
        subtitle = "Each facet highlights one company",
        x = "",
        y = "Avg Adj Close Price"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        plot.title = element_markdown(size = 11, face = "bold", color = "grey20", hjust = 0.5, family = "Candara", margin = margin(t = 2, b = 5)),
        plot.subtitle = element_markdown(size = 9, hjust = 0.5, family = "Candara", color = "grey40", margin = margin(t = 5, b = 20)),
        plot.caption = element_markdown(margin = margin(t = 10), size = 8.5, family = "Candara", hjust = 1),
        
        panel.grid.major = element_line(linewidth = .25, color = "grey80", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_line(linewidth = .25, color = "grey80", linetype = "dashed", lineend = "round"),
        
        axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 8),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        
        plot.margin = margin(20, 20, 20, 20)
    )

g

# Save the plot with custom size and resolution
ggsave("19_day.png", plot = g, width = 10, height = 6, dpi = 600)
