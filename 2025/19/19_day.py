
# import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from plotnine import *


# Load data --------

big_tech_stock_prices = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-02-07/big_tech_stock_prices.csv')
big_tech_companies = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-02-07/big_tech_companies.csv')


# clean data ------

df_clean = tornados.dropna(subset=['inj', 'fat'])



# Convert the 'date' column to datetime if not already
big_tech_stock_prices['date'] = pd.to_datetime(big_tech_stock_prices['date'])

# Extract the year from the date
big_tech_stock_prices['year'] = big_tech_stock_prices['date'].dt.year

# Calculate the yearly average closing price per company
yearly_avg = big_tech_stock_prices.groupby(['stock_symbol', 'year'])['adj_close'].mean().reset_index()



# plot --------

g = (
    ggplot(yearly_avg) +

    aes(x = 'inj', y = 'fat', color='quadrant') +
    
    geom_point(alpha = 0.85, size = 3, stroke = 0.15) +

    geom_vline(xintercept = line_1, color = "#7B726F", linetype = "dashed", size = 0.5) + # Vertical line at median injuries
    geom_hline(yintercept = line_2, color = "#7B726F", linetype = "dashed", size = 0.5) + # Horizontal line at median fatalities

    scale_color_manual(values={"Bottom-Left": "#6F99AD", "Bottom-Right": "#D78D50", "Top-Left": "#BC3C29", "Top-Right": "#6f6e9a"}) + # Custom colors for each quadrant
    theme_minimal(base_family = "Candara") +

    geom_text(
            aes(label='label'),  # Use the new 'label' column
            size=8, 
            color='black', 
            nudge_y=5  # Adjust vertical text position
        ) +
    
    labs(
        title = "Tornado Injuries vs Fatalities: A State-by-State Breakdown",
        subtitle = "Exploring the relationship between the number of injuries and fatalities caused by tornadoes across U.S. states.",
        caption = "Source: Tornado dataset | Graphic: Natasa Anastasiadou",
        x = 'Number of Injuries',
        y = 'Number of Fatalities'
    ) +

    theme(
        legend_position = 'none',
        
        axis_text = element_text(family = 'Candara', size = 8),
        axis_title = element_text(family = 'Candara', size = 10),
 
        plot_title = element_text(size = 12, weight='bold', ha='center'),
        plot_subtitle = element_text(size = 10, ha='center'),
        plot_caption = element_text(size = 7, ha='right'),
                
        panel_grid_major = element_line(color = '#c9c9c9', alpha = 0.75, size = 0.65, linetype = "dashed"),
        panel_grid_minor = element_blank(),
        
        plot_background = element_rect(fill = '#e9e9e9', color = '#e9e9e9'),
        panel_background = element_rect(fill = '#e9e9e9', color = '#e9e9e9'),
                
        figure_size = (10, 6)
    ) +

    #  Add text labels for each quadrant with static text
    geom_text(x = line_1 - 350, y = line_2 - 5, label = "Low Injuries, Low Fatalities", color = "#6F99AD", size = 9, fontweight = 'bold') +
    geom_text(x = line_1 + 1000, y = line_2 - 5 , label="High Injuries, Low Fatalities", color = "#D78D50", size = 9, fontweight = 'bold') +
    geom_text(x = line_1 - 350, y = line_2 + 80, label="Low Injuries, High Fatalities", color = "#BC3C29", size = 9, fontweight = 'bold') +
    geom_text(x = line_1 + 1000, y = line_2 + 80, label="High Injuries, High Fatalities", color = "#6f6e9a", size = 9, fontweight='bold')

)

g

# Save the plot with custom size and resolution
ggsave(g, "12_day.png", width = 10, height = 6, dpi = 600)

