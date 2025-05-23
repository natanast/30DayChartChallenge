
# import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from plotnine import *


# Load data --------

df = pd.read_csv('global_urbanization_climate_metrics.csv')

df['year'].unique()


# clean data ------

df_long = df[['year', 'country', 'urban_pop_perc', 'rural_pop_perc']].melt(
    id_vars=['year', 'country'],
    value_vars=['urban_pop_perc', 'rural_pop_perc'],
    var_name='Population_Type',
    value_name='Percentage'
)

# Clean the 'Population Type' column
df_long['Population_Type'] = df_long['Population_Type'].str.replace('_pop_perc', '').str.capitalize()


# selected_countries = ['NGA', 'EGY', 'USA', 'CAN', 'CHN', 'DEU', 'GRC', 'BRA', 'AUS'] 

selected_countries = ['United States', 'China', 'Greece', 'Germany', 'Brazil', 'Australia', 'Nigeria', 'Canada', 'South Africa']


df_filtered = df_long[df_long['country'].isin(selected_countries)]


# Plot --------
g = (
    ggplot(df_filtered) +

    aes(x='year', y='Percentage', fill='Population_Type') + 

    geom_area(alpha=0.7) +

    facet_wrap('~country') +  

    scale_fill_manual(values={"Urban": "#6F99AD", "Rural": "#BC3C29"}) + # Custom colors for each quadrant

    labs(
        title = "Urbanization Over Time Across the Globe",
        subtitle = "Tracking the shift from rural to urban living in selected countries (1960–2023)",
        caption = "30DayChartChallenge 2025: Day 20 | Source: Urbanization & Climate Metrics Insights (Kaggle) | Graphic: Natasa Anastasiadou",
        x = '',
        y = 'Population (%)',
        fill = 'Type'
    ) +

    theme_minimal() +

    theme(
        axis_text = element_text(family = 'Candara', size = 8),
        axis_title = element_text(family = 'Candara', size = 10),
        
        plot_title = element_text(size = 12, weight = 'bold', ha = 'center'),
        plot_subtitle = element_text(size = 10, ha = 'center'),
        plot_caption = element_text(size = 6, ha = 'right'),
        
        panel_grid_major = element_line(color = '#c9c9c9', alpha = 0.75, size = 0.65, linetype="dashed"),
        plot_background = element_rect(fill = '#f8f8f8', color = '#f8f8f8'),

        legend_title=element_text(size = 8),
        legend_text=element_text(size = 7),
        
        figure_size=(10, 6)
    )
)

# Show the plot
g

#  Save the plot with custom size and resolution
ggsave(g, "20_day.png", width = 10, height = 6, dpi = 600)

