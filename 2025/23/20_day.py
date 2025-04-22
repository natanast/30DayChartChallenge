
import matplotlib.pyplot as plt
import pandas as pd
from plotnine import *


# Load data --------

daily_accidents = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-22/daily_accidents.csv')
daily_accidents_420 = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-22/daily_accidents_420.csv', parse_dates=['date'])


# clean data ------


# Mark True for both July 4 and 4/20
daily_accidents_420['e420'] = ((daily_accidents_420['date'].dt.month == 4) & (daily_accidents_420['date'].dt.day == 20)) | ((daily_accidents_420['date'].dt.month == 7) & (daily_accidents_420['date'].dt.day == 4))

# Keep only the rows where e420 is True
df_july4_420 = daily_accidents_420[daily_accidents_420['e420']]


df_july4_420['date'] = pd.to_datetime(df_july4_420['date']).dt.date




# Group by date and sum fatalities_count
df_july4_420_aggregated = df_july4_420.groupby('date').agg({'fatalities_count': 'sum'}).reset_index()

df_july4_420_aggregated['Group'] = df_july4_420_aggregated['date'].apply(lambda x: '20_04' if x.month == 4 and x.day == 20 else '04_06')

# Now df_july4_420_aggregated will have one row per date with the total fatalities count



# Plot --------

from plotnine import scale_y_log10


import numpy as np

# Apply log transformation to fatalities_count
df_july4_420_aggregated['log_fatalities'] = np.log1p(df_july4_420_aggregated['fatalities_count'])

# Volcano plot idea: compare log-transformed fatalities count for 20/4 vs July 4
df_july4_420_aggregated['difference'] = df_july4_420_aggregated.groupby('Group')['log_fatalities'].transform('mean') - df_july4_420_aggregated['log_fatalities']

# Plot the log-transformed fatalities vs difference
g = (
    ggplot(df_july4_420_aggregated) +
    aes(x='difference', y='log_fatalities', color='Group') +
    geom_point(size=3) +
    theme_minimal() +
    labs(title="Volcano Plot of Fatalities Count (Log Scale)",
         x="Difference Between Groups",
         y="Log(Fatalities Count)")
)
g


g = (
    ggplot(df_july4_420_aggregated) +

    aes(x = 'date', y = 'fatalities_count', fill = 'Group') + 

    geom_point(size = 3) +

    scale_y_log10() +  # Apply log scale to y-axis

    expand_limits(y=[150]) +

    theme_minimal() +

    theme(
        axis_text_x=element_text(rotation=45, hjust=1), 

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


g




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

