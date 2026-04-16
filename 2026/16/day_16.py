
# Import libraries --------
import pandas as pd
from plotnine import *


# Load Data --------
df_temp = pd.read_csv("GLB.Ts+dSST.csv")

df_co2 = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-05-21/emissions.csv')


# Clean Data --------

# Rename temperature columns
df_temp = df_temp[['Year', 'J-D']].rename(columns = {'Year': 'year', 'J-D': 'temp_anomaly'})

# Drop the incomplete 2026 year and force the column back to numbers (floats)
df_temp = df_temp[df_temp['year'] != 2026]
df_temp['temp_anomaly'] = df_temp['temp_anomaly'].astype(float)

# Group CO2 emissions by year to get global totals
df_co2_yearly = df_co2.groupby('year', as_index = False)['total_emissions_MtCO2e'].sum()

df = pd.merge(df_co2_yearly, df_temp, on = 'year')
df = df.dropna()

# Create custom historical eras using pd.cut()
bins = [1800, 1940, 1960, 2000, 2030]
era_labels = ['1880-1939', '1940-1959', '1960-1999', '2000-2022']
df['era'] = pd.cut(df['year'], bins = bins, labels = era_labels, right = False)


# plot --------

col = [
    '#2c5769',  
    '#6F99AD',  
    '#ffb39a',  
    '#ab403f'   
]

plot = (
    ggplot(df, aes(x = 'total_emissions_MtCO2e', y = 'temp_anomaly')) +
    
    geom_point(
        aes(fill = 'era'), 
        size = 3.5, 
        shape = 'o', 
        stroke = 0.25, 
        color = 'white'
    ) +

    geom_smooth(
        method = 'lm', 
        color = '#C03028', 
        linetype = 'dashed', 
        se = True, 
        size = 1
    ) +
    
    scale_fill_manual(values = col) +
    
    theme_minimal(base_family = 'Candara') +
    
    labs(
        title = "The Warming Effect: Carbon Emissions and Global Temperatures",
        subtitle = "As major corporate carbon emissions increase, global surface temperature anomalies rise.",
        caption = "Data: NASA GISS & TidyTuesday (2024) | Graphic: Natasa Anastasiadou",
        x = "Total Emissions (MtCO2e)",
        y = "Temperature Anomaly (°C)",
        fill = "Time Period"
    ) +
    
    theme(
        legend_position = 'right',
        legend_title = element_text(size = 9, weight = 'bold'),
        legend_text = element_text(size = 8),
        legend_background = element_rect(fill = 'none', color = 'none'), 
        
        panel_grid_major = element_line(size = 0.45, color = "#d3d3d3"),
        panel_grid_minor = element_blank(),
        
        plot_title = element_text(size = 14, weight = 'bold', hjust = 0),
        plot_subtitle = element_text(size = 10, hjust = 0),
        plot_caption = element_text(size = 7, hjust = 1, color = "#666666"),
        
        plot_background = element_rect(fill = '#EDEDED', color = 'none'),
        panel_background = element_rect(fill = '#EDEDED', color = 'none'),
        
        figure_size = (9, 6)
    )
)

plot

# 6. Save --------
plot.save("plot.png", width=9, height=6, dpi=300)
