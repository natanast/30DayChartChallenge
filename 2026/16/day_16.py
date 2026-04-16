
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
        title = "Cause and Effect: Global Warming",
        subtitle = "Mapping the direct causation between rising carbon emissions and temperature anomalies.",
        caption = "30DayChartChallenge 2026: Day 16 | Source: NASA GISS & Carbon Majors (TidyTuesday) | Graphic: Natasa Anastasiadou",
        x = "Total Emissions (MtCO2e)",
        y = "Temperature Anomaly (°C)",
        fill = "Time Period"
    ) +
    
    theme(
        legend_position = 'right',
        legend_title = element_text(size = 8, weight = 'bold'),
        legend_text = element_text(size = 7),
        legend_background = element_rect(fill = 'none', color = 'none'), 
        
        panel_grid_major = element_line(size = 0.45, color = "#d3d3d3"),
        panel_grid_minor = element_blank(),
        
        plot_title = element_text(size = 15, weight = 'bold',  hjust = 0.5, margin = {'t': 15, 'b': 5}),
        plot_subtitle = element_text(size = 12, hjust = 0.5, color='#4d4d4d', margin = {'t': 5, 'b': 25}),
        plot_caption = element_text(size = 8, hjust = 1, margin = {'t': 35}),
                
        plot_background = element_rect(fill = '#EDEDED', color = 'none'),
        panel_background = element_rect(fill = '#EDEDED', color = 'none'),
        
        figure_size = (8, 6)
    )
)


# save --------

plot.save("plot.png", width = 8, height = 6, dpi = 300)
