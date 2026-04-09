
import numpy as np
import pandas as pd
import seaborn as sns
from plotnine import *


# Load data --------

df = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-07-15/bl_funding.csv')

# clean data ------

col = ['#6890F0', '#C03028']

# Reshape your data from wide to long
df_long = pd.melt(
    df,
    id_vars = 'year',
    value_vars = ['nominal_gbp_millions', 'total_y2000_gbp_millions'],
    var_name = 'type',
    value_name = 'funding'
)

# Create mapping for display labels and colors
label_map = {
    'nominal_gbp_millions': 'Nominal',
    'total_y2000_gbp_millions': 'Inflation-adjusted'
}

color_map = {
    'nominal_gbp_millions': '#6890F0',
    'total_y2000_gbp_millions': '#C03028'
}


# plot --------

plot1 = (
    ggplot(df_long, aes(x = 'year', y = 'funding', color = 'type')) +
    
    geom_line(size = 1, linejoin = 'round') +

    geom_point(size = 3, shape = 'o', stroke = 0.55, fill = 'white') +
    
    scale_color_manual(values = color_map, labels = label_map) +

    scale_fill_manual(values = color_map, labels = label_map) +

    theme_minimal(base_family = 'Candara') +

    labs(
        title = "British Library Funding: The Illusion of Growth",
        subtitle = "Nominal budgets may rise, but after adjusting for inflation, real funding has dropped significantly over two decades.",
        caption = "Source: British Library funding | Graphic: Natasa Anastasiadou",
        y = "Funding (£ Millions)",
        x = "Year",
        color = "",  # Remove "type" legend title
        fill = ""
    ) +

    theme(

        legend_text=element_text(size=8),
        legend_title=element_text(size=9),
        legend_key_size=10,
        legend_spacing=6,
         
        panel_grid_major=element_line(size=0.45, color="#e4e4e3"),  # major gridlines
        panel_grid_minor=element_blank(),

        plot_title = element_text(size = 12, weight = 'bold', hjust = 0.5),
        plot_subtitle = element_text(size = 10, hjust = 0.5),
        plot_caption = element_text(size = 7, hjust = 1),
        plot_background = element_rect(fill = 'white', color = 'white'),
        panel_background = element_rect(fill = 'white', color = 'white'),

        figure_size = (8, 6)
    )
)

# Save
plot1.save("plot.png", width = 8, height = 6, dpi = 300)



import numpy as np
import pandas as pd
from plotnine import *

# 1. Load Data --------
# Note: You would merge TidyTuesday temperature data with a CO2 dataset (like Mauna Loa data).
# Assuming 'df' is already merged and cleaned with columns: ['year', 'co2_ppm', 'temp_anomaly']

# Simulated sample data structure for the code to run
df = pd.DataFrame({
    'year': range(1958, 2024),
    'co2_ppm': np.linspace(315, 420, 66) + np.random.normal(0, 2, 66),
    'temp_anomaly': np.linspace(-0.1, 1.1, 66) + np.random.normal(0, 0.1, 66)
})

# 2. Plot --------
plot2 = (
    ggplot(df, aes(x='co2_ppm', y='temp_anomaly')) +
    
    # The Causal Trendline (The "Cause" element)
    geom_smooth(method='lm', color='#C03028', linetype='dashed', se=False, size=1) +
    
    # The Data Points (Colored by Year to show time progression)
    geom_point(aes(fill='year'), size=3.5, shape='o', stroke=0.55, color='white') +
    
    # Continuous color scale for the years (using a cool-to-warm palette)
    scale_fill_cmap(cmap_name='viridis') + 
    
    theme_minimal(base_family='Candara') +
    
    labs(
        title="The Warming Effect: CO2 and Global Temperatures",
        subtitle="As atmospheric CO2 concentration increases, global surface temperature anomalies predictably rise.",
        caption="Data: TidyTuesday (Global Temperatures) & Mauna Loa CO2 | Graphic: Natasa Anastasiadou",
        x="Atmospheric CO2 (Parts Per Million)",
        y="Temperature Anomaly (°C)",
        fill="Year"
    ) +
    
    theme(
        legend_position='right',
        legend_title=element_text(size=9, weight='bold'),
        legend_text=element_text(size=8),
        legend_key_height=30,
        
        panel_grid_major=element_line(size=0.45, color="#e4e4e3"),
        panel_grid_minor=element_blank(),
        
        plot_title=element_text(size=14, weight='bold', hjust=0),
        plot_subtitle=element_text(size=10, hjust=0, margin={'b': 15}),
        plot_caption=element_text(size=7, hjust=1, color="#666666"),
        
        plot_background=element_rect(fill='white', color='white'),
        panel_background=element_rect(fill='white', color='white'),
        
        figure_size=(9, 6)
    )
)

# 3. Save --------
plot2.save("causation_plot.png", width=9, height=6, dpi=300)