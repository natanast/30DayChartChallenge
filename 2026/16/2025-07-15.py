
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



import pandas as pd
from plotnine import *

# 1. Load and Clean Temperature Data (TidyTuesday 2023-07-11) --------
df_temp = pd.read_csv("GLB.Ts+dSST.csv")

# We only need the Year and the 'J-D' (Jan-Dec average) column
df_temp = df_temp[['Year', 'J-D']].rename(columns={'Year': 'year', 'J-D': 'temp_anomaly'})


# 2. Load and Clean Emissions Data (TidyTuesday 2024-05-21) --------
url_co2 = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-05-21/emissions.csv'
df_co2 = pd.read_csv(url_co2)

# Group by year to get total global corporate emissions per year
df_co2_yearly = df_co2.groupby('year', as_index=False)['total_emissions_MtCO2e'].sum()


# 3. Merge Datasets --------
# Combine both datasets where the years match
df = pd.merge(df_co2_yearly, df_temp, on='year')
df = df.dropna() # Drop any rows with missing values


# 4. Plot --------
plot = (
    ggplot(df, aes(x='total_emissions_MtCO2e', y='temp_anomaly')) +
    
    # Causal Trendline
    geom_smooth(method='lm', color='#C03028', linetype='dashed', se=False, size=1) +
    
    # Data Points mapped to time
    geom_point(aes(fill='year'), size=3.5, shape='o', stroke=0.55, color='white') +
    
    scale_fill_cmap(cmap_name='viridis') + 
    
    theme_minimal(base_family='Candara') +
    
    labs(
        title="The Warming Effect: Carbon Emissions and Global Temperatures",
        subtitle="As major corporate carbon emissions increase, global surface temperature anomalies rise.",
        caption="Data: TidyTuesday (2023 & 2024) | Graphic: Natasa Anastasiadou",
        x="Total Emissions (MtCO2e)",
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
        plot_subtitle=element_text(size=10, hjust=0),
        plot_caption=element_text(size=7, hjust=1, color="#666666"),
        
        plot_background=element_rect(fill='white', color='white'),
        panel_background=element_rect(fill='white', color='white'),
        
        figure_size=(9, 6)
    )
)

plot

# 5. Save --------
plot.save("causation_plot_real_data.png", width=9, height=6, dpi=300)