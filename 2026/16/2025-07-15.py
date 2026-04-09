
# import libraries ---- 

import pandas as pd
from plotnine import *


# Load data --------

df_temp = pd.read_csv("GLB.Ts+dSST.csv")

df_co2 = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-05-21/emissions.csv')


# clean data ------

# Rename the columns
df_temp = df_temp[['Year', 'J-D']].rename(columns={'Year': 'year', 'J-D': 'temp_anomaly'})

df_temp = df_temp[df_temp['year'] != 2026]

df_temp['temp_anomaly'] = df_temp['temp_anomaly'].astype(float)


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
        caption="Data: NASA GISS & TidyTuesday (2024) | Graphic: Natasa Anastasiadou",
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