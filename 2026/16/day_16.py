
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


# merge datasets --------

df = pd.merge(df_co2_yearly, df_temp, on='year')
df = df.dropna() # Drop any rows with missing values

# Create a categorical 'era' column (20-year chunks)
era_start = df['year'] // 20 * 20
df['era'] = era_start.astype(str) + "-" + (era_start + 19).astype(str)

# Create a categorical 'era' column (40-year chunks)
era_start = df['year'] // 40 * 40
df['era'] = era_start.astype(str) + "-" + (era_start + 39).astype(str)

col = [
    '#2c5769',  # 1880-1899 (Your dark muted blue)
    '#4d788b',  # 1900-1919 (Bridge)
    '#6F99AD',  # 1920-1939 (Your light muted blue)
    '#8db7cc',  # 1940-1959 (Bridge: Muted warm grey/taupe)
    '#ffb39a',  # 1960-1979 (Your soft warm peach)
    '#ef9588',  # 1980-1999 (Bridge: Soft dusty rose)
    '#df7775',  # 2000-2019 (Your muted rose red)
    '#ab403f'   # 2020-2039 (Your dark muted red)
]

col = [
    '#2c5769',  # 1880-1919 (Deep muted blue)
    '#6F99AD',  # 1920-1959 (Light muted blue)
    '#ffb39a',  # 1960-1999 (Soft warm peach)
    '#ab403f'   # 2000-2039 (Deep muted red)
]


# plot --------

plot = (
    ggplot(df, aes(x='total_emissions_MtCO2e', y='temp_anomaly')) +
    
    geom_point(aes(fill='era'), size=3.5, shape='o', stroke=0.25, color='white') +

    geom_smooth(method='lm', color='#C03028', linetype='dashed', se=True, size=1) +
    
    scale_fill_manual(values=col) +
    
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
        
        plot_background=element_rect(fill='#EDEDED', color='none'),
        panel_background=element_rect(fill='#EDEDED', color='none'),
        
        figure_size=(9, 6)
    )
)

plot

# save --------

plot.save("plot.png", width=9, height=6, dpi=300)




# Import libraries --------
import pandas as pd
from plotnine import *

# 1. Load Data --------
# Ensure "GLB.Ts+dSST.csv" is in the same folder as your script
df_temp = pd.read_csv("GLB.Ts+dSST.csv")

url_co2 = 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-05-21/emissions.csv'
df_co2 = pd.read_csv(url_co2)

# 2. Clean Data --------
# Rename temperature columns
df_temp = df_temp[['Year', 'J-D']].rename(columns={'Year': 'year', 'J-D': 'temp_anomaly'})

# Drop the incomplete 2026 year and force the column back to numbers (floats)
df_temp = df_temp[df_temp['year'] != 2026]
df_temp['temp_anomaly'] = df_temp['temp_anomaly'].astype(float)

# Group CO2 emissions by year to get global totals
df_co2_yearly = df_co2.groupby('year', as_index=False)['total_emissions_MtCO2e'].sum()

# 3. Merge & Group Eras --------
# Combine datasets and drop missing values
df = pd.merge(df_co2_yearly, df_temp, on='year')
df = df.dropna()

# Create custom historical eras using pd.cut()
bins = [1800, 1940, 1960, 2000, 2030]
era_labels = ['1880-1939', '1940-1959', '1960-1999', '2000-2022']
df['era'] = pd.cut(df['year'], bins=bins, labels=era_labels, right=False)

# 4. Define Palette --------
col = [
    '#2c5769',  # 1880-1939 (Deep muted blue)
    '#6F99AD',  # 1940-1959 (Light muted blue)
    '#ffb39a',  # 1960-1999 (Soft warm peach)
    '#ab403f'   # 2000-2022 (Deep muted red)
]

# 5. Plot --------
plot = (
    ggplot(df, aes(x='total_emissions_MtCO2e', y='temp_anomaly')) +
    
    # Data points mapped to the new eras
    geom_point(aes(fill='era'), size=3.5, shape='o', stroke=0.25, color='white') +

    # Causal trendline
    geom_smooth(method='lm', color='#C03028', linetype='dashed', se=True, size=1) +
    
    # Apply custom palette
    scale_fill_manual(values=col) +
    
    theme_minimal(base_family='Candara') +
    
    labs(
        title="The Warming Effect: Carbon Emissions and Global Temperatures",
        subtitle="As major corporate carbon emissions increase, global surface temperature anomalies rise.",
        caption="Data: NASA GISS & TidyTuesday (2024) | Graphic: Natasa Anastasiadou",
        x="Total Emissions (MtCO2e)",
        y="Temperature Anomaly (°C)",
        fill="Time Period"
    ) +
    
    theme(
        # Legend styling
        legend_position='right',
        legend_title=element_text(size=9, weight='bold'),
        legend_text=element_text(size=8),
        legend_background=element_rect(fill='none', color='none'), 
        
        # Gridlines (slightly darker grey to contrast with the background)
        panel_grid_major=element_line(size=0.45, color="#d3d3d3"),
        panel_grid_minor=element_blank(),
        
        # Text alignment
        plot_title=element_text(size=14, weight='bold', hjust=0),
        plot_subtitle=element_text(size=10, hjust=0),
        plot_caption=element_text(size=7, hjust=1, color="#666666"),
        
        # Soft grey background
        plot_background=element_rect(fill='#EDEDED', color='none'),
        panel_background=element_rect(fill='#EDEDED', color='none'),
        
        figure_size=(9, 6)
    )
)

plot

# 6. Save --------
plot.save("plot.png", width=9, height=6, dpi=300)
