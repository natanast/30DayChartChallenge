
import numpy as np
import pandas as pd
from plotnine import *


# Load data --------

pokemon_df = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')


# clean data ------

plt.rcParams["font.family"] = "Candara"

stats = ['attack', 'defense', 'speed']
selected_types = ['fire', 'water', 'grass', 'electric']

col = ['#f4cd2c', '#C03028', '#78C850', '#6890F0']


df = pokemon_df[['pokemon', 'type_1'] + stats]
df = df[df['type_1'].isin(selected_types)].dropna()


# Capitalize first letter 
df['type_1'] = df['type_1'].str.capitalize()  

# long format
df_long = df.melt(id_vars=['pokemon', 'type_1'], value_vars=stats, var_name='stat', value_name='value')

df_long['stat'] = df_long['stat'].str.capitalize()


# plot --------

g = (
    ggplot(df_long)

    + aes(x='value', fill='type_1')

    # + coord_flip()
    
    + geom_histogram(binwidth = 10, alpha = 0.85, color = 'white', size = 0.15)

    + facet_grid('stat~type_1')  # One row per stat, one column per type

    + scale_fill_manual(values = col)

    + theme_minimal()
    
    + labs(
        title="Pokémon Stat Distributions by Type",
        subtitle="Histograms of key stats (Attack, Defense, Speed) for Electric, Fire, Grass, and Water types",
        caption="Source: {pokemon} R package | Graphic: Natasa Anastasiadou",
        x = 'Pokémon Stat Score',
        y = 'Number of Pokémon'
    )
    + theme(
        legend_position = 'none',
        
        axis_text = element_text(family = 'Candara', size = 8),
        axis_title = element_text(family = 'Candara', size = 9),

        plot_title = element_text(size = 12, weight='bold', family='Candara', ha='center'),
        plot_subtitle = element_text(size = 10, family='Candara', ha='center'),
        plot_caption = element_text(size = 7, family='Candara', ha='right'),
        
        strip_text = element_text(size = 8, family='Candara'),
        
        panel_grid_major = element_line(color = '#e5e5e5', alpha = 0.75, size = 0.65, linetype = "dashed"),
        # panel_grid_major = element_blank(),
        panel_grid_minor = element_blank(),

        panel_border = element_rect(color = '#e5e5e5', alpha = 0.7, size = 0.5, fill = None),
        
        plot_background = element_rect(fill = 'white', color = 'white'),
        panel_background = element_rect(fill = 'white', color = 'white'),
        
        axis_ticks= element_blank(),
        
        figure_size = (10, 6)
    )
)

g

# Save the plot with custom size and resolution
ggsave(g, "08_day.png", width = 10, height = 6, dpi = 600)

