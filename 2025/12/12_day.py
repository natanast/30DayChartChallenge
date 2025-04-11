
# import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from plotnine import *


# Load data --------

df = pd.read_csv('obesity_data.csv')


# clean data ------

plt.rcParams["font.family"] = "Candara"

col = ['#f4cd2c', '#C03028', '#78C850', '#6890F0']


# Calculate the average obesity percentage across the states
average_obesity = df['Obesity'].mean()


# Calculate the difference from the average for each state
df['Difference_from_Avg'] = df['Obesity'] - average_obesity

# plot --------

g = (
    ggplot(df)

    + aes(x='NAME', y = 'Difference_from_Avg', fill='NAME') 

    + geom_col(width = 0.5, alpha = 0.8)

    + coord_flip()

    + theme_minimal(base_family = "Candara")

    + theme(
        legend_position = "none",  # No legend
        axis_text_x = element_text(),
        plot_background = element_rect(fill = 'white', color = 'white'),
        panel_background = element_rect(fill = 'white', color = 'white')
    )
)

#     # + coord_flip()
    
#     + geom_histogram(binwidth = 10, alpha = 0.85, color = 'white', size = 0.15)

#     + facet_grid('stat~type_1')  # One row per stat, one column per type

#     + scale_fill_manual(values = col)

#     + theme_minimal()
    
#     + labs(
#         title="Pokémon Stat Distributions by Type",
#         subtitle="Histograms of key stats (Attack, Defense, Speed) for Electric, Fire, Grass, and Water types",
#         caption="Source: {pokemon} R package | Graphic: Natasa Anastasiadou",
#         x = 'Pokémon Stat Score',
#         y = 'Number of Pokémon'
#     )
#     + theme(
#         legend_position = 'none',
        
#         axis_text = element_text(family = 'Candara', size = 8),
#         axis_title = element_text(family = 'Candara', size = 9),

#         plot_title = element_text(size = 12, weight='bold', family='Candara', ha='center'),
#         plot_subtitle = element_text(size = 10, family='Candara', ha='center'),
#         plot_caption = element_text(size = 7, family='Candara', ha='right'),
        
#         strip_text = element_text(size = 8, family='Candara'),
        
#         panel_grid_major = element_line(color = '#e5e5e5', alpha = 0.75, size = 0.65, linetype = "dashed"),
#         # panel_grid_major = element_blank(),
#         panel_grid_minor = element_blank(),

#         panel_border = element_rect(color = '#e5e5e5', alpha = 0.7, size = 0.5, fill = None),
        
#         plot_background = element_rect(fill = 'white', color = 'white'),
#         panel_background = element_rect(fill = 'white', color = 'white'),
        
#         axis_ticks= element_blank(),
        
#         figure_size = (10, 6)
#     )
# )

g

# Save the plot with custom size and resolution
ggsave(g, "08_day.png", width = 10, height = 6, dpi = 600)
