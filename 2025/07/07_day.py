
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import plotnine as gg


# Load data --------

pokemon_df = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')


# clean data ------

# font family
plt.rcParams["font.family"] = "Candara"

pokemon_df.columns



# Select relevant columns
stats = ['attack', 'defense', 'speed']
selected_types = ['fire', 'water', 'grass', 'electric']
# col = ['#78C850', '#C03028', '#6890F0', '#f4cd2c']
# 
col = ['#f4cd2c', '#C03028', '#78C850', '#6890F0']


df = pokemon_df[['pokemon', 'type_1'] + stats]
df = df[df['type_1'].isin(selected_types)].dropna()


# Capitalize first letter of type_1 and stat columns
df['type_1'] = df['type_1'].str.capitalize()  # Capitalize first letter of type_1



# Reshape the data for a long-format dataframe to plot with ggplot
df_long = df.melt(id_vars=['pokemon', 'type_1'], value_vars=stats, var_name='stat', value_name='value')

df_long['stat'] = df_long['stat'].str.capitalize()

g = (
    gg.ggplot(df_long)

    + gg.aes(x = "stat", y = "value", fill = "type_1")

    + gg.geom_violin(size = 0.25, alpha = 0.4, show_legend = False, trim = False)

    + gg.geom_jitter(size = 2.5, width = 0.1, height = 0, alpha = 0.9, color = 'white', show_legend = False, stroke=0.2)

    + gg.facet_wrap('~type_1')

    + gg.scale_fill_manual(values=col)

    + gg.theme_minimal()

    + gg.labs(
        title = "Pokémon Stat Comparison Across Types",
        subtitle = "A closer look at key stats (Attack, Defense, and Speed) for various Pokémon types (Electric, Fire, Grass, Water).",
        caption = "Source: {pokemon} R package | Graphic: Natasa Anastasiadou"
    )

    + gg.theme(

        legend_position= "none", 
        axis_title = gg.element_blank(),

        axis_text_x = gg.element_text(margin={'t': 40, 'units': 'pt'}, family="Candara", size = 8),
        axis_text_y = gg.element_text(margin={'r': 40, 'units': 'pt'}, family="Candara", size = 8),

        plot_title = gg.element_text(size = 12, color = 'black', weight = 'bold', hjust = 0.5, family="Candara"),
        plot_subtitle = gg.element_text(size= 10, color = 'black', hjust = 0.5, family="Candara"),
        plot_caption =  gg.element_text(size= 7, color = 'black', hjust = 1, family="Candara"),

        plot_background=gg.element_rect(fill='white', color='white'),
        panel_background=gg.element_rect(fill='white', color='white'),

        panel_grid_major_y = gg.element_line(color = '#e5e5e5', alpha = 0.9, size = 0.75),
        panel_grid_major_x = gg.element_line(color = '#e5e5e5', alpha = 0.9, size = 0.75),
        panel_border = gg.element_rect(color = '#e5e5e5', alpha = 0.7, size = 0.5),

        strip_text_x = gg.element_text(size = 8, family="Candara"),
        axis_ticks = gg.element_line(color='#e5e5e5', alpha = 0.7),

        figure_size=(8, 4.5)
    ) 
)

g



# Save the plot with custom size and resolution
gg.ggsave(g, "day_05.png", width=10, height=6, dpi=300)

