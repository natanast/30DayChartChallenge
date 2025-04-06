
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import joypy
import seaborn as


# Load data --------

pokemon_df = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')


# clean data ------

# font family
plt.rcParams["font.family"] = "Candara"

pokemon_df.columns



# Select relevant columns
stats = ['attack', 'defense', 'speed']
selected_types = ['fire', 'water', 'grass', 'electric']
col = ['#78C850', '#C03028', '#6890F0', '#f4cd2c']


df = pokemon_df[['pokemon', 'type_1'] + stats]
df = df[df['type_1'].isin(selected_types)].dropna()

# Long format
df_long = df.melt(
    id_vars=['pokemon', 'type_1'],
    value_vars=stats,
    var_name='stat',
    value_name='value'
)

# Create FacetGrid
g = sns.FacetGrid(
    df_long,
    col='type_1',
    col_order=selected_types,
    sharey=True,
    height=4,
    aspect=0.8
)

# Loop over axes and draw violin + points manually
for ax, t, c in zip(g.axes.flat, selected_types, col):
    subset = df_long[df_long['type_1'] == t]
    
    sns.violinplot(
        data =subset,
        x = 'stat',
        y = 'value',
        inner = None,
        linewidth = 0.8,
        edgecolor = 'black',
        bw = 0.3,
        color = c,
        ax = ax, 
        alpha = 0.4
    )
    
    sns.stripplot(
        data = subset,
        x = 'stat',
        y = 'value',
        color = c,
        size = 5,
        jitter = .15,
        alpha = 0.9,
        ax = ax,
        edgecolor = 'white',  # This sets the edgecolor to white
        linewidth = 0.25        # This controls the thickness of the edge
    
    )
    
    ax.set_title(t.capitalize())
    ax.set_xlabel("")
    ax.set_ylabel("")

# Final layout
plt.suptitle("Distribution of Pokémon Stats by Type", fontsize=16)
plt.tight_layout()
plt.show()





# plt.title(
    # "Premier League: Goals Conceded vs Goals Scored (Season 2021-2022). ",
    # fontsize = 12,
    # fontweight = "bold",
    # pad = 35,
    # x = 0.5,
    # y = 0.99
# )
# 
Add the subtitle for clarification
# plt.text(x = 0.5, y = 1.05, 
        # s =  "The further a team’s points extend from the center,\n"
        #    "the more dominant or vulnerable they were in attack or defense.",
        # ha = 'center', 
        # va = 'center', 
        # fontsize = 10, 
        # style = 'italic', 
        # color = "#8C8380",
        # transform = plt.gca().transAxes
# )
# 
# 
Add a caption to the plot
# plt.text(
    # x = 0.98, y = -0.15,  # Adjust x, y to position the
    # s = "Source:  Premier League Match Data 2021-2022 | Graphic: Natasa Anastasiadou",
    # ha = 'center', 
    # va = 'center', 
    # fontsize = 6,
    # fontweight = "bold",  
    # style = 'italic', 
    # color = "#8C8380",
    # transform = plt.gca().transAxes
# )
# 
# 
# 
Add gridlines only for the x-axis
# ax.grid(axis="x", linestyle="--", alpha=0.2)
# 
# 
Display the plot
# plt.tight_layout()
# plt.show()
# 
# 
# plt.savefig("plot.png", dpi = 600, bbox_inches='tight')
# 