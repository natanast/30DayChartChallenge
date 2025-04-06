
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import joypy

# Load data --------

pokemon_df = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-01/pokemon_df.csv')


# clean data ------
pokemon_df.columns


df = pokemon_df[[
    'pokemon', 'type_1', 'attack', 'defense', 'special_attack', 'special_defense', 'speed'
]]


# Drop NAs in type_1 or attack
df = df[['pokemon', 'type_1', 'attack', 'defense', 'special_attack', 'special_defense', 'speed']].dropna()

# Reshape it to long format
df_long = df.melt(
    id_vars=['pokemon', 'type_1'],
    value_vars=['attack', 'defense', 'special_attack', 'special_defense', 'speed'],
    var_name='stat',
    value_name='value'
)

# colors
col = ['#78C850', '#C03028', '#6890F0', '#F8D030', '#705898']


# Create ridge plot
plt.figure(figsize=(10, 6))

joypy.joyplot(
    data = df_long,
    by = 'stat',
    column = 'value',
    colormap = plt.cm.viridis,
    kind = 'kde',
    linewidth = 0.5,
    overlap = 3,
    alpha = 0.85,
    color = col
)

plt.title("Distribution of Pokémon Stats")
plt.xlabel("Stat Value")
plt.tight_layout()
plt.show()


# font family
plt.rcParams["font.family"] = "Candara"


# Plot
fig, ax = plt.subplots(figsize=(10, 6))

# Lollipops for goals scored
ax.hlines(
    goal_comparison["Team"], 
    0, 
    goal_comparison["Goals Scored"], 
    color = "#A6ACAD",
    alpha = 0.6
)

ax.scatter(
    goal_comparison["Goals Scored"], 
    goal_comparison["Team"], 
    color = "#5f899d", 
    label = "Goals Scored", 
    s = 80,
    zorder = 3
)

# Add text for goals scored
for i, (team, goals) in enumerate(zip(goal_comparison["Team"], goal_comparison["Goals Scored"])):
    ax.text(
        goals + 4, 
        team, 
        str(goals), 
        va = "center", 
        ha = "left", 
        fontweight = "bold",
        fontsize = 9, 
        color = "#5f899d"
    )

# Lollipops for goals conceded
ax.hlines(
    goal_comparison["Team"], 
    goal_comparison["Goals Conceded"], 
    0, 
    color = "#A6ACAD", 
    alpha = 0.6
)

ax.scatter(
    goal_comparison["Goals Conceded"], 
    goal_comparison["Team"], 
    color = "#dc756e", 
    label="Goals Conceded", 
    s = 80,
    zorder = 3
)

# Add text for goals conceded
for i, (team, goals) in enumerate(zip(goal_comparison["Team"], goal_comparison["Goals Conceded"])):
    ax.text(
        goals - 4, 
        team, 
        str(abs(goals)), 
        va = "center", 
        ha = "right", 
        fontweight = "bold",
        fontsize = 9, 
        color = "#dc756e"
    )

# Add a vertical line at 0
ax.axvline(0, color = "black", linewidth = 1 , linestyle="dashed")


# Update x-axis labels to show absolute values (convert negatives to positives)
ax.set_xticks(ax.get_xticks())  # Keep the original tick positions
ax.set_xticklabels([f"{int(abs(tick))}" for tick in ax.get_xticks()])

# Customization
ax.set_xlabel("Goals")

# legend
handles, labels = ax.get_legend_handles_labels()
ax.legend(handles[::-1], labels[::-1], loc = 'center left', bbox_to_anchor=(1.02, 0.5), fontsize = 8)  

plt.title(
    "Premier League: Goals Conceded vs Goals Scored (Season 2021-2022). ",
    fontsize = 12,
    fontweight = "bold",
    pad = 35,
    x = 0.5,
    y = 0.99
)

# Add the subtitle for clarification
plt.text(x = 0.5, y = 1.05, 
        s =  "The further a team’s points extend from the center,\n"
           "the more dominant or vulnerable they were in attack or defense.",
        ha = 'center', 
        va = 'center', 
        fontsize = 10, 
        style = 'italic', 
        color = "#8C8380",
        transform = plt.gca().transAxes
)


# Add a caption to the plot
plt.text(
    x = 0.98, y = -0.15,  # Adjust x, y to position the
    s = "Source:  Premier League Match Data 2021-2022 | Graphic: Natasa Anastasiadou",
    ha = 'center', 
    va = 'center', 
    fontsize = 6,
    fontweight = "bold",  
    style = 'italic', 
    color = "#8C8380",
    transform = plt.gca().transAxes
)



# Add gridlines only for the x-axis
ax.grid(axis="x", linestyle="--", alpha=0.2)


# Display the plot
plt.tight_layout()
plt.show()


plt.savefig("plot.png", dpi = 600, bbox_inches='tight')
