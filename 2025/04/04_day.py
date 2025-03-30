

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


# Load and clean data
soccer = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-04-04/soccer21-22.csv')


import pandas as pd
import matplotlib.pyplot as plt

# Load dataset
url = "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-08-22/epl_matches.csv"
soccer = pd.read_csv(url)

# Calculate total goals per team
home_goals = soccer.groupby("HomeTeam")["FTHG"].sum()
away_goals = soccer.groupby("AwayTeam")["FTAG"].sum()
total_goals = home_goals.add(away_goals, fill_value=0).reset_index()
total_goals.columns = ["Team", "Total Goals"]

# Get top 5 and bottom 5 teams
top_teams = total_goals.nlargest(5, "Total Goals")
bottom_teams = total_goals.nsmallest(5, "Total Goals")

# Assign values: top teams are positive, bottom teams are negative
bottom_teams["Total Goals"] *= -1  # Flip bottom teams to negative

# Combine data
goal_plot_data = pd.concat([top_teams, bottom_teams])

# Plot
fig, ax = plt.subplots(figsize=(10, 6))

# Draw lines from 0 to value
for index, row in goal_plot_data.iterrows():
    ax.plot([0, row["Total Goals"]], [row["Team"], row["Team"]], color="gray", lw=1)

# Add lollipops (scatter points)
ax.scatter(goal_plot_data["Total Goals"], goal_plot_data["Team"],
           color=["steelblue" if x > 0 else "red" for x in goal_plot_data["Total Goals"]],
           s=100, edgecolors="black", zorder=3)

# Customization
ax.set_xlabel("Total Goals")
ax.set_ylabel("Team")
ax.set_title("Premier League Teams with Most & Least Goals", fontsize=14)
ax.axvline(0, color="black", linewidth=1.2)  # Center baseline at 0
ax.grid(axis="x", linestyle="--", alpha=0.7)

# Show plot
plt.show()
