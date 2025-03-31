
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


# Load and clean data
soccer = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-04-04/soccer21-22.csv')


# Calculate goals scored and conceded per team
home_goals_scored = soccer.groupby("HomeTeam")["FTHG"].sum()
away_goals_scored = soccer.groupby("AwayTeam")["FTAG"].sum()
goals_scored = home_goals_scored.add(away_goals_scored, fill_value = 0)

home_goals_conceded = soccer.groupby("HomeTeam")["FTAG"].sum()
away_goals_conceded = soccer.groupby("AwayTeam")["FTHG"].sum()
goals_conceded = home_goals_conceded.add(away_goals_conceded, fill_value = 0)

# Create DataFrame
goal_comparison = pd.DataFrame({
    "Team": goals_scored.index,
    "Goals Scored": goals_scored.values,
    "Goals Conceded": -goals_conceded.values  # Make conceded goals negative
})  # Sort by scored goals

goal_comparison = goal_comparison.sort_values("Goals Scored", ascending = True)


# Plot
fig, ax = plt.subplots(figsize=(10, 6))

# Lollipops for goals scored
ax.hlines(
    goal_comparison["Team"], 
    0, 
    goal_comparison["Goals Scored"], 
    color = "grey", 
    alpha = 0.6
)
ax.scatter(
    goal_comparison["Goals Scored"], 
    goal_comparison["Team"], 
    color = "#5f899d", 
    label = "Goals Scored", 
    s = 100,
    zorder = 3
)

# Add text for goals scored
for i, (team, goals) in enumerate(zip(goal_comparison["Team"], goal_comparison["Goals Scored"])):
    ax.text(goals + 2, team, str(goals), va="center", ha="left", fontsize=10, color="#5f899d")

# Lollipops for goals conceded
ax.hlines(
    goal_comparison["Team"], 
    goal_comparison["Goals Conceded"], 
    0, 
    color = "grey", 
    alpha = 0.6
)

ax.scatter(
    goal_comparison["Goals Conceded"], 
    goal_comparison["Team"], 
    color = "#dc756e", 
    label="Goals Conceded", 
    s = 100,
    zorder = 3
)

# Add text for goals conceded
for i, (team, goals) in enumerate(zip(goal_comparison["Team"], goal_comparison["Goals Conceded"])):
    ax.text(goals - 2, team, str(abs(goals)), va="center", ha="right", fontsize=10, color="#dc756e")

# Add a vertical line at 0
ax.axvline(0, color = "black", linewidth = 1 , linestyle="dashed")


# Update x-axis labels to show absolute values (convert negatives to positives)
ax.set_xticks(ax.get_xticks())  # Keep the original tick positions
ax.set_xticklabels([f"{int(abs(tick))}" for tick in ax.get_xticks()])

# Customization
ax.set_xlabel("Goals")
ax.set_title("Premier League: Goals Scored vs. Goals Conceded", fontsize=14)
ax.legend()
ax.grid(axis="x", linestyle="--", alpha = 0.5)

plt.show()


