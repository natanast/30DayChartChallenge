

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

team_results <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-03-26/team-results.csv')
public_picks <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-03-26/public-picks.csv')


# data cleaning -----------

# Sort by PASE in descending order and select the top 5
top_5_teams <- team_results[order(-PASE)][1:10]

# Sort by PASE in ascending order and select the bottom 5
# bottom_5_teams <- team_results[order(PASE)][1:5]



library(ggplot2)

# Create the bar plot
ggplot(top_5_teams, aes(x = TEAM, y = CHAMPPERCENT, fill = TEAM)) +
    geom_bar(width = 0.5, stat = "identity") +
    coord_polar(start = 0) +
    # coord_flip() +  # Flip coordinates to make it horizontal
    labs(title = "Top 5 and Bottom 5 Teams based on PASE",
         x = "Team",
         y = "PASE (Performance Against Seed Expectations)",
         fill = "Group") +
    theme_minimal() +
    # scale_fill_manual(values = c("Top" = "blue", "Bottom" = "red")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels if needed


rm(list = ls())
gc()

# Load libraries -------
library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)

# Load data --------
team_results <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-03-26/team-results.csv')

# Data cleaning -----------

# Sort by PASE in descending order and select the top 10
top_10_teams <- team_results[order(-PASE)][1:10]



top_10_teams$CHAMPPERCENT <- as.numeric(top_10_teams$CHAMPPERCENT)

# Create a radial plot (polar coordinates)
ggplot(top_10_teams, aes(x = factor(TEAM), y = CHAMPPERCENT, fill = TEAM)) +
    geom_bar(stat = "identity", width = 0.8) +  # Bars represent CHAMPercent
    coord_flip() +
    # coord_polar(start = 0) +  # Convert to radial (polar) coordinates
    labs(title = "Top 10 Teams based on PASE and their CHAMPPercent",
         x = "Team",
         y = "CHAMPPercent",
         fill = "Team") +
    theme_minimal() +
    # scale_fill_manual(values = ggplot2::hue_pal()(10)) +  # Use different colors for each team
    theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
          plot.title = element_text(hjust = 0.5))  # Center the title
