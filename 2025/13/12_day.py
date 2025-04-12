
# import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from plotnine import *



# Load data --------

tornados = pd.read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-05-16/tornados.csv')


# clean data ------

# plt.rcParams["font.family"] = "Candara"

# Remove rows with missing values in 'inj' or 'fat'
df_clean = tornados.dropna(subset=['inj', 'fat'])


# Calculate median values to draw the lines
line_1 = 450
line_2 = 60

# Create a new column to categorize points into quadrants based on their position
df_clean['quadrant'] = pd.cut(
    df_clean['inj'],
    bins=[-float('inf'), line_1, float('inf')], 
    labels=["Left", "Right"]
)

df_clean['quadrant'] = pd.Series([
    'Bottom-Left' if (row['inj'] <= line_1 and row['fat'] <= line_2) else 
    'Bottom-Right' if (row['inj'] > line_1 and row['fat'] <= line_2) else 
    'Top-Left' if (row['inj'] <= line_1 and row['fat'] > line_2) else 
    'Top-Right' for _, row in df_clean.iterrows()
])



# Create a new column for the formatted label
df_clean['label'] = df_clean['yr'].apply(lambda x: f"Year {x}")

# Filter the data for high injuries and fatalities categories
df_clean['label'] = df_clean.apply(
    lambda row: f"Year {row['yr']}" if (
        (row['inj'] > line_1 + 500 and row['fat'] > line_2) or  # High Injuries, High Fatalities
        (row['inj'] > line_1 + 500 and row['fat'] <= line_2)    # High Injuries, Low Fatalities
    ) else None,
    axis=1
)

# plot --------

g = (
    ggplot(df_clean) +

    aes(x = 'inj', y = 'fat', color='quadrant') +
    
    geom_point(alpha = 0.85, size = 3, stroke = 0.15) +

    geom_vline(xintercept = line_1, color = "#7B726F", linetype = "dashed", size = 0.5) + # Vertical line at median injuries
    geom_hline(yintercept = line_2, color = "#7B726F", linetype = "dashed", size = 0.5) + # Horizontal line at median fatalities

    scale_color_manual(values={"Bottom-Left": "#6F99AD", "Bottom-Right": "#D78D50", "Top-Left": "#BC3C29", "Top-Right": "#6f6e9a"}) + # Custom colors for each quadrant
    theme_minimal(base_family = "Candara") +

    geom_text(
            aes(label='label'),  # Use the new 'label' column
            size=8, 
            color='black', 
            nudge_y=5  # Adjust vertical text position
        ) +
    
    labs(
        title = "Tornado Injuries vs Fatalities: A State-by-State Breakdown",
        subtitle = "Exploring the relationship between the number of injuries and fatalities caused by tornadoes across U.S. states.",
        caption = "Source: Tornado dataset | Graphic: Natasa Anastasiadou",
        x = 'Number of Injuries',
        y = 'Number of Fatalities'
    ) +

    theme(
        legend_position = 'none',
        
        axis_text = element_text(family = 'Candara', size = 8),
        axis_title = element_text(family = 'Candara', size = 10),
 
        plot_title = element_text(size = 12, weight='bold', ha='center'),
        plot_subtitle = element_text(size = 10, ha='center'),
        plot_caption = element_text(size = 7, ha='right'),
                
        panel_grid_major = element_line(color = '#c9c9c9', alpha = 0.75, size = 0.65, linetype = "dashed"),
        panel_grid_minor = element_blank(),
        
        plot_background = element_rect(fill = '#e9e9e9', color = '#e9e9e9'),
        panel_background = element_rect(fill = '#e9e9e9', color = '#e9e9e9'),
                
        figure_size = (10, 6)
    ) 

    #  Add text labels for each quadrant with static text
    # geom_text(x = line_1 - 350, y = line_2 - 10, label = "Low Injuries, Low Fatalities", color = "#6F99AD", size = 8, fontweight = 'bold') +
    # geom_text(x = line_1 + 1000, y = line_2 - 5 , label="High Injuries, Low Fatalities", color = "#D78D50", size = 8, fontweight = 'bold') +
    # geom_text(x = line_1 - 350, y = line_2 + 5, label="Low Injuries, High Fatalities", color = "#BC3C29", size = 8, fontweight = 'bold') +
    # geom_text(x = line_1 + 1000, y = line_2 + 80, label="High Injuries, High Fatalities", color = "#6f6e9a", size = 8, fontweight='bold')

)

g

# Save the plot with custom size and resolution
ggsave(g, "08_day.png", width = 10, height = 6, dpi = 600)

