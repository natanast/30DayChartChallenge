
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
line_2 = 40

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


# plot --------

g = (
    ggplot(df_clean) +

    aes(x = 'inj', y = 'fat', color='quadrant') +
    
    geom_point(alpha = 0.85, size = 3) +

    # + facet_grid('stat~type_1')  # One row per stat, one column per type

    # + scale_fill_manual(values = col)

    # Add lines to separate the plot into 4 quadrants
    geom_vline(xintercept = line_1, color = "#7B726F", linetype = "dashed", size = 0.5) + # Vertical line at median injuries
    geom_hline(yintercept = line_2, color = "#7B726F", linetype = "dashed", size = 0.5) + # Horizontal line at median fatalities

    scale_color_manual(values={"Bottom-Left": "#6F99AD", "Bottom-Right": "#D78D50", "Top-Left": "#BC3C29", "Top-Right": "#6f6e9a"}) + # Custom colors for each quadrant
    theme_minimal(base_family = "Candara") +
    
    labs(
        title="Tornado Injuries vs Fatalities",
        # subtitle="Histograms of key stats (Attack, Defense, Speed) for Electric, Fire, Grass, and Water types",
        # caption="Source: {pokemon} R package | Graphic: Natasa Anastasiadou",
        x = 'No of Injuries',
        y = 'No of Fatalities'
    ) +

    theme(
        legend_position = 'none',
        
        # axis_text = element_text(family = 'Candara', size = 8),
        # axis_title = element_text(family = 'Candara', size = 9),

        # plot_title = element_text(size = 12, weight='bold', family='Candara', ha='center'),
        # plot_subtitle = element_text(size = 10, family='Candara', ha='center'),
        # plot_caption = element_text(size = 7, family='Candara', ha='right'),
        
        # strip_text = element_text(size = 8, family='Candara'),
        
        panel_grid_major = element_line(color = '#c9c9c9', alpha = 0.75, size = 0.65, linetype = "dashed"),
        panel_grid_minor = element_blank(),

        # panel_border = element_rect(color = '#e5e5e5', alpha = 0.7, size = 0.5, fill = None),
        
        plot_background = element_rect(fill = '#e9e9e9', color = '#e9e9e9'),
        panel_background = element_rect(fill = '#e9e9e9', color = '#e9e9e9'),
        
        # axis_ticks= element_blank(),
        
        figure_size = (10, 6)
    )
)

g

# Save the plot with custom size and resolution
ggsave(g, "08_day.png", width = 10, height = 6, dpi = 600)

