
rm(list = ls())
gc()


# libraries ---------

library(ggplot2)
library(dplyr)
library(stringr)
library(data.table)
library(ggtext)
library(extrafont)
library(ggstream)

# Load data -------

emissions <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-05-21/emissions.csv')


# Clean data ------

top_emitters_dt <- emissions[
    , .(total_emissions = sum(total_emissions_MtCO2e, na.rm = TRUE)),
    by = parent_entity
]


top_emitters_dt = top_emitters_dt[order(-total_emissions)]

# Extract top 10 emitter names as a character vector
top_emitters <- top_emitters_dt[1:10, parent_entity]

# Step 2: Filter and aggregate emissions over time for top emitters
df <- emissions[parent_entity %in% top_emitters]


df_plot <- df[, .(total_emissions = sum(total_emissions_MtCO2e, na.rm = TRUE)),
    by = .(year, parent_entity)
]



# plot -------

col = c('#6f6e9a',"#A65628","#b24745", "#00429d", "#396375", "#D54C45FF","#db9044", 
        "#e37b78","#73a2c6", "#7f9faa", "#33608CFF","#9768A5FF","#E7718AFF", "#ED7846FF")




# Plot
g <- ggplot(df, aes(x = as.numeric(year), y = adj_close, color = company)) +
    
    geom_smooth(size = 0.4, se = FALSE, span = .4) +
    
    geom_point(data = df_2022, aes(x = as.numeric(year), y = adj_close, fill = company), 
               size = 2.5, shape = 21, stroke = 0.1, color = "white") +  
    
    gghighlight(use_direct_label = FALSE,
                unhighlighted_params = list(colour = alpha("grey83", 1))) +
    
    geom_text(
        data = df_2022,
        aes(
            x = as.numeric(year),
            y = adj_close,
            label = round(adj_close),
            color = company
        ),
        size = 2,
        vjust = 2.5,
        hjust = 0.6,
        fontface = "bold"
    ) +
    
    facet_wrap('~company', ncol = 7) +
    

    scale_x_continuous( breaks = c(2010, 2016, 2022)) +
    
    scale_color_manual(values = col) +
    
    scale_fill_manual(values = col) +
    
    labs(
        title = "Stock Trends of Big Tech Companies Over the Years",
        subtitle = "Tracking the Annual Adjusted Closing Prices of Leading Tech Giants from 2010 to 2022",
        caption = "30DayChartChallenge 2025: <b> Day 19</b> | Source: <b> Big Tech Stock Prices (Kaggle) </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "",
        y = "Avg Adj Close Price"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        plot.title = element_markdown(size = 11, face = "bold", color = "grey20", hjust = 0.5, family = "Candara", margin = margin(t = 2, b = 5)),
        plot.subtitle = element_markdown(size = 9, hjust = 0.5, color = "grey40", margin = margin(t = 5, b = 20)),
        plot.caption = element_markdown(margin = margin(t = 5), size = 5.5, hjust = 1),
        
        panel.grid.major = element_line(linewidth = .25, color = "grey80", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_line(linewidth = .25, color = "grey80", linetype = "dashed", lineend = "round"),
        
        axis.title.y = element_text(size = 8, vjust = 5),
        
        axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5),
        axis.text.y = element_text(size = 6),
        
        strip.text.x.top = element_text(size = 7),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        
        plot.margin = margin(20, 20, 20, 20)
    ) 

g

# Save the plot with custom size and resolution
ggsave("19_day.png", plot = g, width = 10, height = 6, dpi = 600)




# Load libraries
library(tidyverse)
library(ggstream)

# Read the data
emissions <- readr::read_csv("emissions.csv")

# Step 1: Get Top 10 Emitters by Total Emissions
top_emitters <- emissions %>%
    group_by(parent_entity) %>%
    summarise(total_emissions = sum(total_emissions_MtCO2e, na.rm = TRUE)) %>%
    arrange(desc(total_emissions)) %>%
    slice_head(n = 10) %>%
    pull(parent_entity)

# Step 2: Filter for Top Emitters and Summarize per Year
stream_data <- emissions %>%
    filter(parent_entity %in% top_emitters) %>%
    group_by(year, parent_entity) %>%
    summarise(total_emissions = sum(total_emissions_MtCO2e, na.rm = TRUE)) %>%
    ungroup()

# Step 3: Create Streamgraph
ggplot(stream_data, aes(x = year, y = total_emissions, fill = parent_entity)) +
    geom_stream(type = "ridge") +
    scale_fill_brewer(palette = "Paired") +
    labs(
        title = "Top 10 Emitting Entities Over Time",
        subtitle = "Emissions measured in million tonnes of CO₂ equivalent (MtCO₂e)",
        x = "Year",
        y = "Emissions (MtCO₂e)",
        fill = "Entity",
        caption = "#30DayChartChallenge | Data: Carbon Majors via TidyTuesday"
    ) +
    theme_minimal(base_family = "Helvetica") +
    theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 8, face = "italic"),
        panel.grid.minor = element_blank()
    )
