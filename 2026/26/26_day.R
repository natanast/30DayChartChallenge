

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(colorspace)


# load data ------

emissions <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-05-21/emissions.csv')


# clean data ------


dt_viz <- emissions[year >= 1950, .(total_MtCO2e = sum(total_emissions_MtCO2e, na.rm = TRUE)), by = .(year, parent_entity)]


# Plot -------

gr <- ggplot(dt_viz, aes(x = year, y = total_MtCO2e)) +
    
    geom_line(
        aes(group = parent_entity), 
        color = "grey70", 
        alpha = 0.25,       
        linewidth = 0.3
    ) +
    
    
    geom_smooth(
        aes(group = 1),     
        method = "loess",   
        color = "#a33a3a", 
        fill = "#ffb5ac", 
        alpha = 0.20, 
        linewidth = 1.5,
        span = 0.3          
    ) +
    
    # Scales
    scale_y_continuous(labels = scales::comma) +
    scale_x_continuous(breaks = seq(1950, 2024, 10)) +
    
    coord_cartesian(ylim = c(0, 1500)) + 
    
    labs(
        title = "THE SIGNAL THROUGH THE SMOG",
        subtitle = "Tracking the carbon emissions of 100+ major fossil fuel producers since 1950.<br>While individual corporate trajectories are chaotic and <span style='color:#7a7a85'><b>uncertain</b></span>, the collective<br><span style='color:#d92525'><b>upward trend</b></span> is undeniable.",
        caption = "30DayChartChallenge 2026: <b> Day 26 (Uncertainties) </b> | Prompt: <b>Trend</b> | Source: <b>Carbon Majors</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "",
        y = "Total Emissions (MtCO2e)"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        # Light Theme Backgrounds
        plot.background = element_rect(fill = bg_light, color = NA),
        panel.background = element_rect(fill = bg_light, color = NA),
        
        # Grid lines (Subtle grey for readability)
        panel.grid.major.y = element_line(color = "#e0e0e6", linewidth = 0.4, linetype = "dashed"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        
        # Axis text
        axis.text = element_text(color = text_sub, size = 10, face = "bold"),
        axis.title.y = element_text(color = text_main, size = 11, margin = margin(r = 15), face = "bold"),
        
        # Titles
        plot.title = element_markdown(size = 22, face = "bold", hjust = 0, color = text_main, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0, color = text_sub, lineheight = 1.3, margin = margin(b = 25)),
        plot.caption = element_markdown(margin = margin(t = 30), size = 8, color = "#8a8a94", hjust = 1),
        
        plot.margin = margin(20, 20, 20, 20)
    )

gr


# plot --------

gr = ggplot(df_heatmap, aes(x = sending_country_code, y = receiving_country_code, fill = N)) +
    
    geom_tile(color = "grey20", linewidth = .25) +
    
    geom_shadowtext(
        aes(label = N), 
        color = "black",
        family = "Candara",
        bg.color = "grey95", 
        bg.r = .1, 
        size = 3.5 
    ) +
    
    scale_fill_stepsn(
        colors = c('#2c5769', '#6F99AD', 'grey96', '#ffb5ac', '#a33a3a'),
        breaks = c(1, 20, 50, 70, 100),  
        transform = pseudo_log_trans(base = 10),
        name = "Total Participants",
        na.value = "grey96",
        guide = guide_colorsteps(
            barheight = unit(10, "lines"), 
            barwidth = unit(0.4, "lines")
        )
    ) +

    theme_minimal(base_family = "Candara") +
    
    labs(
        title = "The Student Trade: Erasmus Mobility",
        subtitle = "Mapping the **relationships** and student **trade** between Europe's top 15 academic destinations.",
        caption = "30DayChartChallenge 2026: <b> Day 14</b>
                   | Source: <b> Erasmus Data (TidyTuesday)</b>
                   | Graphic: <b>Natasa Anastasiadou</b>",
         x = "Sending Country",
         y = "Receiving Country"
    ) +
    
    theme(
        legend.position = "right",
        legend.title.position = "left",
        
        legend.title = element_text(size = 9, angle = 90, hjust = .5, face = "bold", family = "Candara", color = "grey30"),
        legend.text = element_text(size = 8, color = "grey30"),
        
        axis.title.x = element_text(size = 10, face = "bold", margin = margin(t = 10)),
        axis.title.y = element_text(size = 10, face = "bold", margin = margin(r = 10)),
        
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        
        panel.grid = element_blank(),
        
        plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.2),
        
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "grey93", color = NA)
    )  

gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 8, units = "in", dpi = 600
)

