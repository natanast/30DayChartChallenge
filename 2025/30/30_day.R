

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)



# load data --------

df <- fread('world-data-2023.csv')



# data cleaning -----------

df = df[, .(Country, `Co2-Emissions`, `Forested Area (%)`)]

df = df[!is.na(`Co2-Emissions`) & !is.na(`Forested Area (%)`)]

df$`Co2-Emissions` <- as.numeric(str_replace_all(df$`Co2-Emissions`, ",", ""))

df$`Forested Area (%)` <- df$`Forested Area (%)` |> str_replace_all("%", "") |> as.numeric()


# Keep only the top 15 countries by CO2 emissions
top_emitters <- df[order(-`Co2-Emissions`)][1:15]


top_emitters[, `Co2-Emissions` := round(`Co2-Emissions` / 100000)]


df_long <- melt(top_emitters, id.vars = "Country", 
                measure.vars = c("Forested Area (%)", "Co2-Emissions"),
                variable.name = "Measurment",
                value.name = "Value")


df_long[, Value := ifelse(Measurment == "Co2-Emissions", -Value, Value)]


# Reorder countries by total combined value for better visual structure (optional)
df_long[, Country := factor(Country, levels = unique(df[order(`Co2-Emissions`)]$Country))]




# plot ------


# Left: CO2 emissions (negative to go left)
p1 <- ggplot(top_emitters, aes(x = -`Co2-Emissions`, y = Country)) +
    geom_col(fill = "#F44336", width = 0.6) +
    scale_x_continuous(labels = abs) +
    # labs(x = "CO₂ Emissions (×100K)", y = NULL) +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank())

# Middle: Country labels (as a separate text-only plot)
p_labels <- ggplot(top_emitters, aes(y = Country, x = 0, label = Country)) +
    geom_text(hjust = 0.5, size = 4.2) +
    theme_void()

# Right: Forested Area
p2 <- ggplot(top_emitters, aes(x = `Forested Area (%)`, y = Country)) +
    geom_col(fill = "#4CAF50", width = 0.6) +
    # labs(x = "Forested Area (%)", y = NULL) +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major.y = element_blank())

# Combine plots with patchwork
(p1 | p_labels | p2) +
    plot_layout(widths = c(1, 0.25, 1)) +
    plot_annotation(
        title = "Butterfly Chart: CO₂ Emissions vs Forested Area",
        subtitle = "Top 15 CO₂ emitting countries and their forest coverage",
        theme = theme(plot.title = element_text(size = 14, face = "bold"),
                      plot.subtitle = element_text(size = 11))
    )






p = ggplot(df1, aes(x = N , y = country_code, fill = country_code)) +
    
    geom_bar(stat = "identity", width = 0.5, alpha = 0.85) +
    
    coord_radial(start = 0, inner.radius = 0.2) +
    
    scale_fill_manual(values = rev(col)) +
    
    scale_x_continuous(
        limits = c(0, 3900), 
        expand = c(0, 0)
    ) +

    labs(
        title = "Global UFO Sightings: A Closer Look at the Frequency by Country",
        subtitle = "This visualization highlights the number of UFO sightings across various countries, <br>with a focus on countries with notable sighting frequencies. <br>
                    The <b>United States</b> leads with over <b>85,000</b> reports (not shown).",
        x = "",
        y = "",
        caption = "30DayChartChallenge 2025: <b> Day 29</b>
                   | Source: <b> UFO Sightings Redux (TidyTuesday)</b>
                   | Graphic: <b>Natasa Anastasiadou</b>",
    ) +
    

    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        
        axis.text.y = element_blank(),

        panel.grid.major.x = element_line(color = "grey78", linewidth = 0.25, linetype = "dashed", lineend = "round"),
        panel.grid.major.y = element_line(color = "grey78", linewidth = 0.25),
        
        panel.grid.minor.x = element_line(color = "grey78", linewidth = 0.25, linetype = "dashed", lineend = "round"),
        panel.grid.minor.y = element_blank(),
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 15, hjust = 0.5, color = "grey30", margin = margin(t = 5, b = 10)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 10, hjust = 1.75),
        
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey90", color = NA)
    ) 


p 

ggsave(
    plot = p, filename = "29_day.png",
    width = 10, height = 10, units = "in", dpi = 600
)    

