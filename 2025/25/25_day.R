

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(dplyr)


# load data --------

df = "world_risk_index.csv" |> fread()
    
    
# data cleaning -----------

df = df[, .(Region, WRI, Exposure, `Lack of Coping Capabilities`, `WRI Category`)]


# Country name mapping
country_map <- c(
    "Albanien" = "Albania", "Griechenland" = "Greece", "Vereinigte Staaten" = "United States", 
    "Vereinigte Staaten von Amerika" = "United States", "Philippinen" = "Philippines", 
    "Bangladesch" = "Bangladesh", "Solomon Islands" = "Solomon Islands", 
    "Kambodscha" = "Cambodia", "Madagaskar" = "Madagascar", "Indonesien" = "Indonesia", 
    "Elfenbeinküste" = "Ivory Coast", "Zentralafrikanische Republik" = "Central African Republic", 
    "Venezuela" = "Venezuela", "Mosambik" = "Mozambique", "Mauretanien" = "Mauritania", 
    "Simbabwe" = "Zimbabwe", "Äthiopien" = "Ethiopia", "Algerien" = "Algeria", 
    "Usbekistan" = "Uzbekistan", "Kasachstan" = "Kazakhstan", "Irak" = "Iraq", 
    "Türkei" = "Turkey", "Tschad" = "Chad", "Jamaika" = "Jamaica", 
    "Südafrika" = "South Africa", "Südkorea" = "South Korea", "Australien" = "Australia", 
    "Neuseeland" = "New Zealand", "Brasilien" = "Brazil", "Kanada" = "Canada", 
    "Deutschland" = "Germany", "Vereinigtes Königreich" = "United Kingdom", 
    "Spanien" = "Spain", "Frankreich" = "France", "Italien" = "Italy", "Portugal" = "Portugal", 
    "Polen" = "Poland", "Russische Föderation" = "Russia", "China" = "China", 
    "Vietnam" = "Vietnam", "Syrien" = "Syria", "Irland" = "Ireland", 
    "Österreich" = "Austria", "Belgien" = "Belgium", "Schweiz" = "Switzerland", 
    "Luxemburg" = "Luxembourg", "Finnland" = "Finland", "Norwegen" = "Norway"
)

# Replace inconsistent names in Region column using stringr's str_replace_all
for (key in names(country_map)) {
    df$Region <- str_replace_all(df$Region, key, country_map[key])
}


# Aggregate data by Region (country), calculating the average of relevant columns
df_avg_country <- df[, .(
    avg_Exposure = mean(Exposure, na.rm = TRUE),
    avg_Coping = mean(`Lack of Coping Capabilities`, na.rm = TRUE),
    avg_WRI = mean(WRI, na.rm = TRUE),
    RiskCategory = names(sort(table(`WRI Category`), decreasing = TRUE))[1]  # Mode calculation without a custom function
), by = Region]


df_avg_country = df_avg_country[RiskCategory != ""]


# Set the factor levels for avg_RiskCategory to control the order of the colors in the plot
df_avg_country$RiskCategory <- factor(df_avg_country$RiskCategory, levels = c("Very High", "High", "Medium", "Low", "Very Low"))



# Plotting -------

col =  c('#2c5769', '#6F99AD', '#ffb39a', '#df7775', '#ab403f')



p = ggplot(df_avg_country, aes(x = avg_Exposure, y = avg_Coping, size = avg_WRI, fill = RiskCategory)) +
    
    geom_point(shape = 21, alpha = 0.85, stroke = 0.2, color = "white") +
    
    scale_size(range = c(1.5, 10)) +
    
    # geom_text(aes(label = Region), hjust = 0.5, vjust = -0.5, size = 3, color = "black") +
    
    scale_fill_manual(values = rev(col)) +
    
    labs(
        title = "Average Risk Amplified by Lack of Coping Capacity by Country",
        subtitle = "Global average values across countries for 2011–2021",
        x = "Average Exposure to Hazards",
        y = "Average Lack of Coping Capabilities",
        size = "Average WRI",
        color = "Risk Category"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom",
        
        panel.grid.major = element_line(color = "grey65", linewidth = 0.25, linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA),
    )


p


# plot -----------

p = ggplot(df_plot, aes(x = Year, y = LE_birth_both_sexes, group = 1)) +
    
    geom_segment(
        aes(x = Year, xend = Year, y = 0, yend = LE_birth_both_sexes, col = Income_group), 
        size = 0.75
    ) +
    
    geom_point(
        aes(fill = Income_group, col = Income_group, size = size_group),
        shape = 21, 
        stroke = .2, 
        color = "white"
    ) +
    
    scale_fill_manual(values = col, guide = "none") +
    
    scale_color_manual(values = col, guide = "none") +
    
    scale_size_manual(
        values = c(small = 4, medium = 5.5, large = 7),
        labels = c("> 75", "65–75", "< 65")
    ) +
    
    facet_wrap(~Income_group, ncol = 2) +
    
    labs(
        title = "Life Expectancy Trends at Birth by Income Group from 2000 to 2019",
        subtitle = "Comparing life expectancy changes across different income groups over two decades, based on WHO data.",
        caption = "30DayChartChallenge 2025: <b> Day 24</b> 
                   | Source: <b> Life Expectancy WHO data (Kaggle) </b> 
                   | Graphic: <b>Natasa Anastasiadou</b>",
        y = "Life Expectancy (Years)",
        x = "Year",
        size = "Years"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        strip.text = element_text(face = "bold", size = 11),
        
        legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),

        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12.5, vjust = 3),
        
        axis.text.x = element_text(size = 11, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 11),
        

        panel.grid.major = element_line(color = "grey65", linewidth = 0.25, linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 2, b = 2)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5,  color = "grey30", margin = margin(t = 5, b = 20)),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8.5, hjust = 1.45),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA),
        
    ) +
    
    guides(
        size = guide_legend(override.aes = list(shape = 21, color = "grey30", stroke = 0.5))
    )



p 

ggsave(
    plot = p, filename = "25_day.png",
    width = 10, height = 10, units = "in", dpi = 600
)    

