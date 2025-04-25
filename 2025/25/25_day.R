

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



# Label top 5 
top5 <- df_avg_country[order(-avg_Exposure)][1:5, Region]

df_avg_country[, label := ifelse(Region %in% top5, Region, NA)]



# Plotting -------

col =  c('#2c5769', '#6F99AD', '#ffb39a', '#df7775', '#ab403f')



p = df_avg_country |>
    
    ggplot(aes(x = avg_Exposure, y = avg_Coping, size = avg_WRI, fill = RiskCategory)) +
    
    geom_point(
        shape = 21, 
        alpha = 0.85, 
        stroke = 0.2, 
        color = "white"
    ) +
    
    scale_size(range = c(1.5, 9)) +
    
    geom_text(
        aes(label = label),
        hjust = 0.5, 
        vjust = -1.75, 
        size = 3, 
        color = "black"
    ) +

    scale_fill_manual(
        values = rev(col)
    ) +
    
    labs(
        title = "Disaster Risk Across Countries: How Exposure and Coping Capacity Shape Vulnerability (2011-2021)",
        subtitle = "Each point represents a country’s average disaster risk (WRI) based on its exposure to hazards and its ability to cope. <br> Larger circles indicate higher overall risk.</br>",
        caption = "30DayChartChallenge 2025: <b> Day 25</b> 
                   | Source: <b> World Disaster Risk Dataset (Kaggle) </b> 
                   | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Average Exposure to Hazards",
        y = "Average Lack of Coping Capabilities",
        size = "Average WRI",
        fill = "Risk Category"
    ) +
    
    
    theme_minimal(base_family = "Candara") +
    
    theme(

        legend.position = "right",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.box = "vertical",
        
        axis.title.y = element_text(size = 11, vjust = 5),
        axis.title.x = element_text(size = 11, vjust = -2),
        
        axis.text = element_text(size = 10),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 2, b = 2)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5,  color = "grey30", margin = margin(t = 5, b = 20)),
        plot.caption  = element_markdown(margin = margin(t = 35), size = 8, hjust = 1.45),
        
        panel.grid.major = element_line(color = "grey65", linewidth = 0.25, linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA),
    ) +
    
    guides(
        size = guide_legend(override.aes = list(shape = 21, color = "grey30", stroke = 0.5)),
        fill = guide_legend(override.aes = list(size = 5))
    )


p 

ggsave(
    plot = p, filename = "25_day.png",
    width = 10, height = 10, units = "in", dpi = 600
)    

