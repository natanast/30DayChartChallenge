

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(ggrepel)


# load data ------



# clean data -----


# plot --------


# gr <- ggplot(df_picto, aes(x = x, y = season_label)) +
#     
#     
#     geom_text(label = "🍔", size = 8, family = "Segoe UI Emoji") +
#     
#     scale_x_continuous(limits = c(0, 13), breaks = seq(0, 12, by = 2)) +
#     
#     labs(
#         title = "Bob's Burgers: The Short & Long Seasons",
#         subtitle = "Total unique words spoken per season. Season 2 was cut to just 9 episodes. <br><b>Each 🍔 represents 2,000 words.</b>",
#         caption = "30DayChartChallenge 2026: <b> Day 2</b>
#                    | Source: <b> bobsburgers (TidyTuesday | Nov 2024)</b>
#                    | Graphic: <b>Natasa Anastasiadou</b>",
#         
#     ) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#         
#         axis.title = element_blank(),
#         
#         axis.text.x = element_text(size = 10, color = "grey30"),
#         axis.text.y = element_text(size = 12, face = "bold", color = "black", margin = margin(r = 10)),
#         
#         panel.grid.major = element_line(linewidth = 0.35, color = "grey85"),
#         panel.grid.minor = element_blank(),
#         
        # plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        # plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        # plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
#         
#         plot.background = element_rect(fill = "grey95", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )



# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)



library(tidyverse)

# 1. Load and clean the curated data
url <- "https://raw.githubusercontent.com/ajstarks/dubois-data-portraits/master/plate21/data.csv"
plate21_data <- read_csv(url) %>%
    rename(Value = `Property Valuation`) # Renaming fixes the 'Value not found' error

# 2. Define the annotations (the "Comments" from the original)
# These are placed roughly according to the years they affected the trend
annotations <- tibble(
    Year = c(1872, 1877, 1888, 1892, 1893, 1897),
    Value = c(1000000, 4500000, 7500000, 3000000, 1500000, 4500000),
    label = c("KU-KLUXISM", 
              "POLITICAL UNREST", 
              "RISE OF THE\nNEW INDUSTRIALISM", 
              "LYNCHING", 
              "FINANCIAL\nPANIC", 
              "DISFRANCHISEMENT\nAND PROSCRIPTIVE\nLAWS.")
)

# 3. Create the Plot
ggplot(plate21_data, aes(x = Year, y = Value)) +
    # The signature thick black line
    geom_line(linewidth = 2, color = "black") +
    
    # Adding the historical comments (annotations)
    geom_text(data = annotations, aes(label = label), 
              family = "sans", size = 2.5, lineheight = 0.8, fontface = "bold") +
    
    # Styling the axes to match the original vertical "poster" feel
    scale_y_continuous(
        limits = c(0, 10000000), 
        breaks = seq(0, 10000000, 1000000),
        labels = scales::comma
    ) +
    scale_x_continuous(breaks = seq(1870, 1900, 5)) +
    
    # The "Du Bois" Theme
    theme_minimal() +
    theme(
        plot.background = element_rect(fill = "#e3d4ba", color = NA), # Aged paper tan
        panel.grid.major = element_line(color = "#d9cbb4", linewidth = 0.5), # Grid paper look
        panel.grid.minor = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(family = "mono", face = "bold"),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b=20)),
        plot.margin = margin(30, 30, 30, 30)
    ) +
    labs(title = "VALUATION OF TOWN AND CITY PROPERTY OWNED\nBY GEORGIA NEGROES.")

