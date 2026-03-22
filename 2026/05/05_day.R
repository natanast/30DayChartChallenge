

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)



# load data ------



# clean data ------




# plot --------


# 
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
#         plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
#         plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
#         
#         plot.background = element_rect(fill = "grey95", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )
# 
# gr



# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 9, units = "in", dpi = 600
)


rm(list = ls())
gc()

# libraries ---------
library(data.table)
library(ggplot2)
library(ggtext)
library(ggdist) # The magic package for Raincloud plots!

# 1. Load data -------
# Kaggle's Drug Classification Dataset (Mirrored on GitHub for easy loading)
url <- "https://raw.githubusercontent.com/pratikgarai/Dataset/master/drug200.csv"
dt <- fread(url)

# 2. Clean data ------
# Make sure Drug is a factor so we can control the order on the plot
# We will compare the 4 main standard drugs vs the outlier "Drug Y"
dt[, Drug := factor(Drug, levels = c("drugA", "drugB", "drugC", "drugX", "DrugY"))]

# Clean up the names for the plot labels
levels(dt$Drug) <- c("Drug A", "Drug B", "Drug C", "Drug X", "Drug Y")


# 3. Plot --------

# Highlight the experimental "Drug Y" in your terracotta color, keep the rest slate blue/grey
col <- c(
    "Drug A" = "#8b949c", 
    "Drug B" = "#8b949c", 
    "Drug C" = "#8b949c", 
    "Drug X" = "#5a8192", 
    "Drug Y" = "#D78D50"
)

g <- ggplot(dt, aes(x = Drug, y = Na_to_K, fill = Drug, color = Drug)) +
    
    # 1. THE CLOUD (Density curve)
    # justification moves it to the right so it doesn't overlap the boxplot
    stat_halfeye(
        adjust = 0.5, 
        justification = -0.15, 
        .width = 0, 
        point_colour = NA, 
        alpha = 0.8
    ) +
    
    # 2. THE BOXPLOT (Strict statistical summary)
    geom_boxplot(
        width = 0.12, 
        outlier.color = NA, 
        alpha = 0.5,
        color = "grey30" # Keep boxplot outlines dark for readability
    ) +
    
    # 3. THE RAIN (Raw jittered data points)
    # width controls how wide the "rain" scatters
    geom_jitter(
        width = 0.05, 
        height = 0, 
        size = 1.5, 
        alpha = 0.6
    ) +
    
    # Flip the coordinates so the clouds float horizontally!
    coord_flip() +
    
    scale_fill_manual(values = col) +
    scale_color_manual(values = col) +
    
    labs(
        title = "Metabolic Biomarkers in Drug Efficacy",
        subtitle = "Comparing the Blood Sodium-to-Potassium (Na:K) ratio across experimental treatments.<br>Notice the distinct metabolic threshold required for <b>Drug Y</b>.",
        caption = "30DayChartChallenge 2026: <b> Day 5 (Experimental)</b> | Source: <b> Drug Classification (Kaggle)</b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "Sodium-to-Potassium Ratio (Na:K)"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 10, face = "bold", color = "grey30", margin = margin(t = 10)),
        
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.text.x = element_text(size = 10, color = "grey30"),
        
        # Keep horizontal grid lines to help measure the Na:K ratio
        panel.grid.major.x = element_line(linewidth = 0.35, color = "grey85"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1, lineheight = 1.2),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

g

# 4. Save ---------
ggsave(
    "Day5_Experimental_Raincloud.png", 
    plot = g, 
    width = 9, 
    height = 7, 
    dpi = 600
)
