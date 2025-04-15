

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

penguins <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-15/penguins.csv')
penguins_raw <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-04-15/penguins_raw.csv')


# data cleaning -----------

avg_ob <- mean(df$Obesity)


df$diff_from_avg <- df$Obesity - avg_ob


df$Direction <- ifelse(df$diff_from_avg >= 0, "Above", "Below")


# Sort states by diff_from_avg
df$NAME <- factor(df$NAME, levels = df$NAME[rev(order(df$diff_from_avg, decreasing = TRUE))])


col = c("#73a2c6", "#b24745")

col = c("#00429d", "#b24745")

col <- c("Below" = "#73a2c6", "Above" = "#b24745")


# plot --------


gr = df |>
    
    ggplot(aes(y = NAME, x = diff_from_avg, fill = Direction)) + 
    
    
    geom_col(width = 0.7, alpha = 0.9) +
    
    geom_vline(xintercept = 0, color = "grey20", linetype = "dashed", size = 0.55) +
    
    scale_fill_manual(values = col) +
    
    labs(
        title = "How Much Each U.S. State's Obesity Rate Differs from the National Average (29.3%)",
        subtitle = "This chart shows the difference in adult obesity rates by state compared to the U.S. average (1990–2022). 
                    <br>States <span style='color:#b24745;'><b>above</b></span> the average are in <span style='color:#b24745;'><b>red</b></span>, 
                    while those <span style='color:#00429d;'><b>below</b></span> are in <span style='color:#00429d;'><b>blue</b></span></br>",
        caption = "Source: <b> data.gov</b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "",
        x = ""
    ) +
    

    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",  
        
        plot.title = element_markdown(size = 14, face = "bold", hjust = .25, margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 10, hjust = 0.3, color = "grey30", margin = margin(b = 15, t = 5)),
        plot.caption = element_markdown(size = 8, hjust = 1, margin = margin(t = 10)),
        
        panel.grid.major = element_line(linewidth = 0.45, color = "grey80"),
        panel.grid.minor = element_blank(),

        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "grey93", color = NA)
    )


gr


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 8, units = "in", dpi = 600
)








library(tidyverse)
library(palmerpenguins)
library(ggtext)

# Filter out missing values
penguins_clean <- penguins |>
    filter(!is.na(bill_len), !is.na(bill_dep), !is.na(species))

# Plot
ggplot(penguins_clean, aes(x = bill_len, y = bill_dep, color = species)) +
    geom_point(alpha = 0.7, size = 2.5) +
    geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
    scale_color_manual(values = c("#2a9d8f", "#e76f51", "#264653")) +
    labs(
        title = "Bill Shape Tradeoff in Penguins",
        subtitle = "Negative relationship between bill length and bill depth reveals species-specific adaptations.",
        x = "Bill Length (mm)",
        y = "Bill Depth (mm)",
        color = "Species",
        caption = "Data: #TidyTuesday | Viz: Natasa Anastasiadou"
    ) +
    theme_minimal(base_family = "Candara") +
    theme(
        plot.title = element_markdown(size = 16, face = "bold"),
        plot.subtitle = element_markdown(size = 11, color = "grey20"),
        plot.caption = element_text(size = 8, color = "grey40"),
        legend.position = "bottom"
    )

