

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

df <- fread("obesity_data.csv")


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
                    <br>States above the average are in red, while those below are in blue.</br>",
        caption = "Source: <b> data.gov</b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "",
        x = ""
    ) +


    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",  
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.75, margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.75, color = "grey30", margin = margin(b = 15, t = 5)),
        plot.caption = element_markdown(size = 8, hjust = 1, margin = margin(t = 35)),
        
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



