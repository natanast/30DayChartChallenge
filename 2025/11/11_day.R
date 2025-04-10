

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)

library(ComplexHeatmap)
library(ggplotify)
library(ggplot2)
library(circlize)


# load data --------
#2023-07-11

global_temps <- fread("GLB.Ts+dSST.csv")


# data cleaning -----------

df <- global_temps[1:145, 1:13]

df <- df[Year >= 1990, ]

df[, 2:13] <- lapply(df[, 2:13], function(x) as.numeric(as.character(x)))


long_data <- melt(df, id.vars = "Year", variable.name = "Month", value.name = "Temperature Anomaly")

long_data$Month <- factor(long_data$Month, levels = rev(unique(long_data$Month)))

col = c("#00429d", "#73a2c6", "#ffffe0", "#ff9a92", "#b24745")


# plot --------

gr <- ggplot(long_data, aes(x = Year, y = Month, fill = `Temperature Anomaly`)) +
    
    geom_tile(linewidth = .25, color = "grey20") +
    
    scale_fill_gradientn(
        colors = col,
        guide = guide_colorbar(
            barwidth = unit(0.3, "cm"),
            barheight = unit(5, "cm")
        ),
        name = "Temperature",
        labels = function(x) paste0(x, "°C")
    ) +

    
    theme_minimal() +
    
    labs(
        title = "Global Surface Temperature Anomalies (°C)",
        subtitle = "Monthly deviations from the 1951–1980 average, from 1990 to 2024",
        caption = "Source: <b> NOAA Global Temperature Data</b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "",
        x = ""
    ) +
    
    theme(
        legend.position = "right",
        legend.title.position = "left",
        legend.title = element_text(size = 10, face = "bold", family = "Candara", angle = 90, color = "grey30", hjust = .5),
        legend.text = element_text(size = 8, family = "Candara", color = "grey30"),
        
        axis.text.x = element_text(size = 12, family = "Candara", angle = 90, hjust = 1, vjust = .35, margin = margin(b = -20)),
        axis.text.y = element_text(size = 12, family = "Candara", margin = margin(r = -15)),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, family = "Candara", margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5, family = "Candara", color = "grey30", margin = margin(b = 15, t = 5)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, family = "Candara", hjust = 1.15),
        
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "grey93", color = NA)
    )


gr

# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 8, units = "in", dpi = 600
)

