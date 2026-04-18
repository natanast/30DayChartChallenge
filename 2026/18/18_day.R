

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)


# load data ------

dt <- fread("Child-Dataset-unicef-November-2024.csv")

# clean data ------

africa_regions <- c("West and Central Africa", "Eastern and Southern Africa")

dt <- dt[`UNICEF Reporting Region` %in% africa_regions & 
             Level == "National" & 
             Indicator %in% c("DIARCARE", "ORSZINC")]


dt_clean <- dt[!is.na(Value)]

# Keep only the most recent year for each country and indicator
dt_clean <- dt_clean[, .SD[which.max(`Latest Year`)], by = .(`Countries and areas`, Indicator)]


dt_plot <- dcast(dt_clean, 
                 `Countries and areas` + `World Bank Income Group (2024)` ~ Indicator, 
                 value.var = "Value")


dt_plot <- dt_plot[!is.na(DIARCARE) & !is.na(ORSZINC)]


dt_plot[dt_plot == -Inf] <- NA
dt_plot <- na.omit(dt_plot)


dt_plot[, zone := fcase(
    DIARCARE >= 50 & ORSZINC <= 15, "High Seeking, Low Medicine",
    DIARCARE < 50  & ORSZINC <= 15, "Low Seeking, Low Medicine",
    DIARCARE >= 50 & ORSZINC > 15,  "High Seeking, High Medicine",
    DIARCARE < 50  & ORSZINC > 15,  "Low Seeking, High Medicine"
)]


dt_plot[, zone := factor(zone, levels = c(
    "High Seeking, High Medicine", "Low Seeking, High Medicine", 
    "High Seeking, Low Medicine", "Low Seeking, Low Medicine"
))]


dt_labels <- rbind(
    dt_plot[zone == "High Seeking, High Medicine"][order(-ORSZINC)][1], # Best performer
    dt_plot[zone == "High Seeking, Low Medicine"][order(-DIARCARE)][1],  # Highest gap
    dt_plot[zone == "Low Seeking, Low Medicine"][order(DIARCARE)][1],    # Most critical
    dt_plot[zone == "Low Seeking, High Medicine"][order(-ORSZINC)][1]    # Rare outlier
)


gr <- ggplot(dt_plot, aes(x = DIARCARE, y = ORSZINC)) +
    
    # Background Shading
    annotate(
        "rect", xmin = 50, xmax = 100, ymin = 15, ymax = 100, 
        fill = "#678e9f", alpha = 0.1
    ) +
    
    annotate(
        "rect", xmin = 0,  xmax = 50,  ymin = 15, ymax = 100, 
        fill = "#a8b2ba", alpha = 0.05
    ) +
    
    annotate(
        "rect", xmin = 50, xmax = 100, ymin = 0,  ymax = 15,  
        fill = "#d18d8d", alpha = 0.1
    ) +
    
    annotate(
        "rect", xmin = 0,  xmax = 50,  ymin = 0,  ymax = 15,  
        fill = "#b24745", alpha = 0.1
    ) +
    
    
    annotate("text", x = 98, y = 98, label = "High Seeking,\nHigh Medicine", 
             hjust = 1, vjust = 1, size = 3.5, fontface = "bold", color = "#466370") +
    
    annotate("text", x = 2, y = 98, label = "Low Seeking,\nHigh Medicine", 
             hjust = 0, vjust = 1, size = 3.5, fontface = "bold", color = "#7b868e") +
    
    annotate("text", x = 98, y = 2, label = "High Seeking,\nLow Medicine", 
             hjust = 1, vjust = 0, size = 3.5, fontface = "bold", color = "#a66b6b") +
    
    annotate("text", x = 2, y = 2, label = "Low Seeking,\nLow Medicine", 
             hjust = 0, vjust = 0, size = 3.5, fontface = "bold", color = "#b24745") +
    
    # Dividers
    geom_vline(xintercept = 50, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_hline(yintercept = 15, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    
    
    geom_point(
        aes(fill = zone), 
        size = 4, 
        shape = 21, 
        color = "white", 
        stroke = 0.45
    ) +
    
    
    geom_text_repel(
        data = dt_labels,
        aes(label = `Countries and areas`),
        family = "Candara", size = 3.5, fontface = "bold", 
        box.padding = 0.8, point.padding = 0.3, color = "black"
    ) +
    
    scale_fill_manual(values = c(
        "High Seeking, High Medicine" = "#466370",
        "Low Seeking, High Medicine"  = "#85a4b2",
        "High Seeking, Low Medicine"  = "#d18d8d",
        "Low Seeking, Low Medicine"   = "#b24745"
    )) +
    
    scale_x_continuous(limits = c(0, 100), expand = c(0,0), labels = function(x) paste0(x, "%")) +
    scale_y_continuous(limits = c(0, 100), expand = c(0,0), labels = function(x) paste0(x, "%")) +
    
    labs(
        title = "Mapping the Access-Treatment Relationship",
        subtitle = "A quadrant analysis of the **systemic relationship** <br>between medical consultations and treatment delivery for children in **Africa**.",
        x = "% Children Seeking Care",
        y = "% Children Receiving ORS + Zinc",
        caption = "30DayChartChallenge 2026: <b> Day 18 </b> | Source: <b> Kaggle (UNICEF) </b> | Graphic: <b>Natasa Anastasiadou</b>",
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(

        legend.position = "none",

        plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1),
        
        panel.grid.major = element_line(linewidth = 0.35, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
    )

gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 9, units = "in", dpi = 600
)

