

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

df <- fread("Child-Dataset-unicef-November-2024.csv")

# clean data ------
# A. The Triple Filter: One Indicator, One Lens, One Year
dt_clean <- dt[Indicator == "DIARCARE" & Stratifier == "WIQ"]




# gr = ggplot(df_heatmap, aes(x = sending_country_code, y = receiving_country_code, fill = N)) +
#     
#     geom_tile(color = "grey20", linewidth = .25) +
#     
#     geom_shadowtext(
#         aes(label = N), 
#         color = "black",
#         family = "Candara",
#         bg.color = "grey95", 
#         bg.r = .1, 
#         size = 3.5 
#     ) +
#     
#     scale_fill_stepsn(
#         colors = c('#2c5769', '#6F99AD', 'grey96', '#ffb5ac', '#a33a3a'),
#         breaks = c(1, 20, 50, 70, 100),  
#         transform = pseudo_log_trans(base = 10),
#         name = "Total Participants",
#         na.value = "grey96",
#         guide = guide_colorsteps(
#             barheight = unit(10, "lines"), 
#             barwidth = unit(0.4, "lines")
#         )
#     ) +
# 
#     theme_minimal(base_family = "Candara") +
#     
#     labs(
#         title = "The Student Trade: Erasmus Mobility",
#         subtitle = "Mapping the **relationships** and student **trade** between Europe's top 15 academic destinations.",
#         caption = "30DayChartChallenge 2026: <b> Day 14</b>
#                    | Source: <b> Erasmus Data (TidyTuesday)</b>
#                    | Graphic: <b>Natasa Anastasiadou</b>",
#          x = "Sending Country",
#          y = "Receiving Country"
#     ) +
#     
#     theme(
#         legend.position = "right",
#         legend.title.position = "left",
#         
#         legend.title = element_text(size = 9, angle = 90, hjust = .5, face = "bold", family = "Candara", color = "grey30"),
#         legend.text = element_text(size = 8, color = "grey30"),
#         
#         axis.title.x = element_text(size = 10, face = "bold", margin = margin(t = 10)),
#         axis.title.y = element_text(size = 10, face = "bold", margin = margin(r = 10)),
#         
#         axis.text.x = element_text(size = 9),
#         axis.text.y = element_text(size = 9),
#         
#         panel.grid = element_blank(),
#         
#         plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
#         plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.2),
#         
#         plot.margin = margin(20, 20, 20, 20),
#         plot.background = element_rect(fill = "grey93", color = NA)
#     )  
# 
# gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 8, units = "in", dpi = 600
)










rm(list = ls())
gc()

# 1. Load Libraries -------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggtext)
library(ggrepel)

# 2. Load and Prep Data ---------------------------------------------------
dt <- fread("Child-Dataset-unicef-November-2024.csv")

# Filter for Africa, National level, and the two related indicators
africa_regions <- c("West and Central Africa", "Eastern and Southern Africa")

dt <- dt[`UNICEF Reporting Region` %in% africa_regions & 
          Level == "National" & 
          Indicator %in% c("DIARCARE", "ORSZINC")]

dt_clean <- dt[!is.na(Value)]

# 1. Start with the Africa-filtered dt
# Ensure we only have rows with actual values
dt_clean <- dt[!is.na(Value)]

# 2. Pivot (dcast)
# We use fun.aggregate = max to pick the highest/latest value if duplicates exist
dt_plot <- dcast(dt_clean, 
                 `Countries and areas` + `World Bank Income Group (2024)` ~ Indicator, 
                 value.var = "Value", 
                 fun.aggregate = function(x) if(length(x) == 0) NA else max(x, na.rm = TRUE))

# 3. FILTER FOR COMPLETE CASES (The most important step)
# This removes countries that are missing one of the two variables.
# You cannot have a 'relationship' dot with only one coordinate!
dt_plot <- dt_plot[!is.na(DIARCARE) & !is.na(ORSZINC)]

# 4. Check for any leftover -Inf (just in case)
# This converts any accidental -Inf to NA so they don't break ggplot
dt_plot[dt_plot == -Inf] <- NA
dt_plot <- na.omit(dt_plot)



# 3. Plotting -------------------------------------------------------------
gr <- ggplot(dt_plot, aes(x = DIARCARE, y = ORSZINC)) +
    
    # # The 1:1 Equality Line (What a perfect system would look like)
    # geom_abline(intercept = 0, slope = 1, linetype = "dotted", color = "grey70") +
    
    # Points: Colored by Income, Size by the severity of the 'Gap'
    geom_point(aes(fill = `World Bank Income Group (2024)`), size = 3, 
               shape = 21, color = "white", stroke = 0.6, alpha = 0.8) +
    
    # Label the Top 10 countries with the LARGEST Gap (Most critical failure)
    geom_text_repel(
        aes(label = `Countries and areas`),
        family = "Candara", size = 3.5, fontface = "bold", 
        box.padding = 0.5, point.padding = 0.3
    ) +
    
    # Aesthetic Scaling
    scale_fill_manual(values = c("Low income" = "#b24745",
                                 "Lower middle income" = "#d18d8d",
                                 "Upper middle income" = "#678e9f")) +

    
    scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    
    labs(
        title = "The Supply Gap: Seeking Care vs. Getting Cured",
        subtitle = "Relationship in **Africa**. Countries further right have high awareness (Seeking Care),<br>but those staying low on the Y-axis face a **Supply Failure** for ORS + Zinc.",
        x = "% Children Seeking Care (DIARCARE)",
        y = "% Children Receiving ORS + Zinc",
        caption = "30DayChartChallenge 2026: <b> Day 17 (Relationships)</b> | Source: <b>UNICEF</b>"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        panel.grid.major = element_line(linewidth = 0.3, color = "grey85"),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        plot.title = element_markdown(size = 22, face = "bold"),
        plot.subtitle = element_markdown(size = 13, color = "grey30", lineheight = 1.2),
        plot.margin = margin(20, 20, 20, 20)
    )

gr

# ... (Keep your existing Data Prep code up to dt_plot) ...


# ... (Keep your Data Prep code) ...
# 1. Create the 4-Quadrant Category ---------------------------------------
dt_plot[, zone := fcase(
    DIARCARE >= 50 & ORSZINC <= 15, "High Seeking, Low Medicine",
    DIARCARE < 50  & ORSZINC <= 15, "Low Seeking, Low Medicine",
    DIARCARE >= 50 & ORSZINC > 15,  "High Seeking, High Medicine",
    DIARCARE < 50  & ORSZINC > 15,  "Low Seeking, High Medicine"
)]

dt_plot[, zone := factor(zone, levels = c("High Seeking, High Medicine", "Low Seeking, High Medicine", 
                                          "High Seeking, Low Medicine", "Low Seeking, Low Medicine"))]

# 2. Plotting -------------------------------------------------------------
gr <- ggplot(dt_plot, aes(x = DIARCARE, y = ORSZINC)) +
    
    # Background Shading
    annotate("rect", xmin = 50, xmax = 100, ymin = 15, ymax = 100, fill = "#678e9f", alpha = 0.1) + # Top-Right
    annotate("rect", xmin = 0,  xmax = 50,  ymin = 15, ymax = 100, fill = "#a8b2ba", alpha = 0.05) + # Top-Left (Added)
    annotate("rect", xmin = 50, xmax = 100, ymin = 0,  ymax = 15,  fill = "#d18d8d", alpha = 0.1) + # Bottom-Right
    annotate("rect", xmin = 0,  xmax = 50,  ymin = 0,  ymax = 15,  fill = "#b24745", alpha = 0.1) + # Bottom-Left
    
    # --- Quadrant Labels ---
    annotate("text", x = 98, y = 98, label = "High Seeking,\nHigh Medicine", 
             hjust = 1, vjust = 1, size = 3.5, fontface = "bold", family = "Candara", color = "#466370") +
    
    annotate("text", x = 2, y = 98, label = "Low Seeking,\nHigh Medicine", 
             hjust = 0, vjust = 1, size = 3.5, fontface = "bold", family = "Candara", color = "#7b868e") +
    
    annotate("text", x = 98, y = 2, label = "High Seeking,\nLow Medicine", 
             hjust = 1, vjust = 0, size = 3.5, fontface = "bold", family = "Candara", color = "#a66b6b") +
    
    annotate("text", x = 2, y = 2, label = "Low Seeking,\nLow Medicine", 
             hjust = 0, vjust = 0, size = 3.5, fontface = "bold", family = "Candara", color = "#b24745") +
    
    # Quadrant Dividers
    geom_vline(xintercept = 50, linetype = "dashed", color = "grey60", linewidth = 0.4) +
    geom_hline(yintercept = 15, linetype = "dashed", color = "grey60", linewidth = 0.4) +
    
    # Points
    geom_point(aes(fill = zone), size = 4.5, shape = 21, color = "white", stroke = 0.7) +
    
    # # Country Labels (Optional: uncomment to show names)
    # geom_text_repel(
    #     aes(label = `Countries and areas`),
    #     family = "Candara", size = 3, fontface = "bold", box.padding = 0.4
    # ) +
    
    scale_fill_manual(values = c(
        "High Seeking, High Medicine" = "#466370",
        "Low Seeking, High Medicine"  = "#a8b2ba",
        "High Seeking, Low Medicine"  = "#d18d8d",
        "Low Seeking, Low Medicine"   = "#b24745"
    )) +
    
    scale_x_continuous(limits = c(0, 100), expand = c(0,0), labels = function(x) paste0(x, "%")) +
    scale_y_continuous(limits = c(0, 100), expand = c(0,0), labels = function(x) paste0(x, "%")) +
    
    labs(
        title = "The Strategic Geography of Child Health",
        subtitle = "Countries categorized by the relationship between **Care-Seeking** (X) and **Treatment** (Y).",
        x = "% Children Seeking Care (DIARCARE)",
        y = "% Children Receiving ORS + Zinc",
        fill = "Health System Zone:",
        caption = "30DayChartChallenge 2026 | Day 17: UNICEF | Graphic: Natasa Anastasiadou"
    ) +
    
    theme_minimal(base_family = "Candara") +
    theme(
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        legend.position = "none", # Legend hidden because annotations describe the zones
        plot.title = element_markdown(size = 22, face = "bold"),
        plot.subtitle = element_markdown(size = 13, color = "grey30"),
        panel.grid.minor = element_blank(),
        plot.margin = margin(20, 20, 20, 20)
    )

gr
