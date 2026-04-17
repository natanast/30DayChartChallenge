

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

library(data.table)
library(ggplot2)
library(ggtext)
library(ggrepel)

# 1. Load 
dt <- fread("Malaria_and_Diarrhoea_Dataset.csv")

# 2. Prep: Get 'Prevention' and 'Treatment' columns
# Filter for National level and latest year
dt_wide <- dcast(dt[Level == "National" & Indicator %in% c("ITNOWN", "ORS")], 
                 `Countries and areas` + `UNICEF Reporting Region` ~ Indicator, 
                 value.var = "Value", 
                 fun.aggregate = mean) # Handling potential duplicates

# Clean NAs
dt_plot <- dt_wide[!is.na(ITNOWN) & !is.na(ORS)]

# Calculate global medians for the quadrant lines
med_itn <- median(dt_plot$ITNOWN)
med_ors <- median(dt_plot$ORS)

# 3. Plot -----------------------------------------------------------------
gr <- ggplot(dt_plot, aes(x = ITNOWN, y = ORS)) +
    
    # 4 Quadrants
    annotate("rect", xmin = med_itn, xmax = 100, ymin = med_ors, ymax = 100, fill = "#678e9f", alpha = 0.1) + # Top Right (Success)
    annotate("rect", xmin = 0, xmax = med_itn, ymin = 0, ymax = med_ors, fill = "#b24745", alpha = 0.1) + # Bottom Left (Burden)
    
    # Quadrant Dividers
    geom_vline(xintercept = med_itn, linetype = "dashed", color = "grey60") +
    geom_hline(yintercept = med_ors, linetype = "dashed", color = "grey60") +
    
    # Points
    geom_point(aes(color = `UNICEF Reporting Region`), size = 3, alpha = 0.7) +
    
    # Strategic Labels
    annotate("text", x = 95, y = 95, label = "HEALTH LEADERS", fontface = "bold", family = "Candara", color = "#466370", hjust = 1) +
    annotate("text", x = 5, y = 5, label = "HIGH BURDEN", fontface = "bold", family = "Candara", color = "#b24745", hjust = 0) +
    
    # Label Outliers
    geom_text_repel(
        data = dt_plot[ITNOWN > 80 | ORS > 80 | (ITNOWN < 10 & ORS < 10)],
        aes(label = `Countries and areas`),
        family = "Candara", size = 3.5, fontface = "bold"
    ) +
    
    scale_color_brewer(palette = "Set2") + # Using a varied palette for regions
    
    labs(
        title = "Prevention vs. Treatment: A Strategic View",
        subtitle = "Relationship between **Bednet Ownership** (Prevention) and **ORS Use** (Treatment).",
        x = "% Households with Bednets (ITNOWN)",
        y = "% Children Receiving ORS",
        caption = "30DayChartChallenge 2026: <b> Day 17 (Relationships)</b> | Source: <b>UNICEF</b>"
    ) +
    
    theme_minimal(base_family = "Candara") +
    theme(
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        legend.position = "bottom",
        plot.title = element_markdown(size = 20, face = "bold"),
        plot.subtitle = element_markdown(size = 12, color = "grey30")
    )

gr

# 4. Save
ggsave("Day17_Quadrant_Strategy.png", gr, width = 10, height = 9, units = "in", dpi = 600)