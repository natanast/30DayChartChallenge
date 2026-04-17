

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

# 1. Load data ------------------------------------------------------------
dt <- fread("Child-Dataset-unicef-November-2024.csv")

# 2. Filter & Pivot -------------------------------------------------------
# Focusing on the two major African regions
africa_regions <- c("West and Central Africa", "Eastern and Southern Africa")

dt_plot <- dcast(dt[Level == "National" & 
                        Indicator %in% c("DIARCARE", "ORSZINC") & 
                        `UNICEF Reporting Region` %in% africa_regions], 
                 `Countries and areas` + `World Bank Income Group (2024)` ~ Indicator, 
                 value.var = "Value", 
                 fun.aggregate = max)

# Remove NAs to keep the plot clean
dt_plot <- dt_plot[!is.na(DIARCARE) & !is.na(ORSZINC)]

# 3. Plot -----------------------------------------------------------------
gr <- ggplot(dt_plot, aes(x = DIARCARE, y = ORSZINC)) +
    
    # THE "ZONE OF CONCERN"
    # Shading the area where care-seeking is high but treatment is low
    # annotate("rect", xmin = 50, xmax = 100, ymin = 0, ymax = 20, 
    #          fill = "#b24745", alpha = 0.08) +
    
    # # Simple Relationship line
    # geom_smooth(method = "lm", color = "grey40", linetype = "dashed", 
    #             linewidth = 0.5, se = FALSE) +
    
    # Points colored by Income
    geom_point(aes(fill = `World Bank Income Group (2024)`), 
               shape = 21, color = "white", size = 4.5, stroke = 0.6, alpha = 0.9) +
    
    # Annotate the Supply Gap
    # annotate("text", x = 95, y = 12, label = "THE SUPPLY GAP:\nCare sought, but medicine unavailable", 
    #          family = "Candara", fontface = "bold", color = "#b24745", hjust = 1, size = 3.5) +
    # 
    # Label notable outliers
    geom_text_repel(
        data = dt_plot[DIARCARE > 70 | ORSZINC > 30 | (DIARCARE < 30 & ORSZINC < 5)],
        aes(label = `Countries and areas`),
        family = "Candara", size = 3.2, fontface = "bold", box.padding = 0.5
    ) +
    
    # Colors matching your aesthetic
    scale_fill_manual(values = c("Low income" = "#b24745", 
                                 "Lower middle income" = "#d18d8d", 
                                 "Upper middle income" = "#678e9f")) +
    
    scale_x_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    scale_y_continuous(limits = c(0, 100), labels = function(x) paste0(x, "%")) +
    
    labs(
        title = "Africa: The Efficiency of the Care Relationship",
        subtitle = "Comparing the relationship between **Seeking Care** (DIARCARE) and receiving the<br>**combined ORS + Zinc treatment**. Poverty creates a 'Supply Gap' even when parents take action.",
        x = "% Children Seeking Care for Diarrhoea",
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
        plot.subtitle = element_markdown(size = 13, color = "grey30", lineheight = 1.2, margin = margin(b = 20)),
        plot.margin = margin(20, 20, 20, 20)
    )

gr
