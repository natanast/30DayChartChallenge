

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)


# load data ------

dt <- as.data.table(ToothGrowth)


# clean data ------


dt[, dose := as.factor(dose)]


dt[, supp := fcase(
    supp == "OJ", "Orange Juice",
    supp == "VC", "Ascorbic Acid (Supplement)"
)]


col <- c("Orange Juice" = "#D78D50", "Ascorbic Acid (Supplement)" = "#5a8192")


# plot --------

g <- ggplot(dt, aes(x = dose, y = len, fill = supp, color = supp)) +
    
    
    geom_boxplot(
        width = 0.5, 
        alpha = 0.3, 
        outlier.shape = NA, # Hide standard outliers because we are plotting all raw points next!
        position = position_dodge(width = 0.6) # Separates the OJ and VC boxes
    ) +
    
    
    geom_point(
        position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.6),
        size = 2,
        alpha = 0.8
    ) +
    
    scale_fill_manual(values = col) +
    scale_color_manual(values = col) +
    
    labs(
        title = "Natural vs. Artificial Vitamin C",
        subtitle = "In a <b>guinea pig model</b>, low doses of Orange Juice promote significantly more cellular<br>growth than artificial supplements. At a high dose (2.0 mg), the advantage vanishes.",
        caption = "30DayChartChallenge 2026: <b> Day 5 (Experimental)</b> | Source: <b> Crampton (1947) J. Nutr.</b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "Odontoblast Length (Cellular Growth)",
        x = "Vitamin C Dosage (mg/day)"
    ) +

    theme_minimal(base_family = "Candara") +
    
    theme(
        
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(size = 11, face = "bold", color = "grey30"),
        
        axis.title.y = element_text(size = 11, face = "bold", color = "grey30", margin = margin(r = 10)),
        axis.title.x = element_text(size = 11, face = "bold", color = "grey30", margin = margin(t = 10)),
        
        axis.text.x = element_text(size = 14, face = "bold", color = "black"),
        axis.text.y = element_text(size = 11, color = "grey30"),
        
        panel.grid.major.y = element_line(linewidth = 0.35, color = "grey85"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1, lineheight = 1.2),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

g

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


