

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


col <- c("Orange Juice" = "#b24745", "Ascorbic Acid (Supplement)" = "#5a8192")


# plot --------

gr <- ggplot(dt, aes(x = dose, y = len, fill = supp)) +
    
    geom_boxplot(
        width = 0.5, 
        alpha = 0.75, 
        outlier.shape = NA, 
        position = position_dodge(width = 0.6) ,
        color = "gray30"
        
    ) +
    
    geom_point(
        position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.6),
        # size = 2,
        # alpha = 0.8,
        shape = 21, size = 3.5, stroke = .25, alpha = 1.5,
        color = "grey20"
    ) +
    
    
    
    scale_fill_manual(values = col) +
    # scale_color_manual(values = col) +
    
    labs(
        title = "Natural vs. Artificial Vitamin C",
        subtitle = "In a <b>guinea pig model</b>, low doses of Orange Juice promote significantly more cellular<br>growth than artificial supplements. At a high dose (2.0 mg), the advantage vanishes.",
        caption = "30DayChartChallenge 2026: <b> Day 5 </b> | Source: <b> Crampton (1947) J. Nutr.</b> | Graphic: <b>Natasa Anastasiadou</b>",
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
        
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        axis.text.y = element_text(size = 12, color = "grey30"),
        
        panel.grid.major = element_line(linewidth = 0.35, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

gr

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
# plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
# plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
# plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
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

# load libraries -------
library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)

# 1. Load data ------
# Extracted from official RSF matrix (2020 vs 2025)
dt <- data.table(
    country = c("Czech Republic", "Montenegro", "Moldova", "Lithuania", "Poland", 
                "Russia", "Belarus", "Georgia", "Cyprus", "Kosovo"),
    score_2020 = c(76.43, 66.17, 68.84, 78.81, 71.35, 
                   51.08, 50.25, 71.41, 79.55, 70.67),
    score_2025 = c(83.96, 72.83, 73.36, 82.27, 74.79, 
                   24.57, 25.73, 50.53, 59.04, 52.73)
)

# 2. Clean data ------
dt[, change := score_2025 - score_2020]
dt[, status := fifelse(change > 0, "Improved", "Declined")]

# Order by the actual change
dt[, country := factor(country, levels = dt[order(change)]$country)]

col <- c("Declined" = "#b24745", "Improved" = "#5a8192")

# 3. Plot --------
gr <- ggplot(dt) +
    
    geom_segment(aes(x = score_2020, xend = score_2025, y = country, yend = country), color = "grey75", linewidth = 2) +
    
    geom_point(aes(x = score_2020, y = country), color = "grey60", size = 3) +
    
    geom_point(aes(x = score_2025, y = country, fill = status), shape = 21, size = 4, stroke = 0.5, color = "grey20") +
    
    # Text labels showing the precise 2025 score (rounded to 1 decimal for cleanliness)
    geom_text(
        aes(x = score_2025, y = country, label = round(score_2025, 1), color = status),
        fontface = "bold", size = 3.5, 
        nudge_x = fifelse(dt$change > 0, 3.5, -3.5)
    ) +
    
    scale_fill_manual(values = col) +
    scale_color_manual(values = col) +
    
    # Expanded the X-axis to comfortably fit the severe drops of Russia and Belarus
    scale_x_continuous(limits = c(15, 90), breaks = seq(20, 100, 20)) +
    
    labs(
        title = "A Divided Continent: Press Freedom in Europe",
        subtitle = "Comparing the 5 largest improvements and declines in RSF scores: <span style='color:grey50;'><b>2020</b></span> vs <span style='color:black;'><b>2025</b></span>.",
        caption = "30DayChartChallenge 2026: <b> Day 6 (Data Day: RSF)</b> | Source: <b> Reporters Without Borders </b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "", x = "RSF Press Freedom Score (100 = Perfect Freedom)"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none", 
        axis.title.x = element_text(size = 11, face = "bold", color = "grey30", margin = margin(t = 10)),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.text.x = element_text(size = 11, color = "grey30"),
        panel.grid.major.x = element_line(linewidth = 0.35, color = "grey85", linetype = "dashed"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1, lineheight = 1.2),
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

gr

# 4. Save ---------
ggsave(
    "Day6_DataDay_Top5_Exact.png", 
    plot = gr, 
    width = 9, 
    height = 7, 
    dpi = 600
)
