

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)


# load data ------



# clean data ------



# plot -----




# plot --------
# 
# gr <- ggplot(dt, aes(x = dose, y = len, fill = supp)) +
#     
#     geom_boxplot(
#         width = 0.5, 
#         alpha = 0.75, 
#         outlier.shape = NA, 
#         position = position_dodge(width = 0.6) ,
#         color = "gray30"
#         
#     ) +
#     
#     geom_point(
#         position = position_jitterdodge(jitter.width = 0.1, dodge.width = 0.6),
#         # size = 2,
#         # alpha = 0.8,
#         shape = 21, size = 3.5, stroke = .25, alpha = 1.5,
#         color = "grey20"
#     ) +
#     
#     
#     
#     scale_fill_manual(values = col) +
#     # scale_color_manual(values = col) +
#     
#     labs(
#         title = "Natural vs. Artificial Vitamin C",
#         subtitle = "In a <b>guinea pig model</b>, low doses of Orange Juice promote significantly more cellular<br>growth than artificial supplements. At a high dose (2.0 mg), the advantage vanishes.",
#         caption = "30DayChartChallenge 2026: <b> Day 5 </b> | Source: <b> Crampton (1947) J. Nutr.</b> | Graphic: <b>Natasa Anastasiadou</b>",
#         y = "Odontoblast Length (Cellular Growth)",
#         x = "Vitamin C Dosage (mg/day)"
#     ) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#         
#         legend.position = "top",
#         legend.title = element_blank(),
#         legend.text = element_text(size = 11, face = "bold", color = "grey30"),
#         
#         axis.title.y = element_text(size = 11, face = "bold", color = "grey30", margin = margin(r = 10)),
#         axis.title.x = element_text(size = 11, face = "bold", color = "grey30", margin = margin(t = 10)),
#         
#         axis.text.x = element_text(size = 12, face = "bold", color = "black"),
#         axis.text.y = element_text(size = 12, color = "grey30"),
#         
#         panel.grid.major = element_line(linewidth = 0.35, color = "grey85"),
#         panel.grid.minor = element_blank(),
#         
#         plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 14, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
#         plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1),
#         
#         plot.background = element_rect(fill = "#e4e4e3", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )



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
library(ggridges) 
library(scales)   

# 1. Load data ------
# TidyTuesday: NASA Meteorite Landings (June 2019)
url <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2019/2019-06-11/meteorites.csv"
dt <- fread(url)

# 2. Clean and classify data ------
# Remove missing masses and impossible 0g weights
dt <- dt[!is.na(mass) & mass > 0]

# Meteorites have hundreds of sub-classes. Let's group them into the 4 major astronomical categories!
dt[, broad_class := fcase(
    grepl("Iron", class, ignore.case = TRUE), "Iron (Core fragments)",
    grepl("Pallasite|Mesosiderite", class, ignore.case = TRUE), "Stony-Iron (Mantle fragments)",
    grepl("Eucrite|Diogenite|Howardite|Achondrite|Lunar|Martian", class, ignore.case = TRUE), "Achondrite (Crust fragments)",
    default = "Chondrite (Primitive solar dust)"
)]

# PRO TIP: Stratified sampling! 
# We take a random sample of 400 meteorites per category so the jittered dots look beautiful and don't turn into a solid block of ink.
set.seed(2026)
dt_plot <- dt[, .SD[sample(.N, min(.N, 400))], by = broad_class]

# Order them logically by density
dt_plot[, broad_class := factor(broad_class, levels = c(
    "Chondrite (Primitive solar dust)", 
    "Achondrite (Crust fragments)", 
    "Stony-Iron (Mantle fragments)", 
    "Iron (Core fragments)"
))]

# Your editorial palette
col <- c(
    "Chondrite (Primitive solar dust)" = "#7f9faa", # Slate
    "Achondrite (Crust fragments)"     = "#6f6e9a", # Muted Purple
    "Stony-Iron (Mantle fragments)"    = "#db9044", # Terracotta/Orange
    "Iron (Core fragments)"            = "#b24745"  # Deep Red/Iron
)

# 3. Plot --------
gr <- ggplot(dt_plot, aes(x = mass, y = broad_class, fill = broad_class)) +
    
    geom_density_ridges(
        jittered_points = TRUE,
        position = position_points_jitter(width = 0.05, height = 0),
        point_shape = 21,
        point_size = 2,
        point_color = "white",
        point_fill = "grey20", 
        alpha = 0.8,
        color = "white",
        scale = 1.2
    ) +
    
    scale_fill_manual(values = col) +
    
    # THE MULTISCALE MAGIC:
    # Converting raw grams into highly readable biological/physical scales
    scale_x_log10(
        breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000),
        labels = c("1g", "10g", "100g", "1 kg", "10 kg", "100 kg", "1 Tonne", "10 Tonnes"),
        expand = expansion(mult = c(0.05, 0.1))
    ) +
    
    labs(
        title = "The Multiscale Mass of Meteorites",
        subtitle = "A logarithmic distribution of meteorite masses fallen to Earth.<br>Notice how dense <b>Iron</b> meteorites survive atmospheric entry at significantly larger scales.",
        caption = "30DayChartChallenge 2026: <b> Day 7 (Distributions)</b> | Source: <b> NASA / TidyTuesday </b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "", 
        x = "Meteorite Mass (Logarithmic Scale)"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none", 
        
        axis.title.x = element_text(size = 11, face = "bold", color = "grey30", margin = margin(t = 15)),
        axis.text.y = element_text(size = 12, face = "bold", color = "black", vjust = 0),
        axis.text.x = element_text(size = 11, face = "bold", color = "grey40"),
        
        panel.grid.major.x = element_line(linewidth = 0.4, color = "grey85", linetype = "dashed"),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(), 
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1, lineheight = 1.2),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

gr

# 4. Save ---------
ggsave(
    "Day7_Distributions_Meteorites.png", 
    plot = gr, 
    width = 9, 
    height = 7, 
    dpi = 600
)