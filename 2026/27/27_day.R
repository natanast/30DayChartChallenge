

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(colorspace)

# load data ------

dt_chars <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-03-19/mutant_moneyball.csv')

# clean data -----

dt_chars[, hero := gsub(" =.*", "", hero)]

top_5 <- c("Wolverine", "Storm", "Cyclops", "Nightcrawler", "Colossus")
dt_viz <- dt_chars[hero %in% top_5]

dt_long <- melt(
    dt_viz, 
    id.vars = c("issue", "hero"), 
    measure.vars = c("speech", "thought", "depicted"), 
    variable.name = "action", 
    value.name = "count"
)

dt_long[, issue_bin := floor(issue / 10) * 10]

dt_binned <- dt_long[, .(total_count = sum(count, na.rm = TRUE)), by = .(issue_bin, hero, action)]

# Cleanup
dt_binned <- dt_binned[total_count > 0]
dt_binned[, action := tools::toTitleCase(as.character(action))]
dt_binned[, hero := factor(hero, levels = top_5)]
dt_binned[, action := factor(action, levels = c("Depicted", "Speech", "Thought"))] 


# plot --------

bg_light <- "#f7f7f9"
text_main <- "#1a1a1c"
text_sub  <- "#5e5e66"

char_colors <- c(
    "Wolverine"    = "#8F7700",    
    "Storm"        = "#466370",        
    "Cyclops"      = "#7AA6DC",      
    "Nightcrawler" = "#c28d75", 
    "Colossus"     = "#a33a3a"      
)



gr <- ggplot(dt_binned, aes(x = issue_bin, y = hero, size = total_count, fill = hero, color = hero)) +
    
    
    geom_vline(xintercept = seq(100, 280, 10), color = "#e0e0e6", linewidth = 0.2) +
    
   
    geom_point(
        alpha = 0.95, 
        shape = 21, 
        stroke = 0.6
    ) +

    facet_grid(action ~ .) +
    
    scale_size_continuous(range = c(2, 10), name = "Total Activity") +
    scale_fill_manual(values = char_colors |> lighten(.05)) +
    scale_color_manual(values = char_colors |> darken(.15)) +
    
    labs(
        title = "The Mutant Pulse",
        subtitle = "Aggregating character focus into 10-issue blocks across the Claremont Run.<br>The <span style='color:#d92525'><b>uncertainty</b></span> of the narrative becomes a visible rhythm; note how <span style='color:#1d4ed8'><b>Cyclops</b></span> nearly flatlines <br>after issue 200 while <span style='color:#7a7a85'><b>Storm</b></span> maintains a dominant, steady heartbeat.",
        caption = "30DayChartChallenge 2026: <b> Day 27 (Uncertainties) </b> | Prompt: <b>Animation</b> | Source: <b>The Claremont Run</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Comic Book Issue Blocks (Groups of 10)",
        y = ""
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none", 
        
        strip.text.y = element_text(angle = 0, face = "bold", size = 12, color = text_main, hjust = 0),
        strip.background = element_blank(),
        
        # Horizontal "track" lines
        panel.grid.major = element_line(linewidth = 0.35, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        axis.text.y = element_text(color = text_main, size = 10, face = "bold"),
        axis.text.x = element_text(color = text_sub, size = 10, face = "bold"),
        axis.title.x = element_text(color = text_main, size = 11, margin = margin(t = 15), face = "bold"),
        
        plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1),
        
        panel.border = element_rect(fill = NA, linewidth = .4),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA)
    )

gr

# 
# 
# gr <- ggplot(dt_plot, aes(x = DIARCARE, y = ORSZINC)) +
#     
#     # Background Shading
#     annotate(
#         "rect", xmin = 50, xmax = 100, ymin = 15, ymax = 100, 
#         fill = "#678e9f", alpha = 0.1
#     ) +
#     
#     annotate(
#         "rect", xmin = 0,  xmax = 50,  ymin = 15, ymax = 100, 
#         fill = "#a8b2ba", alpha = 0.05
#     ) +
#     
#     annotate(
#         "rect", xmin = 50, xmax = 100, ymin = 0,  ymax = 15,  
#         fill = "#d18d8d", alpha = 0.1
#     ) +
#     
#     annotate(
#         "rect", xmin = 0,  xmax = 50,  ymin = 0,  ymax = 15,  
#         fill = "#b24745", alpha = 0.1
#     ) +
#     
#     
#     annotate("text", x = 98, y = 98, label = "High Seeking,\nHigh Medicine", 
#              hjust = 1, vjust = 1, size = 3.5, fontface = "bold", color = "#466370") +
#     
#     annotate("text", x = 2, y = 98, label = "Low Seeking,\nHigh Medicine", 
#              hjust = 0, vjust = 1, size = 3.5, fontface = "bold", color = "#7b868e") +
#     
#     annotate("text", x = 98, y = 2, label = "High Seeking,\nLow Medicine", 
#              hjust = 1, vjust = 0, size = 3.5, fontface = "bold", color = "#a66b6b") +
#     
#     annotate("text", x = 2, y = 2, label = "Low Seeking,\nLow Medicine", 
#              hjust = 0, vjust = 0, size = 3.5, fontface = "bold", color = "#b24745") +
#     
#     # Dividers
#     geom_vline(xintercept = 50, linetype = "dashed", color = "grey50", linewidth = 0.4) +
#     geom_hline(yintercept = 15, linetype = "dashed", color = "grey50", linewidth = 0.4) +
#     
#     
#     geom_point(
#         aes(fill = zone), 
#         size = 4, 
#         shape = 21, 
#         color = "white", 
#         stroke = 0.45
#     ) +
#     
#     
#     geom_text_repel(
#         data = dt_labels,
#         aes(label = `Countries and areas`),
#         family = "Candara", size = 3.5, fontface = "bold", 
#         box.padding = 0.8, point.padding = 0.3, color = "black"
#     ) +
#     
#     scale_fill_manual(values = c(
#         "High Seeking, High Medicine" = "#466370",
#         "Low Seeking, High Medicine"  = "#85a4b2",
#         "High Seeking, Low Medicine"  = "#d18d8d",
#         "Low Seeking, Low Medicine"   = "#b24745"
#     )) +
#     
#     scale_x_continuous(limits = c(0, 100), expand = c(0,0), labels = function(x) paste0(x, "%")) +
#     scale_y_continuous(limits = c(0, 100), expand = c(0,0), labels = function(x) paste0(x, "%")) +
#     
#     labs(
#         title = "Mapping the Access-Treatment Relationship",
#         subtitle = "A quadrant analysis of the **systemic relationship** <br>between medical consultations and treatment delivery for children in **Africa**.",
#         x = "% Children Seeking Care",
#         y = "% Children Receiving ORS + Zinc",
#         caption = "30DayChartChallenge 2026: <b> Day 18 </b> | Source: <b> Kaggle (UNICEF) </b> | Graphic: <b>Natasa Anastasiadou</b>",
#     ) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
# 
#         legend.position = "none",
# 
#         plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
#         plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1),
#         
#         panel.grid.major = element_line(linewidth = 0.35, color = "grey85"),
#         panel.grid.minor = element_blank(),
#         
#         plot.margin = margin(20, 20, 20, 20),
#         
#         plot.background = element_rect(fill = "#e4e4e3", color = NA)
#     )
# 
# gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 9, units = "in", dpi = 600
)



