

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(colorspace)


# load data ------

dt_chars <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-30/character_visualization.csv')


# clean data -------

setnames(dt_chars, "character", "hero")
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
        title = "Activity of Five Major X-Men Characters",
        subtitle = "Aggregating issues into 10-issue animation sequences. Notice the high **uncertainty** as<br><span style='color:#7AA6DC'>**Cyclops**</span> fades from the timeline, while <span style='color:#8F7700'>**Wolverine**</span> remains an absolute constant.",
        caption = "30DayChartChallenge 2026: <b> Day 27 </b> | Source: <b>The Claremont Run X-men </b> | Graphic: <b>Natasa Anastasiadou</b>",
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
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.25),
        
        panel.border = element_rect(fill = NA, linewidth = .4),
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "grey93", color = NA)
    )

gr


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 9, units = "in", dpi = 600
)

