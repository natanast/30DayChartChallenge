

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(ggrepel)

# load data ------

 
dt <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-04-09/eclipse_total_2024.csv" |>
    fread()


# clean data -----

# 2. Calculate duration and filter ------
dt[, duration_min := as.numeric(as.ITime(eclipse_4) - as.ITime(eclipse_3)) / 60]

# Bring back all 6 states
target_states <- c("TX", "AR", "IN", "OH", "NY", "ME")
dt_plot <- dt[state %in% target_states & duration_min > 0]

# Convert state to a factor so the legend stays in geographic order (South to North)
dt_plot[, state := factor(state, levels = c("TX", "AR", "IN", "OH", "NY", "ME"))]

# 3. Create the Palette -------
cols <- c(
    "TX" = "#b25c56",  
    "AR" = "#e8998f",  
    "IN" = "#fcd4be",  
    "OH" = "#85aebc",  
    "NY" = "#8aa39b",  
    "ME" = "#4a6b7c"   
)


dt_extremes <- rbind(
    dt_plot[order(-duration_min)][1], 
    dt_plot[order(duration_min)][1]   
)
# plot --------

gr <- ggplot(dt_plot, aes(x = lon, y = duration_min)) +
    
    geom_point(
        aes(fill = state),
        alpha = 0.8, 
        size = 2, 
        stroke = 0.15, 
        color = "white",
        shape = 21
    ) +
    
    geom_smooth(
        method = "loess", 
        color = "#396375", 
        fill = "#396375",
        linewidth = 0.75, 
        lineend = "round"
    ) +
    
   scale_fill_manual(values = cols, name = "State") +
    
    guides(fill = guide_legend(nrow = 1)) +
    
    labs(
        title = "Fading to black: The 2024 eclipse",
        subtitle = "Visualizing the duration of totality across the United States.<br>As the moon's shadow swept from Texas to Maine, the window of total darkness steadily decreased.",
        caption = "30DayChartChallenge 2026: <b> Day 15 (Correlation) </b> | Source: <b> NASA </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Longitude (Degrees West)",
        y = "Duration of Totality (Minutes)"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        panel.grid.major = element_line(color = "grey85", linetype = "dashed", linewidth = 0.4),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#f2f2f2", color = NA),
        panel.background = element_rect(fill = "#f2f2f2", color = NA),
        
        axis.text = element_text(size = 9, color = "grey40", face = "bold"),
        axis.title.x = element_text(size = 11, face = "bold", color = "grey30", margin = margin(t = 15)),
        axis.title.y = element_text(size = 11, face = "bold", color = "grey30", margin = margin(r = 15)),
        
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        
        plot.title = element_text(size = 20, face = "bold", color = "black", hjust = 0.5),
        plot.subtitle = element_markdown(size = 11, color = "grey40", hjust = 0.5, lineheight = 1.3, margin = margin(b = 20)),
        plot.caption = element_markdown(margin = margin(t = 20), size = 8, color = "grey50", hjust = 1),
        
        plot.margin = margin(30, 30, 30, 30)
    )

gr


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
# 


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)


