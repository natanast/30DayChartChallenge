

rm(list = ls())
gc()


# load libraries -----

library(data.table)
library(ggplot2)
library(ggtext)
library(ggridges) 
library(extrafont)


# load data ------

dt <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-07-18/detectors.csv')


# clean data ------

dt$kind <- dt$kind |> factor(levels = c("Human", "AI"), labels = c("Human Text", "AI Text"))


bg_light  <- "grey93"
text_main <- "#1a1a1c"


fill_colors <- c("Human Text" = "#94a3b8", "AI Text" = "#466370")

gr <- ggplot(dt, aes(x = .pred_AI, y = detector, fill = kind)) +
    
    # THE VISUALIZATION: Density Ridges
    # The wider the "hill", the more uncertain the model is.
    geom_density_ridges(
        alpha = 0.7, 
        color = "white", 
        linewidth = 0.5,
        scale = 1.2, # Allows the ribbons to overlap slightly for depth
        rel_min_height = 0.01
    ) +
    
    # Facet by the actual source of the text
    facet_wrap(~kind) +
    
    # Format X-axis as percentages
    scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
    scale_fill_manual(values = fill_colors) +
    
    labs(
        title = "The Uncertainty of AI Detection Models",
        subtitle = "Visualizing the predicted probabilities of top AI-detection **Models**. A sharp spike near 0% or 100% shows<br>high confidence, while a wide spread across the middle reveals the model's **uncertainty**.",
        caption = "30DayChartChallenge 2026: <b> Day 28</b> | Source: <b> GPT detectors (TidyTuesday)</b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Model's Predicted Probability of being AI-Generated",
        y = ""
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        plot.background = element_rect(fill = bg_light, color = NA),
        panel.background = element_rect(fill = bg_light, color = NA),
        
        strip.text = element_text(face = "bold", size = 12, color = text_main),
        strip.background = element_blank(),
        
        panel.grid.major.x = element_line(linewidth = 0.35, color = "grey85"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        
        axis.text.y = element_text(color = text_main, size = 11, face = "bold"),
        axis.text.x = element_text(color = "grey40", size = 9),
        
        plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.2),
        
        panel.spacing = unit(2, "lines"),
        plot.margin = margin(20, 20, 20, 20)
    )

gr



# plot --------
# 
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
        # plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        # plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        # plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.2),

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

