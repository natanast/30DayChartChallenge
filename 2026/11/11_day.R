#fix labs and size of the x axis letter

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(stringr)


# load data ------

dt <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2020/2020-05-12/volcano.csv" |> 
    fread()


# clean data -----


dt$type_clean <- dt$primary_volcano_type |> str_replace_all("\\(es\\)|\\(s\\)", "")
    
# Get the top 5 types
top_types <- dt[, .N, by = type_clean][order(-N)][1:5, type_clean]
dt_phys <- dt[type_clean %in% top_types]

# Group elevations into 500m bins
dt_phys[, elevation_binned := round(elevation / 500) * 500]


dt_counts <- dt_phys[, .(N = .N), by = .(type_clean, elevation_binned)]


#  plot -----

col <- c("#b24745", "#db9044", "#5a8192", "#7f9faa", "#a8b5b2")


gr <- ggplot(dt_counts, aes(x = type_clean, y = elevation_binned, color = type_clean, fill = type_clean)) +
    
    geom_point(
        aes(size = N), 
        alpha = 0.85,
        shape = 21,
        stroke = 0.5
    ) +
    

    scale_size_continuous(
        range = c(3, 13), 
        name = "Number of\nVolcanoes", 
        breaks = c(10, 25, 50, 75)
    ) +
    
    guides(
        size = guide_legend(
            override.aes = list(
                fill = "grey80", 
                color = "grey40", 
                stroke = 0.5,
                size = c(4, 6, 8, 10) 
            )
        )
    ) +
    
    scale_color_manual(values = col, guide = "none") +
    scale_fill_manual(values = col, guide = "none") +
    
    labs(
        title = "THE PHYSICAL STATURE OF VOLCANOES",
        subtitle = "Distribution of global volcanoes by their physical structure and elevation.<br>Elevations are grouped into 500-meter intervals. **Larger bubbles indicate a higher concentration.**",
        caption = "30DayChartChallenge 2026: <b> Day 11 </b> | Source: <b> Smithsonian Institution </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "", 
        y = "Elevation (Meters above sea level)"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.key.size = unit(0.05, "cm"),
        legend.position = "right",
        legend.title = element_text(size = 10, face = "bold", color = "grey40"),
        legend.text = element_text(size = 9, color = "grey30"),
        
        axis.text.x = element_text(size = 12, face = "bold", color = "black", margin = margin(t = 10)),
        axis.text.y = element_text(size = 10, face = "bold", color = "grey50"),
        axis.title.y = element_text(size = 11, face = "bold", color = "grey30", margin = margin(r = 15)),
        

        panel.grid.major = element_line(linewidth = 0.35, color = "grey85", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
        
        plot.background = element_rect(fill = "grey95", color = NA),
        plot.margin = margin(20, 20, 20, 20)
        

    )

gr


# 
# 
# gr = ggplot(df_dots, aes(x, y)) +
#     
#     geom_point(aes(fill = species), size = 4.5, shape = 21, color = "white", stroke = .25) +
#     
#     facet_wrap(~island, nrow = 1, strip.position = "bottom") +
#     
#     coord_equal() +
#     
#     scale_fill_manual(values = col) +
#     
#     scale_x_continuous(limits = c(0.5, 10.5)) +
#     scale_y_continuous(
#         limits = c(0.5, 10.5), 
#         breaks = c(0.5, 5.5, 10.5), 
#         labels = c("0%", "50%", "100%")
#     ) +
#     
#     labs(
#         title = "Penguin Demographics Across the Palmer Archipelago",
#         subtitle = "<b>Each dot</b> represents <b>1%</b> of the total penguin population on that island.",
#         caption = "30DayChartChallenge 2026: <b> Day 1</b>
#                    | Source: <b> palmerpenguins (R package)</b>
#                    | Graphic: <b>Natasa Anastasiadou</b>",
#         
#     ) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#         legend.position = "bottom",
#         
#         axis.title = element_blank(),
#         # axis.text = element_blank(),
#         
#         axis.text.x = element_blank(),
#         axis.text.y = element_text(size = 9.5),
#         
#         strip.text = element_text(size = 9.5),
#         
        # panel.grid.major = element_line(linewidth = 0.35, color = "grey85"),
        # panel.grid.minor = element_blank(),
        # 
        # plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        # plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        # plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),

#         plot.background = element_rect(fill = "grey95", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 9, height = 9, units = "in", dpi = 600
)
