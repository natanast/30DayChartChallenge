

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(ggrepel)

# load data ------


# clean data -----



# plot --------

    
    # theme(
    #     # The light grey background with dashed gridlines
    #     panel.grid.major = element_line(color = "grey85", linetype = "dashed", linewidth = 0.4),
    #     panel.grid.minor = element_blank(),
    #     plot.background = element_rect(fill = "#f2f2f2", color = NA),
    #     panel.background = element_rect(fill = "#f2f2f2", color = NA),
    #     
    #     # Axis text styling
    #     axis.text = element_text(size = 9, color = "grey40", face = "bold"),
    #     axis.title.x = element_text(size = 11, face = "bold", color = "grey30", margin = margin(t = 15)),
    #     axis.title.y = element_text(size = 11, face = "bold", color = "grey30", margin = margin(r = 15)),
    #     
    #     # Legend styling
    #     legend.position = "bottom",
    #     legend.title = element_text(face = "bold", size = 10),
    #     
    #     
    #     # Titles
    #     plot.title = element_text(size = 20, face = "bold", color = "black", hjust = 0.5),
    #     plot.subtitle = element_markdown(size = 11, color = "grey40", hjust = 0.5, lineheight = 1.3, margin = margin(b = 20)),
    #     plot.caption = element_markdown(margin = margin(t = 20), size = 8, color = "grey50", hjust = 1),
    #     
    #     plot.margin = margin(30, 30, 30, 30)
    # )


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


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)






