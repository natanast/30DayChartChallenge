

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(friends)
library(ggdist)


# load data ------

dt_raw <- friends_info 


# clean data ------


dt_raw |> setDT()

# Ensure season is a factor for the X-axis
dt_raw[, season := factor(season)]

# 3. PLOT (Clean, Centered, Monochrome)
bg_light   <- "grey93"
dot_light  <- "#bdbdbd" 
line_dark  <- "#2b2b2b" 

# 2. DEFINE YOUR MONOCHROME HERO COLOR (From your previous chart)
friends_blue <- "#2c5769"

# 3. PLOT
bg_light <- "grey93"

gr <- ggplot(dt_raw, aes(x = season, y = imdb_rating)) +
    
    # THE UNCERTAINTY: Half-eye density shapes
    # Using a light tint of your blue
    stat_halfeye(
        fill = friends_blue |> lighten(0.6),
        color = NA,
        width = .6, 
        .width = 0, 
        justification = -.3,
        alpha = 0.8
    ) +
    
    # THE RAW DATA: Jittered points
    # Using a sequential gradient of your blue
    geom_jitter(
        # data = dt_raw,
        # aes(color = imdb_rating, fill = imdb_rating),
        color = "white", 
        fill = friends_blue,
        width = .1, 
        alpha = 0.65, 
        size = 2.5,
        shape = 21
    ) +
    
    # THE CENTRAL TREND: Median line and points
    # Using a dark shade of your blue
    stat_summary(
        fun = median, 
        geom = "line", 
        aes(group = 1), 
        color = friends_blue |> darken(0.10), 
        linewidth = .5
    ) +
    
    stat_summary(
        fun = median, 
        geom = "point", 
        color = friends_blue |> darken(0.20),
        fill = friends_blue |> darken(0.15), 
        size = 4, 
        shape = 21
    ) +
    
    scale_y_continuous(limits = c(6.5, 10), breaks = seq(7, 10, 0.5)) +
    
    # labs(
    #     title = "IMDb User Ratings and Distribution for Friends",
    #     subtitle = "A monochrome analysis of viewer consensus across ten seasons. The density shapes highlight the<br>**uncertainty** and variance in episode quality compared to the steady seasonal medians.",
    #     caption = "30DayChartChallenge 2026: **Day 29** | Source: **FRIENDS** | Graphic: **Natasa Anastasiadou**",
    #     x = "",
    #     y = "IMDb Rating"
    # ) +
    
    labs(
        title = "IMDb User Ratings and Distribution for Friends",
        subtitle = "A monochrome analysis of viewer consensus across ten seasons. The density shapes highlight the<br>**uncertainty** and variance in episode quality compared to the steady seasonal medians.",
        caption = "30DayChartChallenge 2026: <b> Day 29 </b>
                   | Source: <b> Friends {R package}</b>
                   | Graphic:<b> Natasa Anastasiadou</b>",
        x = "Season",
        y = "IMDb Rating"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        plot.background = element_rect(fill = bg_light, color = NA),
        panel.background = element_rect(fill = bg_light, color = NA),
        
        # Centered Cédric Scherer-style layout
        plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1.2),
        
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.4, color = "#e0e0e0"),
        panel.grid.minor = element_blank(),
        
        axis.text = element_text(color = "grey40", size = 11, face = "bold"),
        axis.title = element_text(color = "grey30", size = 10, face = "bold"),
        
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
    # labs(
    #     title = "The Student Trade: Erasmus Mobility",
    #     subtitle = "Mapping the **relationships** and student **trade** between Europe's top 15 academic destinations.",
    #     caption = "30DayChartChallenge 2026: <b> Day 14</b>
    #                | Source: <b> Erasmus Data (TidyTuesday)</b>
    #                | Graphic: <b>Natasa Anastasiadou</b>",
    #      x = "Sending Country",
    #      y = "Receiving Country"
    # ) +
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

