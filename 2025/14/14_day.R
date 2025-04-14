

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# create data --------

people <- data.table(
    name = c("Leonard", "Sandra", "Jill", "Amy", "Rachel", 
             "Jack", "Judy", "Carol", "Monica", 
             "Charles", "Nora", "Chandler",
             "Ross", "Ben"),
    x = c(-2, -1, -2, -1, 0, 
          1, 2, 1, 2, 
          3, 4, 3.5,
          0.5, 0.75),
    y = c(2, 2, 1, 1, 1, 
          2, 2, 1, 1, 
          2, 2, 1,
          1, 0)
)


# Add Emma and Susan
people <- rbind(
    people,
    data.table(
        name = c("Susan", "Emma"),
        x = c(1.5, 0.25),
        y = c(1, 0)
    )
)

# Updated plot with Emma and Susan
ggplot() +
    geom_point(data = people, aes(x = x, y = y), size = 6) +
    geom_text(data = people, aes(x = x, y = y, label = name), vjust = -1) +
    
    # Greens
    geom_segment(aes(x = -2, xend = -1, y = 2, yend = 2)) +
    geom_segment(aes(x = -1.5, xend = -1.5, y = 1.5, yend = 2)) +
    geom_segment(aes(x = -1.5, xend = -2, y = 1.5, yend = 1.5)) +
    geom_segment(aes(x = -1.5, xend = 0, y = 1.5, yend = 1.5)) +
    geom_segment(aes(x = -2, xend = -2, y = 1, yend = 1.5)) +
    geom_segment(aes(x = -1, xend = -1, y = 1, yend = 1.5)) +
    geom_segment(aes(x = 0, xend = 0, y = 1, yend = 1.5)) +
    
    # Ross & Rachel
    geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1)) +
    
    # Gellers
    geom_segment(aes(x = 1, xend = 2, y = 2, yend = 2)) +
    geom_segment(aes(x = 1.5, xend = 1.5, y = 1.5, yend = 2)) +
    geom_segment(aes(x = 0.5, xend = 2, y = 1.5, yend = 1.5)) +
    geom_segment(aes(x = 0.5, xend = 0.5, y = 1, yend = 1.5)) +
    geom_segment(aes(x = 2, xend = 2, y = 1, yend = 1.5)) +
    
    # Bings
    geom_segment(aes(x = 3, xend = 4, y = 2, yend = 2)) +
    geom_segment(aes(x = 3.5, xend = 3.5, y = 1.5, yend = 2)) +
    geom_segment(aes(x = 2, xend = 3.5, y = 1, yend = 1)) +
    geom_segment(aes(x = 3.5, xend = 3.5, y = 1, yend = 1.5)) +
    
    # Ross & Carol (Ben)
    geom_segment(aes(x = 0.5, xend = 1, y = 1, yend = 1)) +
    geom_segment(aes(x = 0.65, xend = 0.75, y = 0.98, yend = 1.02), linewidth = .8) +
    geom_segment(aes(x = 0.75, xend = 0.85, y = 0.98, yend = 1.02), linewidth = .8) +
    geom_segment(aes(x = 0.75, xend = 0.75, y = 0, yend = 1)) +
    
    # Ross & Rachel (Emma)
    geom_segment(aes(x = 0, xend = 0.5, y = 1, yend = 1)) +
    geom_segment(aes(x = 0.25, xend = 0.25, y = 0, yend = 1)) +
    
    # Carol & Susan (partnership)
    geom_segment(aes(x = 1, xend = 1.5, y = 1, yend = 1)) +
    
    theme_minimal()



# plot -----------

# 
# gr = df |>
#     
#     ggplot(aes(y = NAME, x = diff_from_avg, fill = Direction)) + 
#     
#     
#     geom_col(width = 0.7, alpha = 0.9) +
#     
#     geom_vline(xintercept = 0, color = "grey20", linetype = "dashed", size = 0.55) +
#     
#     scale_fill_manual(values = col) +
#     
#     labs(
#         title = "How Much Each U.S. State's Obesity Rate Differs from the National Average (29.3%)",
#         subtitle = "This chart shows the difference in adult obesity rates by state compared to the U.S. average (1990–2022). 
#                     <br>States <span style='color:#b24745;'><b>above</b></span> the average are in <span style='color:#b24745;'><b>red</b></span>, 
#                     while those <span style='color:#00429d;'><b>below</b></span> are in <span style='color:#00429d;'><b>blue</b></span></br>",
#         caption = "Source: <b> data.gov</b> | Graphic: <b>Natasa Anastasiadou</b>",
#         y = "",
#         x = ""
#     ) +
#     
# 
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#         legend.position = "none",  
#         
#         plot.title = element_markdown(size = 14, face = "bold", hjust = .25, margin = margin(b = 5, t = 5)),
#         plot.subtitle = element_markdown(size = 10, hjust = 0.3, color = "grey30", margin = margin(b = 15, t = 5)),
#         plot.caption = element_markdown(size = 8, hjust = 1, margin = margin(t = 10)),
#         
#         panel.grid.major = element_line(linewidth = 0.45, color = "grey80"),
#         panel.grid.minor = element_blank(),
# 
#         plot.margin = margin(20, 20, 20, 20),
#         plot.background = element_rect(fill = "grey93", color = NA)
#     )
# 
# 
# gr


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 8, units = "in", dpi = 600
)


# clear environment
rm(list = ls())
gc()

# load libraries
library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)

# create data --------

people <- data.table(
    name = c(
        "Leonard", "Sandra", "Jill", "Amy", "Rachel", 
        "Jack", "Judy", "Carol", "Monica", 
        "Charles", "Nora", "Chandler",
        "Ross", "Ben", "Susan", "Emma",
        "Phoebe", "Mike", "Ursula", "Frank Buffay Sr.", "Lily Buffay", 
        "Triplet 1", "Triplet 2", "Triplet 3",
        "Frank Jr.", "Alice", "Frank's Jr. Mum",
        "Joey", "Giovanni (Joey) Sr", "Gloria", "Joey's sisters"
        ),
    x = c(
        -2, -1, -2, -1, 0,
         1, 2, 1, 2,
         3, 4, 3.5,
         0.5, 0.75, 1.35, 0.25,
         7, 8, 6, 6, 7, 
         4.5, 5, 5.5,
         5.3, 4.5, 5,
         9, 9, 10, 10
    ),
    y = c(
        2, 2, 1, 1, 1,
        2, 2, 1, 1,
        2, 2, 1,
        1, 0, 1, 0,
        1, 1, 1, 2, 2, 
        0, 0, 0,
        1, 1, 2,
        1, 2, 2, 1
    )
)

people[, sex := c(
    # Greens
    "M", "F", "F", "F", "F",  
    # Gellers
    "M", "F", "F", "F",
    # Bings
    "M", "F", "M",
    # Ross & kids
    "M", "M", "F", "F",
    # Phoebe’s side
    "F", "M", "F", "M", "F",
    "F", "M", "F",
    "M", "F", "F",
    # Tribbianis
    "M", "M", "F", "F"
)]


col = c("#00429d", "#b24745")

# plot -----------

gr = ggplot() +

    # Greens
    geom_segment(aes(x = -2, xend = -1, y = 2, yend = 2), color = "grey75") +
    geom_segment(aes(x = -1.5, xend = -1.5, y = 1.5, yend = 2), color = "grey75") +
    geom_segment(aes(x = -1.5, xend = -2, y = 1.5, yend = 1.5), color = "grey75") +
    geom_segment(aes(x = -1.5, xend = 0, y = 1.5, yend = 1.5), color = "grey75") +
    geom_segment(aes(x = -2, xend = -2, y = 1, yend = 1.5), color = "grey75") +
    geom_segment(aes(x = -1, xend = -1, y = 1, yend = 1.5), color = "grey75") +
    geom_segment(aes(x = 0, xend = 0, y = 1, yend = 1.5), color = "grey75") +
    
    # Ross & Rachel
    geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1), color = "grey75") +
    geom_segment(aes(x = 0.25, xend = 0.25, y = 0, yend = 1), color = "grey75") +
    
    # Gellers
    geom_segment(aes(x = 1, xend = 2, y = 2, yend = 2), color = "grey75") +
    geom_segment(aes(x = 1.5, xend = 1.5, y = 1.5, yend = 2), color = "grey75") +
    geom_segment(aes(x = 0.5, xend = 2, y = 1.5, yend = 1.5), color = "grey75") +
    geom_segment(aes(x = 0.5, xend = 0.5, y = 1, yend = 1.5), color = "grey75") +
    geom_segment(aes(x = 2, xend = 2, y = 1, yend = 1.5), color = "grey75") +
    
    # Bings
    geom_segment(aes(x = 3, xend = 4, y = 2, yend = 2), color = "grey75") +
    geom_segment(aes(x = 3.5, xend = 3.5, y = 1.5, yend = 2), color = "grey75") +
    geom_segment(aes(x = 2, xend = 3.5, y = 1, yend = 1), color = "grey75") +
    geom_segment(aes(x = 3.5, xend = 3.5, y = 1, yend = 1.5), color = "grey75") +
    
    # Ross & Carol
    geom_segment(aes(x = 0.5, xend = 1, y = 1, yend = 1), color = "grey75") +
    geom_segment(aes(x = 0.65, xend = 0.75, y = 0.98, yend = 1.02), linewidth = .8, color = "grey75") +
    geom_segment(aes(x = 0.75, xend = 0.85, y = 0.98, yend = 1.02),linewidth = .8, color = "grey75") +
    geom_segment(aes(x = 0.75, xend = 0.75, y = 0, yend = 1), color = "grey75") +
    
    # Buffays: Frank & Lily -> Phoebe & Ursula
    geom_segment(aes(x = 6, xend = 7, y = 2, yend = 2), color = "grey75") +  # Frank Sr. + Lily
    geom_segment(aes(x = 6.5, xend = 6.5, y = 1.5, yend = 2), color = "grey75") +  # Down to children
    geom_segment(aes(x = 5.3, xend = 7, y = 1.5, yend = 1.5), color = "grey75") +  # Horizontal to connect kids
    geom_segment(aes(x = 6, xend = 6, y = 1, yend = 1.5), color = "grey75") +  # Ursula
    geom_segment(aes(x = 7, xend = 7, y = 1, yend = 1.5), color = "grey75") +  # Phoebe
    geom_segment(aes(x = 5.3, xend = 5.3, y = 1, yend = 1.5), color = "grey75") + 
    
    # Phoebe + Mike
    geom_segment(aes(x = 7, xend = 8, y = 1, yend = 1), color = "grey75") +
    
    # Frank Jr + Alice
    geom_segment(aes(x = 4.5, xend = 5.3, y = 1, yend = 1), color = "grey75") +
    
    # triplets
    geom_segment(aes(x = 5, xend = 5, y = 0.5, yend = 1), color = "grey75") +
    geom_segment(aes(x = 4.5, xend = 5.5, y = 0.5, yend = 0.5), color = "grey75") +
    geom_segment(aes(x = 4.5, xend = 4.5, y = 0, yend = 0.5), color = "grey75") +
    geom_segment(aes(x = 5, xend = 5, y = 0, yend = 0.5), color = "grey75") +
    geom_segment(aes(x = 5.5, xend = 5.5, y = 0, yend = 0.5), color = "grey75") +
    
    # susan carol
    geom_segment(aes(x = 0.75, xend = 1.35, y = 1, yend = 1), color = "grey75") +
    
    #tribianni
    geom_segment(aes(x = 9, xend = 10, y = 2, yend = 2), color = "grey75") +
    geom_segment(aes(x = 9.5, xend = 9.5, y = 1.5, yend = 2), color = "grey75") +
    geom_segment(aes(x = 9, xend = 9.5, y = 1.5, yend = 1.5), color = "grey75") +
    geom_segment(aes(x = 9.5, xend = 10, y = 1.5, yend = 1.5), color = "grey75") +
    geom_segment(aes(x = 9, xend = 9, y = 1, yend = 1.5), color = "grey75") +
    geom_segment(aes(x = 10, xend = 10, y = 1, yend = 1.5), color = "grey75") +
    
    geom_text(data = people, aes(x = x, y = y, label = name), vjust = -1, size = 3) +
    
    geom_point(data = people, aes(x = x, y = y, color = sex), size = 4,
               shape = ifelse(people$sex == "M", 15, 16)) +
    
    scale_color_manual(values = col) +
    
    theme_minimal() +
    
    theme(
        legend.position = "none",  
        
        plot.title = element_markdown(size = 14, face = "bold", hjust = .25, margin = margin(b = 5, t = 5)),
        plot.subtitle = element_markdown(size = 10, hjust = 0.3, color = "grey30", margin = margin(b = 15, t = 5)),
        plot.caption = element_markdown(size = 8, hjust = 1, margin = margin(t = 10)),
        
        axis.text = element_blank(),
        axis.title = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        plot.margin = margin(20, 20, 20, 20),
        plot.background = element_rect(fill = "grey93", color = NA)
    )


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 14, height = 8, units = "in", dpi = 600
)



