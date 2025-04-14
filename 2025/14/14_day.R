

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)
library(kinship2)


# load data --------

friends <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv")


# data cleaning -----------


# 
# col = c("#73a2c6", "#b24745")
# 
# col = c("#00429d", "#b24745")
# 
# col <- c("Below" = "#73a2c6", "Above" = "#b24745")
# 

# plot --------


df <- data.frame(
    id = c(1,2,3,4,5,6), 
    sex = c(1,2,1,2,2,2), 
    dadid = c(0,0,0,0,1,3), 
    momid = c(0,0,0,0,2,4), 
    famid = 1
)



relation1 <- matrix(c(2,3,4,1), nrow = 1)

foo <- pedigree(
    id = df$id, 
    dadid = df$dadid, 
    momid = df$momid, 
    sex = df$sex, 
    relation = relation1, 
    famid = df$famid
)

ped <- foo['1']

plot(ped)


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




library(ggplot2)

# Define the 4 people and their positions
people <- data.frame(
    name = c("Carol", "Ross", "Monica", "Chandler"),
    x = c(1, 2, 3, 4),
    y = c(1, 1, 1, 1)
)

# # Relationship text annotations
# annotations <- data.frame(
#     label = c("Ex-Wife", "Siblings", "Married"),
#     x = c(1.5, 2.5, 3.5),
#     y = c(1.2, 1.2, 1.2)
# )

# Plot with points, names, and relationship labels
ggplot() +
    geom_point(data = people, aes(x = x, y = y), size = 5, color = "steelblue") +
    geom_text(vjust = -1.2, size = 5) +
    # geom_text(data = annotations, aes(x = x, y = y, label = label), size = 4, color = "darkred") +
    theme_void() +
    coord_fixed() 
    # ggtitle("Friends Kinship Snippet: Carol – Ross – Monica – Chandler")

library(ggplot2)

# Define the people and their positions
people <- data.frame(
    name = c("Leonard", "Sandra", "Jill", "Amy", "Rachel", "Jack", "Judy", "Ross", "Monica"),
    x = c(-2, -1, -2, -1, 0, 1, 2, 1, 2),
    y = c(2, 2, 1, 1, 1, 2, 2, 1, 1)
)


# Plot
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

    # Gellers
    geom_segment(aes(x = 1, xend = 2, y = 2, yend = 2)) +
    geom_segment(aes(x = 1.5, xend = 1.5, y = 1.5, yend = 2)) +
    geom_segment(aes(x = 1, xend = 1.5, y = 1.5, yend = 1.5)) +
    geom_segment(aes(x = 1.5, xend = 2, y = 1.5, yend = 1.5)) +
    geom_segment(aes(x = 1, xend = 1, y = 1, yend = 1.5)) +
    geom_segment(aes(x = 2, xend = 2, y = 1, yend = 1.5)) +
    
    # geom_segment(aes(x = 1, xend = 2, y = 1, yend = 1)) +
    
    theme_minimal() 


