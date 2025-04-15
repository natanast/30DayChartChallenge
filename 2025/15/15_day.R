

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

team_results <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-03-26/team-results.csv')
public_picks <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-03-26/public-picks.csv')


# data cleaning -----------

top_10_teams <- team_results[order(-PASE)][1:10]

df <- top_10_teams[, .(TEAM, PASE, CHAMPPERCENT)]

df$CHAMPPERCENT <- df$CHAMPPERCENT |> str_remove_all("%") |> as.numeric()

df$PASE



# 🟣 CHAMPPERCENT = what people thought would happen (perception).
# 
# 🟢 PASE = what actually happened (performance).


# plot -----------

# Process the data 
df_plot <- df[, {
    dat.egg <- circleProgressiveLayout(CHAMPPERCENT)
    dat.egg <- circleLayoutVertices(dat.egg, npoints = 100)
    
    cbind(dat.egg, .SD[dat.egg$id])
}]


df_plot_l <- df_plot |>
    group_by(TEAM, CHAMPPERCENT, PASE, id) |>
    summarise(
        x = (min(x) + max(x)) / 2,
        y = (min(y) + max(y)) / 2
    )

# df_plot_l$lbl = df_plot_l$CHAMPPERCENT |> round(digits = 2) |> paste0(" %")
df_plot_l$lbl = df_plot_l$PASE



# my_col = c('#00429d', '#325da9', '#4e78b5', '#6694c1', '#80b1cc', '#9dced6', '#c0eade', '#ffdac4', '#ffb3a7', '#fb8a8c', '#eb6574', '#d5405e', '#BC3C29','#b9b8e7','#3a5cbc', '#b9b8e7','#BC3C29','#E18727','#0072B5', '#dddaea', '#20854E','#FFDC91','#6F99AD', '#80b1cc', '#9dced6','#91D1C2','#F39B7F','#b24745', '#B09C85','#9dced6', '#c0eade', '#ffdac4')
# 
# 
# # Define colors for the top 5 and others
# # colors = c("Top 1" = "#b24745", "Top 2" = "#6A6599", "Top 3" = "#9dced6", "Top 4" = "#F39B7F", "Top 5" = "#FFDC91", "Others" = "grey70")
# 
# col = c('#b24745', '#6A6599', '#4e78b5', '#6694c1', '#80b1cc', '#9dced6', '#c0eade', '#ffdac4', '#ffb3a7', '#fb8a8c')
# 
# colors =  c('#2c5769', '#6F99AD', 'grey96', '#ffb5ac', '#a33a3a')


col = c('#396375', '#5a8192', '#7f9faa', '#a7bec0', '#d2ded1', '#febaad', '#f49992', '#e37b78', '#cc5f5e', '#b24745')


col = c('#396375', '#6c909e',  '#a7bec0',  '#e8efd9', '#ffccba', '#febaad', '#faa99f', '#f49992', '#ec8a85', '#e37b78', '#d86d6b', '#cc5f5e', '#c05352', '#b24745')

p <- df_plot |>
    
    ggplot(aes(x, y, group = id)) +
    
    geom_polygon(aes(fill = TEAM), color = "grey30", linewidth = .25) +
    
    geom_shadowtext(
        data = df_plot_l, 
        aes(x, y, label = lbl, size = CHAMPPERCENT),
        inherit.aes = FALSE,
        color = "grey1", 
        bg.color = "#d9e3f1", 
        bg.r = .05
    ) +
    
    
    scale_fill_manual(
        values = col
    ) +

    scale_size_continuous(guide = "none", range = c(5, 8)) +
    
    
    coord_equal() +
    
    # labs(title = "World's Fairs: Average Cost per Visitor by Decade in Millions",
    # 
    #      subtitle = paste0(
    #          "Highlighting the Top 5 Most Expensive Expositions"
    #      ),
    # 
    #      caption = paste0(
    #          "Source: <b>Wikipedia's list of world expositions</b> | ",
    #          "Graphic: <b>Natasa Anastasiadou</b>"
    #      )
    # ) +
    
    theme_minimal() +
    
    theme(
        
        axis.title = element_blank(),
        axis.text = element_blank(),
        
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 8),
        
        panel.grid.major = element_line(linewidth = .35, color = "grey85"),
        panel.grid.minor = element_line(linewidth = .35, color = "grey85", linetype = "dashed"),
        
        plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "Candara"),
        plot.subtitle = element_text(size = 16, hjust = 0.5, family = "Candara", color = "grey30"),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 10, family = "Candara", hjust = 1.3),
        
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
        
    )

p


ggsave(
    plot = p, filename = "Rplot.png",
    width = 12, height = 10, units = "in", dpi = 600
)    



