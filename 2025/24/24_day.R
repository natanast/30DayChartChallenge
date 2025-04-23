

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

df = "WHO_data.csv" |> fread()
    
    
# data cleaning -----------

df <- df[, .(`World Bank income group`, Year, 
             `Life expectancy at birth (years)  Both sexes`,
             `Life expectancy at birth (years)  Male`, 
             `Life expectancy at birth (years)  Female`)]


colnames(df) <- c("Income_group", "Year", "LE_birth_both_sexes", "LE_birth_male", "LE_birth_female")





library(ggplot2)
library(data.table)

# Filter and factor income groups
df_plot <- df[Year %in% c(2000, 2019) & Income_group != "Global"]
df_plot[, Income_group := factor(Income_group, levels = c("Low-income", "Lower-middle-income", "Upper-middle-income", "High-income"))]

ggplot(df_plot, aes(x = as.factor(Year), y = LE_birth_both_sexes, group = Income_group)) +
    # geom_segment(aes(x = "2000", xend = "2019", 
    #                  y = LE_birth_both_sexes[Year == 2000], 
    #                  yend = LE_birth_both_sexes[Year == 2019]),
    #              data = dcast(df_plot, Income_group ~ Year, value.var = "LE_birth_both_sexes"),
    #              color = "gray70", size = 1.2) +
    geom_point(aes(color = as.factor(Year)), size = 4) +
    facet_wrap(~Income_group, ncol = 4) +
    labs(
        title = "Change in Life Expectancy at Birth (2000 vs 2019)",
        subtitle = "#30DayChartChallenge • Timeseries | WHO Data",
        x = "Year",
        y = "Life Expectancy (Years)",
        color = "Year"
    ) +
    scale_color_manual(values = c("2000" = "#F8766D", "2019" = "#00BFC4")) +
    theme_minimal(base_size = 14) +
    theme(
        panel.grid.major.x = element_blank(),
        legend.position = "top"
    )
# 
# library(data.table)
# library(ggplot2)
# 
# # Filter for years 2000 and 2019 only
# df_plot <- df[Year %in% c(2000, 2019) & Income_group != "Global"]
# 
# # Make sure Income_group is a factor for ordered plotting
# df_plot[, Income_group := factor(Income_group, levels = unique(Income_group))]
#
# # Plot
# ggplot(df_plot, aes(x = LE_birth_both_sexes, 
#                     y = Income_group, 
#                     color = as.factor(Year), 
#                     group = Income_group)) +
#     geom_segment(data = dcast(df_plot, Income_group ~ Year, value.var = "LE_birth_both_sexes"),
#                  aes(x = `2000`, xend = `2019`, y = Income_group, yend = Income_group),
#                  color = "gray70", size = 1.2) +
#     geom_point(size = 4) +
#     labs(
#         title = "Life Expectancy at Birth by Income Group (2000 vs 2019)",
#         subtitle = "Based on WHO data | #30DayChartChallenge - Timeseries • WHO",
#         x = "Life Expectancy (Years)",
#         y = NULL,
#         color = "Year"
#     ) +
#     scale_color_manual(values = c("2000" = "#F8766D", "2019" = "#00BFC4")) +
#     theme_minimal(base_size = 14) +
#     theme(legend.position = "top")




# plot -----------

col = c('#5a8192', '#b24745','#a2a0cf', "#00429d" )


p = ggplot(df_plot, aes(x = year, y = avg_close)) +
    
    geom_line(
        linewidth = .75, aes(color = company)
    ) +
    
    geom_point(
        shape = 21, stroke = .85, size = 4,
        aes(color = company), fill = "grey95"
    ) +
    
    scale_y_log10() +
    
    scale_x_continuous(
        breaks = c(2010, 2012, 2014, 2016, 2018, 2020, 2022)
    ) +
    
    scale_color_manual(
        values = col,
        name = "Companies"
    ) +
    
    labs(
        title = "Annual Closing Price Trends of Big Tech Giants",
        subtitle = "Year-over-year growth of Apple, Amazon, Netflix & Tesla from 2010 to 2022",
        caption = "30DayChartChallenge 2025: <b> Day 23</b> | Source: <b> Big Tech Stock Prices (TidyTuesday) </b> | Graphic: <b>Natasa Anastasiadou</b>",
        y = "Avg Closing Price (log scale)",
        fill = "Type"
    ) +

    theme_minimal(base_family = "Candara") +
    
    theme(
        
        axis.title.x = element_blank(),
        
        axis.title.y = element_text(size = 12),
        
        axis.text = element_text(size = 10),
        
        legend.position = c(.87, .25),
        legend.text = element_text(size = 10),         
        legend.title = element_text(size = 11),        
        legend.key.size = unit(0.8, "lines"),
        legend.spacing.y = unit(0.5, "lines"),
        

        panel.grid.major = element_line(color = "grey75", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 2, b = 2)),
        plot.subtitle = element_markdown(size = 13, hjust = 0.5,  color = "grey30", margin = margin(t = 5, b = 10)),
        plot.caption  = element_markdown(margin = margin(t = 25), size = 8, hjust = 1),
        
        
        plot.margin = margin(20, 20, 20, 20),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
        
    )



p


ggsave(
    plot = p, filename = "Rplot.png",
    width = 10, height = 10, units = "in", dpi = 600
)    

