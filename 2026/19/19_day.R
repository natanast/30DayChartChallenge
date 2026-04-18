
rm(list = ls())
gc()


# load libraries -----

library(ggplot2)
library(dplyr)
library(stringr)
library(data.table)
library(gghighlight)
library(ggtext)
library(extrafont)


# Load data -------

# dataset: 15Y Stock Data: NVDA, AAPL, MSFT, GOOGL & AMZN
dt <- fread("Stock_data.csv")


# Clean data ------

dt_price <- melt(dt, 
                 id.vars = "Date", 
                 measure.vars = patterns("^High_"), 
                 variable.name = "Company", 
                 value.name = "High_Price")


dt_price[, Company := gsub("High_", "", Company)]
dt_price$date <- as.Date(dt_price$Date)


dt_price$year <- format(dt_price$date, "%Y")


yearly_avg <- dt_price[, .(avg_price = mean(High_Price, na.rm = TRUE)), by = .(Company, year)]


company_map <- data.table(
    Company = c("AAPL", "AMZN", "GOOGL", "MSFT", "NVDA"),
    company_name = c("Apple", "Amazon", "Alphabet", "Microsoft", "NVIDIA")
)

df <- merge(yearly_avg, company_map, by = "Company")

df_2024 <- df[year == "2024", ]


# plot -------

col <- c("Alphabet" = "#1c499e", 
         "Amazon" = "#964b22", 
         "Apple" = "#b03030", 
         "Microsoft" = "#cf8f3a", 
         "NVIDIA" = "#6ca1c9")

gr <- ggplot(df, aes(x = as.numeric(year), y = avg_price, color = company_name)) +
    
    # Swapped geom_smooth for the precise geom_line
    geom_line(linewidth = 1) +
    
    # The endpoint dots
    geom_point(data = df_2024, aes(x = as.numeric(year), y = avg_price, fill = company_name), 
               size = 4, shape = 21, stroke = 0.5, color = "white") +  
    
    # The background ecosystem highlights
    gghighlight(use_direct_label = FALSE,
                unhighlighted_params = list(colour = alpha("grey83", 1), linewidth = 0.6)) +
    
    # The price labels
    geom_text(
        data = df_2024,
        aes(
            x = as.numeric(year),
            y = avg_price,
            label = paste0("$", round(avg_price)),
            color = company_name
        ),
        size = 4,
        vjust = -1.2, 
        hjust = 0.5,
        family = "Candara",
        fontface = "bold"
    ) +
    
    facet_wrap('~company_name', ncol = 2) +
    
    scale_x_continuous(breaks = c(2010, 2015, 2020, 2024)) +
    scale_color_manual(values = col) +
    scale_fill_manual(values = col) +
    
    labs(
        title = "Stock Trends of Big Tech Companies Over the Years",
        subtitle = "Tracking the Annual Average High Prices of Leading Tech Giants from 2010 to 2024",
        caption = "30DayChartChallenge 2026: <b> Day 19 </b> | Source: Stock Data <b> (Kaggle) </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "",
        y = "Avg High Price (USD)"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "none",
        
        plot.title = element_markdown(size = 16, face = "bold", color = "grey20", hjust = 0.5, margin = margin(t = 2, b = 5)),
        plot.subtitle = element_markdown(size = 11, hjust = 0.5, color = "grey40", margin = margin(t = 5, b = 20)),
        plot.caption = element_markdown(margin = margin(t = 10), size = 7.5, hjust = 1),
        
        panel.grid.major = element_line(linewidth = .25, color = "grey80", linetype = "dashed", lineend = "round"),
        panel.grid.minor = element_line(linewidth = .25, color = "grey80", linetype = "dashed", lineend = "round"),
        
        axis.title.y = element_text(size = 10, vjust = 5, face = "bold", color = "grey30"),
        
        axis.text.x = element_text(size = 8, angle = 45, hjust = 1, color = "grey40"), 
        axis.text.y = element_text(size = 8, color = "grey40"),
        
        strip.text.x.top = element_text(size = 11, face = "bold", margin = margin(b = 10), color = "grey10"),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA),
        
        plot.margin = margin(20, 25, 20, 20)
    ) 

gr

# gr <- ggplot(dt_counts, aes(x = type_clean, y = elevation_binned, color = type_clean, fill = type_clean)) +
#     
#     geom_point(
#         aes(size = N), 
#         alpha = 0.85,
#         shape = 21,
#         stroke = 0.65
#     ) +
#     
# 
#     scale_size_continuous(
#         range = c(3, 13), 
#         name = "Number of\nVolcanoes", 
#         breaks = c(10, 25, 50, 75)
#     ) +
#     
#     guides(
#         size = guide_legend(
#             override.aes = list(
#                 fill = "grey80", 
#                 color = "grey40", 
#                 stroke = 0.5,
#                 size = c(4, 6, 8, 10) 
#             )
#         )
#     ) +
#     
#     scale_color_manual(
#         values = col |> darken(.15), 
#         guide = "none"
#     ) +
#     
#     scale_fill_manual(
#         values = col , 
#         guide = "none"
#     ) +
#     
#     labs(
#         title = "How Tall Are Different Types of Volcanoes",
#         subtitle = "Comparing the elevation distributions of five common volcanic structures.<br>Bubbles represent 500-meter intervals, with larger sizes indicating more volcanoes.",
#         caption = "30DayChartChallenge 2026: <b> Day 11 </b> | Source: <b> Volcano Eruptions (TidyTuesday) </b> | Graphic: <b>Natasa Anastasiadou</b>",
#         x = "", 
#         y = "Elevation (Meters above sea level)"
#     ) +
#     
#     theme_minimal(base_family = "Candara") +
#     
#     theme(
#         legend.key.size = unit(0.05, "cm"),
#         legend.position = "right",
#         legend.title = element_text(size = 10, face = "bold", color = "grey40"),
#         legend.text = element_text(size = 9, color = "grey30"),
#         
#         axis.text.x = element_text(size = 12, face = "bold", color = "black", margin = margin(t = 10)),
#         axis.text.y = element_text(size = 10, face = "bold", color = "grey50"),
#         axis.title.y = element_text(size = 11, face = "bold", color = "grey30", margin = margin(r = 15)),
#         
# 
#         panel.grid.major = element_line(linewidth = 0.35, color = "grey85", linetype = "dashed"),
#         panel.grid.minor = element_blank(),
#         
#         plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
#         plot.subtitle = element_markdown(size = 14, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
#         plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
#         
#         plot.background = element_rect(fill = "grey95", color = NA),
#         plot.margin = margin(20, 20, 20, 20)
#     )
# 
# gr



# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 9, height = 9, units = "in", dpi = 600
)


