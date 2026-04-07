# update plot, labs, x axis labels 

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)


# load data ------

dt <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-04-23/outer_space_objects.csv")


# clean data -----


# clean data -----

# Remove the "World" aggregate row so we don't double-count
dt <- dt[Entity != "World"]

# Remove the 1950s completely to start the chart cleanly at the 1960s Space Race
dt <- dt[Year >= 1960]

# Create a "Decade" column (e.g., 1960s, 1970s)
dt[, Decade := paste0(floor(Year / 10) * 10, "s")]

# Group entities into the Top 3 + "Other" to keep the colors clean
dt[, country := fcase(
    Entity == "United States", "USA",
    Entity %in% c("Russia", "Soviet Union"), "Russia/USSR",
    Entity == "China", "China",
    default = "Other"
)]

# Convert to factor so the colors always stack in this exact order
dt[, country := factor(country, levels = c("USA", "Russia/USSR", "China", "Other"))]



# 1. Sum up the objects launched for each Country within each Decade
df_counts <- dt[, .(total_objs = sum(num_objects)), by = .(Decade, country)]

# 2. Sum up total objects launched per Decade (This determines column WIDTH)
df_totals <- df_counts[, .(total_decade = sum(total_objs)), by = Decade]

# Order Decades chronologically
setorder(df_totals, Decade)
df_totals[, xmax := cumsum(total_decade)]
df_totals[, xmin := data.table::shift(xmax, fill = 0)]

# 3. Merge widths back and calculate percentages (This determines column HEIGHT)
df_counts <- merge(df_counts, df_totals[, .(Decade, xmin, xmax)], by = "Decade")
df_counts <- merge(df_counts, df_totals[, .(Decade, total_decade)], by = "Decade")
df_counts[, percentage := total_objs / total_decade]

# 4. Calculate y-axis start and end points
setorder(df_counts, Decade, country)
df_counts[, ymax := cumsum(percentage), by = Decade]
df_counts[, ymin := data.table::shift(ymax, fill = 0), by = Decade]

# 5. Calculate the exact middle of the x-axis for our labels
df_counts[, x_mid := (xmin + xmax) / 2]


# Your beautiful color palette!
col <- c("USA" = "#5a8192", 
         "Russia/USSR" = "#D78D50", 
         "China" = "#74b49b", 
         "Other" = "#e9c46a")





# plot --------

gr <- ggplot(df_counts) +
    
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = country),
              color = "white", linewidth = 0.4) + 
    
    scale_x_continuous(breaks = unique(df_counts$x_mid), labels = unique(df_counts$Decade)) +
    scale_y_continuous(labels = scales::percent_format()) +
    
    scale_fill_manual(values = col) +
    
    labs(
        title = "Space Dominance from 1960s to 2020s",
        subtitle = "Space launches have skyrocketed in the 2020s, heavily dominated by the United States.<br><b>Box width</b> = total objects launched. <b>Box height</b> = country of origin.",
        caption = "30DayChartChallenge 2026: <b> Day 3 </b>Source: <b> UNOOSA (TidyTuesday) </b>Graphic: <b>Natasa Anastasiadou</b>",
        fill = ""
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.position = "top",
        
        axis.title = element_blank(),
        axis.text.x = element_text(size = 9, face = "bold", color = "black", 
                                   angle = 45, hjust = 1, margin = margin(t = 5)),
        # axis.text.x = element_text(size = 9, face = "bold", color = "black", margin = margin(t = 5)),
        axis.text.y = element_text(size = 9, color = "grey30"),
        
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(linewidth = 0.35, color = "grey85"),
        panel.grid.minor = element_blank(),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
        
        plot.background = element_rect(fill = "grey95", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )


# save ---------

ggsave(
    plot = gr, filename = "Rplot.png",
    width = 9, height = 7.5, units = "in", dpi = 600
)


