
rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(stringr)

# load data ------

dt <- "https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-09-12/all_countries.csv" |> 
    fread()

# clean data -----

med_iso3 <- c("GRC", "ITA", "ESP", "FRA", "PRT", "CYP")
dt_plot <- dt[country_iso3 %in% med_iso3]


dt_plot <- dt_plot[!Subcategory %in% c("Infrastructure", "Energy", "Materials", "Waste management")]


dt_plot[, Country_Name := fcase(
    country_iso3 == "GRC", "Greece",
    country_iso3 == "ITA", "Italy",
    country_iso3 == "ESP", "Spain",
    country_iso3 == "FRA", "France",
    country_iso3 == "PRT", "Portugal",
    country_iso3 == "CYP", "Cyprus"
)]


order_dt <- dt_plot[, .(avg_hours = mean(hoursPerDayCombined)), by = Subcategory][order(avg_hours)]
dt_plot[, Subcategory := factor(Subcategory, levels = order_dt$Subcategory)]


# plot -----

cols <- c(
    "Food provision" = "#6d8e9c",                  
    "Nonfood provision" = "#d4a373",              
    "Technosphere modification" = "#b25c56",      
    "Maintenance of surroundings" = "#8aa39b",    
    "Somatic maintenance" = "#c28d75",             
    "Deliberate neural restructuring" = "#aba296", 
    "Organization" = "#9b8b99",                    
    "Experience oriented" = "#b3b9a1"              
)

gr <- ggplot(dt_plot, aes(x = hoursPerDayCombined, y = Subcategory, fill = Category)) +
    
    geom_col(width = 0.75, alpha = 0.95) +
    
    facet_wrap(~Country_Name, ncol = 3) +
    
    
    geom_text(aes(label = round(hoursPerDayCombined, 1)), 
              hjust = -0.2, family = "Candara", size = 3, color = "grey30", fontface = "bold") +
    
    
    scale_fill_manual(values = cols, name = "Broad Category:") +
    
    
    scale_x_continuous(expand = expansion(mult = c(0, 0.2))) +
    
    labs(
        title = "The Mediterranean distribution of time",
        subtitle = "How six Southern European nations allocate their average daily hours across all major subcategories.",
        caption = "30DayChartChallenge 2026: <b> Day 12 </b> | Source: <b> Global Human Day </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Average Hours per Day", 
        y = ""
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        
        legend.position = "bottom",
        legend.title = element_text(size = 10, face = "bold", color = "grey40"),
        legend.text = element_text(size = 9, color = "grey30"),
        legend.key.size = unit(0.4, "cm"),
        
        strip.text = element_text(size = 14, face = "bold", color = "black", hjust = 0, margin = margin(b = 10)),
        
        axis.text.y = element_text(size = 9, face = "bold", color = "grey20"), # Slightly smaller Y text
        axis.text.x = element_blank(), 
        axis.title.x = element_text(size = 11, face = "bold", color = "grey50", margin = margin(t = 15)),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        
        panel.spacing.x = unit(1.5, "cm"), 
        panel.spacing.y = unit(1, "cm"),   
        
        plot.title = element_markdown(size = 18, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.85, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 10, hjust = 1),
        
        plot.background = element_rect(fill = "#fcfbf9", color = NA), 
        plot.margin = margin(30, 30, 30, 30)
    )

gr


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 9, units = "in", dpi = 600
)

