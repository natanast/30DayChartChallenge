

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(ggtext)
library(extrafont)
library(stringr) 
library(ggalluvial)


# load data ------

# https://www.kaggle.com/datasets/anoopjohny/birdsoftheworld-unprocessed?resource=download

birds <- "Birdsoftheworld.csv" |> fread(header = TRUE)


# clean data -----

# Get Top 10 Species
top10_species <- birds[, .N, by = .(species)][order(-N)][1:10]

# Filter and clean locations
df2 <- birds[species %in% top10_species$species]
df2 <- df2[location != "United States"] # Remove the "squisher"

# Count by species and location
df2 <- df2[, .(N = .N), by = .(species, location)]

# Get Top 10 Locations
top10_locs <- df2[, .(Total = sum(N)), by = location][order(-Total)][1:10]
df_plot <- df2[location %in% top10_locs$location]


df_plot$species <- df_plot$species |> str_wrap(width = 20)

# plot --------

cols <- c(
    "American Crow" = "#b25c56",           
    "American Goldfinch" = "#d4a373",      
    "American Robin" = "#c28d75",          
    "Belted Kingfisher" = "#7AA6DC",       
    "Blue Jay" = "#5a7a9b",                
    "Downy Woodpecker" = "#8aa39b",        
    "Green Heron" = "#9b8b99",             
    "Northern Flicker" = "#aba296",        
    "Ruby-throated Hummingbird" = "#ADB17D", 
    "Scarlet Tanager" = "#8F7700"            
)



gr <- ggplot(df_plot, aes(axis1 = species, axis2 = location, y = N)) +
    
    # 1. The Rivers (The "Chords")
    # I lowered alpha to 0.4 so they are more transparent than the blocks
    geom_alluvium(aes(fill = species), alpha = 0.75, width = 1/5, color = "white", linewidth = 0.3) +
    
    # 2. THE COLORED BLOCKS (Strata)
    # Mapping fill to after_stat(stratum) makes the blocks use your bird colors
    geom_stratum(aes(fill = after_stat(stratum)), alpha = 0.75, width = 1/5, color = "grey20", linewidth = 0.5) +
    
    # 3. THE TEXT LABELS
    # I changed the color to "white" so it's readable inside the colored blocks
    geom_text(stat = "stratum", 
              aes(label = after_stat(stratum)), 
              color = "black", family = "Candara", fontface = "bold", size = 2.5) +
    
    # 4. Color Mapping
    # scale_fill_manual will apply bird colors to both the rivers and the blocks
    # Locations will automatically turn the 'na.value' color
    scale_fill_manual(values = cols, na.value = "grey70", guide = "none") +
    
    scale_x_discrete(limits = c("Bird Species", "Location Spotted"), expand = c(0.2, 0.2)) +
    
    labs(
        title = "Bird connections around the world",
        subtitle = "A Sankey flow diagram exploring how the top 10 most frequently observed bird species map to their top global locations.",
        caption = "30DayChartChallenge 2026: <b> Day 13 </b> | Source: <b> Birds of the World (Kaggle) </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "", y = ""
    ) +
    
    theme_minimal(base_family = "Candara") +
    theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid = element_blank(),
        
        axis.text.x = element_text(size = 14, face = "bold", color = "black", margin = margin(b = 15)),
        
        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 12, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 8, hjust = 1),
        
        plot.background = element_rect(fill = "#fcfbf9", color = NA), 
        plot.margin = margin(30, 40, 30, 40)
    )

gr

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


