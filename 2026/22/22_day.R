

rm(list = ls())
gc()


# load libraries -----

library(ggplot2)
library(dplyr)
library(stringr)
library(data.table)
library(extrafont)

library(ggstream)


# Load data -------

dt_raw <- "Google_search_chat.csv" |> fread()

# clean data -----

dt_ai <- melt(
    dt_raw, 
    id.vars = "Time", 
    variable.name = "tool", 
    value.name = "interest"
)

dt_ai$interest <- dt_ai$interest |> as.numeric()
dt_ai$Time <- dt_ai$Time |> as.Date()

dt_ai$tool <- dt_ai$tool |> 
    str_replace("Microsoft Copilot", "Copilot") 


custom_order <- c("ChatGPT", "Copilot", "Claude", "DeepSeek", "Gemini")


dt_ai$tool <- dt_ai$tool |> factor(levels = custom_order)



# plot --------

cols <- c(
    "#628b9c",  # ChatGPT
    "#c28d75",  # Copilot
    "#8aa39b",  # Gemini
    "#b25c56",  # Claude
    "#7AA6DC"   # DeepSeek
)

names(cols) <- levels(dt_ai$tool)


gr <- ggplot(dt_ai, aes(x = Time, y = interest, fill = tool)) +
    
    geom_stream(type = "ridge", color = "white", lwd = 0.1) +
    
    # ChatGPT
    annotate(
        "text", 
        x = as.Date("2026-04-05"), 
        y = 80,                    
        label = "ChatGPT",
        hjust = 0,
        size = 3,                
        fontface = "bold",
        color = cols["ChatGPT"]    
    ) +
    
    # Copilot
    annotate(
        "text", 
        x = as.Date("2026-04-05"), 
        y = 46,                    
        label = "Copilot",
        hjust = 0,
        size = 3,                
        fontface = "bold",
        color = cols["Copilot"]    
    ) +
    
    # Claude
    annotate(
        "text", 
        x = as.Date("2026-04-05"), 
        y = 38,                    
        label = "Claude",
        hjust = 0,
        size = 3,                
        fontface = "bold",
        color = cols["Claude"]    
    ) +
    
    # DeepSeek
    annotate(
        "text", 
        x = as.Date("2026-04-05"), 
        y = 32.5,                    
        label = "DeepSeek",
        hjust = 0,
        size = 3,                
        fontface = "bold",
        color = cols["DeepSeek"]    
    ) +
    
    # Gemini
    annotate(
        "text", 
        x = as.Date("2026-04-05"), 
        y = 15,                    
        label = "Gemini",
        hjust = 0,
        size = 3,                
        fontface = "bold",
        color = cols["Gemini"]    
    ) +

    scale_y_continuous(
        expand = c(0, 0)
    ) +
    
    scale_x_date(
        date_breaks = "1 year", 
        date_labels = "%Y",
        expand = expansion(mult = c(0, 0.15)) 
    ) +
    
    scale_fill_manual(values = cols, name = "AI Tool") +
    
    guides(fill = guide_legend(
        title.position = "top", 
        title.hjust = 0.5,      # Centers the legend title
        nrow = 1,               # Keeps it compact at the bottom
        byrow = TRUE
    )) +
    
    labs(
        title = "The AI Gold Rush: Search Interest Over Time",
        subtitle = "Cumulative global search volume for the leading Large Language Models.<br>The stacked area reveals the total growth of the category alongside individual market share.",
        caption = "30DayChartChallenge 2026: <b> Day 22 </b> | Source: <b> Google Trends </b> | Graphic: <b>Natasa Anastasiadou</b>",
        x = "Year",
        y = "Cumulative Search Interest (Index)"
    ) +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        panel.grid.major = element_line(linewidth = .15, color = "grey80", linetype = "dashed"),
        panel.grid.minor = element_blank(),
        
        axis.text.x = element_text(size = 10, color = "grey40", face = "bold", margin = margin(t = 10)),
        axis.text.y = element_text(size = 10, color = "grey40", face = "bold", margin = margin(r = 10)), 
        
        axis.title.x = element_text(size = 12, face = "bold", color = "grey30", margin = margin(t = 15)),
        axis.title.y = element_text(size = 12, face = "bold", color = "grey30", margin = margin(r = 15)),
        
        legend.position = "bottom",
        legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(size = 9),
        
        plot.title = element_markdown(size = 17, face = "bold", hjust = 0.5, margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 14, hjust = 0.5, color = "grey30", margin = margin(t = 2.5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 35), size = 9, hjust = 1),
        
        plot.background = element_rect(fill = "grey95", color = NA),
        plot.margin = margin(20, 20, 20, 20)
    )

gr


# save ---------

ggsave(
   plot = gr, filename = "Rplot.png",
   width = 10, height = 9, units = "in", dpi = 600
)

