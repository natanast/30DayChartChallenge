

rm(list = ls())
gc()


# libraries ---------

library(data.table)
library(stringr)
library(ggplot2)
library(ggrepel)
library(colorspace)
library(ggplot2)
library(shadowtext)

library(airway)
library(DESeq2)

library(genekitr)

library(ggtext)
library(extrafont)
library(showtext)
# library(geneset)


# Load showtext and register Candara
library(showtext)
font_add("Candara", regular = "C:/Windows/Fonts/candara.ttf")  # Path to Candara font
showtext_auto()


# Load data -------

data("airway")
airway$dex <- relevel(airway$dex, ref = "untrt")  


# Data analysis -----

dds <- DESeqDataSet(airway, design = ~ dex)


dds <- DESeq(dds)

res <- results(dds) |>
    as.data.frame() |>
    setDT(keep.rownames = "GeneID")


df = res[which( !is.na(res$pvalue) )]

df$y = -log10(df$pvalue)



gene_map = df$GeneID |> transId(transTo = c("symbol", "entrez", "ensembl"), unique = TRUE) |> setDT()

gene_map = gene_map[which(
    !is.na(symbol) & !is.na(entrezid) & !is.na(ensembl)
)]

df = df |> merge(gene_map, by.x = "GeneID", by.y = "input_id")

df = df[!is.na(padj)]


df$ann = ifelse(
    df$pvalue > .05, "Not significant",
    ifelse(
        df$log2FoldChange > 0, "Up regulated", "Down regulated"
    )
)


df$ann = ifelse(
    df$pvalue <= 0.05 & df$log2FoldChange > -1 & df$log2FoldChange < 1, 
    paste0(df$ann, " (low)"),
    df$ann
)



df2 = df[which(pvalue <= .05 & abs(log2FoldChange) > 1)]

df2 = df2[order( abs(log2FoldChange), decreasing = TRUE )]

df2 = df2[, by = ann, head(.SD, 10) ]



# plot -----

gr = ggplot(data = df) +
    
    geom_point(aes(x = log2FoldChange, y = -log10(pvalue), fill = ann),
               shape = 21, stroke = .05, size = 2, alpha = .5, color = "white") +
    
    geom_vline(xintercept = c(-1, 1), linewidth = .3, linetype = "dashed", lineend = "round") +
    geom_hline(yintercept = -log10(.05), linewidth = .3, linetype = "dashed", lineend = "round") +
    
    geom_point(data = df2, aes(x = log2FoldChange, y = -log10(pvalue), fill = ann), 
               shape = 21, stroke = .2, size = 2.5, color = "white") +
    
    geom_text_repel(
        data = df2, aes(x = log2FoldChange, y = -log10(pvalue), label = symbol),
        max.overlaps = Inf, 
        fontface = "bold", size = 13, bg.color = "white", bg.r = .05
    ) +
    
    scale_fill_manual(
        values = c(
            "Up regulated" = "#990000",
            "Up regulated (low)" = lighten("#990000", .5),
            
            "Down regulated" = "#004d99",
            "Down regulated (low)" = lighten("#004d99", .5),
            
            "Not significant" = "grey"
        ),
        
        breaks = c("Up regulated", "Not significant", "Down regulated"),
        
        guide = guide_legend(
            override.aes = list(size = 3, alpha = 1)
        ),
        
    ) +
    
    scale_x_continuous(
        breaks = c(-5, -2.5, -1, 0, 1, 2.5, 5),
        trans = scales::pseudo_log_trans()
    ) +
    
    scale_y_continuous(
        expand = c(0, 0), breaks = c(2, 5, 10, 20, 30, 40),
        trans = scales::pseudo_log_trans()
    ) +
    
    coord_cartesian(clip = "off") +
    
    theme_minimal(base_family = "Candara") +
    
    theme(
        legend.title = element_blank(),
        legend.position = "bottom",
        
        legend.text = element_text(size = 25),
        
        
        axis.title = element_text(size = 50),
        axis.text = element_text(size = 50),
        
        axis.line = element_line(linewidth = .3, color = "black"),
        axis.ticks = element_line(linewidth = .3, color = "black"),
        
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = .3, linetype = "dashed", lineend = "round", color = "grey80"),
        
        plot.background = element_rect(fill = "grey98", color = NA),
        
        plot.margin = margin(20, 20, 20, 20),
        
        
    ) +
    
    labs(y = "-log10(pvalue)", x = "log2(Fold Change)")


gr

# Save the plot with custom size and resolution
ggsave("23_day.png", plot = gr, width = 9, height = 9, dpi = 600)

