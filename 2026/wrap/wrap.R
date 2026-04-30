# install.packages("magick")
# install.packages("patchwork")
library(magick)
library(patchwork)
library(grid)


# 1. Load the packages
library(magick)
library(patchwork)
library(grid)

# 2. Define the reading function
# We scale them down slightly as they load so R doesn't crash from 30 high-res images!
read_plot <- function(file_path) {
    img <- image_read(file_path)
    img <- image_scale(img, "1000x1000!") # Resizes to a uniform square/box
    rasterGrob(as.raster(img))
}

# 3. THE CRUCIAL STEP: Find the files inside the subfolders
# Replace "path/to/main_folder" with the folder that holds all your Day folders.
# recursive = TRUE tells R to dig into the subfolders.
files <- list.files(
    path = ".", 
    pattern = "\\.png$",          # Looks explicitly for .png files
    recursive = TRUE,             # Digs into Day_01, Day_02, etc.
    full.names = TRUE             # Gets the full file path so magick can find it
)

# NOTE: Check your 'files' list here! Print it to the console to make sure 
# it found all 30 files and they are in the correct order (Day 1 to 30).

# 4. Read all 30 images into a list
plot_list <- lapply(files, read_plot)

# 5. Convert them for patchwork
patch_list <- lapply(plot_list, wrap_elements)

# 6. Stitch them together in a 5x6 grid
final_collage <- wrap_plots(patch_list, ncol = 5) +
    plot_annotation(
        # title = "My 30DayChartChallenge 2026 Portfolio",
        theme = theme(
            plot.title = element_text(size = 35, face = "bold", hjust = 0.5, margin = margin(b = 20)),
            plot.background = element_rect(fill = "#FAFAFA", color = NA)
        )
    )

# 7. Save the final masterpiece
ggsave(
    filename = "30Day_Collage.png", 
    plot = final_collage, 
    width = 20, 
    height = 24, 
    dpi = 300
)
