

rm(list = ls())
gc()


# load libraries -------

library(data.table)
library(ggplot2)
library(stringr)
library(ggtext)
library(extrafont)


# load data --------

patient_risk_profiles <- fread('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2023/2023-10-24/patient_risk_profiles.csv')


# Plot 1 -----------

# data cleaning 

colnames(patient_risk_profiles)

df1 = patient_risk_profiles[, .(
    personId, `Occurrence of Anxiety in prior year`, `Chronic hepatitis in prior year`, `Type 1 diabetes and no prior specific non-T1DM diabetes in prior year`, `Type 2 Diabetes Mellitus (DM), with no type 1 or secondary DM in prior year`, `Any cancer (excl. prostate cancer and benign cancer) in prior year`,
    `Heart failure in prior year`, `Obesity in prior year`, `Occurrence of Alcoholism in prior year`, `Seizure in prior year`, `Smoking in prior year`, `Opioids in prior year`, `Osteoporosis in prior year`
)]


colnames(df1) <- str_replace_all(colnames(df1), " in prior year", "")

colnames(df1) <- str_replace_all(colnames(df1), "Type 1 diabetes and no prior specific non-T1DM diabetes", "Type 1 Diabetes")
colnames(df1) <- str_replace_all(colnames(df1), "Type 2 Diabetes Mellitus \\(DM\\), with no type 1 or secondary DM", "Type 2 Diabetes")
colnames(df1) <- str_replace_all(colnames(df1), "Any cancer \\(excl. prostate cancer and benign cancer\\)", "Any Cancer")


avg_risk_dt <- data.table()

for (col in colnames(df1)[-1]) {  
    avg_risk <- mean(df1[[col]], na.rm = TRUE)  # Proportion of 1's in the column
    avg_risk_dt <- rbind(avg_risk_dt, data.table(Group = col, Proportion = avg_risk))
}



# Labels Preparation 

label_data <- avg_risk_dt
label_data[, id := .I] 
number_of_bar <- nrow(label_data)


label_data[, angle := 90 - 360 * (id - 0.5) / number_of_bar]
label_data[, angle := ifelse(angle < -90, angle + 180, angle)]
label_data[, hjust := ifelse(angle < 0, 1, 0)]  

label_data$Proportion <- label_data$Proportion + 0.05

avg_risk_dt$Group <- avg_risk_dt$Group |> factor(levels = unique(avg_risk_dt$Group))
label_data$Group <- label_data$Group |> factor(levels = unique(label_data$Group))
label_data$Group <- label_data$Group |> str_wrap(width = 15)


# plot -----------

gr1 = ggplot(avg_risk_dt, aes(x = factor(Group), y = Proportion, fill = Group)) +
    
    geom_bar(
        position = "stack", 
        stat = "identity", 
        width = 1, 
        alpha = 0.9, 
        color = "black",
        linewidth = 0.25,
        fill = "#ACD4EC"
    ) +
    
    coord_polar(start = 0) + 
    
    geom_text(data = label_data, aes(x = id, y = Proportion, label = Group, angle = angle),
              color = "black", fontface = "bold", alpha = 0.6, size = 2.3, inherit.aes = FALSE,
              family = "Candara") +


    theme_minimal() +
    
    # labs(title = "Circular Barplot of Proportions of Conditions",
    #      subtitle = "Proportion of individuals with each condition (binary 0/1)",
    #      x = "Group",
    #      y = "Proportion of 1's") +
    

    theme(

        legend.position = "none",
        legend.title.position = "left",

        legend.title = element_blank(),

        axis.title.x = element_blank(),

        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),

        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(linewidth = .35, color = "grey80"),

        plot.title = element_markdown(size = 16, face = "bold", hjust = 0.5, family = "Candara", margin = margin(t = 15, b = 5)),
        plot.subtitle = element_markdown(size = 11, hjust = 0.5, family = "Candara", color = "grey30", margin = margin(t = 5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 5), size = 8, family = "Candara", hjust = 1.25),

        plot.margin = margin(6, 6, 6, 6),

        plot.background = element_rect(fill = "#e4e4e3", color = NA)
    )

gr1


# Plot 2 -----------


# data clean 

index <- c("personId", colnames(patient_risk_profiles)[87:ncol(patient_risk_profiles)])

df2 <- patient_risk_profiles[, ..index]


# colnames ----

# Update the column names for predicted variables
colnames(df2) <- str_replace_all(colnames(df2), "predicted risk of ", "")

# Clean up the column names by removing text inside parentheses (including the parentheses)
colnames(df2)[-1] <- str_replace(colnames(df2)[-1], "\\(.*\\)", "")

# Clean up the column names by keeping only the first part before a comma or additional text
colnames(df2)[-1] <- str_replace(colnames(df2)[-1], ",.*", "")



# Calculate the proportions -----

avg_risk_predicted_dt <- data.table()

for (col in colnames(df2)[-1]) {  
    avg_risk <- mean(df2[[col]], na.rm = TRUE)  # Proportion of 1's in the column
    avg_risk_predicted_dt <- rbind(avg_risk_predicted_dt, data.table(Group = col, Proportion = avg_risk))
}


avg_risk_predicted_dt$Proportion <- round(avg_risk_predicted_dt$Proportion, 4)


# Labels Preparation for predicted values
label_data_predicted <- avg_risk_predicted_dt
label_data_predicted[, id := .I] 

number_of_bar_predicted <- nrow(label_data_predicted)

# Calculate angles for label positions
label_data_predicted[, angle := 90 - 360 * (id - 0.5) / number_of_bar_predicted]
label_data_predicted[, angle := ifelse(angle < -90, angle + 180, angle)]
label_data_predicted[, hjust := ifelse(angle < 0, 1, 0)]  

label_data_predicted$Proportion <- ifelse(label_data_predicted$Group %in% c("Migraine", "Dementia", "Ulcerative colitis"),
                                          label_data_predicted$Proportion + 0.0065,
                                          label_data_predicted$Proportion + 0.012)


avg_risk_predicted_dt$Group <- avg_risk_predicted_dt$Group |> factor(levels = unique(avg_risk_predicted_dt$Group))
label_data_predicted$Group <- label_data_predicted$Group |> factor(levels = unique(label_data_predicted$Group))
# label_data_predicted$Group <- label_data_predicted$Group |> str_wrap(width = 15)

# Plot -----------

gr2 = ggplot(avg_risk_predicted_dt, aes(x = factor(Group), y = Proportion, fill = Group)) +
    geom_bar(
        position = "stack", 
        stat = "identity", 
        width = 1, 
        alpha = 0.9, 
        color = "black",
        linewidth = 0.25,
        fill = "#ACD4EC"  # Set the fill color to #ACD4EC
    ) +
    
    coord_polar(start = 0) + 
    
    geom_text(data = label_data_predicted, aes(x = id, y = Proportion, label = Group, angle = angle),
              color = "black", fontface = "bold", alpha = 0.6, size = 1.7, inherit.aes = FALSE, 
              family = "Candara") +  # Set the font family to Candara
    
    theme_minimal() +
    
    labs(title = "Risk Distribution Across Conditions: Observed vs Predicted",
         subtitle = "A visualization of the average risk proportions for various medical conditions based on <b>observed data (left)</b> and <b>model-predicted outcomes(right)</b>.",
         caption = "Source: <b> Patient Risk Profiles Data</b> | Graphic: <b>Natasa Anastasiadou</b>"
         ) +
    
    theme(
        legend.position = "none",
        legend.title.position = "left",
        
        legend.title = element_blank(),
        
        axis.title.x = element_blank(),
        
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        
        panel.grid.major = element_blank(),
        panel.grid.minor = element_line(linewidth = .35, color = "grey80"),
        
        plot.title = element_markdown(size = 12, face = "bold", hjust = -4, family = "Candara", margin = margin(t = 70, b = 5)),
        plot.subtitle = element_markdown(size = 9, hjust = 1.4, family = "Candara", color = "grey30", margin = margin(t = 5, b = 25)),
        plot.caption = element_markdown(margin = margin(t = 5), size = 6, family = "Candara", hjust = 1),
        
        plot.margin = margin(6, 6, 6, 6),
        
        plot.background = element_rect(fill = "#e4e4e3", color = NA)
    )


gr2


library(patchwork)
combined_plot <- gr1 + gr2 


# save ---------

ggsave(
   plot = combined_plot, filename = "Rplot.png",
   width = 10, height = 8, units = "in", dpi = 600
)

