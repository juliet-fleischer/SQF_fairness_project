library(ggplot2)
library(grid)

# Define a custom professional theme
theme_scientific <- function(base_size = 14, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Text elements
      plot.title = element_text(hjust = 0.5, face = "bold", size = base_size + 2),
      plot.subtitle = element_text(hjust = 0.5, face = "italic", size = base_size),
      plot.caption = element_text(size = base_size - 2, hjust = 1),
      
      # Axis titles and text
      axis.title = element_text(face = "bold"),
      axis.text = element_text(size = base_size - 2),
      axis.line = element_line(color = "black"),
      
      # Legend
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.key = element_blank(),
      legend.background = element_rect(fill = "white", color = "black"),
      
      # Panel background and grid lines
      panel.grid.major = element_line(color = "grey85"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),  # Updated to use linewidth
      
      # Margins
      plot.margin = unit(c(1, 1, 1, 1), "cm")
    )
}

# mosaic plot for Independence
predictions_dt |> 
  select(response, PA_GROUP) |>
  ggplot() +
  geom_mosaic(aes(x = product(PA_GROUP, response)))


# ROC curves by for each group
rocobj1 <- roc(subset(predictions_dt, PA_GROUP == 1)$truth, subset(predictions_dt, PA_GROUP == 1)$prob.Y)
rocobj0 <- roc(subset(predictions_dt, PA_GROUP == 0)$truth, subset(predictions_dt, PA_GROUP == 0)$prob.Y)
ggroc(list(PA_0 = rocobj0, PA_1 = rocobj1)) +
  theme_scientific()
