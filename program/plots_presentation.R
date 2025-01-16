set.seed(024)

# what is the key message of the slide?
# use my titles to communicate the message of the slide

theme_scientific <- function(base_size = 18, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Text elements
      plot.title = element_text(hjust = 0.5, face = "bold", size = base_size + 4),
      plot.subtitle = element_text(hjust = 0.5, face = "italic", size = base_size + 2),
      plot.caption = element_text(size = base_size - 2, hjust = 1),
      
      # Axis titles and text
      axis.title = element_text(face = "bold", size = base_size + 2),
      axis.text = element_text(size = base_size),
      axis.line = element_line(color = "black", linewidth = 1),  # Increased thickness
      
      # Legend
      legend.position = "bottom",
      legend.title = element_text(face = "bold", size = base_size),
      legend.text = element_text(size = base_size - 2),
      legend.key = element_blank(),
      legend.background = element_rect(fill = "white"),
      
      # Panel background and grid lines
      panel.grid.major = element_line(color = "grey75", linewidth = 0.8),  # Increased thickness
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),  # Increased thickness
      
      # Margins
      plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")
    )
}


# 1 Daten
# bar plot for race distribution with percentage points on top
sqf |> 
  group_by(SUSPECT_RACE_DESCRIPTION) |> 
  summarize(count = n()) |> 
  mutate(prop = count / sum(count)) |> 
  ggplot(aes(x = SUSPECT_RACE_DESCRIPTION, y = prop)) +
  geom_col() +
  geom_text(aes(label = scales::percent(prop)), vjust = -0.5) +
  theme_scientific()

table1 <- sqf |> 
  group_by(SUSPECT_RACE_DESCRIPTION) |> 
  summarize(count = n()) |> 
  mutate(prop = count / sum(count)) |>
  mutate(prop = scales::percent(prop)) |> 
  select(SUSPECT_RACE_DESCRIPTION, prop) |>
  rename("race" = "SUSPECT_RACE_DESCRIPTION")
latex.table.1 <- kableExtra::kable(table1, format = "latex", booktabs = TRUE)


# 2 Gruppen Fairness
# extract ten different observations of each group at random for our visualizations
sample_dt <- predictions_dt |> 
  group_by(PA_GROUP) |> 
  sample_n(15)
setDT(sample_dt)
# Gruppen Fainress - Independence
plot1 <- sample_dt |> 
  mutate(PA_GROUP = as.numeric(as.character(PA_GROUP))) |>
  ggplot(aes(x = PA_GROUP, y = prob.Y, color = response)) +
  geom_jitter(size = 5, shape = 21, fill = NA, width = 0.2) +  # Jitter to spread points horizontally
  geom_segment(aes(x = min(PA_GROUP) - 0.3, xend = mean(PA_GROUP) - 0.3, y = 0.48, yend = 0.48), 
               linetype = "dashed") +
  geom_segment(aes(x = mean(PA_GROUP) + 0.3, xend = max(PA_GROUP) + 0.3, y = 0.40, yend = 0.40), 
               linetype = "dashed") +
  theme_scientific() +
  labs(
    x = "",  # X-axis label
    y = expression(P(hat(Y) == 1)) # Y-axis mathematical expression
  ) +
  scale_x_continuous(
    breaks = c(0, 1),  # Specify where to place ticks
    labels = c("Weiß", "People of Color")  # Specify what the tick labels should say
  ) +
  scale_color_manual(
    name = expression(hat(Y)),  # Legend title
    values = c("Y" = "#007A88", "N" = "#883A00"),  # Custom colors
    labels = c("Straftat", "keine Straftat")  # Custom labels
  )
sample_dt_ind <- sample_dt[PA_GROUP == 1 & prob.Y > 0.4, response := "Y"]
plot2 <- sample_dt_ind |> 
  mutate(PA_GROUP = as.numeric(as.character(PA_GROUP))) |>
  ggplot(aes(x = PA_GROUP, y = prob.Y, color = response)) +
  geom_jitter(size = 6, shape = 21, fill = NA, width = 0.2) +  # Increased point size
  geom_segment(aes(x = min(PA_GROUP) - 0.3, xend = mean(PA_GROUP) - 0.3, y = 0.48, yend = 0.48), 
               linetype = "dashed", linewidth = 1) +  # Increased line thickness
  geom_segment(aes(x = mean(PA_GROUP) + 0.3, xend = max(PA_GROUP) + 0.3, y = 0.40, yend = 0.40), 
               linetype = "dashed", linewidth = 1) +  # Increased line thickness
  theme_scientific() +
  labs(
    x = "",  # X-axis label
    y = expression(P(hat(Y) == 1)) # Y-axis mathematical expression
  ) +
  scale_x_continuous(
    breaks = c(0, 1),  # Specify where to place ticks
    labels = c("Weiß", "People of Color")  # Specify what the tick labels should say
  ) +
  scale_color_manual(
    name = expression(hat(Y)),  # Legend title
    values = c("Y" = "#007A88", "N" = "#883A00"),  # Custom colors
    labels = c("Straftat", "keine Straftat")  # Custom labels
  )


# Um Independence zu erfüllen müssen gruppenspezifische Cutoffs gesetzt werden
# dadurch wird die positive prediction ratio (ppr) zwischen Gruppen ausgeglichen
# für beide gruppen gilt hier ppr = 4/30 = 0.1333

# mosaic plot for Independence
predictions_dt |> 
  select(response, PA_GROUP) |>
  ggplot() +
  geom_mosaic(aes(x = product(response, PA_GROUP))) +
  xlab("Gruppe") +
  ylab("Vorhersage") +
  theme_scientific()
predictions_dt |> 
  group_by(PA_GROUP) |> 
  summarize(mean_response = mean(response == "Y"))


# Gruppen Fairness - Separation
plot4 <- sample_dt |> 
  mutate(PA_GROUP = as.numeric(as.character(PA_GROUP))) |>
  ggplot(aes(x = PA_GROUP, y = prob.Y, color = truth)) +
  geom_jitter(size = 6, shape = 21, fill = NA, width = 0.2) +  # Increased point size
  scale_color_manual(
    name = "Y",  # Legend title
    values = c("Y" = "#007A88", "N" = "#883A00"),  # Custom colors
    labels = c("Straftat", "keine Straftat")  # Custom labels
  ) +
  geom_segment(aes(x = min(PA_GROUP) - 0.3, xend = mean(PA_GROUP) - 0.3, y = 0.36, yend = 0.36), 
               linetype = "dashed", linewidth = 1) +  # Increased line thickness
  geom_segment(aes(x = mean(PA_GROUP) + 0.3, xend = max(PA_GROUP) + 0.3, y = 0.4, yend = 0.4), 
               linetype = "dashed", linewidth = 1) +  # Increased line thickness
  theme_scientific(base_size = 18) +  # Increased base size for better visibility
  labs(
    x = "",  # X-axis label
    y = expression(P(hat(Y) == 1)) # Y-axis mathematical expression
  ) +
  scale_x_continuous(
    breaks = c(0, 1),  # Specify where to place ticks
    labels = c("Weiß", "People of Color")  # Specify what the tick labels should say
  )


# um Separation zu erfüllen brauchen wir auch gruppenspezifische cutoffs
# in beiden Gruppen gibt es keine falsch positiven und jeweils vier falsch negative
# FPR und FNR sind gleich zwischen Gruppen (Equality of opportunity)

# Gruppen Fainress - Sufficiency
sample_dt |> 
  filter(response == "Y") |> 
  ggplot(aes(x = PA_GROUP, y = prob.Y, color = truth)) +
  scale_color_manual(values = c("Y" = "#007A88", "N" = "#883A00")) +  # Add your custom color
  geom_jitter(size = 5, shape = 21, fill = NA, width = 0.2) +  # Jitter to spread points horizontally
  theme_scientific() +
  ylim(c(0, 1))
# wir betrachten in beiden gruppen nur die Personen, die als positive vorhergesagt wurden
# der Anteil von wirklich positiven sollte in beiden Gruppen gleich sein
# in Gruppe a sind 100% wirklich positiv, in Gruppe b sind es 83%
# Predictive Parity wäre hier nicht erfüllt

# ROC curves by for each group
rocobj1 <- roc(subset(predictions_dt, PA_GROUP == 1)$truth, subset(predictions_dt, PA_GROUP == 1)$prob.Y)
rocobj0 <- roc(subset(predictions_dt, PA_GROUP == 0)$truth, subset(predictions_dt, PA_GROUP == 0)$prob.Y)
ggroc(list(PA_0 = rocobj0, PA_1 = rocobj1)) +
  theme_scientific()

# 
# # export graphics
# 
# ggsave("figures/independence_01.png", plot = plot1, width = 150, height = 150, dpi= 300, units = "mm",
#        bg = "white")
# ggsave("figures/independence_02.png", plot = plot2, width = 150, height = 150, units = "mm",
#        bg = "white")
# 
# ggsave("figures/separation_01.png", plot = plot4, width = 150, height = 150,dpi= 300, units = "mm",
#        bg = "white")

# Load necessary library
library(ggplot2)

# Create the data frame
set.seed(204) # For reproducibility
data <- data.frame(
  group = c(rep("white", 7), rep("people of color", 10)),
  score = c(
    runif(3, 0, 0.5),  # 3 white people with scores below 0.5
    runif(4, 0.5, 1),  # 4 white people with scores above 0.5
    runif(5, 0, 0.7),  # 5 people of color with scores below 0.6
    runif(5, 0.7, 1)   # 5 people of color with scores above 0.6
  )
)
# Add the true_label column
data$true_label <- c(
  # White group
  c(0, 0, 1),        # 2 out of 3 with score < 0.5 get 0, 1 gets 1
  c(1, 1, 0, 0),     # 1 out of 4 with score > 0.5 gets 0, 2 get 1
  
  # People of color group
  c(1, 0, 0, 0, 0),  # 1 out of 5 with score < 0.7 gets 1
  c(0, 0, 0, 1, 1)   # 2 out of 5 with score > 0.6 get 1
)

# Check the updated dataset
print(data)


# Plot the data
data |> 
  mutate(group = ifelse(group == "white", 0, 1)) |>
  mutate(true_label = factor(true_label, levels = c(0, 1))) |>
  ggplot(aes(x = group, y = score, color = true_label)) +
  geom_jitter(width = 0.2, size = 6) +  # Jittered points for better visibility
  labs(
    title = "",
    x = "",
    y = "Score"
  ) +
  scale_x_continuous(
    breaks = c(0, 1),  # Specify where to place ticks
    labels = c("Weiß", "People of Color")  # Specify what the tick labels should say
  ) +
  geom_segment(aes(x = min(group) - 0.2, xend = mean(group) - 0.3, y = 0.48, yend = 0.48), 
               linetype = "dashed", linewidth = 1) +  # Increased line thickness
  geom_segment(aes(x = mean(group) + 0.1, xend = max(group) + 0.3, y = 0.52, yend = 0.52), 
               linetype = "dashed", linewidth = 1) +  # Increased line thickness
  scale_color_manual(
    name = "wahres label",  # Legend title
    values = c("1" = "#007A88", "0" = "salmon"),  # Custom colors
    labels = c("Straftat", "keine Straftat")  # Custom labels
  ) +
  theme_scientific()
ggsave("figures/separation_01.png", width = 150, height = 150, dpi= 300, units = "mm",
       bg = "white")

data |> 
  mutate(group = ifelse(group == "white", 0, 1)) |>
  mutate(true_label = factor(true_label, levels = c(0, 1))) |>
  ggplot(aes(x = group, y = score, color = true_label)) +
  geom_jitter(width = 0.2, size = 6) +  # Jittered points for better visibility
  labs(
    title = "",
    x = "",
    y = "Score"
  ) +
  scale_x_continuous(
    breaks = c(0, 1),  # Specify where to place ticks
    labels = c("Weiß", "People of Color")  # Specify what the tick labels should say
  ) +
  geom_segment(aes(x = min(group) - 0.2, xend = mean(group) - 0.3, y = 0.18, yend = 0.18), 
               linetype = "dashed", linewidth = 1) +  # Increased line thickness
  geom_segment(aes(x = mean(group) + 0.1, xend = max(group) + 0.3, y = 0.60, yend = 0.60), 
               linetype = "dashed", linewidth = 1) +  # Increased line thickness
  scale_color_manual(
    name = "wahres label",  # Legend title
    values = c("1" = "#007A88", "0" = "salmon"),  # Custom colors
    labels = c("Straftat", "keine Straftat")  # Custom labels
  ) +
  theme_scientific()
ggsave("figures/independence_02.png", width = 150, height = 150, dpi= 300, units = "mm",
       bg = "white")
