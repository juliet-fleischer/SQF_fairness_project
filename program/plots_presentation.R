source("program/fairness_experiment_main.R")
set.seed(024)

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


# 1. data ----
# bar plot for race distribution with percentage points on top
data2023 |> 
  group_by(SUSPECT_RACE_DESCRIPTION) |> 
  summarize(count = n()) |> 
  mutate(prop = count / sum(count)) |> 
  ggplot(aes(x = SUSPECT_RACE_DESCRIPTION, y = prop)) +
  geom_col() +
  geom_text(aes(label = scales::percent(prop)), vjust = -0.5) +
  theme_scientific()

table1 <- data2023 |> 
  group_by(SUSPECT_RACE_DESCRIPTION) |> 
  summarize(count = n()) |> 
  mutate(prop = count / sum(count)) |>
  mutate(prop = scales::percent(prop)) |> 
  select(SUSPECT_RACE_DESCRIPTION, prop) |>
  rename("race" = "SUSPECT_RACE_DESCRIPTION")
latex.table.1 <- kableExtra::kable(table1, format = "latex", booktabs = TRUE)


# 2. group fairness ----
# extract ten different observations of each group at random for our visualizations
sample_dt <- preds_2023_dt |> 
  group_by(pa_group) |> 
  sample_n(15)
setDT(sample_dt)
## Independence ----
plot1 <- sample_dt |> 
  mutate(pa_group = ifelse(pa_group == "POC", 0, 1)) |>
  ggplot(aes(x = pa_group, y = prob.Y, color = response)) +
  geom_jitter(size = 5, shape = 21, fill = NA, width = 0.2) +  # Jitter to spread points horizontally
  geom_segment(aes(x = min(pa_group) - 0.3, xend = mean(pa_group) - 0.3, y = 0.48, yend = 0.48), 
               linetype = "dashed") +
  geom_segment(aes(x = mean(pa_group) + 0.3, xend = max(pa_group) + 0.3, y = 0.40, yend = 0.40), 
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
sample_dt_ind <- sample_dt[pa_group == 1 & prob.Y > 0.4, response := "Y"]
plot2 <- sample_dt_ind |> 
  mutate(pa_group = ifelse(pa_group == "POC", 0, 1)) |>
  ggplot(aes(x = pa_group, y = prob.Y, color = response)) +
  geom_jitter(size = 6, shape = 21, fill = NA, width = 0.2) +  # Increased point size
  geom_segment(aes(x = min(pa_group) - 0.3, xend = mean(pa_group) - 0.3, y = 0.48, yend = 0.48), 
               linetype = "dashed", linewidth = 1) +  # Increased line thickness
  geom_segment(aes(x = mean(pa_group) + 0.3, xend = max(pa_group) + 0.3, y = 0.40, yend = 0.40), 
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


# Separation ----
plot4 <- sample_dt |> 
  mutate(pa_group = ifelse(pa_group == "POC", 0, 1)) |>
  ggplot(aes(x = pa_group, y = prob.Y, color = truth)) +
  geom_jitter(size = 6, shape = 21, fill = NA, width = 0.2) +  # Increased point size
  scale_color_manual(
    name = "Y",  # Legend title
    values = c("Y" = "#007A88", "N" = "#883A00"),  # Custom colors
    labels = c("Straftat", "keine Straftat")  # Custom labels
  ) +
  geom_segment(aes(x = min(pa_group) - 0.3, xend = mean(pa_group) - 0.3, y = 0.36, yend = 0.36), 
               linetype = "dashed", linewidth = 1) +  # Increased line thickness
  geom_segment(aes(x = mean(pa_group) + 0.3, xend = max(pa_group) + 0.3, y = 0.4, yend = 0.4), 
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