# LLM evolutionary trajectory
# Social vs. non-social alignment comparison
# Requires:
# "Outputs/Results_SocialRat.xlsx"
# "Outputs/Results_math.xlsx"
# Programmed by Feng XIAO and Yining MAO (updated on 2026-10-19)


# 0) Preparation
pkg <- c("tidyr", "dplyr", "readxl", "purrr", "stringr", "ggplot2", "patchwork", "openxlsx", "BayesFactor")

lapply(pkg, require, character.only = TRUE)

# Set the script directory as working directory when run in RStudio.
if(requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()){
  script_path <- rstudioapi::getActiveDocumentContext()$path

  if(nzchar(script_path)){
    setwd(dirname(script_path))
  }
}
theme_set(theme_classic(base_size = 8))

# 0.1) Analysis settings
output_dir <- "Outputs"

if(!dir.exists(output_dir)){
  dir.create(output_dir)
}
model_order <- c("3.5", "4", "4o", "4.1", "5", "5.1", "5.2", "5.3", "5.4")

calculate_rank_correlation_bf10 <- function(x, y, rscale_value = "medium"){
  complete_index <- complete.cases(x, y)

  if(sum(complete_index) < 3){
    return(NA_real_)
  }
  bf_result <- BayesFactor::correlationBF(x = rank(x[complete_index]), y = rank(y[complete_index]), rscale = rscale_value)

  as.numeric(exp(bf_result@bayesFactor$bf))
}


# 1) Import final Social and Non-social results

# 1.1) Social rationality
social_overall <- read.xlsx(file.path(output_dir, "Results_SocialRat.xlsx"), sheet = "Alignment_Overall")

# 1.2) Non-social mathematical reasoning
math_overall <- read.xlsx(file.path(output_dir, "Results_math.xlsx"), sheet = "Alignment_Overall")

# 2.3) Standardize Agent and Generation variables
social_overall <- social_overall %>%
  mutate(Agent = as.character(Agent), Generation = as.numeric(Generation))

math_overall <- math_overall %>%
  mutate(Agent = as.character(Agent), Generation = as.numeric(Generation))


# 5) Prepare Social Human alignment
social_alignment <- social_overall %>%
  filter(Agent %in% model_order, Baseline == "Human baseline") %>%
  transmute(Agent, Generation, Social_Human = AlignmentPercent) %>%
  arrange(Generation)


# 6) Prepare Non-social Human alignment
non_social_alignment <- math_overall %>%
  filter(Agent %in% model_order, Baseline == "Human baseline") %>%
  transmute(Agent, Generation, NonSocial_Human = AlignmentPercent) %>%
  arrange(Generation)


# 7) Merge Social and Non-social Human-alignment profiles
social_nonsocial_profile <- social_alignment %>%
  inner_join(non_social_alignment, by = c("Agent", "Generation")) %>%
  mutate(
    # Social-minus-Non-social Human-alignment difference.
    # Positive values:
    #   Social alignment > Non-social alignment
    # Negative values:
    #   Social alignment < Non-social alignment
    AlignmentDifference = Social_Human - NonSocial_Human
    ) %>%
  arrange(Generation)


# 8) Model-specific descriptive alignment differences
model_specific_difference <- social_nonsocial_profile %>%
  transmute(Agent, Generation, Social_Human, NonSocial_Human, AlignmentDifference)


# 9) PRIMARY: Social vs. Non-social Human-alignment difference trajectory
# AlignmentDifference =
#   Social_Human - NonSocial_Human
#
# Positive rho:
#   Social Human alignment becomes increasingly high relative
#   to Non-social Human alignment.
#
# Negative rho:
#   Non-social Human alignment becomes increasingly high relative
#   to Social Human alignment.
social_nonsocial_test <- cor.test(
  social_nonsocial_profile$Generation,
  social_nonsocial_profile$AlignmentDifference,
  method = "spearman",
  exact = FALSE
)

social_nonsocial_result <- data.frame(
  Analysis = "Social minus Non-social Human-alignment difference",
  Baseline = "Human baseline",
  N_Models =
    sum(complete.cases(social_nonsocial_profile$Generation, social_nonsocial_profile$AlignmentDifference)),
  Spearman_rho = unname(social_nonsocial_test$estimate),
  p_value = social_nonsocial_test$p.value,
  Significant = social_nonsocial_test$p.value <= 0.050,
  Direction = case_when(
      unname(social_nonsocial_test$estimate) > 0 ~ "Increasing relative Social alignment",
      unname(social_nonsocial_test$estimate) < 0 ~ "Increasing relative Non-social alignment",
      TRUE ~ "No monotonic direction"
    )
)

# 9.1) ROBUSTNESS: Average Social vs. separate mathematical tests
# Compare average social alignment with overall math and each math test separately.
math_test_alignment <- read.xlsx(file.path(output_dir, "Results_math.xlsx"), sheet = "Alignment_By_Test") %>%
  filter(Agent %in% model_order, Baseline == "Human baseline") %>%
  transmute(
    Agent = as.character(Agent),
    Generation = as.numeric(Generation),
    Math_Test = as.character(Test),
    Math_Test_Human = AlignmentPercent
  ) %>%
  arrange(Math_Test, Generation)

social_math_test_profile <- social_alignment %>%
  inner_join(math_test_alignment, by = c("Agent", "Generation")) %>%
  mutate(AlignmentDifference = Social_Human - Math_Test_Human) %>%
  arrange(Math_Test, Generation)

social_math_test_result <- social_math_test_profile %>%
  group_by(Math_Test) %>%
  group_modify(
    function(current_data, key){
      current_test <- cor.test(current_data$Generation, current_data$AlignmentDifference, method = "spearman", exact = FALSE)

      data.frame(
        Analysis = "Average Social minus Math-test Human-alignment difference",
        Baseline = "Human baseline",
        N_Models = sum(complete.cases(current_data$Generation, current_data$AlignmentDifference)),
        Spearman_rho = unname(current_test$estimate),
        p_value = current_test$p.value,
        Direction = case_when(
            unname(current_test$estimate) > 0 ~ "Increasing relative Social alignment",
            unname(current_test$estimate) < 0 ~ "Increasing relative Math-test alignment",
            TRUE ~ "No monotonic direction"
          )
      )
    }
  ) %>%
  ungroup() %>%
  mutate(
    Adjusted_p_value = p.adjust(p_value, method = "holm"),
    Significant = Adjusted_p_value <= 0.050,
    BF_p_reference = if_else(Adjusted_p_value > 0.050, "adjusted", NA_character_)
  ) %>%
  left_join(social_math_test_profile, by = "Math_Test") %>%
  group_by(Math_Test) %>%
  mutate(
    BF10 = if_else(
        Adjusted_p_value >
          0.050, calculate_rank_correlation_bf10(Generation, AlignmentDifference, rscale_value = "medium"),
        NA_real_
      ),
    BF_method = if_else(Adjusted_p_value > 0.050, "rank correlation", NA_character_),
    BF_rscale = if_else(Adjusted_p_value > 0.050, "medium", NA_character_)
  ) %>%
  ungroup() %>%
  distinct(Math_Test, .keep_all = TRUE) %>%
  select(
    Analysis, Math_Test, Baseline, N_Models, Spearman_rho, p_value, Adjusted_p_value, Significant, Direction,
    BF_p_reference, BF10, BF_method, BF_rscale
  )


# 11) Long-format alignment-difference data
social_nonsocial_difference_long <- social_nonsocial_profile %>%
  transmute(Agent, Generation, AlignmentDifference) %>%
  arrange(Generation)


# 13) Export analysis results
write.xlsx(
  list(
    Alignment_Difference_Overall = model_specific_difference,
    Alignment_Difference_By_Test = social_math_test_profile,
    Alignment_Diff_Overall_Trend = social_nonsocial_result,
    Alignment_Difference_Test_Trend = social_math_test_result
  ), file = file.path(output_dir, "Results_social_vs_nonsocial.xlsx"), overwrite = TRUE
)


# 14) Completion messages
message("Social vs. Non-social Human-alignment comparison completed.")
message("One developmental trajectory was tested:")
message("Social minus Non-social Human-alignment difference.")
message("Results saved to: ", file.path(output_dir, "Results_social_vs_nonsocial.xlsx"))


# 15) Visualization: Social vs. Non-social developmental difference
# Plot social minus non-social human alignment; positive values favor social alignment.
# Add a fitted line only for a significant trend.
model_labels <- c("GPT-3.5", "GPT-4", "GPT-4o", "GPT-4.1", "GPT-5", "GPT-5.1", "GPT-5.2", "GPT-5.3", "GPT-5.4")

blue_color <- "#3b85bd"

blue_trend_color <- scales::alpha(blue_color, 0.80)

reference_color <- "grey70"

# Convert text size from points to mm.
figure_text_size <- 6 / ggplot2::.pt

# 15.2) Prepare visualization data
gap_plot_data <- social_nonsocial_difference_long %>%
  arrange(Generation)

# 15.3) Extract formal trend significance
alignment_difference_significant <- social_nonsocial_result$Significant[1]

# 15.4) Determine symmetric y-axis limits
# Center the y axis on zero with space for labels.
max_gap <- max(abs(gap_plot_data$AlignmentDifference), na.rm = TRUE)

y_limit <- ceiling((max_gap + 7) / 10) * 10

# Maintain at least ±20 percentage points
# for sufficient plotting space.
y_limit <- max(y_limit, 20)

# 15.5) Base figure
viso_social_nonsocial <- ggplot(gap_plot_data, aes(x = Generation, y = AlignmentDifference)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.30, color = reference_color)

# 15.6) Significant fitted trend
if(length(alignment_difference_significant) == 1 && isTRUE(alignment_difference_significant)){
  viso_social_nonsocial <- viso_social_nonsocial +
    geom_smooth(
      aes(x = Generation, y = AlignmentDifference),
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.90,
      color = blue_trend_color,
      show.legend = FALSE
    )

}

# 15.7) Add trajectory, points, labels, scales, and theme
viso_social_nonsocial <- viso_social_nonsocial +
  geom_line(color = blue_color, linewidth = 0.48) +
  geom_point(color = blue_color, shape = 1, size = 1.75, stroke = 0.70) +
  geom_text(
    aes(x = Generation, y = AlignmentDifference, label = sprintf("%.1f%%", AlignmentDifference)),
    color = blue_color,
    size = figure_text_size,
    vjust = -0.95,
    show.legend = FALSE
  ) +
  scale_x_continuous(limits = c(1, 9), breaks = 1:9, labels = model_labels, expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(breaks = seq(-y_limit, y_limit, by = 10), labels = scales::label_number(suffix = "%")) +
  coord_cartesian(ylim = c(-y_limit, y_limit), clip = "off") +
  labs(title = "Social vs. Non-social alignment", x = "LLM generation", y = "Alignment difference") +
  theme_classic(base_size = 6) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, color = "black", size = 6),
    axis.title.x = element_text(color = "black", size = 6),
    axis.text.y = element_text(color = "black", size = 6),
    axis.title.y = element_text(color = "black", size = 6),
    legend.position = "none",
    plot.title.position = "panel",
    plot.title = element_text(size = 6, face = "bold", hjust = 0.5),
    plot.margin = margin(4, 5, 4, 4)
  )

viso_social_nonsocial

# 15.8) Save figure
ggsave(
  file.path(output_dir, "viso_social_vs_nonsocial.pdf"),
  plot = viso_social_nonsocial,
  width = 3,
  height = 3,
  units = "in",
  device = grDevices::pdf
)


# 16) Visualization completion messages
message("Social vs. Non-social Human-alignment-difference visualization completed.")
message("Figure saved to: ", file.path(output_dir, "viso_social_vs_nonsocial.pdf"))
