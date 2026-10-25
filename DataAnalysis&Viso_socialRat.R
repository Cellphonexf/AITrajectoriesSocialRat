# LLM evolutionary trajectory
# Overall social rationality profile
# Dual-reference developmental analysis
#
# Requires:
# "Outputs/Results_socialcue.xlsx"
# "Outputs/Results_ToM.xlsx"
# "Outputs/Results_moral.xlsx"
#
# Programmed by Feng XIAO (updated on 2026-9-6)


# 0) Preparation
pkg <- c(
  "tidyr", "dplyr", "readxl", "purrr", "stringr", "ggplot2", "patchwork", "openxlsx", "scales", "BayesFactor"
)

lapply(pkg, require, character.only = TRUE)
# Working directory (RStudio only)
if(requireNamespace("rstudioapi", quietly = TRUE)){
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
# Global ggplot defaults
theme_set(theme_classic(base_size = 8))

# 0.1) Analysis settings
output_dir <- "Outputs"

if(!dir.exists(output_dir)){
  dir.create(output_dir)
}

# 0.2) Helper function for Bayesian trend evidence
bayesian_rank_correlation_bf10 <- function(x, y){
  valid <- complete.cases(x, y)

  x_valid <- x[valid]

  y_valid <- y[valid]

  if(length(x_valid) < 3 || length(unique(x_valid)) < 2 || length(unique(y_valid)) < 2){
    return(NA_real_)
  }
  bayes_result <- BayesFactor::correlationBF(
    x = rank(x_valid, ties.method = "average"), y = rank(y_valid, ties.method = "average"), rscale = "medium"
  )

  as.numeric(BayesFactor::extractBF(bayes_result, onlybf = TRUE))
}

add_bayesian_trend_evidence <- function(trend_table, source_data, y_col, p_col = NULL){
  p_reference <- p_col

  if(is.null(p_reference)){
    p_reference <- case_when(
      "p_adjusted" %in% names(trend_table) ~ "p_adjusted", "p_value" %in% names(trend_table) ~ "p_value",
      "p_raw" %in% names(trend_table) ~ "p_raw", TRUE ~ NA_character_
    )

  }
  if(is.na(p_reference)){
    stop("No p-value column found for Bayesian evidence calculation.")
  }
  should_calculate <- any(!is.na(trend_table[[p_reference]]) & trend_table[[p_reference]] > 0.050)

  BF10_tmp <- if(should_calculate){
    bayesian_rank_correlation_bf10(source_data$Generation, source_data[[y_col]])
  } else {
    NA_real_
  }

  trend_table %>%
    mutate(
      BF_p_reference = p_reference, BF10 =
        ifelse(!is.na(.data[[p_reference]]) & .data[[p_reference]] > 0.050, BF10_tmp, NA_real_), BF01 =
        ifelse(!is.na(BF10), 1 / BF10, NA_real_), BF_method =
        ifelse(!is.na(BF10), "Bayesian rank-transformed correlation (H0: rho = 0)", NA_character_), BF_rscale =
        ifelse(!is.na(BF10), "medium", NA_character_)
    )
}

# GPT model generation order
model_order <- c("3.5", "4", "4o", "4.1", "5", "5.1", "5.2", "5.3", "5.4")

# Figure labels
model_labels <- c("GPT-3.5", "GPT-4", "GPT-4o", "GPT-4.1", "GPT-5", "GPT-5.1", "GPT-5.2", "GPT-5.3", "GPT-5.4")

# Figure colors
blue_color <- "#3b85bd"

green_color <- "#9db648"

blue_trend_color <- scales::alpha(blue_color, 0.55)

green_trend_color <- scales::alpha(green_color, 0.55)


# 1) Import domain-level Human-alignment results
# Overall social rationality equally weights human alignment across the three domains.
adaptive_overall_raw <- read.xlsx(file.path(output_dir, "Results_socialcue.xlsx"), sheet = "Alignment_Overall")

adaptive_overall <- adaptive_overall_raw %>%
  transmute(
    Agent = as.character(Agent),
    Generation = match(Agent, model_order),
    Module = "Adaptive cues",
    Baseline = as.character(Baseline),
    AlignmentPercent = as.numeric(AlignmentPercent)
  ) %>%
  filter(Agent %in% model_order, Baseline == "Human baseline")

# 1.2) Social inference
social_inference_overall_raw <- read.xlsx(file.path(output_dir, "Results_ToM.xlsx"), sheet = "Alignment_Overall")

social_inference_overall <- social_inference_overall_raw %>%
  transmute(
    Agent = as.character(Agent),
    Generation = match(Agent, model_order),
    Module = "Social inference",
    Baseline = as.character(Baseline),
    AlignmentPercent = as.numeric(AlignmentPercent)
  ) %>%
  filter(Agent %in% model_order, Baseline == "Human baseline")

# 1.3) Normative reasoning
normative_overall_raw <- read.xlsx(file.path(output_dir, "Results_moral.xlsx"), sheet = "Alignment_Original")

normative_overall <- normative_overall_raw %>%
  transmute(
    Agent = as.character(Agent),
    Generation = match(Agent, model_order),
    Module = "Normative reasoning",
    Baseline = as.character(Baseline),
    AlignmentPercent = as.numeric(AlignmentPercent)
  ) %>%
  filter(Agent %in% model_order, Baseline == "Human baseline")

# 1.4) Bootstrap domain-level Human-alignment results
# Reuse bootstrap workbooks when available; otherwise run the upstream bootstrap prefix.
load_domain_bootstrap <- function(workbook, scripts, result_object){
  if ("Bootstrap_alignment" %in% getSheetNames(workbook)) {
    return(read.xlsx(workbook, sheet = "Bootstrap_alignment"))
  }
  script <- scripts[file.exists(scripts)][1]

  if (is.na(script)) stop("Upstream analysis script required for bootstrap: ", paste(scripts, collapse = " or "))
  analysis_env <- new.env(parent = globalenv())

  for (expr in parse(file = script)) {
    eval(expr, envir = analysis_env)
    if (exists(result_object, envir = analysis_env, inherits = FALSE)) {
      return(map_dfr(get(result_object, envir = analysis_env, inherits = FALSE), "overall"))
    }
  }
  stop("Bootstrap results not found in ", script)
}

adaptive_bootstrap_raw <- load_domain_bootstrap(file.path(output_dir, "Results_socialcue.xlsx"), c("DataAnalysis&Viso_socialcue(5)(1)_HolmGapAdjusted_副本.R", "DataAnalysis&Viso_socialcue(5)(1)_HolmGapAdjusted.R"), "socialcue_bootstrap_results")

adaptive_bootstrap <- adaptive_bootstrap_raw %>%
  transmute(
    BootstrapID = as.integer(BootstrapID), Agent = as.character(Agent), Generation = match(Agent, model_order),
    Module = "Adaptive cues", Baseline = as.character(Baseline), AlignmentPercent = as.numeric(AlignmentPercent)
  ) %>%
  filter(Agent %in% model_order, Baseline == "Human baseline")

social_inference_bootstrap_raw <- load_domain_bootstrap(file.path(output_dir, "Results_ToM.xlsx"), "DataAnalysis&Viso_ToM(2).R", "tom_bootstrap_results")

social_inference_bootstrap <- social_inference_bootstrap_raw %>%
  transmute(
    BootstrapID = as.integer(BootstrapID), Agent = as.character(Agent), Generation = match(Agent, model_order),
    Module = "Social inference", Baseline = as.character(Baseline), AlignmentPercent = as.numeric(AlignmentPercent)
  ) %>%
  filter(Agent %in% model_order, Baseline == "Human baseline")

normative_bootstrap_raw <- load_domain_bootstrap(file.path(output_dir, "Results_moral.xlsx"), "DataAnalysis&Viso_moral(2).R", "moral_bootstrap_results")

normative_bootstrap <- normative_bootstrap_raw %>%
  transmute(
    BootstrapID = as.integer(BootstrapID), Agent = as.character(Agent), Generation = match(Agent, model_order),
    Module = "Normative reasoning", Baseline = as.character(Baseline), AlignmentPercent =
      as.numeric(AlignmentPercent)
  ) %>%
  filter(Agent %in% model_order, Baseline == "Human baseline")


# 2) Combine three Social Rationality dimensions
social_module_alignment <- bind_rows(adaptive_overall, social_inference_overall, normative_overall) %>%
  mutate(Module = factor(Module, levels = c("Adaptive cues", "Social inference", "Normative reasoning"))) %>%
  arrange(Generation, Module)


# 3) Overall Social Rationality Human alignment
# Average adaptive-cue, social-inference and original-dilemma human alignment equally.
social_overall_alignment <- social_module_alignment %>%
  group_by(Agent, Generation) %>%
  summarise(
    N_Modules = sum(!is.na(AlignmentPercent)),
    AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Module = "Overall social rationality", Baseline = "Human baseline") %>%
  select(Agent, Generation, Module, Baseline, AlignmentPercent, N_Modules) %>%
  arrange(Generation)

# 3.2) Bootstrap Overall Social Rationality Human alignment
social_bootstrap_module_alignment <- bind_rows(adaptive_bootstrap, social_inference_bootstrap, normative_bootstrap) %>%
  mutate(Module = factor(Module, levels = c("Adaptive cues", "Social inference", "Normative reasoning"))) %>%
  arrange(BootstrapID, Generation, Module)

social_bootstrap_overall_alignment <- social_bootstrap_module_alignment %>%
  group_by(BootstrapID, Agent, Generation) %>%
  summarise(
    N_Modules = sum(!is.na(AlignmentPercent)), AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Module = "Overall social rationality", Baseline = "Human baseline") %>%
  select(BootstrapID, Agent, Generation, Module, Baseline, AlignmentPercent, N_Modules) %>%
  arrange(BootstrapID, Generation)

social_bootstrap_rho <- social_bootstrap_overall_alignment %>%
  group_by(BootstrapID) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, AlignmentPercent)), Spearman_rho =
      cor(Generation, AlignmentPercent, method = "spearman", use = "complete.obs"), .groups = "drop"
  )

social_bootstrap_ci <- social_bootstrap_rho %>%
  filter(is.finite(Spearman_rho)) %>%
  summarise(
    CI_Method = "Percentile bootstrap", Confidence_Level = 0.95, CI_Lower =
      quantile(Spearman_rho, probs = 0.025, na.rm = TRUE, names = FALSE), CI_Upper =
      quantile(Spearman_rho, probs = 0.975, na.rm = TRUE, names = FALSE), N_Boot_Valid = n()
  )


# 4) PRIMARY developmental trend analysis
social_human_test <- cor.test(
  social_overall_alignment$Generation,
  social_overall_alignment$AlignmentPercent,
  method = "spearman",
  exact = FALSE
)

social_human_trend_result <- data.frame(
  Baseline = "Human baseline",
  N_Models = sum(complete.cases(social_overall_alignment$Generation, social_overall_alignment$AlignmentPercent)),
  Spearman_rho = unname(social_human_test$estimate),
  CI_Lower = social_bootstrap_ci$CI_Lower,
  CI_Upper = social_bootstrap_ci$CI_Upper,
  CI_Method = social_bootstrap_ci$CI_Method,
  N_Boot_Valid = social_bootstrap_ci$N_Boot_Valid,
  p_value = social_human_test$p.value,
  Significant = social_human_test$p.value <= 0.050,
  Direction = case_when(
      unname(social_human_test$estimate) > 0 ~ "Increasing alignment",
      unname(social_human_test$estimate) < 0 ~ "Decreasing alignment",
      TRUE ~ "No monotonic direction"
    )
)


# 5) Prepare wide Social Rationality profile
# Wide profile for radar plots.
social_profile <- social_module_alignment %>%
  mutate(
    ProfileKey = case_when(
      Module == "Adaptive cues" ~ "AdaptiveCueHuman",
      Module == "Social inference" ~ "SocialInferenceHuman",
      Module == "Normative reasoning" ~ "NormativeReasoningHuman"
    )
  ) %>%
  select(Agent, Generation, ProfileKey, AlignmentPercent) %>%
  pivot_wider(names_from = ProfileKey, values_from = AlignmentPercent) %>%
  left_join(social_overall_alignment %>%
    select(Agent, OverallHumanAlignment = AlignmentPercent), by = "Agent") %>%
  arrange(Generation)

social_human_trend_result <- add_bayesian_trend_evidence(
  trend_table = social_human_trend_result, source_data = social_overall_alignment, y_col = "AlignmentPercent"
)


# 6) Export analysis results
write.xlsx(
  list(
    Alignment_By_Domain = social_module_alignment, Alignment_Overall = social_overall_alignment,
    Alignment_Overall_Trend = social_human_trend_result
  ), file = file.path(output_dir, "Results_SocialRat.xlsx"), overwrite = TRUE
)
message("Overall Social Rationality Human-alignment analysis completed.")
message("Results saved to: ", file.path(output_dir, "Results_SocialRat.xlsx"))


# 7) Visualization: Overall Social Rationality trajectory

# 7.1) Figure settings
reference_color <- "grey70"

# geom_text() uses mm-based sizing.
# Convert to approximately 6 pt for Panel a.
figure_text_size <- 6 / ggplot2::.pt

# 7.2) Prepare plot data
social_trajectory_plot <- social_overall_alignment %>%
  select(Agent, Generation, AlignmentPercent) %>%
  arrange(Generation)

# 7.3) Extract significance flag
social_human_significant <- social_human_trend_result$Significant[1]

# 7.4) Base trajectory panel
pan_social_line <- ggplot(social_trajectory_plot, aes(x = Generation, y = AlignmentPercent)) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.30, color = reference_color)

# 7.5) Add significant Human-alignment trend
if(length(social_human_significant) == 1 && isTRUE(social_human_significant)){
  pan_social_line <- pan_social_line +
    geom_smooth(
      aes(x = Generation, y = AlignmentPercent),
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.90,
      color = blue_trend_color,
      show.legend = FALSE
    )

}

# 7.6) Add raw trajectory, points, labels, scales, and theme
pan_social_line <- pan_social_line +
  geom_line(color = blue_color, linewidth = 0.48) +
  geom_point(color = blue_color, shape = 1, size = 1.75, stroke = 0.70) +
  geom_text(
    aes(x = Generation, y = AlignmentPercent, label = sprintf("%.1f%%", AlignmentPercent)),
    color = blue_color,
    size = figure_text_size,
    vjust = -1.00,
    show.legend = FALSE
  ) +
  scale_x_continuous(breaks = 1:9, labels = model_labels, expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 25),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(ylim = c(0, 105), clip = "off") +
  labs(tag = "a", title = "Social rationality", x = "LLM generation", y = "Alignment percentage") +
  theme_classic(base_size = 6) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, color = "black", size = 6),
    axis.title.x = element_text(color = "black", size = 6),
    axis.text.y = element_text(color = "black", size = 6),
    axis.title.y = element_text(color = "black", size = 6),
    legend.position = "none",
    plot.title.position = "panel",
    plot.title = element_text(size = 6, face = "bold", hjust = 0.5),
    plot.tag = element_text(face = "bold", size = 6),
    plot.tag.position = c(0, 1),
    plot.margin = margin(t = 3, r = 5, b = 4, l = 4)
  )

pan_social_line


# 8) Radar visualization
# Radar scores are expressed as proportions from 0 to 1.
#
# Each radar contains only one Human-alignment polygon.
# The outer boundary (100%) represents perfect empirical-Human alignment.
radar_profile_all <- social_profile %>%
  mutate(
    Agent = factor(Agent, levels = model_order),
    AdaptiveCueHuman = AdaptiveCueHuman / 100,
    SocialInferenceHuman = SocialInferenceHuman / 100,
    NormativeReasoningHuman = NormativeReasoningHuman / 100,
    OverallHumanAlignment = OverallHumanAlignment / 100
  ) %>%
  arrange(Agent)

# 8.1) Radar axes
radar_axes <- data.frame(
  Dimension = c("Adaptive cues", "Social inference", "Normative reasoning"),
  AxisOrder = 1:3,
  Angle = c(pi / 2, 7 * pi / 6, 11 * pi / 6)
)

# 8.2) Function to draw one radar
make_radar_plot <- function(current_model, show_b = FALSE){
  radar_scale <- 1.50

  # Dimension-specific Human-alignment values
  human_values <- c(
    as.numeric(current_model$AdaptiveCueHuman),
    as.numeric(current_model$SocialInferenceHuman),
    as.numeric(current_model$NormativeReasoningHuman)
  )

  # Human-alignment coordinates
  human_data <- radar_axes %>%
    mutate(Value = human_values, x = radar_scale * Value * cos(Angle), y = radar_scale * Value * sin(Angle))

  # Simplified radar grid
  grid_levels <- c(0.50, 1.00)

  radar_grid <- tidyr::expand_grid(Radius = grid_levels, radar_axes) %>%
    mutate(x = radar_scale * Radius * cos(Angle), y = radar_scale * Radius * sin(Angle)) %>%
    arrange(Radius, AxisOrder)

  radar_grid_closed <- radar_grid %>%
    group_by(Radius) %>%
    group_modify(~ bind_rows(.x, .x[1, ])) %>%
    ungroup()

  # Radar spokes
  radar_spokes <- radar_axes %>%
    mutate(xend = radar_scale * cos(Angle), yend = radar_scale * sin(Angle))

  # Dimension labels
  adaptive_label <- data.frame(x = 0, y = 1.58, Label = "Adaptive cues")

  social_label <- data.frame(x = -1.43, y = -0.93, Label = "Social\ninference")

  normative_label <- data.frame(x = 1.43, y = -0.93, Label = "Normative\nreasoning")

  # Panel b label
  b_label <- data.frame(x = -1.56, y = 1.91, Label = "b")

  # Draw radar
  radar_plot <- ggplot() +
    geom_path(data = radar_grid_closed, aes(x = x, y = y, group = Radius), linewidth = 0.18, color = "grey88") +
    geom_segment(data = radar_spokes, aes(x = 0, y = 0, xend = xend, yend = yend), linewidth = 0.18, color = "grey85") +
    geom_polygon(
      data = human_data,
      aes(x = x, y = y),
      fill = blue_color,
      color = blue_color,
      alpha = 0.12,
      linewidth = 0.45
    ) +
    geom_point(data = human_data, aes(x = x, y = y), shape = 1, size = 0.9, stroke = 0.4, color = blue_color) +
    geom_text(data = adaptive_label, aes(x = x, y = y, label = Label), size = 1.10, color = "black") +
    geom_text(
      data = social_label,
      aes(x = x, y = y, label = Label),
      size = 1.05,
      lineheight = 0.84,
      hjust = 0.5,
      color = "black"
    ) +
    geom_text(
      data = normative_label,
      aes(x = x, y = y, label = Label),
      size = 1.05,
      lineheight = 0.84,
      hjust = 0.5,
      color = "black"
    ) +
    labs(title = paste0("GPT-", as.character(current_model$Agent))) +
    coord_equal(xlim = c(-1.52, 1.52), ylim = c(-1.05, 1.90), clip = "off") +
    theme_void() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 5.5, face = "bold", hjust = 0.5, color = "black", margin = margin(b = 0)),
      plot.margin = margin(t = 0, r = -3, b = 0, l = -3, unit = "pt")
    )

  if(show_b){
    radar_plot <- radar_plot +
      geom_text(
        data = b_label,
        aes(x = x, y = y, label = Label),
        size = 2.8,
        fontface = "bold",
        hjust = 0,
        vjust = 1,
        color = "black"
      )

  }
  return(radar_plot)
}

# 8.3) Generate nine radar panels
radar_plot_list <- lapply(
  seq_len(nrow(radar_profile_all)),
  function(i){
    make_radar_plot(radar_profile_all[ i, ], show_b = i == 1)
  }
)


# 9) Combine nine radar panels
pan_social_radar <- wrap_plots(radar_plot_list, ncol = 3, nrow = 3, byrow = TRUE) +
  plot_layout(widths = c(1, 1, 1), heights = c(1, 1, 1))

pan_social_radar


# 10) Combine overall trajectory and radar profiles
Pic_social_complete <-
  pan_social_line +
  pan_social_radar +
  plot_layout(widths = c(0.85, 1.35))

# 10.1) Export final figure
ggsave(
  file.path(output_dir, "viso_social_rationality.pdf"),
  plot = Pic_social_complete,
  width = 6,
  height = 3,
  units = "in",
  device = grDevices::pdf
)
message("Social Rationality visualization completed.")
message("Figure saved to: ", file.path(output_dir, "viso_social_rationality.pdf"))
