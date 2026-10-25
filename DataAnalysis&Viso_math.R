# Mathematical reasoning: AQuA-RAT and SAT-MATH
# Programmed by Yining MAO & Feng XIAO (updated on 2026-10-19)
# Input: rawdata_math.xlsx, one sheet per agent.
# Human to GPT-4.1: direct 0/1 scores; GPT-5 series: 10 responses per item.
# Majority coding: >5 correct = 1, <5 = 0; 5:5 ties are excluded.


# 0) Preparation
pkg <- c("tidyr", "dplyr", "readxl", "purrr", "stringr", "ggplot2", "patchwork", "openxlsx", "BayesFactor")

lapply(pkg, require, character.only = TRUE)
# Use the script directory in RStudio.
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getActiveDocumentContext()$path

  if (nzchar(script_path)) {
    setwd(dirname(script_path))
  }
}
theme_set(theme_classic(base_size = 8))
input_file <- "rawdata_math.xlsx"

output_dir <- "Outputs"

single_score_agents <- c("human", "3.5", "4", "4o", "4.1")

repeated_score_agents <- c("5", "5.1", "5.2", "5.3", "5.4")

model_order <- c("3.5", "4", "4o", "4.1", "5", "5.1", "5.2", "5.3", "5.4")

test_order <- c("AQuA-RAT", "SAT-MATH")

# 0.1) Output directory
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

add_bayesian_trend_evidence <- function(trend_table, source_data, by_cols = character(0), y_col, p_col = NULL){
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
  if(length(by_cols) == 0){
    should_calculate <- any(!is.na(trend_table[[p_reference]]) & trend_table[[p_reference]] > 0.050)

    bf_lookup <- tibble(
      BF10_tmp = if(should_calculate){
          bayesian_rank_correlation_bf10(source_data$Generation, source_data[[y_col]])
        } else {
          NA_real_
        }
    )

    enriched_table <- bind_cols(trend_table, bf_lookup[rep(1, nrow(trend_table)), ])

  } else {
    bf_targets <- trend_table %>%
      mutate(BF_p_value_for_calc = .data[[p_reference]]) %>%
      select(all_of(by_cols), BF_p_value_for_calc) %>%
      filter(!is.na(BF_p_value_for_calc), BF_p_value_for_calc > 0.050)

    if(nrow(bf_targets) == 0){
      bf_lookup <- trend_table %>%
        select(all_of(by_cols)) %>%
        distinct() %>%
        mutate(BF10_tmp = NA_real_)

    } else {
      bf_lookup <- source_data %>%
        inner_join(bf_targets, by = by_cols) %>%
        group_by(across(all_of(by_cols))) %>%
        summarise(BF10_tmp = bayesian_rank_correlation_bf10(Generation, .data[[y_col]]), .groups = "drop")

    }
    enriched_table <- trend_table %>%
      left_join(bf_lookup, by = by_cols)

  }
  enriched_table %>%
    mutate(
      BF_p_reference = p_reference, BF10 =
        ifelse(!is.na(.data[[p_reference]]) & .data[[p_reference]] > 0.050, BF10_tmp, NA_real_), BF01 =
        ifelse(!is.na(BF10), 1 / BF10, NA_real_), BF_method =
        ifelse(!is.na(BF10), "Bayesian rank-transformed correlation (H0: rho = 0)", NA_character_), BF_rscale =
        ifelse(!is.na(BF10), "medium", NA_character_)
    ) %>%
      select(-BF10_tmp)
}


# 1) Data input
required_sheets <- c("human", model_order)

# 1.1) Read each agent sheet
math_raw <- set_names(required_sheets) %>%
  map(
    function(sheet_name){
      read_excel(input_file, sheet = sheet_name) %>%
        mutate(Agent = sheet_name)
    }
  )


# 2) Code item-level accuracy

# 2.1) Human and GPT-3.5 to GPT-4o: direct 0/1 coding
math_coded_early <- map_dfr(
  single_score_agents,
  function(agent_name){
    math_raw[[agent_name]] %>%
      select(No., `AQuA-RAT`, `SAT-MATH`, Agent) %>%
      pivot_longer(cols = all_of(test_order), names_to = "Test", values_to = "FinalScore") %>%
      mutate(
        FinalScore = suppressWarnings(as.numeric(FinalScore)),
        N_Responses = if_else(is.na(FinalScore), 0L, 1L),
        CorrectResponses = FinalScore,
        CodingStatus =
          case_when(is.na(FinalScore) ~ "MISSING", 
                    FinalScore %in% c(0, 1) ~ "DIRECT_0_1", 
                    TRUE ~ "INVALID_DIRECT_SCORE")
      )
  }
)

# 2.3) GPT-5 to GPT-5.4: repeated-response columns
repeated_columns <- c(paste0("AQuA-RAT", 1:10), paste0("SAT-MATH", 1:10))

# 2.4) Convert repeated responses into long format
math_repeated_long <- map_dfr(
  repeated_score_agents,
  function(agent_name){
    math_raw[[agent_name]] %>%
      select(No., all_of(repeated_columns), Agent) %>%
      pivot_longer(
        cols = all_of(repeated_columns),
        names_to = c("Test", "Repetition"),
        names_pattern = "^(AQuA-RAT|SAT-MATH)([0-9]+)$",
        values_to = "Score"
      ) %>%
      mutate(Score = suppressWarnings(as.numeric(Score)), Repetition = as.integer(Repetition))
  }
)

# 2.6) GPT-5 to GPT-5.4: majority coding across 10 responses
math_coded_5x <- math_repeated_long %>%
  group_by(Agent, No., Test) %>%
  summarise(N_Responses = sum(!is.na(Score)), CorrectResponses = sum(Score, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    # Require 10 responses; exclude 5:5 ties.
    FinalScore = case_when(
        N_Responses != 10 ~ NA_real_,
        CorrectResponses > 5 ~ 1,
        CorrectResponses < 5 ~ 0,
        CorrectResponses == 5 ~ NA_real_,
        TRUE ~ NA_real_
      ),
    CodingStatus = case_when(
        N_Responses != 10 ~ "INCOMPLETE_REPETITIONS",
        CorrectResponses > 5 ~ "MAJORITY_CORRECT",
        CorrectResponses < 5 ~ "MAJORITY_INCORRECT",
        CorrectResponses == 5 ~ "TIE_REMOVED",
        TRUE ~ "UNCLASSIFIED"
      )
  )

# 2.9) Combine all agents into one item-level dataset
math_coded <- bind_rows(math_coded_early, math_coded_5x) %>%
  mutate(Agent = factor(Agent, levels = c("human", model_order)), Test = factor(Test, levels = test_order)) %>%
  arrange(Agent, Test, No.)


# 4) Accuracy by test
accuracy_by_test <- math_coded %>%
  filter(!is.na(FinalScore)) %>%
  group_by(Agent, Test) %>%
  summarise(
    N_Included = n(),
    N_Correct = sum(FinalScore),
    N_Incorrect = N_Included -
      N_Correct,
    Accuracy = mean(FinalScore),
    .groups = "drop"
  ) %>%
  mutate(Generation = match(as.character(Agent), model_order)) %>%
  arrange(Test, Generation)


# 6) MAIN ANALYSIS: Dual-reference test-level alignment
# Human alignment = 100 * (1 - absolute accuracy difference).
# Perfect-accuracy alignment = 100 * model accuracy.
# Calculate alignment within each test before averaging.

# 6.1) Human reference accuracy by test
human_test_accuracy <- accuracy_by_test %>%
  filter(Agent == "human") %>%
  select(Test, HumanAccuracy = Accuracy)

# 6.2) Human-baseline alignment by test
math_human_alignment <- accuracy_by_test %>%
  filter(Agent != "human") %>%
  left_join(human_test_accuracy, by = "Test") %>%
  mutate(
    SignedDeviation = Accuracy -
      HumanAccuracy,
    AbsoluteDistance = abs(SignedDeviation),
    AlignmentPercent = 100 *
      (1 - AbsoluteDistance),
    Baseline = "Human baseline"
  ) %>%
  select(
    Agent, Generation, Test, Baseline, Accuracy, HumanAccuracy, SignedDeviation, 
    AbsoluteDistance, AlignmentPercent, N_Included
  ) %>%
  arrange(Test, Generation)

# 6.3) Deterministic-baseline alignment by test
math_deterministic_alignment <- accuracy_by_test %>%
  filter(Agent != "human") %>%
  mutate(
    DeterministicAccuracy = 1,
    AbsoluteDistance = abs(Accuracy - DeterministicAccuracy),
    AlignmentPercent = 100 *
      (1 - AbsoluteDistance),
    Baseline = "Deterministic baseline"
  ) %>%
  select(
    Agent, Generation, Test, Baseline, Accuracy, DeterministicAccuracy, 
    AbsoluteDistance, AlignmentPercent, N_Included
  ) %>%
  arrange(Test, Generation)

# 6.4) Combine dual-reference test-level alignment
math_test_alignment <- bind_rows(
  math_human_alignment %>%
    select(Agent, Generation, Test, Baseline, AlignmentPercent, N_Included),
  math_deterministic_alignment %>%
    select(Agent, Generation, Test, Baseline, AlignmentPercent, N_Included)
) %>%
  mutate(
    Test = factor(Test, levels = test_order),
    Baseline = factor(Baseline, levels = c("Human baseline", "Deterministic baseline"))
  ) %>%
  arrange(Baseline, Test, Generation)


# 7) Overall Mathematical reasoning alignment
# Overall alignment equally weights the two test-specific alignments.
math_overall_alignment <- math_test_alignment %>%
  group_by(Agent, Generation, Baseline) %>%
  summarise(
    N_Tests = sum(!is.na(AlignmentPercent)),
    AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Module = "Mathematical reasoning") %>%
  select(Agent, Generation, Module, Baseline, AlignmentPercent, N_Tests) %>%
  arrange(Baseline, Generation)


# 8) PRIMARY developmental trajectories
# Test the two overall baseline hypotheses separately, without multiplicity correction.

# 8.1) Overall Human-baseline developmental trajectory
math_overall_human <- math_overall_alignment %>%
  filter(Baseline == "Human baseline") %>%
  arrange(Generation)

math_human_test <- cor.test(math_overall_human$Generation, math_overall_human$AlignmentPercent, method = "spearman", exact = FALSE)

math_human_trend_result <- data.frame(
  Baseline = "Human baseline",
  N_Models = sum(complete.cases(math_overall_human$Generation, math_overall_human$AlignmentPercent)),
  Spearman_rho = unname(math_human_test$estimate),
  p_value = math_human_test$p.value,
  Significant = math_human_test$p.value <= 0.050,
  Direction = case_when(
      unname(math_human_test$estimate) > 0 ~ "Increasing alignment",
      unname(math_human_test$estimate) < 0 ~ "Decreasing alignment",
      TRUE ~ "No monotonic direction"
    )
)

# 8.2) Overall Deterministic-baseline developmental trajectory
math_overall_deterministic <- math_overall_alignment %>%
  filter(Baseline == "Deterministic baseline") %>%
  arrange(Generation)

math_deterministic_test <- cor.test(
  math_overall_deterministic$Generation,
  math_overall_deterministic$AlignmentPercent,
  method = "spearman",
  exact = FALSE
)

math_deterministic_trend_result <- data.frame(
  Baseline = "Deterministic baseline",
  N_Models =
    sum(complete.cases(math_overall_deterministic$Generation, math_overall_deterministic$AlignmentPercent)),
  Spearman_rho = unname(math_deterministic_test$estimate),
  p_value = math_deterministic_test$p.value,
  Significant = math_deterministic_test$p.value <= 0.050,
  Direction = case_when(
      unname(math_deterministic_test$estimate) > 0 ~ "Increasing alignment",
      unname(math_deterministic_test$estimate) < 0 ~ "Decreasing alignment",
      TRUE ~ "No monotonic direction"
    )
)

# 8.3) Summary of primary developmental trajectories
math_primary_trends <- bind_rows(math_human_trend_result, math_deterministic_trend_result)


# 9) Test-specific developmental trajectories
# Apply Holm correction across the two tests within each baseline.
math_test_trends_raw <- math_test_alignment %>%
  group_by(Baseline, Test) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, AlignmentPercent)),
    Spearman_rho = cor(Generation, AlignmentPercent, method = "spearman", use = "complete.obs"),
    p_raw = cor.test(Generation, AlignmentPercent, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  )

math_test_trends <- math_test_trends_raw %>%
  group_by(Baseline) %>%
  mutate(
    p_adjusted = p.adjust(p_raw, method = "holm"),
    Correction = "Holm within baseline (2 test-specific tests)",
    Significant_raw = p_raw <= 0.050,
    Significant_adjusted = p_adjusted <= 0.050,
    Direction = case_when(
        Spearman_rho > 0 ~ "Increasing alignment",
        Spearman_rho < 0 ~ "Decreasing alignment",
        TRUE ~ "No monotonic direction"
      )
  ) %>%
  ungroup()


# 10) Direct Human-vs-Deterministic developmental comparison
# BaselineGap = perfect-accuracy alignment minus human alignment.
# Positive gaps favor perfect accuracy; positive rho indicates an increasing gap.

# 10.1) Overall mathematical baseline gap
math_overall_gap <- math_overall_alignment %>%
  select(Agent, Generation, Baseline, AlignmentPercent) %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`) %>%
  arrange(Generation)

# 10.2) Overall mathematical baseline-gap trajectory
math_overall_gap_test <- cor.test(math_overall_gap$Generation, math_overall_gap$BaselineGap, method = "spearman", exact = FALSE)

math_overall_gap_result <- data.frame(
  Analysis = "Deterministic minus Human baseline alignment",
  N_Models = sum(complete.cases(math_overall_gap$Generation, math_overall_gap$BaselineGap)),
  Spearman_rho = unname(math_overall_gap_test$estimate),
  p_value = math_overall_gap_test$p.value,
  Significant = math_overall_gap_test$p.value <= 0.050,
  Direction = case_when(
      unname(math_overall_gap_test$estimate) > 0 ~ "Increasing relative deterministic alignment",
      unname(math_overall_gap_test$estimate) < 0 ~ "Increasing relative human alignment",
      TRUE ~ "No monotonic direction"
    )
)

# 10.3) Test-specific baseline gaps
math_test_gap <- math_test_alignment %>%
  select(Agent, Generation, Test, Baseline, AlignmentPercent) %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`) %>%
  arrange(Test, Generation)

# 10.4) Test-specific baseline-gap trajectories
# Apply Holm correction across the two test-specific gap trends.
math_test_gap_trends <- math_test_gap %>%
  group_by(Test) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, BaselineGap)),
    Spearman_rho = cor(Generation, BaselineGap, method = "spearman", use = "complete.obs"),
    p_raw = cor.test(Generation, BaselineGap, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    p_adjusted = p.adjust(p_raw, method = "holm"),
    Correction = "Holm (2 test-specific baseline-gap tests)",
    Significant_raw = p_raw <= 0.050,
    Significant_adjusted = p_adjusted <= 0.050,
    Direction = case_when(
        Spearman_rho > 0 ~ "Increasing relative deterministic alignment",
        Spearman_rho < 0 ~ "Increasing relative human alignment",
        TRUE ~ "No monotonic direction"
      )
  )

math_primary_trends <- add_bayesian_trend_evidence(
  trend_table = math_primary_trends, source_data = math_overall_alignment, by_cols = "Baseline",
  y_col = "AlignmentPercent"
)

math_test_trends <- add_bayesian_trend_evidence(
  trend_table = math_test_trends, source_data = math_test_alignment, by_cols = c("Baseline", "Test"),
  y_col = "AlignmentPercent"
)

math_overall_gap_result <- add_bayesian_trend_evidence(trend_table = math_overall_gap_result, source_data = math_overall_gap, y_col = "BaselineGap")

math_test_gap_trends <- add_bayesian_trend_evidence(
  trend_table = math_test_gap_trends, source_data = math_test_gap, by_cols = "Test", y_col = "BaselineGap"
)


# 14) Export analysis results
write.xlsx(
  list(
    Alignment_By_Test = math_test_alignment, Alignment_Overall = math_overall_alignment,
    Alignment_Test_Trend = math_test_trends, Alignment_Overall_Trend = math_primary_trends,
    Baseline_Gap_By_Test = math_test_gap, Baseline_Gap_overall = math_overall_gap,
    Baseline_Gap_Test_Trend = math_test_gap_trends, Baseline_Gap_Overall_Trend = math_overall_gap_result
  ), file = file.path(output_dir, "Results_math.xlsx"), overwrite = TRUE
)


# 15) Completion messages
message("Mathematical reasoning dual-reference analysis completed.")
message("Results saved to: ", file.path(output_dir, "Results_math.xlsx"))


# 16) Visualization: Mathematical reasoning development
# Overall panel on the left; AQuA-RAT and SAT-MATH on the right.
# Human: blue circles; perfect accuracy: green triangles.
# Show fitted lines only for significant trends (p <= .050).
# Trend significance uses raw p overall and Holm-adjusted p by test.

# 16.1) Figure settings
model_labels <- c("GPT-3.5", "GPT-4", "GPT-4o", "GPT-4.1", "GPT-5", "GPT-5.1", "GPT-5.2", "GPT-5.3", "GPT-5.4")

blue_color <- "#3b85bd"

green_color <- "#9db648"

# More visible significant fitted trends
blue_trend_color <- scales::alpha(blue_color, 0.80)

green_trend_color <- scales::alpha(green_color, 0.80)

# 100% reference line
reference_color <- "grey70"

# Convert text size from points to mm.
figure_text_size <- 6 / ggplot2::.pt

# 16.2) Prepare overall visualization data
math_overall_viso <- math_overall_alignment %>%
  transmute(Agent, Generation, Alignment = AlignmentPercent, Baseline = Baseline) %>%
  mutate(
    DisplayBaseline = case_when(
      as.character(Baseline) == "Human baseline" ~ "Human baseline",
      as.character(Baseline) == "Deterministic baseline" ~ "Perfect accuracy (100%)", TRUE ~ as.character(Baseline)
    ), DisplayBaseline = factor(DisplayBaseline, levels = c("Human baseline", "Perfect accuracy (100%)"))
  ) %>%
  arrange(DisplayBaseline, Generation)

# 16.3) Prepare test-specific visualization data
math_test_viso <- math_test_alignment %>%
  transmute(Agent, Test, Generation, Alignment = AlignmentPercent, Baseline = Baseline) %>%
  mutate(
    Test = factor(Test, levels = c("AQuA-RAT", "SAT-MATH")),
    DisplayBaseline = case_when(
      as.character(Baseline) == "Human baseline" ~ "Human baseline",
      as.character(Baseline) == "Deterministic baseline" ~ "Perfect accuracy (100%)", TRUE ~ as.character(Baseline)
    ), DisplayBaseline = factor(DisplayBaseline, levels = c("Human baseline", "Perfect accuracy (100%)"))
  ) %>%
  arrange(Test, DisplayBaseline, Generation)

# 16.4) Extract significance indicators
# Overall Human baseline
overall_human_significant <- math_primary_trends %>%
  filter(Baseline == "Human baseline") %>%
  pull(Significant)

# Overall Deterministic baseline
overall_deterministic_significant <- math_primary_trends %>%
  filter(Baseline == "Deterministic baseline") %>%
  pull(Significant)

# AQuA-RAT Human baseline
AQuA_human_significant <- math_test_trends %>%
  filter(Baseline == "Human baseline", Test == "AQuA-RAT") %>%
  pull(Significant_adjusted)

# AQuA-RAT Deterministic baseline
AQuA_deterministic_significant <- math_test_trends %>%
  filter(Baseline == "Deterministic baseline", Test == "AQuA-RAT") %>%
  pull(Significant_adjusted)

# SAT-MATH Human baseline
SAT_human_significant <- math_test_trends %>%
  filter(Baseline == "Human baseline", Test == "SAT-MATH") %>%
  pull(Significant_adjusted)

# SAT-MATH Deterministic baseline
SAT_deterministic_significant <- math_test_trends %>%
  filter(Baseline == "Deterministic baseline", Test == "SAT-MATH") %>%
  pull(Significant_adjusted)

# 17) Panel a: Mathematical reasoning
pan_math_a <- ggplot(
  math_overall_viso,
  aes(
    x = Generation, y = Alignment, color = DisplayBaseline, shape = DisplayBaseline, group = DisplayBaseline
  )
) +
# 100% reference line
geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.30, color = reference_color)

# 17.1) Significant overall Human-baseline trend
if(length(overall_human_significant) == 1 && isTRUE(overall_human_significant)){
  pan_math_a <- pan_math_a +
    geom_smooth(
      data = math_overall_viso %>%
        filter(DisplayBaseline == "Human baseline"),
      aes(x = Generation, y = Alignment),
      inherit.aes = FALSE,
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.90,
      color = blue_trend_color,
      show.legend = FALSE
    )

}

# 17.2) Significant overall Perfect-accuracy trend
if(length(overall_deterministic_significant) == 1 && isTRUE(overall_deterministic_significant)){
  pan_math_a <- pan_math_a +
    geom_smooth(
      data = math_overall_viso %>%
        filter(DisplayBaseline == "Perfect accuracy (100%)"),
      aes(x = Generation, y = Alignment),
      inherit.aes = FALSE,
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.90,
      color = green_trend_color,
      show.legend = FALSE
    )

}

# 17.3) Complete Panel a
pan_math_a <- pan_math_a +
# Raw developmental trajectories
geom_line(linewidth = 0.48) +
  geom_point(size = 1.75, stroke = 0.70) +
# Human-baseline percentage labels
geom_text(
  data = math_overall_viso %>%
    filter(DisplayBaseline == "Human baseline"),
  aes(x = Generation, y = Alignment, label = sprintf("%.1f%%", Alignment)),
  inherit.aes = FALSE,
  color = blue_color,
  size = figure_text_size,
  vjust = -0.95,
  show.legend = FALSE
) +
# Perfect-accuracy percentage labels
geom_text(
  data = math_overall_viso %>%
    filter(DisplayBaseline == "Perfect accuracy (100%)"),
  aes(x = Generation, y = Alignment, label = sprintf("%.1f%%", Alignment)),
  inherit.aes = FALSE,
  color = green_color,
  size = figure_text_size,
  vjust = 1.35,
  show.legend = FALSE
) +
scale_color_manual(values = c("Human baseline" = blue_color, "Perfect accuracy (100%)" = green_color)) +
  scale_shape_manual(values = c("Human baseline" = 1, "Perfect accuracy (100%)" = 2)) +
scale_x_continuous(limits = c(1, 9), breaks = 1:9, labels = model_labels, expand = expansion(mult = c(0.025, 0.025))) +
scale_y_continuous(breaks = seq(0, 100, by = 25), labels = scales::label_number(suffix = "%")) +
  coord_cartesian(ylim = c(0, 105), clip = "off") +
labs(
  tag = "a",
  title = "Mathematical reasoning",
  x = "LLM generation",
  y = "Alignment percentage",
  color = NULL,
  shape = NULL
) +
  guides(color = guide_legend(ncol = 1, byrow = TRUE), shape = guide_legend(ncol = 1, byrow = TRUE)) +
theme_classic(base_size = 6) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, color = "black", size = 6),
    axis.text.y = element_text(color = "black", size = 6),
    axis.title.x = element_text(color = "black", size = 6),
    axis.title.y = element_text(color = "black", size = 6),
    # Legend: lower-right, vertical
    legend.position = c(0.69, 0.13),
    legend.direction = "vertical",
    legend.justification = c(0, 0),
    legend.background = element_blank(),
    legend.key.width = grid::unit(0.65, "lines"),
    legend.key.height = grid::unit(0.65, "lines"),
    legend.spacing.y = grid::unit(0, "pt"),
    legend.text = element_text(size = 6),
    # Centered panel title
    plot.title.position = "panel",
    plot.title = element_text(size = 6, face = "bold", hjust = 0.5),
    plot.tag = element_text(face = "bold", size = 6),
    plot.tag.position = c(0, 1),
    plot.margin = margin(4, 5, 4, 4)
  )

pan_math_a


# 18) Prepare AQuA-RAT visualization data
math_AQuA_viso <- math_test_viso %>%
  filter(Test == "AQuA-RAT")

# 18.1) Panel b: AQuA-RAT
pan_math_b <- ggplot(
  math_AQuA_viso,
  aes(
    x = Generation, y = Alignment, color = DisplayBaseline, shape = DisplayBaseline, group = DisplayBaseline
  )
) +
# 100% reference line
geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.30, color = reference_color)

# 18.2) Significant AQuA-RAT Human-baseline trend
if(length(AQuA_human_significant) == 1 && isTRUE(AQuA_human_significant)){
  pan_math_b <- pan_math_b +
    geom_smooth(
      data = math_AQuA_viso %>%
        filter(DisplayBaseline == "Human baseline"),
      aes(x = Generation, y = Alignment),
      inherit.aes = FALSE,
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.90,
      color = blue_trend_color,
      show.legend = FALSE
    )

}

# 18.3) Significant AQuA-RAT Perfect-accuracy trend
if(length(AQuA_deterministic_significant) == 1 && isTRUE(AQuA_deterministic_significant)){
  pan_math_b <- pan_math_b +
    geom_smooth(
      data = math_AQuA_viso %>%
        filter(DisplayBaseline == "Perfect accuracy (100%)"),
      aes(x = Generation, y = Alignment),
      inherit.aes = FALSE,
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.90,
      color = green_trend_color,
      show.legend = FALSE
    )

}

# 18.4) Complete Panel b
pan_math_b <- pan_math_b +
# Raw developmental trajectories
geom_line(linewidth = 0.48) +
  geom_point(size = 1.75, stroke = 0.70) +
# Human-baseline percentage labels
geom_text(
  data = math_AQuA_viso %>%
    filter(DisplayBaseline == "Human baseline"),
  aes(x = Generation, y = Alignment, label = sprintf("%.1f%%", Alignment)),
  inherit.aes = FALSE,
  color = blue_color,
  size = figure_text_size,
  vjust = -0.95,
  show.legend = FALSE
) +
# Perfect-accuracy percentage labels
geom_text(
  data = math_AQuA_viso %>%
    filter(DisplayBaseline == "Perfect accuracy (100%)"),
  aes(x = Generation, y = Alignment, label = sprintf("%.1f%%", Alignment)),
  inherit.aes = FALSE,
  color = green_color,
  size = figure_text_size,
  vjust = 1.35,
  show.legend = FALSE
) +
  scale_color_manual(values = c("Human baseline" = blue_color, "Perfect accuracy (100%)" = green_color)) +
  scale_shape_manual(values = c("Human baseline" = 1, "Perfect accuracy (100%)" = 2)) +
  scale_x_continuous(limits = c(1, 9), breaks = 1:9, labels = model_labels, expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(breaks = seq(0, 100, by = 25), labels = scales::label_number(suffix = "%")) +
  coord_cartesian(ylim = c(0, 105), clip = "off") +
  labs(tag = "b", title = "AQuA-RAT (relatively easy task)", x = NULL, y = NULL, color = NULL, shape = NULL) +
  theme_classic(base_size = 6) +
  theme(
    # Complete x axis removed
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.line.x = element_blank(),
    axis.title.x = element_blank(),
    axis.text.y = element_text(color = "black", size = 6),
    axis.title.y = element_blank(),
    legend.position = "none",
    plot.title.position = "panel",
    plot.title = element_text(size = 6, face = "bold", hjust = 0.5),
    plot.tag = element_text(face = "bold", size = 6),
    plot.tag.position = c(0, 1),
    plot.margin = margin(4, 5, 4, 4)
  )

pan_math_b


# 19) Prepare SAT-MATH visualization data
math_SAT_viso <- math_test_viso %>%
  filter(Test == "SAT-MATH")

# 19.1) Panel c: SAT-MATH
pan_math_c <- ggplot(
  math_SAT_viso,
  aes(
    x = Generation, y = Alignment, color = DisplayBaseline, shape = DisplayBaseline, group = DisplayBaseline
  )
) +
# 100% reference line
geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.30, color = reference_color)

# 19.2) Significant SAT-MATH Human-baseline trend
if(length(SAT_human_significant) == 1 && isTRUE(SAT_human_significant)){
  pan_math_c <- pan_math_c +
    geom_smooth(
      data = math_SAT_viso %>%
        filter(DisplayBaseline == "Human baseline"),
      aes(x = Generation, y = Alignment),
      inherit.aes = FALSE,
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.90,
      color = blue_trend_color,
      show.legend = FALSE
    )

}

# 19.3) Significant SAT-MATH Perfect-accuracy trend
if(length(SAT_deterministic_significant) == 1 && isTRUE(SAT_deterministic_significant)){
  pan_math_c <- pan_math_c +
    geom_smooth(
      data = math_SAT_viso %>%
        filter(DisplayBaseline == "Perfect accuracy (100%)"),
      aes(x = Generation, y = Alignment),
      inherit.aes = FALSE,
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.90,
      color = green_trend_color,
      show.legend = FALSE
    )

}

# 19.4) Complete Panel c
pan_math_c <- pan_math_c +
# Raw developmental trajectories
geom_line(linewidth = 0.48) +
  geom_point(size = 1.75, stroke = 0.70) +
# Human-baseline percentage labels
geom_text(
  data = math_SAT_viso %>%
    filter(DisplayBaseline == "Human baseline"),
  aes(x = Generation, y = Alignment, label = sprintf("%.1f%%", Alignment)),
  inherit.aes = FALSE,
  color = blue_color,
  size = figure_text_size,
  vjust = -0.95,
  show.legend = FALSE
) +
# Perfect-accuracy percentage labels
geom_text(
  data = math_SAT_viso %>%
    filter(DisplayBaseline == "Perfect accuracy (100%)"),
  aes(x = Generation, y = Alignment, label = sprintf("%.1f%%", Alignment)),
  inherit.aes = FALSE,
  color = green_color,
  size = figure_text_size,
  vjust = 1.35,
  show.legend = FALSE
) +
  scale_color_manual(values = c("Human baseline" = blue_color, "Perfect accuracy (100%)" = green_color)) +
  scale_shape_manual(values = c("Human baseline" = 1, "Perfect accuracy (100%)" = 2)) +
  scale_x_continuous(limits = c(1, 9), breaks = 1:9, labels = model_labels, expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(breaks = seq(0, 100, by = 25), labels = scales::label_number(suffix = "%")) +
  coord_cartesian(ylim = c(0, 105), clip = "off") +
  labs(
    tag = "c",
    title = "SAT-MATH (relatively difficult task)",
    x = "LLM generation",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  theme_classic(base_size = 6) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, color = "black", size = 6),
    axis.text.y = element_text(color = "black", size = 6),
    axis.title.x = element_text(color = "black", size = 6),
    axis.title.y = element_blank(),
    legend.position = "none",
    plot.title.position = "panel",
    plot.title = element_text(size = 6, face = "bold", hjust = 0.5),
    plot.tag = element_text(face = "bold", size = 6),
    plot.tag.position = c(0, 1),
    plot.margin = margin(4, 5, 4, 4)
  )

pan_math_c


# 20) Fixed combined figure design
# Overall panel on the left; AQuA-RAT above SAT-MATH on the right.
math_design <- "
AB
AC
"

viso_math <-
  pan_math_a +
  pan_math_b +
  pan_math_c +
  plot_layout(design = math_design, widths = c(1, 1), heights = c(1, 1))

viso_math


# 21) Export final mathematical-reasoning figure
ggsave(
  file.path(output_dir, "viso_math.pdf"),
  plot = viso_math,
  width = 6,
  height = 3,
  units = "in",
  device = grDevices::pdf
)


# 22) Visualization completion messages
message("Mathematical reasoning visualization completed.")
message("Figure saved to: ", file.path(output_dir, "viso_math.pdf"))
