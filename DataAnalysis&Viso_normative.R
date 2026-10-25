# LLM evolutionary trajectory
# Normative reasoning: consequence-based vs. deontological responses
# Dual-reference developmental analysis
# Requires: "rawdata_moraldilemmas.xlsx"
# Programmed by Feng XIAO and Yining MAO (updated on 2026-10-20)


# 0) Preparation
# Load packages actually used
pkg <- c("tidyr", "dplyr", "readxl", "purrr", "stringr", "ggplot2", "patchwork", "openxlsx", "BayesFactor")

lapply(pkg, require, character.only = TRUE)
# Working directory (RStudio only)
if (requireNamespace("rstudioapi", quietly = TRUE)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
# Global ggplot defaults
theme_set(theme_classic(base_size = 8))

# 0.1) Analysis settings
output_dir <- "Outputs"

if(!dir.exists(output_dir)){
  dir.create(output_dir)
}
# Define LLM generation order
model_order <- c("3.5", "4", "4o", "4.1", "5", "5.1", "5.2", "5.3", "5.4")

# Labels used in figures
model_labels <- c("GPT-3.5", "GPT-4", "GPT-4o", "GPT-4.1", "GPT-5", "GPT-5.1", "GPT-5.2", "GPT-5.3", "GPT-5.4")

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
# Function for importing multi-sheet Excel files
# Each sheet represents one agent/model
# Non-data sheets (i.e., Note) are excluded
read_agent_data <- function(file){
  sheets <- excel_sheets(file)

  sheets <- sheets[
    !tolower(sheets) %in% c("note")
  ]

  data <- map_dfr(
    sheets,
    function(sheet_name){
      df <- read_excel(file, sheet = sheet_name)

      # Keep rows containing at least one valid response
      df <- df %>%
        filter(if_any(-No., ~ !is.na(.) & . != ""))

      # Add agent/model identifier
      df <- df %>%
        mutate(Agent = sheet_name)

      return(df)
    }
  )

  return(data)
}

# Import raw dataset
moral <- read_agent_data("rawdata_moraldilemmas.xlsx")

# Check imported agents
# 1.1) Convert to long format
moral_long <- moral %>%
  pivot_longer(cols = -c(No., Agent), names_to = "Scenario", values_to = "Response") %>%
  mutate(
    # Convert responses to numeric values
    # 1 = consequence-based response
    # 0 = deontological response
    ResponseNumeric = suppressWarnings(as.numeric(Response)),
    # Extract framing information
    Framing = case_when(
      str_detect(Scenario, "-Ori$") ~ "Original framing",
      str_detect(Scenario, "-Omi$") ~ "Omission framing",
      str_detect(Scenario, "-YN$") ~ "Yes/No framing",
      TRUE ~ NA_character_
    ),
    # Extract base scenario name
    ScenarioName = str_remove(Scenario, "-(Ori|Omi|YN)$")
  )


# 2) Scenario-level moral preference
# Calculate consequence-based response preference for each:
# Agent × Scenario × Framing
# 1 = consequence-based response
# 0 = deontological response
moral_summary <- moral_long %>%
  filter(!is.na(ResponseNumeric), !is.na(Framing)) %>%
  group_by(Agent, ScenarioName, Framing) %>%
  summarise(
    N_Response = sum(!is.na(ResponseNumeric)),
    CBRPreference = mean(ResponseNumeric, na.rm = TRUE),
    .groups = "drop"
  )


# 3) MAIN ANALYSIS: Original-framing normative profile
# Original-dilemma alignment = 100 * (1 - mean absolute difference in utilitarian proportions).
# References: empirical humans and uniformly utilitarian responding.

# 3.1) Original-framing behavioral profiles
moral_original <- moral_summary %>%
  filter(Framing == "Original framing") %>%
  mutate(Generation = match(Agent, model_order)) %>%
  arrange(Agent, ScenarioName)

# 3.2) Human Original-framing reference profile
human_moral_original_profile <- moral_original %>%
  filter(Agent == "human") %>%
  group_by(ScenarioName) %>%
  summarise(HumanCBRPreference = mean(CBRPreference, na.rm = TRUE), .groups = "drop")

# 3.3) Human-baseline scenario-level alignment
moral_human_condition_alignment <- moral_original %>%
  filter(Agent != "human") %>%
  left_join(human_moral_original_profile, by = "ScenarioName") %>%
  mutate(
    SignedDeviation = CBRPreference -
      HumanCBRPreference,
    AbsoluteDistance = abs(SignedDeviation),
    Alignment = 1 -
      AbsoluteDistance
  )

# 3.4) Overall Human-baseline alignment
normative_human_alignment <- moral_human_condition_alignment %>%
  group_by(Agent) %>%
  summarise(
    N_Scenarios = sum(complete.cases(CBRPreference, HumanCBRPreference)),
    AlignmentPercent = 100 *
      mean(Alignment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Generation = match(Agent, model_order), Module = "Normative reasoning", Baseline = "Human baseline") %>%
  filter(!is.na(Generation)) %>%
  select(Agent, Generation, Module, Baseline, AlignmentPercent, N_Scenarios) %>%
  arrange(Generation)

# 3.5) Deterministic-baseline scenario-level alignment
moral_deterministic_condition_alignment <- moral_original %>%
  filter(Agent != "human") %>%
  mutate(
    DeterministicCBRPreference = 1,
    AbsoluteDistance = abs(CBRPreference - DeterministicCBRPreference),
    Alignment = 1 -
      AbsoluteDistance
  )

# 3.6) Overall Deterministic-baseline alignment
normative_deterministic_alignment <- moral_deterministic_condition_alignment %>%
  group_by(Agent) %>%
  summarise(
    N_Scenarios = sum(!is.na(CBRPreference)),
    AlignmentPercent = 100 *
      mean(Alignment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Generation = match(Agent, model_order),
    Module = "Normative reasoning",
    Baseline = "Deterministic baseline"
  ) %>%
  filter(!is.na(Generation)) %>%
  select(Agent, Generation, Module, Baseline, AlignmentPercent, N_Scenarios) %>%
  arrange(Generation)

# 3.7) Combine dual-reference normative alignment
normative_overall_alignment <- bind_rows(normative_human_alignment, normative_deterministic_alignment) %>%
  mutate(Baseline = factor(Baseline, levels = c("Human baseline", "Deterministic baseline"))) %>%
  arrange(Baseline, Generation)

# 3.8) PRIMARY: Human-baseline developmental trajectory
normative_human_trend <- normative_overall_alignment %>%
  filter(Baseline == "Human baseline")

normative_human_test <- cor.test(
  normative_human_trend$Generation,
  normative_human_trend$AlignmentPercent,
  method = "spearman",
  exact = FALSE
)

normative_human_trend_result <- data.frame(
  Baseline = "Human baseline",
  N_Models = sum(complete.cases(normative_human_trend$Generation, normative_human_trend$AlignmentPercent)),
  Spearman_rho = unname(normative_human_test$estimate),
  p_value = normative_human_test$p.value,
  Significant = normative_human_test$p.value <= 0.050,
  Direction = case_when(
      unname(normative_human_test$estimate) > 0 ~ "Increasing alignment",
      unname(normative_human_test$estimate) < 0 ~ "Decreasing alignment",
      TRUE ~ "No monotonic direction"
    )
)

# 3.9) PRIMARY: Deterministic-baseline developmental trajectory
normative_deterministic_trend <- normative_overall_alignment %>%
  filter(Baseline == "Deterministic baseline")

normative_deterministic_test <- cor.test(
  normative_deterministic_trend$Generation,
  normative_deterministic_trend$AlignmentPercent,
  method = "spearman",
  exact = FALSE
)

normative_deterministic_trend_result <- data.frame(
  Baseline = "Deterministic baseline",
  N_Models = sum(
      complete.cases(normative_deterministic_trend$Generation, normative_deterministic_trend$AlignmentPercent)
    ),
  Spearman_rho = unname(normative_deterministic_test$estimate),
  p_value = normative_deterministic_test$p.value,
  Significant = normative_deterministic_test$p.value <= 0.050,
  Direction = case_when(
      unname(normative_deterministic_test$estimate) > 0 ~ "Increasing alignment",
      unname(normative_deterministic_test$estimate) < 0 ~ "Decreasing alignment",
      TRUE ~ "No monotonic direction"
    )
)

# 3.10) Summary of primary developmental trajectories
normative_primary_trends <- bind_rows(normative_human_trend_result, normative_deterministic_trend_result)

# 4) Direct Human-vs-Deterministic developmental comparison
# BaselineGap =
#   Deterministic alignment - Human alignment
# Positive values indicate greater alignment with the deterministic reference.
# A positive developmental rho indicates increasing relative
# deterministic alignment across model generations.

# 4.1) Overall normative baseline gap
normative_overall_gap <- normative_overall_alignment %>%
  select(Agent, Generation, Baseline, AlignmentPercent) %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`) %>%
  arrange(Generation)

# 4.2) Overall normative baseline-gap trend
# Single direct differential analysis.
normative_overall_gap_test <- cor.test(normative_overall_gap$Generation, normative_overall_gap$BaselineGap, method = "spearman", exact = FALSE)

normative_overall_gap_result <- data.frame(
  Analysis = "Deterministic minus Human baseline alignment",
  N_Models = sum(complete.cases(normative_overall_gap$Generation, normative_overall_gap$BaselineGap)),
  Spearman_rho = unname(normative_overall_gap_test$estimate),
  p_value = normative_overall_gap_test$p.value,
  Significant = normative_overall_gap_test$p.value <= 0.050,
  Direction = case_when(
      unname(normative_overall_gap_test$estimate) > 0 ~ "Increasing relative deterministic alignment",
      unname(normative_overall_gap_test$estimate) < 0 ~ "Increasing relative human alignment",
      TRUE ~ "No monotonic direction"
    )
)


# 5) SECONDARY ANALYSIS: Context-sensitive normative regulation
# Context effects = reformulated minus original utilitarian response proportions.
# Match effects to human effects or zero (context invariance), then average across dilemmas.

# 5.1) Scenario-specific framing effects
moral_context_wide <- moral_summary %>%
  select(Agent, ScenarioName, Framing, CBRPreference) %>%
  pivot_wider(names_from = Framing, values_from = CBRPreference) %>%
  mutate(
    OmissionEffect = `Omission framing` -
      `Original framing`,
    YesNoEffect = `Yes/No framing` -
      `Original framing`
  )

# 5.2) Long-format scenario-specific contextual effects
moral_context_effects <- moral_context_wide %>%
  select(Agent, ScenarioName, OmissionEffect, YesNoEffect) %>%
  pivot_longer(cols = c(OmissionEffect, YesNoEffect), names_to = "EffectType", values_to = "ContextEffect") %>%
  mutate(
    EffectType = factor(EffectType, levels = c("OmissionEffect", "YesNoEffect"), labels = c("Omission effect", "Yes/No effect")),
    Generation = match(Agent, model_order)
  ) %>%
  arrange(EffectType, Agent, ScenarioName)

# 5.4) Human scenario-specific contextual reference
human_context_reference <- moral_context_effects %>%
  filter(Agent == "human") %>%
  group_by(ScenarioName, EffectType) %>%
  summarise(HumanContextEffect = mean(ContextEffect, na.rm = TRUE), .groups = "drop")

# 5.5) Human scenario-level contextual-sensitivity alignment
# Context alignment = 100 * (1 - mean absolute difference from human scenario-specific effects).
context_human_condition_alignment <- moral_context_effects %>%
  filter(Agent != "human") %>%
  left_join(human_context_reference, by = c("ScenarioName", "EffectType")) %>%
  mutate(
    SignedDeviation = ContextEffect -
      HumanContextEffect,
    AbsoluteDistance = abs(SignedDeviation),
    Alignment = 1 -
      AbsoluteDistance
  )

# 5.6) Aggregate Human contextual alignment
context_human_alignment <- context_human_condition_alignment %>%
  group_by(Agent, EffectType) %>%
  summarise(
    N_Scenarios = sum(complete.cases(ContextEffect, HumanContextEffect)),
    AlignmentPercent = 100 *
      mean(Alignment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Generation = match(Agent, model_order), Baseline = "Human baseline") %>%
  filter(!is.na(Generation)) %>%
  select(Agent, Generation, EffectType, Baseline, AlignmentPercent, N_Scenarios) %>%
  arrange(EffectType, Generation)

# 5.7) Deterministic scenario-level contextual-sensitivity alignment
# Deterministic reference:
# ContextEffect = 0
# Alignment_D,s =
#   1 - |ContextEffect(LLM,s) - 0|
# 100% indicates complete framing invariance for every scenario.
context_deterministic_condition_alignment <- moral_context_effects %>%
  filter(Agent != "human") %>%
  mutate(
    DeterministicContextEffect = 0,
    AbsoluteDistance = abs(ContextEffect - DeterministicContextEffect),
    Alignment = 1 -
      AbsoluteDistance
  )

# 5.8) Aggregate Deterministic contextual alignment
context_deterministic_alignment <- context_deterministic_condition_alignment %>%
  group_by(Agent, EffectType) %>%
  summarise(
    N_Scenarios = sum(!is.na(ContextEffect)),
    AlignmentPercent = 100 *
      mean(Alignment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Generation = match(Agent, model_order), Baseline = "Deterministic baseline") %>%
  filter(!is.na(Generation)) %>%
  select(Agent, Generation, EffectType, Baseline, AlignmentPercent, N_Scenarios) %>%
  arrange(EffectType, Generation)

# 5.9) Combine dual-reference contextual-sensitivity alignment
normative_context_alignment <- bind_rows(context_human_alignment, context_deterministic_alignment) %>%
  mutate(
    EffectType = factor(EffectType, levels = c("Omission effect", "Yes/No effect")),
    Baseline = factor(Baseline, levels = c("Human baseline", "Deterministic baseline"))
  ) %>%
  arrange(Baseline, EffectType, Generation)

# 5.10) Overall contextual-regulation alignment
# Omission and Yes/No effects are equally weighted.
normative_context_overall <- normative_context_alignment %>%
  group_by(Agent, Generation, Baseline) %>%
  summarise(
    N_Effects = sum(!is.na(AlignmentPercent)),
    AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Module = "Contextual normative regulation") %>%
  select(Agent, Generation, Module, Baseline, AlignmentPercent, N_Effects) %>%
  arrange(Baseline, Generation)

# 5.11) Overall contextual-regulation developmental trends
# Secondary summary analyses.
# Human and Deterministic baselines represent two theoretically
# distinct contextual-regulation references and are tested separately.
context_overall_human <- normative_context_overall %>%
  filter(Baseline == "Human baseline")

context_overall_deterministic <- normative_context_overall %>%
  filter(Baseline == "Deterministic baseline")

context_overall_human_test <- cor.test(
  context_overall_human$Generation,
  context_overall_human$AlignmentPercent,
  method = "spearman",
  exact = FALSE
)

context_overall_deterministic_test <- cor.test(
  context_overall_deterministic$Generation,
  context_overall_deterministic$AlignmentPercent,
  method = "spearman",
  exact = FALSE
)

normative_context_overall_trends <- bind_rows(
  data.frame(
    Baseline = "Human baseline",
    N_Models = sum(complete.cases(context_overall_human$Generation, context_overall_human$AlignmentPercent)),
    Spearman_rho = unname(context_overall_human_test$estimate),
    p_value = context_overall_human_test$p.value,
    Significant = context_overall_human_test$p.value <= 0.050,
    Direction = case_when(
        unname(context_overall_human_test$estimate) > 0 ~ "Increasing alignment",
        unname(context_overall_human_test$estimate) < 0 ~ "Decreasing alignment",
        TRUE ~ "No monotonic direction"
      )
  ),
  data.frame(
    Baseline = "Deterministic baseline",
    N_Models = sum(
        complete.cases(context_overall_deterministic$Generation, context_overall_deterministic$AlignmentPercent)
      ),
    Spearman_rho = unname(context_overall_deterministic_test$estimate),
    p_value = context_overall_deterministic_test$p.value,
    Significant = context_overall_deterministic_test$p.value <= 0.050,
    Direction = case_when(
        unname(context_overall_deterministic_test$estimate) > 0 ~ "Increasing alignment",
        unname(context_overall_deterministic_test$estimate) < 0 ~ "Decreasing alignment",
        TRUE ~ "No monotonic direction"
      )
  )
)

# 5.12) Effect-specific contextual-regulation trends
# Omission and Yes/No are decomposition analyses.
# Within each baseline:
#   Omission + Yes/No = 2 related tests.
# Holm correction is applied separately within each baseline.
normative_context_trends_raw <- normative_context_alignment %>%
  group_by(Baseline, EffectType) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, AlignmentPercent)),
    Spearman_rho = cor(Generation, AlignmentPercent, method = "spearman", use = "complete.obs"),
    p_raw = cor.test(Generation, AlignmentPercent, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  )

normative_context_trends <- normative_context_trends_raw %>%
  group_by(Baseline) %>%
  mutate(
    p_adjusted = p.adjust(p_raw, method = "holm"),
    Correction = "Holm within baseline (2 context-specific tests)",
    Significant_raw = p_raw <= 0.050,
    Significant_adjusted = p_adjusted <= 0.050,
    Direction = case_when(
        Spearman_rho > 0 ~ "Increasing alignment",
        Spearman_rho < 0 ~ "Decreasing alignment",
        TRUE ~ "No monotonic direction"
      )
  ) %>%
  ungroup()


# 6) Direct contextual Human-vs-Deterministic comparison

# 6.1) Overall contextual-regulation baseline gap
normative_context_overall_gap <- normative_context_overall %>%
  select(Agent, Generation, Baseline, AlignmentPercent) %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`) %>%
  arrange(Generation)

# 6.2) Overall contextual-regulation gap trend
context_overall_gap_test <- cor.test(
  normative_context_overall_gap$Generation,
  normative_context_overall_gap$BaselineGap,
  method = "spearman",
  exact = FALSE
)

normative_context_overall_gap_result <- data.frame(
  Analysis = "Contextual regulation: Deterministic minus Human baseline alignment",
  N_Models =
    sum(complete.cases(normative_context_overall_gap$Generation, normative_context_overall_gap$BaselineGap)),
  Spearman_rho = unname(context_overall_gap_test$estimate),
  p_value = context_overall_gap_test$p.value,
  Significant = context_overall_gap_test$p.value <= 0.050,
  Direction = case_when(
      unname(context_overall_gap_test$estimate) > 0 ~ "Increasing relative deterministic alignment",
      unname(context_overall_gap_test$estimate) < 0 ~ "Increasing relative human alignment",
      TRUE ~ "No monotonic direction"
    )
)

# 6.3) Effect-specific contextual baseline gaps
normative_context_gap <- normative_context_alignment %>%
  select(Agent, Generation, EffectType, Baseline, AlignmentPercent) %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`) %>%
  arrange(EffectType, Generation)

# 6.4) Effect-specific contextual baseline-gap trends
# Omission + Yes/No = 2 related differential tests.
# Holm correction is applied across the two gap tests.
normative_context_gap_trends <- normative_context_gap %>%
  group_by(EffectType) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, BaselineGap)),
    Spearman_rho = cor(Generation, BaselineGap, method = "spearman", use = "complete.obs"),
    p_raw = cor.test(Generation, BaselineGap, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    p_adjusted = p.adjust(p_raw, method = "holm"),
    Correction = "Holm (2 context-specific baseline-gap tests)",
    Significant_raw = p_raw <= 0.050,
    Significant_adjusted = p_adjusted <= 0.050,
    Direction = case_when(
        Spearman_rho > 0 ~ "Increasing relative deterministic alignment",
        Spearman_rho < 0 ~ "Increasing relative human alignment",
        TRUE ~ "No monotonic direction"
      )
  )


# 9.1) Bootstrap Human-baseline Normative-reasoning alignment
set.seed(2632)
n_bootstrap <- 1000

bootstrap_moral_once <- function(bootstrap_id){
  bootstrap_moral_summary <- moral_long %>%
    filter(Agent != "human", !is.na(ResponseNumeric), !is.na(Framing)) %>%
    group_by(Agent, ScenarioName, Framing) %>%
    summarise(
      N_Response = n(), CBRPreference = mean(sample(ResponseNumeric, size = n(), replace = TRUE)), .groups = "drop"
    )

  bootstrap_scenario_alignment <- bootstrap_moral_summary %>%
    filter(Framing == "Original framing") %>%
    left_join(human_moral_original_profile, by = "ScenarioName") %>%
    mutate(
      BootstrapID = bootstrap_id, Generation = match(Agent, model_order), AlignmentPercent = 100 *
        (1 - abs(CBRPreference - HumanCBRPreference))
    ) %>%
    filter(!is.na(Generation)) %>%
    select(
      BootstrapID, Agent, Generation, ScenarioName, Framing, CBRPreference, HumanCBRPreference, AlignmentPercent
    )

  bootstrap_overall_alignment <- bootstrap_scenario_alignment %>%
    group_by(BootstrapID, Agent, Generation) %>%
    summarise(
      N_Scenarios = sum(complete.cases(CBRPreference, HumanCBRPreference)), AlignmentPercent =
        mean(AlignmentPercent, na.rm = TRUE), .groups = "drop"
    ) %>%
    mutate(Module = "Normative reasoning", Baseline = "Human baseline") %>%
    select(BootstrapID, Agent, Generation, Module, Baseline, AlignmentPercent, N_Scenarios)

  list(
    summary = bootstrap_moral_summary %>%
      mutate(BootstrapID = bootstrap_id, Generation = match(Agent, model_order)) %>%
        filter(!is.na(Generation)),
    scenario = bootstrap_scenario_alignment, overall = bootstrap_overall_alignment
  )
}

moral_bootstrap_results <- map(seq_len(n_bootstrap), bootstrap_moral_once)

bootstrap_moral_summary <- map_dfr(moral_bootstrap_results, "summary")

# 9.2) Bootstrap confidence intervals for monotonic trends
bootstrap_original_dual_alignment <- bind_rows(
  bootstrap_moral_summary %>%
    filter(Framing == "Original framing") %>%
    left_join(human_moral_original_profile, by = "ScenarioName") %>%
      transmute(
      BootstrapID, Agent, Generation, ScenarioName, Baseline = "Human baseline", AlignmentPercent = 100 *
        (1 - abs(CBRPreference - HumanCBRPreference))
    ), bootstrap_moral_summary %>%
      filter(Framing == "Original framing") %>%
      transmute(
      BootstrapID, Agent, Generation, ScenarioName, Baseline = "Deterministic baseline", AlignmentPercent = 100 *
        (1 - abs(CBRPreference - 1))
    )
)

bootstrap_normative_dual_alignment <- bootstrap_original_dual_alignment %>%
  group_by(BootstrapID, Agent, Generation, Baseline) %>%
  summarise(AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE), .groups = "drop")

bootstrap_normative_gap <- bootstrap_normative_dual_alignment %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`)

bootstrap_context_effects <- bootstrap_moral_summary %>%
  select(BootstrapID, Agent, Generation, ScenarioName, Framing, CBRPreference) %>%
  pivot_wider(names_from = Framing, values_from = CBRPreference) %>%
    mutate(
    `Omission effect` = `Omission framing` -
      `Original framing`, `Yes/No effect` = `Yes/No framing` -
      `Original framing`
  ) %>%
    select(BootstrapID, Agent, Generation, ScenarioName, `Omission effect`, `Yes/No effect`) %>%
  pivot_longer(cols = c(`Omission effect`, `Yes/No effect`), names_to = "EffectType", values_to = "ContextEffect")

bootstrap_context_dual_alignment <- bind_rows(
  bootstrap_context_effects %>%
    left_join(
      human_context_reference %>%
        mutate(EffectType = as.character(EffectType)),
      by = c("ScenarioName", "EffectType")
    ) %>%
      transmute(
      BootstrapID, Agent, Generation, ScenarioName, EffectType, Baseline = "Human baseline", AlignmentPercent =
        100 *
        (1 - abs(ContextEffect - HumanContextEffect))
    ), bootstrap_context_effects %>%
      transmute(
      BootstrapID, Agent, Generation, ScenarioName, EffectType, Baseline = "Deterministic baseline",
      AlignmentPercent = 100 *
        (1 - abs(ContextEffect))
    )
)

bootstrap_context_alignment <- bootstrap_context_dual_alignment %>%
  group_by(BootstrapID, Agent, Generation, Baseline, EffectType) %>%
  summarise(AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE), .groups = "drop")

bootstrap_context_overall <- bootstrap_context_alignment %>%
  group_by(BootstrapID, Agent, Generation, Baseline) %>%
  summarise(AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE), .groups = "drop")

bootstrap_context_gap <- bootstrap_context_alignment %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`)

bootstrap_context_overall_gap <- bootstrap_context_overall %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`)

bootstrap_trend_values <- bind_rows(
  bootstrap_normative_dual_alignment %>%
    transmute(BootstrapID, Generation, Analysis = "Original-framing alignment", Baseline, EffectType = NA_character_, Value = AlignmentPercent),
  bootstrap_normative_gap %>%
    transmute(BootstrapID, Generation, Analysis = "Original-framing baseline gap", Baseline = NA_character_, EffectType = NA_character_, Value = BaselineGap),
  bootstrap_context_overall %>%
    transmute(BootstrapID, Generation, Analysis = "Overall contextual alignment", Baseline, EffectType = NA_character_, Value = AlignmentPercent),
  bootstrap_context_alignment %>%
    transmute(BootstrapID, Generation, Analysis = "Effect-specific contextual alignment", Baseline, EffectType, Value = AlignmentPercent),
  bootstrap_context_overall_gap %>%
    transmute(BootstrapID, Generation, Analysis = "Overall contextual baseline gap", Baseline = NA_character_, EffectType = NA_character_, Value = BaselineGap),
  bootstrap_context_gap %>%
    transmute(BootstrapID, Generation, Analysis = "Effect-specific contextual baseline gap", Baseline = NA_character_, EffectType, Value = BaselineGap)
)

bootstrap_trend_rho <- bootstrap_trend_values %>%
  group_by(BootstrapID, Analysis, Baseline, EffectType) %>%
  summarise(Spearman_rho = cor(Generation, Value, method = "spearman", use = "complete.obs"), .groups = "drop")

bootstrap_trend_ci <- bootstrap_trend_rho %>%
  filter(is.finite(Spearman_rho)) %>%
  group_by(Analysis, Baseline, EffectType) %>%
    summarise(
    CI_Lower = quantile(Spearman_rho, probs = 0.025, na.rm = TRUE, names = FALSE), CI_Upper =
      quantile(Spearman_rho, probs = 0.975, na.rm = TRUE, names = FALSE), CI_Method = "Percentile bootstrap",
    Confidence_Level = 0.95, N_Boot_Valid = n(), .groups = "drop"
  )

normative_primary_trends <- normative_primary_trends %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Original-framing alignment") %>%
      select(Baseline, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid), by = "Baseline"
  )

normative_overall_gap_result <- bind_cols(
  normative_overall_gap_result, bootstrap_trend_ci %>%
    filter(Analysis == "Original-framing baseline gap") %>%
    select(CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid)
)

normative_context_overall_trends <- normative_context_overall_trends %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Overall contextual alignment") %>%
      select(Baseline, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid), by = "Baseline"
  )

normative_context_trends <- normative_context_trends %>%
  mutate(EffectType = as.character(EffectType)) %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Effect-specific contextual alignment") %>%
      select(Baseline, EffectType, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid),
    by = c("Baseline", "EffectType")
  )

normative_context_overall_gap_result <- bind_cols(
  normative_context_overall_gap_result, bootstrap_trend_ci %>%
    filter(Analysis == "Overall contextual baseline gap") %>%
    select(CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid)
)

normative_context_gap_trends <- normative_context_gap_trends %>%
  mutate(EffectType = as.character(EffectType)) %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Effect-specific contextual baseline gap") %>%
      select(EffectType, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid), by = "EffectType"
  )

normative_primary_trends <- add_bayesian_trend_evidence(
  trend_table = normative_primary_trends, source_data = normative_overall_alignment, by_cols = "Baseline",
  y_col = "AlignmentPercent"
)

normative_overall_gap_result <- add_bayesian_trend_evidence(trend_table = normative_overall_gap_result, source_data = normative_overall_gap, y_col = "BaselineGap")

normative_context_overall_trends <- add_bayesian_trend_evidence(
  trend_table = normative_context_overall_trends, source_data = normative_context_overall, by_cols = "Baseline",
  y_col = "AlignmentPercent"
)

normative_context_trends <- add_bayesian_trend_evidence(
  trend_table = normative_context_trends, source_data = normative_context_alignment,
  by_cols = c("Baseline", "EffectType"), y_col = "AlignmentPercent"
)

normative_context_overall_gap_result <- add_bayesian_trend_evidence(
  trend_table = normative_context_overall_gap_result, source_data = normative_context_overall_gap,
  y_col = "BaselineGap"
)

normative_context_gap_trends <- add_bayesian_trend_evidence(
  trend_table = normative_context_gap_trends, source_data = normative_context_gap, by_cols = "EffectType",
  y_col = "BaselineGap"
)


# 10) Export analysis results
bootstrap_normative_alignment <- map_dfr(moral_bootstrap_results, "overall")

write.xlsx(
  list(
    Bootstrap_alignment = bootstrap_normative_alignment,
    Alignment_Original = normative_overall_alignment,
    Alignment_Original_Trend = normative_primary_trends,
    Baseline_Gap_Original = normative_overall_gap,
    Baseline_Gap_Original_Trend = normative_overall_gap_result,
    Ctx_Alignment_By_Effect = normative_context_alignment,
    Ctx_Alignment_Overall = normative_context_overall,
    Ctx_Alignment_Effect_Trend = normative_context_trends,
    Ctx_Alignment_Overall_Trend = normative_context_overall_trends,
    Ctx_Baseline_Gap_By_Effect = normative_context_gap,
    Ctx_Baseline_Gap_Overall = normative_context_overall_gap,
    Ctx_Baseline_Gap_Effect_Trend = normative_context_gap_trends,
    Ctx_Baseline_Gap_Overall_Trend = normative_context_overall_gap_result
  ),
  file = file.path(output_dir, "Results_moral.xlsx"), overwrite = TRUE
)

message("Normative reasoning dual-reference analysis completed.")
message("Results saved to: ", file.path(output_dir, "Results_moral.xlsx"))


# 12) Visualization: Normative reasoning development
# Original reasoning and contextual regulation in two panels.
# Human: blue circles; deterministic reference: green triangles.
# Add fitted lines only for significant trends.

# 12.1) Figure settings
blue_color <- "#3b85bd"

green_color <- "#9db648"

# More visible significant fitted trends
blue_trend_color <- scales::alpha(blue_color, 0.80)

green_trend_color <- scales::alpha(green_color, 0.80)

reference_color <- "grey70"

# geom_text() uses mm-based sizing.
# Convert to approximately 6 pt.
figure_text_size <- 6 / ggplot2::.pt

# 12.2) Display-label settings
baseline_display_levels <- c("Human baseline", "Extreme utilitarianism")

make_display_baseline <- function(x){
  case_when(
    as.character(x) == "Human baseline" ~ "Human baseline",
    as.character(x) == "Deterministic baseline" ~ "Extreme utilitarianism",
    TRUE ~ as.character(x)
  )
}

# 12.3) Prepare overall normative-reasoning plot data
normative_overall_plot <- normative_overall_alignment %>%
  mutate(
    DisplayBaseline = make_display_baseline(Baseline),
    DisplayBaseline = factor(DisplayBaseline, levels = baseline_display_levels)
  ) %>%
  arrange(DisplayBaseline, Generation)

# 12.4) Extract primary normative-reasoning significance
normative_human_significant <- normative_primary_trends %>%
  filter(Baseline == "Human baseline") %>%
  pull(Significant)

normative_extreme_significant <- normative_primary_trends %>%
  filter(Baseline == "Deterministic baseline") %>%
  pull(Significant)

# 12.6) Panel a: Normative reasoning
pan_normative_overall <- ggplot(
  normative_overall_plot,
  aes(
    x = Generation, y = AlignmentPercent, color = DisplayBaseline, shape = DisplayBaseline, group = DisplayBaseline
  )
) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.30, color = reference_color)

# 12.7) Significant Human-baseline trend
if(length(normative_human_significant) == 1 && isTRUE(normative_human_significant)){
  pan_normative_overall <- pan_normative_overall +
    geom_smooth(
      data = normative_overall_plot %>%
        filter(DisplayBaseline == "Human baseline"),
      aes(x = Generation, y = AlignmentPercent),
      inherit.aes = FALSE,
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.90,
      color = blue_trend_color,
      show.legend = FALSE
    )

}

# 12.8) Significant Extreme-utilitarianism trend
if(length(normative_extreme_significant) == 1 && isTRUE(normative_extreme_significant)){
  pan_normative_overall <- pan_normative_overall +
    geom_smooth(
      data = normative_overall_plot %>%
        filter(DisplayBaseline == "Extreme utilitarianism"),
      aes(x = Generation, y = AlignmentPercent),
      inherit.aes = FALSE,
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.90,
      color = green_trend_color,
      show.legend = FALSE
    )

}

# 12.9) Complete Panel a
pan_normative_overall <- pan_normative_overall +
  geom_line(linewidth = 0.48) +
  geom_point(size = 1.75, stroke = 0.70) +
  geom_text(
    data = normative_overall_plot %>%
      filter(DisplayBaseline == "Human baseline"),
    aes(x = Generation, y = AlignmentPercent, label = sprintf("%.1f%%", AlignmentPercent)),
    inherit.aes = FALSE,
    color = blue_color,
    size = figure_text_size,
    vjust = -0.95,
    show.legend = FALSE
  ) +
  geom_text(
    data = normative_overall_plot %>%
      filter(DisplayBaseline == "Extreme utilitarianism"),
    aes(x = Generation, y = AlignmentPercent, label = sprintf("%.1f%%", AlignmentPercent)),
    inherit.aes = FALSE,
    color = green_color,
    size = figure_text_size,
    vjust = 1.35,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Human baseline" = blue_color, "Extreme utilitarianism" = green_color)) +
  scale_shape_manual(values = c("Human baseline" = 1, "Extreme utilitarianism" = 2)) +
  scale_x_continuous(limits = c(1, 9), breaks = 1:9, labels = model_labels, expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(breaks = seq(0, 100, by = 25), labels = scales::label_number(suffix = "%")) +
  coord_cartesian(ylim = c(0, 105), clip = "off") +
  labs(
    tag = "a",
    title = "Normative reasoning",
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
    axis.title = element_text(color = "black", size = 6),
    legend.position = c(0.50, 0.12),
    legend.direction = "vertical",
    legend.justification = c(0.5, 0),
    legend.background = element_blank(),
    legend.key.width = grid::unit(0.65, "lines"),
    legend.key.height = grid::unit(0.65, "lines"),
    legend.spacing.y = grid::unit(0, "pt"),
    legend.text = element_text(size = 6),
    plot.title.position = "panel",
    plot.title = element_text(size = 6, face = "bold", hjust = 0.5),
    plot.tag = element_text(face = "bold", size = 6),
    plot.tag.position = c(0, 1),
    plot.margin = margin(4, 5, 4, 4)
  )

pan_normative_overall


# 13) Prepare framing-regulation plot data
normative_context_overall_plot <- normative_context_overall %>%
  mutate(
    DisplayBaseline = case_when(
      as.character(Baseline) == "Human baseline" ~ "Human baseline",
      as.character(Baseline) == "Deterministic baseline" ~ "Complete framing invariance",
      TRUE ~ as.character(Baseline)
    ),
    DisplayBaseline = factor(DisplayBaseline, levels = c("Human baseline", "Complete framing invariance"))
  ) %>%
  arrange(DisplayBaseline, Generation)

# 13.1) Extract overall framing-regulation significance
context_overall_human_significant <- normative_context_overall_trends %>%
  filter(Baseline == "Human baseline") %>%
  pull(Significant)

context_overall_extreme_significant <- normative_context_overall_trends %>%
  filter(Baseline == "Deterministic baseline") %>%
  pull(Significant)

# 14) Panel b: Normative reasoning (framing regulation)
pan_context_overall <- ggplot(
  normative_context_overall_plot,
  aes(
    x = Generation, y = AlignmentPercent, color = DisplayBaseline, shape = DisplayBaseline, group = DisplayBaseline
  )
) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.30, color = reference_color)

# 14.1) Significant Human-baseline trend
if(length(context_overall_human_significant) == 1 && isTRUE(context_overall_human_significant)){
  pan_context_overall <- pan_context_overall +
    geom_smooth(
      data = normative_context_overall_plot %>%
        filter(DisplayBaseline == "Human baseline"),
      aes(x = Generation, y = AlignmentPercent),
      inherit.aes = FALSE,
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.90,
      color = blue_trend_color,
      show.legend = FALSE
    )

}

# 14.2) Significant Extreme-utilitarianism trend
if(length(context_overall_extreme_significant) == 1 && isTRUE(context_overall_extreme_significant)){
  pan_context_overall <- pan_context_overall +
    geom_smooth(
      data = normative_context_overall_plot %>%
        filter(DisplayBaseline == "Complete framing invariance"),
      aes(x = Generation, y = AlignmentPercent),
      inherit.aes = FALSE,
      method = "lm",
      formula = y ~ x,
      se = FALSE,
      linewidth = 0.90,
      color = green_trend_color,
      show.legend = FALSE
    )

}

# 14.3) Complete Panel b
pan_context_overall <- pan_context_overall +
  geom_line(linewidth = 0.48) +
  geom_point(size = 1.75, stroke = 0.70) +
  geom_text(
    data = normative_context_overall_plot %>%
      filter(DisplayBaseline == "Human baseline"),
    aes(x = Generation, y = AlignmentPercent, label = sprintf("%.1f%%", AlignmentPercent)),
    inherit.aes = FALSE,
    color = blue_color,
    size = figure_text_size,
    vjust = -0.95,
    show.legend = FALSE
  ) +
  geom_text(
    data = normative_context_overall_plot %>%
      filter(DisplayBaseline == "Complete framing invariance"),
    aes(x = Generation, y = AlignmentPercent, label = sprintf("%.1f%%", AlignmentPercent)),
    inherit.aes = FALSE,
    color = green_color,
    size = figure_text_size,
    vjust = 1.35,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Human baseline" = blue_color, "Complete framing invariance" = green_color)) +
  scale_shape_manual(values = c("Human baseline" = 1, "Complete framing invariance" = 2)) +
  scale_x_continuous(limits = c(1, 9), breaks = 1:9, labels = model_labels, expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(breaks = seq(0, 100, by = 25), labels = scales::label_number(suffix = "%")) +
  coord_cartesian(ylim = c(0, 105), clip = "off") +
  labs(
    tag = "b",
    title = "Normative reasoning (framing regulation)",
    x = "LLM generation",
    y = NULL,
    color = NULL,
    shape = NULL
  ) +
  guides(color = guide_legend(ncol = 1, byrow = TRUE), shape = guide_legend(ncol = 1, byrow = TRUE)) +
  theme_classic(base_size = 6) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, color = "black", size = 6),
    axis.text.y = element_text(color = "black", size = 6),
    axis.title.x = element_text(color = "black", size = 6),
    axis.title.y = element_blank(),
    legend.position = c(0.55, 0.12),
    legend.direction = "vertical",
    legend.justification = c(0.5, 0),
    legend.background = element_blank(),
    legend.key.width = grid::unit(0.65, "lines"),
    legend.key.height = grid::unit(0.65, "lines"),
    legend.spacing.y = grid::unit(0, "pt"),
    legend.text = element_text(size = 6),
    plot.title.position = "panel",
    plot.title = element_text(size = 6, face = "bold", hjust = 0.5),
    plot.tag = element_text(face = "bold", size = 6),
    plot.tag.position = c(0, 1),
    plot.margin = margin(4, 5, 4, 4)
  )

pan_context_overall


# 15) Final two-panel layout
Pic_normative_complete <-
  pan_normative_overall +
  pan_context_overall +
  plot_layout(ncol = 2, widths = c(1, 1))


# 16) Export final combined normative-reasoning figure
ggsave(
  file.path(output_dir, "viso_moral_complete.pdf"),
  plot = Pic_normative_complete,
  width = 6,
  height = 3,
  units = "in",
  device = grDevices::pdf
)


# 17) Visualization completion messages
message("Combined normative-reasoning visualization completed.")
message("Figure saved to: ", file.path(output_dir, "viso_moral_complete.pdf"))
