# LLM evolutionary trajectory
# Adaptive social cue sensitivity: group size cue & kinship cue
# Requires: "rawdata_socialcue.xlsx"
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

# 0.3) Helper function for slope estimation
# Safely estimate a linear slope.
# Returns NA if there are insufficient valid observations
# or fewer than two distinct x values.
safe_lm_slope <- function(x, y){
  valid <- complete.cases(x, y)

  x_valid <- x[valid]

  y_valid <- y[valid]

  if(length(x_valid) < 2 || length(unique(x_valid)) < 2){
    return(NA_real_)
  }
  slope <- coef(lm(y_valid ~ x_valid))[2]

  return(unname(slope))
}


# 1) Data input
# Function for importing multi-sheet Excel files
# Each sheet represents one agent/model
# Non-data sheets (i.e., Note) are excluded
read_agent_data <- function(file){
  # Obtain sheet names
  sheets <- excel_sheets(file)

  # Remove non-data sheets
  sheets <- sheets[
    !tolower(sheets) %in% c("note")
  ]

  # Read all agent sheets
  data <- map_dfr(
    sheets,
    function(sheet_name){
      df <- read_excel(file, sheet = sheet_name)

      # Add agent/model identifier
      df <- df %>%
        mutate(Agent = sheet_name)

      return(df)
    }
  )

  return(data)
}

# Import raw dataset
socialcue <- read_agent_data("rawdata_socialcue.xlsx")


# 2) Condition-level behavioral profiles
# Convert wide format into long format
socialcue_long <- socialcue %>%
  pivot_longer(cols = -c(No., Agent), names_to = "Condition", values_to = "Response")

# Calculate response probability for each condition
# A = risk-averse response
socialcue_summary <- socialcue_long %>%
  group_by(Agent, Condition) %>%
  summarise(
    N_Response = sum(!is.na(Response)),
    P_A = ifelse(N_Response > 0, mean(Response == "A", na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# 2.1) Group size conditions
group_size_data <- socialcue_summary %>%
  filter(str_detect(Condition, "life")) %>%
  mutate(
    # Framing information
    Framing = str_sub(Condition, 1, 1),
    # Group size
    GroupSize = case_when(
      str_detect(Condition, "6000") ~ 6000,
      str_detect(Condition, "600") ~ 600,
      str_detect(Condition, "60") ~ 60,
      str_detect(Condition, "6") ~ 6,
      TRUE ~ NA_real_
    ),
    # Log transformation
    logGroupSize = log10(GroupSize)
  )

# 2.2) Kinship conditions
kinship_data <- socialcue_summary %>%
  filter(str_detect(Condition, "kin")) %>%
  mutate(
    # Framing information
    Framing = str_sub(Condition, 1, 1),
    # Number of kin members
    KinNumber = case_when(
      str_detect(Condition, "6kin") ~ 6,
      str_detect(Condition, "3kin") ~ 3,
      str_detect(Condition, "2kin") ~ 2,
      str_detect(Condition, "1kin") ~ 1,
      TRUE ~ NA_real_
    ),
    # Total group size is fixed at 6
    KinProportion = KinNumber / 6
  )


# 3) MAIN ANALYSIS: Dual-reference behavioral alignment
# Alignment = 100 * (1 - mean absolute difference in condition-level P(A)).
# References: empirical humans and P(A) = 1 (risk aversion).
# Overall baselines are tested separately; decomposition tests use the stated Holm families.

# 3.1) Human reference profiles
# Group size
# Aggregate by experimental factors to ensure one human reference
# value per Framing × GroupSize combination.
human_group_profile <- group_size_data %>%
  filter(Agent == "human") %>%
  group_by(Framing, GroupSize) %>%
  summarise(HumanPA = mean(P_A, na.rm = TRUE), .groups = "drop")

# Kinship
# Aggregate by experimental factors to ensure one human reference
# value per Framing × KinNumber combination.
human_kinship_profile <- kinship_data %>%
  filter(Agent == "human") %>%
  group_by(Framing, KinNumber) %>%
  summarise(HumanPA = mean(P_A, na.rm = TRUE), .groups = "drop")

# 3.2) Group size: Human-baseline alignment
group_human_condition_alignment <- group_size_data %>%
  filter(Agent != "human") %>%
  left_join(human_group_profile, by = c("Framing", "GroupSize")) %>%
  mutate(AbsoluteDistance = abs(P_A - HumanPA), Alignment = 1 - AbsoluteDistance)

group_human_alignment <- group_human_condition_alignment %>%
  group_by(Agent) %>%
  summarise(
    N_Conditions = sum(complete.cases(P_A, HumanPA)),
    AlignmentPercent = 100 *
      mean(Alignment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Cue = "Group size", Baseline = "Human baseline", Generation = match(Agent, model_order)) %>%
  filter(!is.na(Generation)) %>%
  arrange(Generation)

# 3.3) Group size: Deterministic-baseline alignment
group_deterministic_condition_alignment <- group_size_data %>%
  filter(Agent != "human") %>%
  mutate(DeterministicPA = 1, AbsoluteDistance = abs(P_A - DeterministicPA), Alignment = 1 - AbsoluteDistance)

group_deterministic_alignment <- group_deterministic_condition_alignment %>%
  group_by(Agent) %>%
  summarise(
    N_Conditions = sum(!is.na(P_A)),
    AlignmentPercent = 100 *
      mean(Alignment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Cue = "Group size", Baseline = "Deterministic baseline", Generation = match(Agent, model_order)) %>%
  filter(!is.na(Generation)) %>%
  arrange(Generation)

# 3.4) Kinship: Human-baseline alignment
kinship_human_condition_alignment <- kinship_data %>%
  filter(Agent != "human") %>%
  left_join(human_kinship_profile, by = c("Framing", "KinNumber")) %>%
  mutate(AbsoluteDistance = abs(P_A - HumanPA), Alignment = 1 - AbsoluteDistance)

kinship_human_alignment <- kinship_human_condition_alignment %>%
  group_by(Agent) %>%
  summarise(
    N_Conditions = sum(complete.cases(P_A, HumanPA)),
    AlignmentPercent = 100 *
      mean(Alignment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Cue = "Kinship", Baseline = "Human baseline", Generation = match(Agent, model_order)) %>%
  filter(!is.na(Generation)) %>%
  arrange(Generation)

# 3.5) Kinship: Deterministic-baseline alignment
kinship_deterministic_condition_alignment <- kinship_data %>%
  filter(Agent != "human") %>%
  mutate(DeterministicPA = 1, AbsoluteDistance = abs(P_A - DeterministicPA), Alignment = 1 - AbsoluteDistance)

kinship_deterministic_alignment <- kinship_deterministic_condition_alignment %>%
  group_by(Agent) %>%
  summarise(
    N_Conditions = sum(!is.na(P_A)),
    AlignmentPercent = 100 *
      mean(Alignment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Cue = "Kinship", Baseline = "Deterministic baseline", Generation = match(Agent, model_order)) %>%
  filter(!is.na(Generation)) %>%
  arrange(Generation)

# 3.6) Combine cue-specific alignment
adaptive_cue_alignment <- bind_rows(
  group_human_alignment,
  group_deterministic_alignment,
  kinship_human_alignment,
  kinship_deterministic_alignment
) %>%
  mutate(
    Cue = factor(Cue, levels = c("Group size", "Kinship")),
    Baseline = factor(Baseline, levels = c("Human baseline", "Deterministic baseline"))
  ) %>%
  arrange(Baseline, Cue, Generation)

# 3.7) Overall Adaptive cues alignment
# Group size and Kinship are equally weighted.
adaptive_overall_alignment <- adaptive_cue_alignment %>%
  group_by(Agent, Generation, Baseline) %>%
  summarise(
    N_Cues = sum(!is.na(AlignmentPercent)),
    AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Cue = "Adaptive cues") %>%
  select(Agent, Generation, Cue, AlignmentPercent, N_Cues, Baseline) %>%
  arrange(Baseline, Generation)

# 3.8) PRIMARY: Overall Human-baseline developmental trajectory
adaptive_human_trend <- adaptive_overall_alignment %>%
  filter(Baseline == "Human baseline")

adaptive_human_test <- cor.test(
  adaptive_human_trend$Generation,
  adaptive_human_trend$AlignmentPercent,
  method = "spearman",
  exact = FALSE
)

adaptive_human_trend_result <- data.frame(
  Baseline = "Human baseline",
  N_Models = sum(complete.cases(adaptive_human_trend$Generation, adaptive_human_trend$AlignmentPercent)),
  Spearman_rho = unname(adaptive_human_test$estimate),
  p_value = adaptive_human_test$p.value,
  Significant = adaptive_human_test$p.value <= 0.050,
  Direction = case_when(
      unname(adaptive_human_test$estimate) > 0 ~ "Increasing alignment",
      unname(adaptive_human_test$estimate) < 0 ~ "Decreasing alignment",
      TRUE ~ "No monotonic direction"
    )
)

# 3.9) PRIMARY: Overall Deterministic-baseline developmental trajectory
adaptive_deterministic_trend <- adaptive_overall_alignment %>%
  filter(Baseline == "Deterministic baseline")

adaptive_deterministic_test <- cor.test(
  adaptive_deterministic_trend$Generation,
  adaptive_deterministic_trend$AlignmentPercent,
  method = "spearman",
  exact = FALSE
)

adaptive_deterministic_trend_result <- data.frame(
  Baseline = "Deterministic baseline",
  N_Models =
    sum(complete.cases(adaptive_deterministic_trend$Generation, adaptive_deterministic_trend$AlignmentPercent)),
  Spearman_rho = unname(adaptive_deterministic_test$estimate),
  p_value = adaptive_deterministic_test$p.value,
  Significant = adaptive_deterministic_test$p.value <= 0.050,
  Direction = case_when(
      unname(adaptive_deterministic_test$estimate) > 0 ~ "Increasing alignment",
      unname(adaptive_deterministic_test$estimate) < 0 ~ "Decreasing alignment",
      TRUE ~ "No monotonic direction"
    )
)

# 3.10) Summary of primary developmental hypotheses
adaptive_primary_trends <- bind_rows(adaptive_human_trend_result, adaptive_deterministic_trend_result)

# 3.11) Cue-specific trends
# Group size and Kinship represent two distinct social-cue tests and correspond to
# separate hypotheses. Their developmental trajectories are therefore evaluated
# separately without multiple-comparison correction.
adaptive_cue_trends <- adaptive_cue_alignment %>%
  group_by(Baseline, Cue) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, AlignmentPercent)),
    Spearman_rho = cor(Generation, AlignmentPercent, method = "spearman", use = "complete.obs"),
    p_value = cor.test(Generation, AlignmentPercent, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    Correction = "None; Group size and Kinship are separate hypotheses",
    Significant = p_value <= 0.050,
    Direction = case_when(
        Spearman_rho > 0 ~ "Increasing alignment",
        Spearman_rho < 0 ~ "Decreasing alignment",
        TRUE ~ "No monotonic direction"
      )
  )


# 4) Direct Human-vs-Deterministic developmental comparison
# BaselineGap = deterministic alignment minus human alignment; positive values favor the deterministic baseline.

# 4.1) Overall Adaptive cues baseline gap
adaptive_overall_gap <- adaptive_overall_alignment %>%
  select(Agent, Generation, Baseline, AlignmentPercent) %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`) %>%
  arrange(Generation)

# 4.2) Overall baseline-gap trend
# Single direct differential analysis.
adaptive_overall_gap_test <- cor.test(adaptive_overall_gap$Generation, adaptive_overall_gap$BaselineGap, method = "spearman", exact = FALSE)

adaptive_overall_gap_result <- data.frame(
  Analysis = "Deterministic minus Human baseline alignment",
  N_Models = sum(complete.cases(adaptive_overall_gap$Generation, adaptive_overall_gap$BaselineGap)),
  Spearman_rho = unname(adaptive_overall_gap_test$estimate),
  p_value = adaptive_overall_gap_test$p.value,
  Significant = adaptive_overall_gap_test$p.value <= 0.050
)

# 4.3) Cue-specific baseline gaps
adaptive_cue_gap <- adaptive_cue_alignment %>%
  select(Agent, Generation, Cue, Baseline, AlignmentPercent) %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`) %>%
  arrange(Cue, Generation)

# 4.4) Cue-specific baseline-gap trends
# Group size and Kinship correspond to separate hypotheses.
# Their baseline-gap trajectories are therefore evaluated without
# multiple-comparison correction.
adaptive_cue_gap_trends <- adaptive_cue_gap %>%
  group_by(Cue) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, BaselineGap)),
    Spearman_rho = cor(Generation, BaselineGap, method = "spearman", use = "complete.obs"),
    p_value = cor.test(Generation, BaselineGap, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    Correction = "None; Group size and Kinship are separate hypotheses",
    Significant = p_value <= 0.050,
    Direction = case_when(
        Spearman_rho > 0 ~ "Increasing relative deterministic alignment",
        Spearman_rho < 0 ~ "Increasing relative human alignment",
        TRUE ~ "No monotonic direction"
      )
  )


# 5) Framing-specific dual-reference alignment

# 5.1) Group size: Human baseline by framing
group_human_alignment_framing <- group_human_condition_alignment %>%
  group_by(Agent, Framing) %>%
  summarise(
    N_Conditions = sum(complete.cases(P_A, HumanPA)),
    AlignmentPercent = 100 *
      mean(Alignment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Cue = "Group size", Baseline = "Human baseline", Generation = match(Agent, model_order)) %>%
  filter(!is.na(Generation))

# 5.2) Group size: Deterministic baseline by framing
group_deterministic_alignment_framing <- group_deterministic_condition_alignment %>%
  group_by(Agent, Framing) %>%
  summarise(
    N_Conditions = sum(!is.na(P_A)),
    AlignmentPercent = 100 *
      mean(Alignment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Cue = "Group size", Baseline = "Deterministic baseline", Generation = match(Agent, model_order)) %>%
  filter(!is.na(Generation))

# 5.3) Kinship: Human baseline by framing
kinship_human_alignment_framing <- kinship_human_condition_alignment %>%
  group_by(Agent, Framing) %>%
  summarise(
    N_Conditions = sum(complete.cases(P_A, HumanPA)),
    AlignmentPercent = 100 *
      mean(Alignment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Cue = "Kinship", Baseline = "Human baseline", Generation = match(Agent, model_order)) %>%
  filter(!is.na(Generation))

# 5.4) Kinship: Deterministic baseline by framing
kinship_deterministic_alignment_framing <- kinship_deterministic_condition_alignment %>%
  group_by(Agent, Framing) %>%
  summarise(
    N_Conditions = sum(!is.na(P_A)),
    AlignmentPercent = 100 *
      mean(Alignment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Cue = "Kinship", Baseline = "Deterministic baseline", Generation = match(Agent, model_order)) %>%
  filter(!is.na(Generation))

# 5.5) Combine framing-specific alignment
adaptive_framing_alignment <- bind_rows(
  group_human_alignment_framing,
  group_deterministic_alignment_framing,
  kinship_human_alignment_framing,
  kinship_deterministic_alignment_framing
) %>%
  mutate(
    Cue = factor(Cue, levels = c("Group size", "Kinship")),
    Baseline = factor(Baseline, levels = c("Human baseline", "Deterministic baseline")),
    Framing = factor(Framing, levels = c("P", "N"), labels = c("Positive framing", "Negative framing"))
  ) %>%
  arrange(Baseline, Cue, Framing, Generation)

# 5.6) Framing-specific developmental trends
# Apply Holm correction across positive and negative framing within each Cue x Baseline.
adaptive_framing_trends <- adaptive_framing_alignment %>%
  group_by(Baseline, Cue, Framing) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, AlignmentPercent)),
    Spearman_rho = cor(Generation, AlignmentPercent, method = "spearman", use = "complete.obs"),
    p_raw = cor.test(Generation, AlignmentPercent, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  ) %>%
  group_by(Baseline, Cue) %>%
  mutate(
    p_adjusted = p.adjust(p_raw, method = "holm"),
    Correction = "Holm within cue x baseline (2 framing-specific choice-alignment tests)",
    Significant_raw = p_raw <= 0.050,
    Significant_adjusted = p_adjusted <= 0.050,
    Direction = case_when(
        Spearman_rho > 0 ~ "Increasing alignment",
        Spearman_rho < 0 ~ "Decreasing alignment",
        TRUE ~ "No monotonic direction"
      )
  ) %>%
  ungroup() %>%
  arrange(Baseline, Cue, Framing) %>%
  select(
    Baseline, Cue, Framing, N_Models, Spearman_rho, p_raw, p_adjusted, Correction, Significant_raw,
    Significant_adjusted, Direction
  )


# 5.9) Framing-specific baseline gaps
adaptive_framing_gap <- adaptive_framing_alignment %>%
  select(Agent, Generation, Cue, Framing, Baseline, AlignmentPercent) %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`) %>%
  arrange(Cue, Framing, Generation)

# 5.10) Framing-specific baseline-gap trends
# Apply Holm correction across the two framing conditions within each cue.
adaptive_framing_gap_trends <- adaptive_framing_gap %>%
  group_by(Cue, Framing) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, BaselineGap)),
    Spearman_rho = cor(Generation, BaselineGap, method = "spearman", use = "complete.obs"),
    p_raw = cor.test(Generation, BaselineGap, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  ) %>%
  group_by(Cue) %>%
  mutate(
    p_adjusted = p.adjust(p_raw, method = "holm"),
    Correction = "Holm within cue (2 framing-specific baseline-gap tests)",
    Significant_raw = p_raw <= 0.050,
    Significant_adjusted = p_adjusted <= 0.050,
    Direction = case_when(
        Spearman_rho > 0 ~ "Increasing relative deterministic alignment",
        Spearman_rho < 0 ~ "Increasing relative human alignment",
        TRUE ~ "No monotonic direction"
      )
  ) %>%
  ungroup()

# 5.11) Framing-effect dual-reference alignment
# Framing effect = P(A | positive) - P(A | negative).
# Alignment = 100 * (1 - absolute effect difference), using human effects or zero as reference.
group_framing_effects <- group_size_data %>%
  select(Agent, GroupSize, Framing, P_A) %>%
  pivot_wider(names_from = Framing, values_from = P_A) %>%
  mutate(FramingEffect = P - N, Cue = "Group size", CueValue = as.character(GroupSize))

kinship_framing_effects <- kinship_data %>%
  select(Agent, KinNumber, Framing, P_A) %>%
  pivot_wider(names_from = Framing, values_from = P_A) %>%
  mutate(FramingEffect = P - N, Cue = "Kinship", CueValue = as.character(KinNumber))

adaptive_framing_effects <- bind_rows(
  group_framing_effects %>%
    select(Agent, Cue, CueValue, FramingEffect), kinship_framing_effects %>%
    select(Agent, Cue, CueValue, FramingEffect)
) %>%
  mutate(Generation = match(Agent, model_order)) %>%
  arrange(Cue, Agent, CueValue)

human_framing_effect_reference <- adaptive_framing_effects %>%
  filter(Agent == "human") %>%
  select(Cue, CueValue, HumanFramingEffect = FramingEffect)

framing_effect_human_condition_alignment <- adaptive_framing_effects %>%
  filter(Agent != "human") %>%
  left_join(human_framing_effect_reference, by = c("Cue", "CueValue")) %>%
  mutate(
    SignedDeviation = FramingEffect -
      HumanFramingEffect, AbsoluteDistance = abs(SignedDeviation), Alignment = 1 -
      AbsoluteDistance
  )

framing_effect_human_alignment <- framing_effect_human_condition_alignment %>%
  group_by(Agent, Generation, Cue) %>%
  summarise(
    N_CueValues = sum(complete.cases(FramingEffect, HumanFramingEffect)), AlignmentPercent = 100 *
      mean(Alignment, na.rm = TRUE), .groups = "drop"
  ) %>%
  mutate(Baseline = "Human baseline") %>%
  arrange(Cue, Generation)

framing_effect_invariance_condition_alignment <- adaptive_framing_effects %>%
  filter(Agent != "human") %>%
  mutate(
    InvarianceFramingEffect = 0, SignedDeviation = FramingEffect -
      InvarianceFramingEffect, AbsoluteDistance = abs(SignedDeviation), Alignment = 1 -
      AbsoluteDistance
  )

framing_effect_condition_alignment <- bind_rows(
  framing_effect_human_condition_alignment %>%
    mutate(Baseline = "Human baseline", ReferenceFramingEffect = HumanFramingEffect),
  framing_effect_invariance_condition_alignment %>%
    mutate(Baseline = "Complete-framing-invariance baseline", ReferenceFramingEffect = InvarianceFramingEffect)
) %>%
  select(
    Agent, Generation, Cue, CueValue, Baseline, FramingEffect, ReferenceFramingEffect, SignedDeviation,
    AbsoluteDistance, Alignment
  ) %>%
  arrange(Baseline, Cue, Agent, CueValue)

framing_effect_invariance_alignment <- framing_effect_invariance_condition_alignment %>%
  group_by(Agent, Generation, Cue) %>%
  summarise(
    N_CueValues = sum(complete.cases(FramingEffect, InvarianceFramingEffect)), AlignmentPercent = 100 *
      mean(Alignment, na.rm = TRUE), .groups = "drop"
  ) %>%
  mutate(Baseline = "Complete-framing-invariance baseline") %>%
  arrange(Cue, Generation)

framing_effect_alignment <- bind_rows(framing_effect_human_alignment, framing_effect_invariance_alignment) %>%
  mutate(Baseline = factor(Baseline, levels = c("Human baseline", "Complete-framing-invariance baseline"))) %>%
  arrange(Baseline, Cue, Generation)

framing_effect_overall_alignment <- framing_effect_alignment %>%
  group_by(Agent, Generation, Baseline) %>%
  summarise(
    N_Cues = sum(!is.na(AlignmentPercent)), AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Cue = "Adaptive cues") %>%
  arrange(Generation)

framing_effect_cue_trends <- framing_effect_alignment %>%
  group_by(Baseline, Cue) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, AlignmentPercent)), Spearman_rho =
      cor(Generation, AlignmentPercent, method = "spearman", use = "complete.obs"), p_value =
      cor.test(Generation, AlignmentPercent, method = "spearman", exact = FALSE)$p.value, .groups = "drop"
  ) %>%
  mutate(
    Correction = "None; Group size and Kinship are separate hypotheses within each baseline", Significant =
      p_value <= 0.050, Direction = case_when(
        Baseline == "Human baseline" & Spearman_rho > 0 ~ "Increasing human framing-effect alignment",
        Baseline == "Human baseline" & Spearman_rho < 0 ~ "Decreasing human framing-effect alignment",
        Baseline == "Complete-framing-invariance baseline" & Spearman_rho > 0 ~
          "Increasing complete-framing-invariance alignment", Baseline == "Complete-framing-invariance baseline" &
          Spearman_rho < 0 ~ "Decreasing complete-framing-invariance alignment", TRUE ~ "No monotonic direction"
      )
  )

framing_effect_overall_trend_result <- framing_effect_overall_alignment %>%
  group_by(Baseline) %>%
  summarise(
    Cue = "Adaptive cues", N_Models = sum(complete.cases(Generation, AlignmentPercent)), Spearman_rho =
      cor(Generation, AlignmentPercent, method = "spearman", use = "complete.obs"), p_value =
      cor.test(Generation, AlignmentPercent, method = "spearman", exact = FALSE)$p.value, .groups = "drop"
  ) %>%
  mutate(
    Correction = "None; single overall composite hypothesis within each baseline", Significant = p_value <= 0.050,
    Direction = case_when(
        Baseline == "Human baseline" & Spearman_rho > 0 ~ "Increasing human framing-effect alignment",
        Baseline == "Human baseline" & Spearman_rho < 0 ~ "Decreasing human framing-effect alignment",
        Baseline == "Complete-framing-invariance baseline" & Spearman_rho > 0 ~
          "Increasing complete-framing-invariance alignment", Baseline == "Complete-framing-invariance baseline" &
          Spearman_rho < 0 ~ "Decreasing complete-framing-invariance alignment", TRUE ~ "No monotonic direction"
      )
  )

# 5.12) Framing-effect Human-vs-Invariance comparison
# 5.12.1) Overall framing-effect baseline gap
framing_effect_overall_gap <- framing_effect_overall_alignment %>%
  select(Agent, Generation, Baseline, AlignmentPercent) %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Complete-framing-invariance baseline` - `Human baseline`) %>%
  arrange(Generation)

framing_effect_overall_gap_test <- cor.test(
  framing_effect_overall_gap$Generation,
  framing_effect_overall_gap$BaselineGap,
  method = "spearman",
  exact = FALSE
)

framing_effect_overall_gap_result <- data.frame(
  Analysis = "Framing effect: Complete-framing-invariance minus Human baseline alignment",
  N_Models = sum(complete.cases(framing_effect_overall_gap$Generation, framing_effect_overall_gap$BaselineGap)),
  Spearman_rho = unname(framing_effect_overall_gap_test$estimate),
  p_value = framing_effect_overall_gap_test$p.value,
  Significant = framing_effect_overall_gap_test$p.value <= 0.050,
  Direction = case_when(
      unname(framing_effect_overall_gap_test$estimate) > 0 ~
        "Increasing relative complete-framing-invariance alignment",
      unname(framing_effect_overall_gap_test$estimate) < 0 ~ "Increasing relative human framing-effect alignment",
      TRUE ~ "No monotonic direction"
    )
)

# 5.12.2) Cue-specific framing-effect baseline gaps
framing_effect_cue_gap <- framing_effect_alignment %>%
  select(Agent, Generation, Cue, Baseline, AlignmentPercent) %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Complete-framing-invariance baseline` - `Human baseline`) %>%
  arrange(Cue, Generation)

framing_effect_cue_gap_trends <- framing_effect_cue_gap %>%
  group_by(Cue) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, BaselineGap)),
    Spearman_rho = cor(Generation, BaselineGap, method = "spearman", use = "complete.obs"),
    p_raw = cor.test(Generation, BaselineGap, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    p_adjusted = p.adjust(p_raw, method = "holm"),
    Correction = "Holm (2 cue-specific framing-effect baseline-gap tests)",
    Significant_raw = p_raw <= 0.050,
    Significant_adjusted = p_adjusted <= 0.050,
    Direction = case_when(
        Spearman_rho > 0 ~ "Increasing relative complete-framing-invariance alignment",
        Spearman_rho < 0 ~ "Increasing relative human framing-effect alignment",
        TRUE ~ "No monotonic direction"
      )
  )


# 6) SECONDARY ANALYSIS: Human alignment in adaptive cue sensitivity
# Sensitivity: slope of P(A) on log10(group size) or kin proportion.
# Alignment = 100 * (1 - absolute slope difference / theoretical slope range).
# Slope ranges: 0.8 for group size and 18/7 for kinship.

# 6.1) Group size sensitivity
group_size_sensitivity <- group_size_data %>%
  group_by(Agent, Framing) %>%
  summarise(GroupSensitivity = safe_lm_slope(logGroupSize, P_A), .groups = "drop")

# 6.2) Overall group size sensitivity
group_size_cue <- group_size_sensitivity %>%
  group_by(Agent) %>%
  summarise(GroupSensitivity = mean(GroupSensitivity, na.rm = TRUE), .groups = "drop")

# 6.3) Overall Human alignment in group size sensitivity
human_group_sensitivity <- group_size_cue %>%
  filter(Agent == "human") %>%
  pull(GroupSensitivity)

group_size_sensitivity_alignment <- group_size_cue %>%
  filter(Agent != "human") %>%
  mutate(
    HumanGroupSensitivity = human_group_sensitivity,
    GroupDistance = abs(GroupSensitivity - HumanGroupSensitivity),
    # Normalize the slope distance by the theoretical
    # group-size slope range: 0.4 - (-0.4) = 0.8
    SensitivityAlignmentPercent = 100 *
      (1 - GroupDistance / 0.8),
    Generation = match(Agent, model_order)
  ) %>%
  filter(!is.na(Generation)) %>%
  arrange(Generation)

# 6.4) Framing-specific Human alignment in group size sensitivity
human_group_framing <- group_size_sensitivity %>%
  filter(Agent == "human") %>%
  select(Framing, HumanGroupSensitivity = GroupSensitivity)

group_framing_sensitivity_alignment <- group_size_sensitivity %>%
  filter(Agent != "human") %>%
  left_join(human_group_framing, by = "Framing") %>%
  mutate(
    GroupDistance = abs(GroupSensitivity - HumanGroupSensitivity),
    # Normalize the slope distance by the theoretical
    # group-size slope range: 0.4 - (-0.4) = 0.8
    SensitivityAlignmentPercent = 100 *
      (1 - GroupDistance / 0.8),
    Generation = match(Agent, model_order)
  ) %>%
  filter(!is.na(Generation)) %>%
  arrange(Framing, Generation)

# 6.5) Kinship sensitivity
kinship_sensitivity <- kinship_data %>%
  group_by(Agent, Framing) %>%
  summarise(KinshipSensitivity = safe_lm_slope(KinProportion, P_A), .groups = "drop")

# 6.6) Overall kinship sensitivity
kinship_cue <- kinship_sensitivity %>%
  group_by(Agent) %>%
  summarise(KinshipSensitivity = mean(KinshipSensitivity, na.rm = TRUE), .groups = "drop")

# 6.7) Overall Human alignment in kinship sensitivity
human_kinship_sensitivity <- kinship_cue %>%
  filter(Agent == "human") %>%
  pull(KinshipSensitivity)

kinship_sensitivity_alignment <- kinship_cue %>%
  filter(Agent != "human") %>%
  mutate(
    HumanKinshipSensitivity = human_kinship_sensitivity,
    KinshipDistance = abs(KinshipSensitivity - HumanKinshipSensitivity),
    # Normalize the slope distance by the theoretical
    # kinship slope range: 9/7 - (-9/7) = 18/7
    SensitivityAlignmentPercent = 100 *
      (1 - KinshipDistance / (18 / 7)),
    Generation = match(Agent, model_order)
  ) %>%
  filter(!is.na(Generation)) %>%
  arrange(Generation)

# 6.8) Framing-specific Human alignment in kinship sensitivity
human_kinship_framing <- kinship_sensitivity %>%
  filter(Agent == "human") %>%
  select(Framing, HumanKinshipSensitivity = KinshipSensitivity)

kinship_framing_sensitivity_alignment <- kinship_sensitivity %>%
  filter(Agent != "human") %>%
  left_join(human_kinship_framing, by = "Framing") %>%
  mutate(
    KinshipDistance = abs(KinshipSensitivity - HumanKinshipSensitivity),
    # Normalize the slope distance by the theoretical
    # kinship slope range: 9/7 - (-9/7) = 18/7
    SensitivityAlignmentPercent = 100 *
      (1 - KinshipDistance / (18 / 7)),
    Generation = match(Agent, model_order)
  ) %>%
  filter(!is.na(Generation)) %>%
  arrange(Framing, Generation)

# 6.9) Overall slope-based Human-alignment trends
# Test overall cue-specific sensitivity trends separately, without multiplicity correction.
group_sensitivity_test <- cor.test(
  group_size_sensitivity_alignment$Generation,
  group_size_sensitivity_alignment$SensitivityAlignmentPercent,
  method = "spearman",
  exact = FALSE
)

kinship_sensitivity_test <- cor.test(
  kinship_sensitivity_alignment$Generation,
  kinship_sensitivity_alignment$SensitivityAlignmentPercent,
  method = "spearman",
  exact = FALSE
)

cue_sensitivity_trends <- data.frame(
  Cue = c("Group size", "Kinship"),
  N_Models = c(
    sum(
      complete.cases(
        group_size_sensitivity_alignment$Generation, group_size_sensitivity_alignment$SensitivityAlignmentPercent
        )
    ),
    sum(
      complete.cases(
        kinship_sensitivity_alignment$Generation, kinship_sensitivity_alignment$SensitivityAlignmentPercent
        )
    )
  ),
  Spearman_rho = c(unname(group_sensitivity_test$estimate), unname(kinship_sensitivity_test$estimate)),
  p_value = c(group_sensitivity_test$p.value, kinship_sensitivity_test$p.value)
) %>%
  mutate(
    Correction = "None; Group size and Kinship are separate hypotheses",
    Significant = p_value <= 0.050,
    Direction = case_when(
        Spearman_rho > 0 ~ "Increasing human sensitivity alignment",
        Spearman_rho < 0 ~ "Decreasing human sensitivity alignment",
        TRUE ~ "No monotonic direction"
      )
  )

# 6.9A) Overall adaptive-cue sensitivity alignment and developmental trend
# Overall sensitivity alignment equally weights the two normalized cue alignments.
overall_sensitivity_alignment <- bind_rows(
  group_size_sensitivity_alignment %>%
    transmute(Agent, Generation, Cue = "Group size", SensitivityAlignmentPercent),
  kinship_sensitivity_alignment %>%
    transmute(Agent, Generation, Cue = "Kinship", SensitivityAlignmentPercent)
) %>%
  group_by(Agent, Generation) %>%
  summarise(
    N_Cues = sum(!is.na(SensitivityAlignmentPercent)),
    SensitivityAlignmentPercent = mean(SensitivityAlignmentPercent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Cue = "Adaptive cues") %>%
  select(Agent, Generation, Cue, SensitivityAlignmentPercent, N_Cues) %>%
  arrange(Generation)

overall_sensitivity_test <- cor.test(
  overall_sensitivity_alignment$Generation,
  overall_sensitivity_alignment$SensitivityAlignmentPercent,
  method = "spearman",
  exact = FALSE
)

overall_sensitivity_trend_result <- data.frame(
  Cue = "Adaptive cues",
  N_Models = sum(
      complete.cases(overall_sensitivity_alignment$Generation, overall_sensitivity_alignment$SensitivityAlignmentPercent)
    ),
  Spearman_rho = unname(overall_sensitivity_test$estimate),
  p_value = overall_sensitivity_test$p.value,
  Correction = "None; single overall composite hypothesis",
  Significant = overall_sensitivity_test$p.value <= 0.050,
  Direction = case_when(
      unname(overall_sensitivity_test$estimate) > 0 ~ "Increasing human sensitivity alignment",
      unname(overall_sensitivity_test$estimate) < 0 ~ "Decreasing human sensitivity alignment",
      TRUE ~ "No monotonic direction"
    )
)

# 6.10) Framing-specific slope-based Human-alignment trends
# Apply Holm correction across the two framing conditions within each cue.
group_sensitivity_framing_trends <- group_framing_sensitivity_alignment %>%
  group_by(Framing) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, SensitivityAlignmentPercent)),
    Spearman_rho = cor(Generation, SensitivityAlignmentPercent, method = "spearman", use = "complete.obs"),
    p_raw = cor.test(Generation, SensitivityAlignmentPercent, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  ) %>%
  mutate(Cue = "Group size")

kinship_sensitivity_framing_trends <- kinship_framing_sensitivity_alignment %>%
  group_by(Framing) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, SensitivityAlignmentPercent)),
    Spearman_rho = cor(Generation, SensitivityAlignmentPercent, method = "spearman", use = "complete.obs"),
    p_raw = cor.test(Generation, SensitivityAlignmentPercent, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  ) %>%
  mutate(Cue = "Kinship")

cue_sensitivity_framing_trends <- bind_rows(group_sensitivity_framing_trends, kinship_sensitivity_framing_trends) %>%
  mutate(Framing = factor(Framing, levels = c("P", "N"), labels = c("Positive framing", "Negative framing"))) %>%
  group_by(Cue) %>%
  mutate(
    p_adjusted = p.adjust(p_raw, method = "holm"),
    Correction = "Holm within cue (2 framing-specific slope tests)",
    Significant_raw = p_raw <= 0.050,
    Significant_adjusted = p_adjusted <= 0.050,
    Direction = case_when(
        Spearman_rho > 0 ~ "Increasing human sensitivity alignment",
        Spearman_rho < 0 ~ "Decreasing human sensitivity alignment",
        TRUE ~ "No monotonic direction"
      )
  ) %>%
  ungroup() %>%
  select(
    Cue, Framing, N_Models, Spearman_rho, p_raw, p_adjusted, Correction, Significant_raw, Significant_adjusted,
    Direction
  )


# 8) Bootstrap confidence intervals

# 8.1) Bootstrap Human-baseline Adaptive-cues alignment
set.seed(1234)
n_bootstrap <- 1000

bootstrap_socialcue_once <- function(bootstrap_id){
  bootstrap_summary <- socialcue_long %>%
    filter(Agent != "human", !is.na(Response)) %>%
    group_by(Agent, Condition) %>%
    summarise(N_Response = n(), P_A = mean(sample(Response, size = n(), replace = TRUE) == "A"), .groups = "drop")

  bootstrap_group_condition <- bootstrap_summary %>%
    filter(str_detect(Condition, "life")) %>%
    mutate(
      Framing = str_sub(Condition, 1, 1),
      GroupSize = case_when(
          str_detect(Condition, "6000") ~ 6000, str_detect(Condition, "600") ~ 600,
          str_detect(Condition, "60") ~ 60, str_detect(Condition, "6") ~ 6, TRUE ~ NA_real_
        )
    ) %>%
    left_join(human_group_profile, by = c("Framing", "GroupSize")) %>%
    mutate(Alignment = 1 - abs(P_A - HumanPA), Cue = "Group size")

  bootstrap_kinship_condition <- bootstrap_summary %>%
    filter(str_detect(Condition, "kin")) %>%
    mutate(
      Framing = str_sub(Condition, 1, 1),
      KinNumber = case_when(
          str_detect(Condition, "6kin") ~ 6, str_detect(Condition, "3kin") ~ 3, str_detect(Condition, "2kin") ~ 2,
          str_detect(Condition, "1kin") ~ 1, TRUE ~ NA_real_
        )
    ) %>%
    left_join(human_kinship_profile, by = c("Framing", "KinNumber")) %>%
    mutate(Alignment = 1 - abs(P_A - HumanPA), Cue = "Kinship")

  bootstrap_condition_alignment <- bind_rows(
    bootstrap_group_condition %>%
      select(Agent, Condition, Cue, P_A, HumanPA, Alignment),
    bootstrap_kinship_condition %>%
      select(Agent, Condition, Cue, P_A, HumanPA, Alignment)
  ) %>%
    mutate(BootstrapID = bootstrap_id, Generation = match(Agent, model_order), AlignmentPercent = 100 * Alignment) %>%
    filter(!is.na(Generation)) %>%
    select(BootstrapID, Agent, Generation, Cue, Condition, P_A, HumanPA, AlignmentPercent)

  bootstrap_overall_alignment <- bootstrap_condition_alignment %>%
    group_by(BootstrapID, Agent, Generation, Cue) %>%
    summarise(AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE), .groups = "drop") %>%
    group_by(BootstrapID, Agent, Generation) %>%
    summarise(
      N_Cues = sum(!is.na(AlignmentPercent)), AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Cue = "Adaptive cues", Baseline = "Human baseline") %>%
    select(BootstrapID, Agent, Generation, Cue, Baseline, AlignmentPercent, N_Cues)

  list(condition = bootstrap_condition_alignment, overall = bootstrap_overall_alignment)
}

socialcue_bootstrap_results <- map(seq_len(n_bootstrap), bootstrap_socialcue_once)

bootstrap_condition_alignment <- map_dfr(socialcue_bootstrap_results, "condition")

# 8.2) Bootstrap confidence intervals for monotonic trends
bootstrap_condition_profile <- bootstrap_condition_alignment %>%
  mutate(
    Framing = str_sub(Condition, 1, 1), GroupSize = case_when(
        Cue == "Group size" & str_detect(Condition, "6000") ~ 6000,
        Cue == "Group size" & str_detect(Condition, "600") ~ 600,
        Cue == "Group size" & str_detect(Condition, "60") ~ 60,
        Cue == "Group size" & str_detect(Condition, "6") ~ 6, TRUE ~ NA_real_
      ), KinNumber = case_when(
        Cue == "Kinship" & str_detect(Condition, "6kin") ~ 6, Cue == "Kinship" & str_detect(Condition, "3kin") ~ 3,
        Cue == "Kinship" & str_detect(Condition, "2kin") ~ 2, Cue == "Kinship" & str_detect(Condition, "1kin") ~ 1,
        TRUE ~ NA_real_
      ), CueValue = ifelse(Cue == "Group size", GroupSize, KinNumber), Predictor =
      ifelse(Cue == "Group size", log10(GroupSize), KinNumber / 6)
  )

bootstrap_choice_condition_dual <- bind_rows(
  bootstrap_condition_profile %>%
    transmute(
      BootstrapID, Agent, Generation, Cue, Framing, CueValue, P_A, Baseline = "Human baseline", AlignmentPercent
    ), bootstrap_condition_profile %>%
      transmute(
      BootstrapID, Agent, Generation, Cue, Framing, CueValue, P_A, Baseline = "Deterministic baseline",
      AlignmentPercent = 100 *
        (1 - abs(P_A - 1))
    )
)

bootstrap_choice_cue_framing <- bootstrap_choice_condition_dual %>%
  group_by(BootstrapID, Agent, Generation, Baseline, Cue, Framing) %>%
  summarise(AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE), .groups = "drop")

bootstrap_choice_cue <- bootstrap_choice_cue_framing %>%
  group_by(BootstrapID, Agent, Generation, Baseline, Cue) %>%
  summarise(AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE), .groups = "drop")

bootstrap_choice_overall <- bootstrap_choice_cue %>%
  group_by(BootstrapID, Agent, Generation, Baseline) %>%
  summarise(AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE), .groups = "drop")

bootstrap_choice_overall_framing <- bootstrap_choice_cue_framing %>%
  group_by(BootstrapID, Agent, Generation, Baseline, Framing) %>%
  summarise(AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE), .groups = "drop")

bootstrap_cue_gap <- bootstrap_choice_cue %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`)

bootstrap_overall_gap <- bootstrap_choice_overall %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`)

bootstrap_framing_gap <- bootstrap_choice_cue_framing %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`)

bootstrap_sensitivity_framing <- bootstrap_condition_profile %>%
  group_by(BootstrapID, Agent, Generation, Cue, Framing) %>%
  summarise(Sensitivity = safe_lm_slope(Predictor, P_A), .groups = "drop") %>%
    left_join(
    bind_rows(
      human_group_framing %>%
        transmute(Cue = "Group size", Framing, HumanSensitivity = HumanGroupSensitivity),
      human_kinship_framing %>%
        transmute(Cue = "Kinship", Framing, HumanSensitivity = HumanKinshipSensitivity)
    ), by = c("Cue", "Framing")
  ) %>%
    mutate(
    SensitivityAlignmentPercent = 100 *
      (1 - abs(Sensitivity - HumanSensitivity) / ifelse(Cue == "Group size", 0.8, 18 / 7))
  )

bootstrap_sensitivity_cue <- bootstrap_sensitivity_framing %>%
  group_by(BootstrapID, Agent, Generation, Cue) %>%
  summarise(Sensitivity = mean(Sensitivity, na.rm = TRUE), .groups = "drop") %>%
    mutate(
    HumanSensitivity = ifelse(Cue == "Group size", human_group_sensitivity, human_kinship_sensitivity),
    SensitivityAlignmentPercent = 100 *
      (1 - abs(Sensitivity - HumanSensitivity) / ifelse(Cue == "Group size", 0.8, 18 / 7))
  )

bootstrap_sensitivity_overall <- bootstrap_sensitivity_cue %>%
  group_by(BootstrapID, Agent, Generation) %>%
  summarise(SensitivityAlignmentPercent = mean(SensitivityAlignmentPercent, na.rm = TRUE), .groups = "drop")

bootstrap_framing_effect_condition <- bootstrap_condition_profile %>%
  select(BootstrapID, Agent, Generation, Cue, CueValue, Framing, P_A) %>%
  pivot_wider(names_from = Framing, values_from = P_A) %>%
  mutate(FramingEffect = P - N, CueValue = as.character(CueValue)) %>%
  left_join(human_framing_effect_reference, by = c("Cue", "CueValue")) %>%
  mutate(AlignmentPercent = 100 * (1 - abs(FramingEffect - HumanFramingEffect)))

bootstrap_framing_effect_human_condition <- bootstrap_framing_effect_condition %>%
  mutate(Baseline = "Human baseline")

bootstrap_framing_effect_invariance_condition <- bootstrap_framing_effect_condition %>%
  mutate(Baseline = "Complete-framing-invariance baseline", AlignmentPercent = 100 * (1 - abs(FramingEffect)))

bootstrap_framing_effect_dual_condition <- bind_rows(bootstrap_framing_effect_human_condition, bootstrap_framing_effect_invariance_condition)

bootstrap_framing_effect_cue <- bootstrap_framing_effect_dual_condition %>%
  group_by(BootstrapID, Agent, Generation, Baseline, Cue) %>%
  summarise(AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE), .groups = "drop")

bootstrap_framing_effect_overall <- bootstrap_framing_effect_cue %>%
  group_by(BootstrapID, Agent, Generation, Baseline) %>%
  summarise(AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE), .groups = "drop")

bootstrap_framing_effect_cue_gap <- bootstrap_framing_effect_cue %>%
  select(BootstrapID, Agent, Generation, Cue, Baseline, AlignmentPercent) %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Complete-framing-invariance baseline` - `Human baseline`)

bootstrap_framing_effect_overall_gap <- bootstrap_framing_effect_overall %>%
  select(BootstrapID, Agent, Generation, Baseline, AlignmentPercent) %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Complete-framing-invariance baseline` - `Human baseline`)

bootstrap_trend_values <- bind_rows(
  bootstrap_choice_overall %>%
    transmute(BootstrapID, Generation, Analysis = "Overall choice alignment", Baseline, Cue = NA_character_, Framing = NA_character_, Value = AlignmentPercent),
  bootstrap_choice_cue %>%
    transmute(BootstrapID, Generation, Analysis = "Cue choice alignment", Baseline, Cue, Framing = NA_character_, Value = AlignmentPercent),
  bootstrap_overall_gap %>%
    transmute(BootstrapID, Generation, Analysis = "Overall baseline gap", Baseline = NA_character_, Cue = NA_character_, Framing = NA_character_, Value = BaselineGap),
  bootstrap_cue_gap %>%
    transmute(BootstrapID, Generation, Analysis = "Cue baseline gap", Baseline = NA_character_, Cue, Framing = NA_character_, Value = BaselineGap),
  bootstrap_choice_cue_framing %>%
    transmute(BootstrapID, Generation, Analysis = "Framing-specific choice alignment", Baseline, Cue, Framing, Value = AlignmentPercent),
  bootstrap_choice_overall_framing %>%
    transmute(BootstrapID, Generation, Analysis = "Overall framing-specific choice alignment", Baseline, Cue = NA_character_, Framing, Value = AlignmentPercent),
  bootstrap_framing_gap %>%
    transmute(BootstrapID, Generation, Analysis = "Framing-specific baseline gap", Baseline = NA_character_, Cue, Framing, Value = BaselineGap),
  bootstrap_sensitivity_cue %>%
    transmute(BootstrapID, Generation, Analysis = "Overall sensitivity alignment", Baseline = NA_character_, Cue, Framing = NA_character_, Value = SensitivityAlignmentPercent),
  bootstrap_sensitivity_overall %>%
    transmute(BootstrapID, Generation, Analysis = "Adaptive-cue sensitivity alignment", Baseline = NA_character_, Cue = NA_character_, Framing = NA_character_, Value = SensitivityAlignmentPercent),
  bootstrap_sensitivity_framing %>%
    transmute(BootstrapID, Generation, Analysis = "Framing-specific sensitivity alignment", Baseline = NA_character_, Cue, Framing, Value = SensitivityAlignmentPercent),
  bootstrap_framing_effect_cue %>%
    transmute(BootstrapID, Generation, Analysis = "Framing-effect alignment", Baseline, Cue, Framing = NA_character_, Value = AlignmentPercent),
  bootstrap_framing_effect_overall %>%
    transmute(BootstrapID, Generation, Analysis = "Overall framing-effect alignment", Baseline, Cue = NA_character_, Framing = NA_character_, Value = AlignmentPercent),
  bootstrap_framing_effect_cue_gap %>%
    transmute(BootstrapID, Generation, Analysis = "Framing-effect baseline gap", Baseline = NA_character_, Cue, Framing = NA_character_, Value = BaselineGap),
  bootstrap_framing_effect_overall_gap %>%
    transmute(BootstrapID, Generation, Analysis = "Overall framing-effect baseline gap", Baseline = NA_character_, Cue = NA_character_, Framing = NA_character_, Value = BaselineGap)
)

bootstrap_trend_rho <- bootstrap_trend_values %>%
  group_by(BootstrapID, Analysis, Baseline, Cue, Framing) %>%
  summarise(Spearman_rho = cor(Generation, Value, method = "spearman", use = "complete.obs"), .groups = "drop")

bootstrap_trend_ci <- bootstrap_trend_rho %>%
  filter(is.finite(Spearman_rho)) %>%
  group_by(Analysis, Baseline, Cue, Framing) %>%
    summarise(
    CI_Lower = quantile(Spearman_rho, probs = 0.025, na.rm = TRUE, names = FALSE), CI_Upper =
      quantile(Spearman_rho, probs = 0.975, na.rm = TRUE, names = FALSE), CI_Method = "Percentile bootstrap",
    Confidence_Level = 0.95, N_Boot_Valid = n(), .groups = "drop"
  )

adaptive_primary_trends <- adaptive_primary_trends %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Overall choice alignment") %>%
      select(Baseline, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid), by = "Baseline"
  )

adaptive_cue_trends <- adaptive_cue_trends %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Cue choice alignment") %>%
      select(Baseline, Cue, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid),
    by = c("Baseline", "Cue")
  )

adaptive_overall_gap_result <- bind_cols(
  adaptive_overall_gap_result, bootstrap_trend_ci %>%
    filter(Analysis == "Overall baseline gap") %>%
    select(CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid)
)

adaptive_cue_gap_trends <- adaptive_cue_gap_trends %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Cue baseline gap") %>%
      select(Cue, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid), by = "Cue"
  )

adaptive_framing_trends <- adaptive_framing_trends %>%
  mutate(Framing = as.character(Framing)) %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Framing-specific choice alignment") %>%
      mutate(Framing = recode(Framing, P = "Positive framing", N = "Negative framing")) %>%
      select(Baseline, Cue, Framing, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid),
    by = c("Baseline", "Cue", "Framing")
  )

adaptive_framing_gap_trends <- adaptive_framing_gap_trends %>%
  mutate(Framing = as.character(Framing)) %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Framing-specific baseline gap") %>%
      mutate(Framing = recode(Framing, P = "Positive framing", N = "Negative framing")) %>%
      select(Cue, Framing, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid), by = c("Cue", "Framing")
  )

cue_sensitivity_trends <- cue_sensitivity_trends %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Overall sensitivity alignment") %>%
      select(Cue, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid), by = "Cue"
  )

overall_sensitivity_trend_result <- bind_cols(
  overall_sensitivity_trend_result, bootstrap_trend_ci %>%
    filter(Analysis == "Adaptive-cue sensitivity alignment") %>%
    select(CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid)
)

cue_sensitivity_framing_trends <- cue_sensitivity_framing_trends %>%
  mutate(Framing = as.character(Framing)) %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Framing-specific sensitivity alignment") %>%
      mutate(Framing = recode(Framing, P = "Positive framing", N = "Negative framing")) %>%
      select(Cue, Framing, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid), by = c("Cue", "Framing")
  )

framing_effect_cue_trends <- framing_effect_cue_trends %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Framing-effect alignment") %>%
      select(Baseline, Cue, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid),
    by = c("Baseline", "Cue")
  )

framing_effect_overall_trend_result <- framing_effect_overall_trend_result %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Overall framing-effect alignment") %>%
      select(Baseline, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid), by = "Baseline"
  )

framing_effect_cue_gap_trends <- framing_effect_cue_gap_trends %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Framing-effect baseline gap") %>%
      select(Cue, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid), by = "Cue"
  )

framing_effect_overall_gap_result <- bind_cols(
  framing_effect_overall_gap_result, bootstrap_trend_ci %>%
    filter(Analysis == "Overall framing-effect baseline gap") %>%
    select(CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid)
)

adaptive_primary_trends <- add_bayesian_trend_evidence(
  trend_table = adaptive_primary_trends, source_data = adaptive_overall_alignment, by_cols = "Baseline",
  y_col = "AlignmentPercent"
)

adaptive_cue_trends <- add_bayesian_trend_evidence(
  trend_table = adaptive_cue_trends, source_data = adaptive_cue_alignment, by_cols = c("Baseline", "Cue"),
  y_col = "AlignmentPercent"
)

adaptive_overall_gap_result <- add_bayesian_trend_evidence(trend_table = adaptive_overall_gap_result, source_data = adaptive_overall_gap, y_col = "BaselineGap")

adaptive_cue_gap_trends <- add_bayesian_trend_evidence(
  trend_table = adaptive_cue_gap_trends, source_data = adaptive_cue_gap, by_cols = "Cue", y_col = "BaselineGap"
)

adaptive_framing_trends <- add_bayesian_trend_evidence(
  trend_table = adaptive_framing_trends, source_data = adaptive_framing_alignment,
  by_cols = c("Baseline", "Cue", "Framing"), y_col = "AlignmentPercent"
)

adaptive_framing_gap_trends <- add_bayesian_trend_evidence(
  trend_table = adaptive_framing_gap_trends, source_data = adaptive_framing_gap, by_cols = c("Cue", "Framing"),
  y_col = "BaselineGap"
)

framing_effect_cue_trends <- add_bayesian_trend_evidence(
  trend_table = framing_effect_cue_trends, source_data = framing_effect_alignment, by_cols = c("Baseline", "Cue"),
  y_col = "AlignmentPercent"
)

framing_effect_overall_trend_result <- add_bayesian_trend_evidence(
  trend_table = framing_effect_overall_trend_result, source_data = framing_effect_overall_alignment,
  by_cols = "Baseline", y_col = "AlignmentPercent"
)

framing_effect_cue_gap_trends <- add_bayesian_trend_evidence(
  trend_table = framing_effect_cue_gap_trends, source_data = framing_effect_cue_gap, by_cols = "Cue",
  y_col = "BaselineGap"
)

framing_effect_overall_gap_result <- add_bayesian_trend_evidence(
  trend_table = framing_effect_overall_gap_result, source_data = framing_effect_overall_gap, y_col = "BaselineGap"
)

cue_sensitivity_trends <- add_bayesian_trend_evidence(
  trend_table = cue_sensitivity_trends, source_data = bind_rows(
    group_size_sensitivity_alignment %>%
      mutate(Cue = "Group size"), kinship_sensitivity_alignment %>%
      mutate(Cue = "Kinship")
  ), by_cols = "Cue", y_col = "SensitivityAlignmentPercent"
)

overall_sensitivity_trend_result <- add_bayesian_trend_evidence(
  trend_table = overall_sensitivity_trend_result, source_data = overall_sensitivity_alignment,
  y_col = "SensitivityAlignmentPercent"
)

cue_sensitivity_framing_trends <- add_bayesian_trend_evidence(
  trend_table = cue_sensitivity_framing_trends, source_data = bind_rows(
    group_framing_sensitivity_alignment %>%
      mutate(Cue = "Group size"), kinship_framing_sensitivity_alignment %>%
      mutate(Cue = "Kinship")
  ) %>%
    mutate(Framing = recode(Framing, P = "Positive framing", N = "Negative framing")),
  by_cols = c("Cue", "Framing"), y_col = "SensitivityAlignmentPercent"
)


# 9) Export analysis results
# Combined sensitivity exports
cue_sensitivity_alignment <- bind_rows(
  group_size_sensitivity_alignment %>%
    transmute(Cue = "Group size", Agent, Generation, Sensitivity = GroupSensitivity,
    HumanSensitivity = HumanGroupSensitivity, Distance = GroupDistance, SensitivityAlignmentPercent),
  kinship_sensitivity_alignment %>%
    transmute(Cue = "Kinship", Agent, Generation, Sensitivity = KinshipSensitivity,
    HumanSensitivity = HumanKinshipSensitivity, Distance = KinshipDistance, SensitivityAlignmentPercent)
)

cue_framing_sensitivity_alignment <- bind_rows(
  group_framing_sensitivity_alignment %>%
    transmute(Cue = "Group size", Agent, Generation, Framing,
    Sensitivity = GroupSensitivity, HumanSensitivity = HumanGroupSensitivity, Distance = GroupDistance,
    SensitivityAlignmentPercent),
  kinship_framing_sensitivity_alignment %>%
    transmute(Cue = "Kinship", Agent, Generation, Framing,
    Sensitivity = KinshipSensitivity, HumanSensitivity = HumanKinshipSensitivity, Distance = KinshipDistance,
    SensitivityAlignmentPercent)
) %>%
  mutate(Framing = recode(as.character(Framing), P = "Positive framing", N = "Negative framing"))

bootstrap_adaptive_alignment <- map_dfr(socialcue_bootstrap_results, "overall")

write.xlsx(
  list(
    Bootstrap_alignment = bootstrap_adaptive_alignment,
    Alignment_By_Cue_Framing = adaptive_framing_alignment, Alignment_By_Cue = adaptive_cue_alignment,
    Alignment_Overall = adaptive_overall_alignment, Alignment_Cue_Framing_Trend = adaptive_framing_trends,
    Alignment_Cue_Trend = adaptive_cue_trends, Alignment_Overall_Trend = adaptive_primary_trends,
    Baseline_Gap_By_Cue_Framing = adaptive_framing_gap, Baseline_Gap_By_Cue = adaptive_cue_gap,
    Baseline_Gap_Overall = adaptive_overall_gap, Baseline_Gap_Cue_Framing_Trend = adaptive_framing_gap_trends,
    Baseline_Gap_Cue_Trend = adaptive_cue_gap_trends, Baseline_Gap_Overall_Trend = adaptive_overall_gap_result,
    FE_Alignment_By_Cue = framing_effect_alignment, FE_Alignment_Overall = framing_effect_overall_alignment,
    FE_Alignment_Cue_Trend = framing_effect_cue_trends,
    FE_Alignment_Overall_Trend = framing_effect_overall_trend_result,
    FE_Baseline_Gap_By_Cue = framing_effect_cue_gap, FE_Baseline_Gap_Overall = framing_effect_overall_gap,
    FE_Baseline_Gap_Cue_Trend = framing_effect_cue_gap_trends,
    FE_Baseline_Gap_Overall_Trend = framing_effect_overall_gap_result,
    Sens_Alignment_By_Cue_Framing = cue_framing_sensitivity_alignment,
    Sens_Alignment_By_Cue = cue_sensitivity_alignment,
    Sens_Alignment_Overall = overall_sensitivity_alignment,
    Sens_Align_Cue_Framing_Trend = cue_sensitivity_framing_trends,
    Sens_Alignment_Cue_Trend = cue_sensitivity_trends,
    Sens_Alignment_Overall_Trend = overall_sensitivity_trend_result
  ), file = file.path(output_dir, "Results_socialcue.xlsx"), overwrite = TRUE
)


# 10) Completion messages
message("Adaptive social cue analysis completed.")
message("Results saved to: ", file.path(output_dir, "Results_socialcue.xlsx"))


# 11) Visualization: Adaptive social cue development
# Overall panel on the left; group size above kinship on the right.
# Human: blue circles; extreme risk aversion: green triangles.
# Use the existing significance flags for fitted trends and gap labels.

# 11.1) Figure settings
blue_color <- "#3b85bd"

green_color <- "#9db648"

# More visible fitted trend lines
blue_trend_color <- scales::alpha(blue_color, 0.80)

green_trend_color <- scales::alpha(green_color, 0.80)

reference_color <- "grey70"

# geom_text() uses mm-based sizing.
# Convert to approximately 6 pt.
figure_text_size <- 6 / ggplot2::.pt

# 11.2) Helper function for significant baseline-gap p labels
# Display significant gap p values for extreme risk aversion minus empirical human alignment.
format_gap_p <- function(p){
  if(length(p) == 0 || is.na(p) || p > 0.050){
    return(NA_character_)
  }
  if(p < 0.001){
    return("'RA-Human gap: ' * italic(p) < .001")
  }else{
    p_text <- sub("^0", "", sprintf("%.3f", p))

    return(paste0("'RA-Human gap: ' * italic(p) == ", p_text))
  }
}

# 11.3) Prepare display baseline labels
baseline_display_levels <- c("Empirical human baseline", "Extreme risk-averse")

make_display_baseline <- function(x){
  case_when(
    as.character(x) == "Human baseline" ~ "Empirical human baseline",
    as.character(x) == "Deterministic baseline" ~ "Extreme risk-averse",
    TRUE ~ as.character(x)
  )
}

# 11.4) Overall Adaptive cues plot data
adaptive_overall_plot <- adaptive_overall_alignment %>%
  mutate(
    DisplayBaseline = make_display_baseline(Baseline),
    DisplayBaseline = factor(DisplayBaseline, levels = baseline_display_levels)
  ) %>%
  arrange(DisplayBaseline, Generation)

# 11.5) Overall trend significance
overall_human_significant <- adaptive_human_test$p.value <= 0.050

overall_extreme_riskaverse_significant <- adaptive_deterministic_test$p.value <= 0.050

# 11.6) Overall blue-green trend-difference significance
overall_gap_p <- adaptive_overall_gap_result$p_value[1]

overall_gap_label <- format_gap_p(overall_gap_p)

# 11.7) Panel a: Adaptive cues
pan_adaptive_overall <- ggplot(
  adaptive_overall_plot,
  aes(
    x = Generation, y = AlignmentPercent, color = DisplayBaseline, shape = DisplayBaseline, group = DisplayBaseline
  )
) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.30, color = reference_color)

# 11.8) Significant empirical-human trend
if(length(overall_human_significant) == 1 && isTRUE(overall_human_significant)){
  pan_adaptive_overall <- pan_adaptive_overall +
    geom_smooth(
      data = adaptive_overall_plot %>%
        filter(DisplayBaseline == "Empirical human baseline"),
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

# 11.9) Significant extreme-risk-averse trend
if(length(overall_extreme_riskaverse_significant) == 1 && isTRUE(overall_extreme_riskaverse_significant)){
  pan_adaptive_overall <- pan_adaptive_overall +
    geom_smooth(
      data = adaptive_overall_plot %>%
        filter(DisplayBaseline == "Extreme risk-averse"),
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

# 11.10) Complete Panel a
pan_adaptive_overall <- pan_adaptive_overall +
  geom_line(linewidth = 0.48) +
  geom_point(size = 1.75, stroke = 0.70) +
  geom_text(
    data = adaptive_overall_plot %>%
      filter(DisplayBaseline == "Empirical human baseline"),
    aes(x = Generation, y = AlignmentPercent, label = sprintf("%.1f%%", AlignmentPercent)),
    inherit.aes = FALSE,
    color = blue_color,
    size = figure_text_size,
    vjust = -1.00,
    show.legend = FALSE
  ) +
  geom_text(
    data = adaptive_overall_plot %>%
      filter(DisplayBaseline == "Extreme risk-averse"),
    aes(x = Generation, y = AlignmentPercent, label = sprintf("%.1f%%", AlignmentPercent)),
    inherit.aes = FALSE,
    color = green_color,
    size = figure_text_size,
    vjust = 1.45,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Empirical human baseline" = blue_color, "Extreme risk-averse" = green_color)) +
  scale_shape_manual(values = c("Empirical human baseline" = 1, "Extreme risk-averse" = 2)) +
  scale_x_continuous(limits = c(1, 9), breaks = 1:9, labels = model_labels, expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 25),
    labels = scales::label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(ylim = c(0, 105), clip = "off") +
  labs(
    tag = "a",
    title = "Adaptive cues",
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
    legend.position = c(0.51, 0.12),
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

# 11.11) Add significant overall blue-green trend-difference p value
if(!is.na(overall_gap_label)){
  pan_adaptive_overall <- pan_adaptive_overall +
    annotate(
      "text",
      x = 6.6,
      y = 7,
      label = overall_gap_label,
      parse = TRUE,
      color = "black",
      size = figure_text_size,
      hjust = 0.5
    )

}
pan_adaptive_overall


# 12) Cue-specific alignment data
group_alignment_plot <- adaptive_cue_alignment %>%
  filter(Cue == "Group size") %>%
  mutate(
    DisplayBaseline = make_display_baseline(Baseline),
    DisplayBaseline = factor(DisplayBaseline, levels = baseline_display_levels)
  ) %>%
  arrange(DisplayBaseline, Generation)

kinship_alignment_plot <- adaptive_cue_alignment %>%
  filter(Cue == "Kinship") %>%
  mutate(
    DisplayBaseline = make_display_baseline(Baseline),
    DisplayBaseline = factor(DisplayBaseline, levels = baseline_display_levels)
  ) %>%
  arrange(DisplayBaseline, Generation)

# 12.1) Cue-specific trend significance
# Use raw cue-specific significance flags in the figure.
group_human_significant <- adaptive_cue_trends %>%
  filter(Baseline == "Human baseline", Cue == "Group size") %>%
  pull(Significant)

group_extreme_riskaverse_significant <- adaptive_cue_trends %>%
  filter(Baseline == "Deterministic baseline", Cue == "Group size") %>%
  pull(Significant)

kinship_human_significant <- adaptive_cue_trends %>%
  filter(Baseline == "Human baseline", Cue == "Kinship") %>%
  pull(Significant)

kinship_extreme_riskaverse_significant <- adaptive_cue_trends %>%
  filter(Baseline == "Deterministic baseline", Cue == "Kinship") %>%
  pull(Significant)

# 12.2) Cue-specific blue-green trend-difference significance
group_gap_p <- adaptive_cue_gap_trends %>%
  filter(Cue == "Group size") %>%
  pull(p_value)

kinship_gap_p <- adaptive_cue_gap_trends %>%
  filter(Cue == "Kinship") %>%
  pull(p_value)

group_gap_label <- format_gap_p(group_gap_p)

kinship_gap_label <- format_gap_p(kinship_gap_p)


# 13) Panel b: Group-size cue
pan_adaptive_group <- ggplot(
  group_alignment_plot,
  aes(
    x = Generation, y = AlignmentPercent, color = DisplayBaseline, shape = DisplayBaseline, group = DisplayBaseline
  )
) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.30, color = reference_color)

# 13.1) Significant empirical-human trend
if(length(group_human_significant) == 1 && isTRUE(group_human_significant)){
  pan_adaptive_group <- pan_adaptive_group +
    geom_smooth(
      data = group_alignment_plot %>%
        filter(DisplayBaseline == "Empirical human baseline"),
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

# 13.2) Significant extreme-risk-averse trend
if(length(group_extreme_riskaverse_significant) == 1 && isTRUE(group_extreme_riskaverse_significant)){
  pan_adaptive_group <- pan_adaptive_group +
    geom_smooth(
      data = group_alignment_plot %>%
        filter(DisplayBaseline == "Extreme risk-averse"),
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

# 13.3) Complete Panel b
pan_adaptive_group <- pan_adaptive_group +
  geom_line(linewidth = 0.48) +
  geom_point(size = 1.75, stroke = 0.70) +
  geom_text(
    data = group_alignment_plot %>%
      filter(DisplayBaseline == "Empirical human baseline"),
    aes(x = Generation, y = AlignmentPercent, label = sprintf("%.1f%%", AlignmentPercent)),
    inherit.aes = FALSE,
    color = blue_color,
    size = figure_text_size,
    vjust = -0.95,
    show.legend = FALSE
  ) +
  geom_text(
    data = group_alignment_plot %>%
      filter(DisplayBaseline == "Extreme risk-averse"),
    aes(x = Generation, y = AlignmentPercent, label = sprintf("%.1f%%", AlignmentPercent)),
    inherit.aes = FALSE,
    color = green_color,
    size = figure_text_size,
    vjust = 1.35,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Empirical human baseline" = blue_color, "Extreme risk-averse" = green_color)) +
  scale_shape_manual(values = c("Empirical human baseline" = 1, "Extreme risk-averse" = 2)) +
  scale_x_continuous(limits = c(1, 9), breaks = 1:9, labels = model_labels, expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(breaks = seq(0, 100, by = 25), labels = scales::label_number(suffix = "%")) +
  coord_cartesian(ylim = c(0, 105), clip = "off") +
  labs(tag = "b", title = "Group-size cue", x = NULL, y = NULL, color = NULL, shape = NULL) +
  theme_classic(base_size = 6) +
  theme(
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
    plot.margin = margin(4, 5, 2, 4)
  )

# 13.4) Add significant Group-size blue-green trend-difference p value
if(!is.na(group_gap_label)){
  pan_adaptive_group <- pan_adaptive_group +
    annotate(
      "text",
      x = 6.6,
      y = 7,
      label = group_gap_label,
      parse = TRUE,
      color = "black",
      size = figure_text_size,
      hjust = 0.5
    )

}
pan_adaptive_group


# 14) Panel c: Kinship cue
pan_adaptive_kinship <- ggplot(
  kinship_alignment_plot,
  aes(
    x = Generation, y = AlignmentPercent, color = DisplayBaseline, shape = DisplayBaseline, group = DisplayBaseline
  )
) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.30, color = reference_color)

# 14.1) Significant empirical-human trend
if(length(kinship_human_significant) == 1 && isTRUE(kinship_human_significant)){
  pan_adaptive_kinship <- pan_adaptive_kinship +
    geom_smooth(
      data = kinship_alignment_plot %>%
        filter(DisplayBaseline == "Empirical human baseline"),
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

# 14.2) Significant extreme-risk-averse trend
if(length(kinship_extreme_riskaverse_significant) == 1 && isTRUE(kinship_extreme_riskaverse_significant)){
  pan_adaptive_kinship <- pan_adaptive_kinship +
    geom_smooth(
      data = kinship_alignment_plot %>%
        filter(DisplayBaseline == "Extreme risk-averse"),
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

# 14.3) Complete Panel c
pan_adaptive_kinship <- pan_adaptive_kinship +
  geom_line(linewidth = 0.48) +
  geom_point(size = 1.75, stroke = 0.70) +
  geom_text(
    data = kinship_alignment_plot %>%
      filter(DisplayBaseline == "Empirical human baseline"),
    aes(x = Generation, y = AlignmentPercent, label = sprintf("%.1f%%", AlignmentPercent)),
    inherit.aes = FALSE,
    color = blue_color,
    size = figure_text_size,
    vjust = -0.95,
    show.legend = FALSE
  ) +
  geom_text(
    data = kinship_alignment_plot %>%
      filter(DisplayBaseline == "Extreme risk-averse"),
    aes(x = Generation, y = AlignmentPercent, label = sprintf("%.1f%%", AlignmentPercent)),
    inherit.aes = FALSE,
    color = green_color,
    size = figure_text_size,
    vjust = 1.35,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Empirical human baseline" = blue_color, "Extreme risk-averse" = green_color)) +
  scale_shape_manual(values = c("Empirical human baseline" = 1, "Extreme risk-averse" = 2)) +
  scale_x_continuous(limits = c(1, 9), breaks = 1:9, labels = model_labels, expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(breaks = seq(0, 100, by = 25), labels = scales::label_number(suffix = "%")) +
  coord_cartesian(ylim = c(0, 105), clip = "off") +
  labs(tag = "c", title = "Kinship cue", x = "LLM generation", y = NULL, color = NULL, shape = NULL) +
  theme_classic(base_size = 6) +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1, color = "black", size = 6),
    axis.text.y = element_text(color = "black", size = 6),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 6),
    legend.position = "none",
    plot.title.position = "panel",
    plot.title = element_text(size = 6, face = "bold", hjust = 0.5),
    plot.tag = element_text(face = "bold", size = 6),
    plot.tag.position = c(0, 1),
    plot.margin = margin(2, 5, 4, 4)
  )

# 14.4) Add significant Kinship blue-green trend-difference p value
if(!is.na(kinship_gap_label)){
  pan_adaptive_kinship <- pan_adaptive_kinship +
    annotate(
      "text",
      x = 6.6,
      y = 7,
      label = kinship_gap_label,
      parse = TRUE,
      color = "black",
      size = figure_text_size,
      hjust = 0.5
    )

}
pan_adaptive_kinship


# 15) Final three-panel layout
# Panel a occupies the full left column.
# Panels b and c are stacked in the right column.
adaptive_design <- "
AB
AC
"

Pic_adaptive_complete <-
  pan_adaptive_overall +
  pan_adaptive_group +
  pan_adaptive_kinship +
  plot_layout(design = adaptive_design, widths = c(1, 1), heights = c(1, 1))


# 16) Export final combined figure
ggsave(
  file.path(output_dir, "viso_socialcue_complete.pdf"),
  plot = Pic_adaptive_complete,
  width = 6,
  height = 3,
  units = "in",
  device = grDevices::pdf
)


# 17) Completion messages
message("Adaptive social cue visualization completed.")
message("Figure saved to: ", file.path(output_dir, "viso_socialcue_complete.pdf"))
