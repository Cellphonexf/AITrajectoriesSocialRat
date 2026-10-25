# LLM evolutionary trajectory
# Theory of Mind: false belief, irony, faux pas
# Dual-reference developmental analysis
# Requires:
# "rawdata_falsebelief.xlsx"
# "rawdata_irony.xlsx"
# "rawdata_fauxpas.xlsx"
# Programmed by Feng XIAO and Yining MAO (updated on 2026-10-23)


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

# Import raw datasets
falsebelief <- read_agent_data("rawdata_falsebelief.xlsx")

irony <- read_agent_data("rawdata_irony.xlsx")

fauxpas <- read_agent_data("rawdata_fauxpas.xlsx")

# Check imported agents


# 2) Scenario-level performance

# 2.1) False belief
falsebelief_long <- falsebelief %>%
  pivot_longer(cols = -c(No., Agent), names_to = "Scenario", values_to = "Response")

falsebelief_summary <- falsebelief_long %>%
  group_by(Agent, Scenario) %>%
  summarise(
    N_Response = sum(!is.na(Response)),
    Accuracy = ifelse(N_Response > 0, mean(as.numeric(Response), na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# 2.2) Irony
irony_long <- irony %>%
  pivot_longer(cols = -c(No., Agent), names_to = "Scenario", values_to = "Response")

irony_summary <- irony_long %>%
  group_by(Agent, Scenario) %>%
  summarise(
    N_Response = sum(!is.na(Response)),
    Accuracy = ifelse(N_Response > 0, mean(as.numeric(Response), na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

# 2.3) Faux pas
fauxpas_long <- fauxpas %>%
  pivot_longer(cols = -c(No., Agent), names_to = "Scenario", values_to = "Response")

fauxpas_summary <- fauxpas_long %>%
  group_by(Agent, Scenario) %>%
  summarise(
    N_Response = sum(!is.na(Response)),
    Accuracy = ifelse(N_Response > 0, mean(as.numeric(Response), na.rm = TRUE), NA_real_),
    .groups = "drop"
  )


# 3) Module-level performance

# 3.1) False belief
falsebelief_module <- falsebelief_summary %>%
  group_by(Agent) %>%
  summarise(N_Scenarios = sum(!is.na(Accuracy)), Accuracy = mean(Accuracy, na.rm = TRUE), .groups = "drop") %>%
  mutate(Module = "False belief")

# 3.2) Irony
irony_module <- irony_summary %>%
  group_by(Agent) %>%
  summarise(N_Scenarios = sum(!is.na(Accuracy)), Accuracy = mean(Accuracy, na.rm = TRUE), .groups = "drop") %>%
  mutate(Module = "Irony")

# 3.3) Faux pas
fauxpas_module <- fauxpas_summary %>%
  group_by(Agent) %>%
  summarise(N_Scenarios = sum(!is.na(Accuracy)), Accuracy = mean(Accuracy, na.rm = TRUE), .groups = "drop") %>%
  mutate(Module = "Faux pas")

# 3.4) Combine ToM module performance
tom_module_performance <- bind_rows(falsebelief_module, irony_module, fauxpas_module) %>%
  mutate(
    Module = factor(Module, levels = c("False belief", "Irony", "Faux pas")),
    Generation = match(Agent, model_order)
  ) %>%
  arrange(Module, Generation)


# 4) MAIN ANALYSIS: Scenario-level dual-reference alignment
# Match each GPT scenario with the SAME empirical-human scenario first.
# Scenario alignment (%) = 100 * (1 - abs(P_LLM_scenario - P_Human_scenario)).
# Within each module, average scenario-level alignments equally.
# False belief, irony, and faux pas are then equally weighted in section 5.
# Perfect-detection reference (100% accuracy) is likewise computed per scenario.

# 4.1) Combine scenario-level accuracy data, including human observations
# Keep module and scenario keys to prevent inaccurate aggregate-to-aggregate matching.
tom_scenario_performance <- bind_rows(
  falsebelief_summary %>% mutate(Module = "False belief"),
  irony_summary %>% mutate(Module = "Irony"),
  fauxpas_summary %>% mutate(Module = "Faux pas")
) %>%
  mutate(
    Module = factor(Module, levels = c("False belief", "Irony", "Faux pas")),
    Generation = match(Agent, model_order)
  ) %>%
  arrange(Module, Scenario, Generation)

# 4.2) Human reference: one accuracy per module x scenario
human_tom_scenario_reference <- tom_scenario_performance %>%
  filter(Agent == "human") %>%
  select(Module, Scenario, HumanAccuracy = Accuracy)

# All GPT scenarios should have a matching human reference.
# Missing keys would silently inflate alignment if omitted, so fail explicitly.
missing_human_scenarios <- tom_scenario_performance %>%
  filter(!is.na(Generation)) %>%
  anti_join(human_tom_scenario_reference, by = c("Module", "Scenario"))
if (nrow(missing_human_scenarios) > 0) {
  stop("Unmatched human scenarios: verify Module and Scenario names in the three rawdata files.")
}
if (anyNA(human_tom_scenario_reference$HumanAccuracy)) {
  stop("Some human scenarios have no valid accuracy. Verify the raw human data.")
}

# 4.3) Human-baseline alignment at the SCENARIO level
# Mirrors adaptiveCue: first absolute difference per condition, then mean alignment.
tom_human_scenario_alignment <- tom_scenario_performance %>%
  filter(!is.na(Generation)) %>%
  left_join(human_tom_scenario_reference, by = c("Module", "Scenario")) %>%
  mutate(
    SignedDeviation = Accuracy - HumanAccuracy,
    AbsoluteDistance = abs(SignedDeviation),
    Alignment = 1 - AbsoluteDistance,
    AlignmentPercent = 100 * Alignment,
    Baseline = "Human baseline"
  ) %>%
  arrange(Module, Generation, Scenario)

# 4.4) Average scenario-level human alignment within each module
# Accuracy and HumanAccuracy columns are descriptive module averages only.
# Crucially, AlignmentPercent is NOT recalculated from those module averages.
tom_human_alignment <- tom_human_scenario_alignment %>%
  group_by(Agent, Generation, Module) %>%
  summarise(
    N_Scenarios = sum(complete.cases(Accuracy, HumanAccuracy)),
    Accuracy = mean(Accuracy, na.rm = TRUE),
    HumanAccuracy = mean(HumanAccuracy, na.rm = TRUE),
    SignedDeviation = mean(SignedDeviation, na.rm = TRUE),
    AbsoluteDistance = mean(AbsoluteDistance, na.rm = TRUE),
    AlignmentPercent = 100 * mean(Alignment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Baseline = "Human baseline") %>%
  select(
    Agent, Generation, Module, Baseline, Accuracy, HumanAccuracy, SignedDeviation, AbsoluteDistance,
    AlignmentPercent, N_Scenarios
  ) %>%
  arrange(Module, Generation)

# 4.5) Perfect-detection alignment at the scenario level
# Algebraically unchanged from the old calculation because all accuracies are in [0, 1].
tom_deterministic_scenario_alignment <- tom_scenario_performance %>%
  filter(!is.na(Generation)) %>%
  mutate(
    DeterministicAccuracy = 1,
    SignedDeviation = Accuracy - DeterministicAccuracy,
    AbsoluteDistance = abs(SignedDeviation),
    Alignment = 1 - AbsoluteDistance,
    AlignmentPercent = 100 * Alignment,
    Baseline = "Deterministic baseline"
  ) %>%
  arrange(Module, Generation, Scenario)

tom_deterministic_alignment <- tom_deterministic_scenario_alignment %>%
  group_by(Agent, Generation, Module) %>%
  summarise(
    N_Scenarios = sum(!is.na(Accuracy)),
    Accuracy = mean(Accuracy, na.rm = TRUE),
    DeterministicAccuracy = 1,
    AbsoluteDistance = mean(AbsoluteDistance, na.rm = TRUE),
    AlignmentPercent = 100 * mean(Alignment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Baseline = "Deterministic baseline") %>%
  select(
    Agent, Generation, Module, Baseline, Accuracy, DeterministicAccuracy, AbsoluteDistance,
    AlignmentPercent, N_Scenarios
  ) %>%
  arrange(Module, Generation)

# 4.6) Scenario-level audit table (new supplementary export)
tom_scenario_alignment <- bind_rows(
  tom_human_scenario_alignment %>%
    transmute(Agent, Generation, Module, Scenario, N_Response, Baseline,
              Accuracy, ReferenceAccuracy = HumanAccuracy, SignedDeviation,
              AbsoluteDistance, AlignmentPercent),
  tom_deterministic_scenario_alignment %>%
    transmute(Agent, Generation, Module, Scenario, N_Response, Baseline,
              Accuracy, ReferenceAccuracy = DeterministicAccuracy, SignedDeviation,
              AbsoluteDistance, AlignmentPercent)
) %>%
  mutate(Baseline = factor(Baseline, levels = c("Human baseline", "Deterministic baseline"))) %>%
  arrange(Baseline, Module, Generation, Scenario)

# 4.7) Combine dual-reference module alignment (same object/columns as original)
tom_module_alignment <- bind_rows(
  tom_human_alignment %>%
    select(Agent, Generation, Module, Baseline, AlignmentPercent, N_Scenarios),
  tom_deterministic_alignment %>%
    select(Agent, Generation, Module, Baseline, AlignmentPercent, N_Scenarios)
) %>%
  mutate(
    Module = factor(Module, levels = c("False belief", "Irony", "Faux pas")),
    Baseline = factor(Baseline, levels = c("Human baseline", "Deterministic baseline"))
  ) %>%
  arrange(Baseline, Module, Generation)


# 5) Overall Theory of Mind / Social inference alignment
# The three ToM modules are equally weighted.
tom_overall_alignment <- tom_module_alignment %>%
  group_by(Agent, Generation, Baseline) %>%
  summarise(
    N_Modules = sum(!is.na(AlignmentPercent)),
    AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Module = "Social inference") %>%
  select(Agent, Generation, Module, Baseline, AlignmentPercent, N_Modules) %>%
  arrange(Baseline, Generation)

# 5.1) PRIMARY: Overall Human-baseline developmental trajectory
tom_human_trend <- tom_overall_alignment %>%
  filter(Baseline == "Human baseline")

tom_human_test <- cor.test(tom_human_trend$Generation, tom_human_trend$AlignmentPercent, method = "spearman", exact = FALSE)

tom_human_trend_result <- data.frame(
  Baseline = "Human baseline",
  N_Models = sum(complete.cases(tom_human_trend$Generation, tom_human_trend$AlignmentPercent)),
  Spearman_rho = unname(tom_human_test$estimate),
  p_value = tom_human_test$p.value,
  Significant = tom_human_test$p.value <= 0.050,
  Direction = case_when(
      unname(tom_human_test$estimate) > 0 ~ "Increasing alignment",
      unname(tom_human_test$estimate) < 0 ~ "Decreasing alignment",
      TRUE ~ "No monotonic direction"
    )
)

# 5.2) PRIMARY: Overall Deterministic-baseline developmental trajectory
tom_deterministic_trend <- tom_overall_alignment %>%
  filter(Baseline == "Deterministic baseline")

tom_deterministic_test <- cor.test(
  tom_deterministic_trend$Generation,
  tom_deterministic_trend$AlignmentPercent,
  method = "spearman",
  exact = FALSE
)

tom_deterministic_trend_result <- data.frame(
  Baseline = "Deterministic baseline",
  N_Models = sum(complete.cases(tom_deterministic_trend$Generation, tom_deterministic_trend$AlignmentPercent)),
  Spearman_rho = unname(tom_deterministic_test$estimate),
  p_value = tom_deterministic_test$p.value,
  Significant = tom_deterministic_test$p.value <= 0.050,
  Direction = case_when(
      unname(tom_deterministic_test$estimate) > 0 ~ "Increasing alignment",
      unname(tom_deterministic_test$estimate) < 0 ~ "Decreasing alignment",
      TRUE ~ "No monotonic direction"
    )
)

# 5.3) Summary of primary developmental trajectories
tom_primary_trends <- bind_rows(tom_human_trend_result, tom_deterministic_trend_result)


# 6) Module-specific developmental trajectories
# False belief, Irony, and Faux pas represent three distinct social-inference tests
# corresponding to separate hypotheses.
# Therefore, the developmental trajectory for each module is evaluated independently.
# No multiple-comparison correction is applied across the three modules.
tom_module_trends <- tom_module_alignment %>%
  group_by(Baseline, Module) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, AlignmentPercent)),
    Spearman_rho = cor(Generation, AlignmentPercent, method = "spearman", use = "complete.obs"),
    p_value = cor.test(Generation, AlignmentPercent, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    Correction = "None: False belief, Irony, and Faux pas are treated as separate hypothesis tests",
    Significant = p_value <= 0.050,
    Direction = case_when(
        Spearman_rho > 0 ~ "Increasing alignment",
        Spearman_rho < 0 ~ "Decreasing alignment",
        TRUE ~ "No monotonic direction"
      )
  )


# 7) Direct Human-vs-Deterministic developmental comparison
# BaselineGap = perfect-detection alignment minus human alignment.

# 7.1) Overall Social inference baseline gap
tom_overall_gap <- tom_overall_alignment %>%
  select(Agent, Generation, Baseline, AlignmentPercent) %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`) %>%
  arrange(Generation)

# 7.2) Overall baseline-gap trend
tom_overall_gap_test <- cor.test(tom_overall_gap$Generation, tom_overall_gap$BaselineGap, method = "spearman", exact = FALSE)

tom_overall_gap_result <- data.frame(
  Analysis = "Deterministic minus Human baseline alignment",
  N_Models = sum(complete.cases(tom_overall_gap$Generation, tom_overall_gap$BaselineGap)),
  Spearman_rho = unname(tom_overall_gap_test$estimate),
  p_value = tom_overall_gap_test$p.value,
  Significant = tom_overall_gap_test$p.value <= 0.050
)

# 7.3) Module-specific baseline gaps
tom_module_gap <- tom_module_alignment %>%
  select(Agent, Generation, Module, Baseline, AlignmentPercent) %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`) %>%
  arrange(Module, Generation)

# 7.4) Module-specific baseline-gap trends
# False belief, Irony, and Faux pas correspond to separate hypothesis tests.
# Their Human-vs-perfect-detection gap trajectories are therefore tested independently.
# No multiple-comparison correction is applied across the three modules.
tom_module_gap_trends <- tom_module_gap %>%
  group_by(Module) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, BaselineGap)),
    Spearman_rho = cor(Generation, BaselineGap, method = "spearman", use = "complete.obs"),
    p_value = cor.test(Generation, BaselineGap, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    Correction = "None: False belief, Irony, and Faux pas are treated as separate hypothesis tests",
    Significant = p_value <= 0.050,
    Direction = case_when(
        Spearman_rho > 0 ~ "Increasing relative perfect-detection alignment",
        Spearman_rho < 0 ~ "Increasing relative empirical-human alignment",
        TRUE ~ "No monotonic direction"
      )
  )


# 9) Irony context-specific analysis

# 9.1) Extract irony context
irony_context_data <- irony_summary %>%
  mutate(
    Context = case_when(
      str_detect(Scenario, "-I$") ~ "Irony context",
      str_detect(Scenario, "-NI$") ~ "Non-irony context",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Context))

# 9.2) Context-specific accuracy
irony_context_performance <- irony_context_data %>%
  group_by(Agent, Context) %>%
  summarise(N_Scenarios = sum(!is.na(Accuracy)), Accuracy = mean(Accuracy, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    Context = factor(Context, levels = c("Irony context", "Non-irony context")),
    Generation = match(Agent, model_order)
  ) %>%
  arrange(Context, Generation)

# 9.3) Human-baseline alignment within each irony context
# IMPORTANT: compute scenario alignment first; never compare context means.
irony_context_human_alignment <- tom_human_scenario_alignment %>%
  filter(Module == "Irony") %>%
  mutate(
    Context = case_when(
      str_detect(Scenario, "-I$") ~ "Irony context",
      str_detect(Scenario, "-NI$") ~ "Non-irony context",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Context)) %>%
  group_by(Agent, Generation, Context) %>%
  summarise(
    N_Scenarios = sum(complete.cases(Accuracy, HumanAccuracy)),
    Accuracy = mean(Accuracy, na.rm = TRUE),
    HumanAccuracy = mean(HumanAccuracy, na.rm = TRUE),
    SignedDeviation = mean(SignedDeviation, na.rm = TRUE),
    AbsoluteDistance = mean(AbsoluteDistance, na.rm = TRUE),
    AlignmentPercent = 100 * mean(Alignment, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Context = factor(Context, levels = c("Irony context", "Non-irony context")),
    Baseline = "Human baseline"
  ) %>%
  arrange(Context, Generation)

# 9.5) Deterministic-baseline alignment by irony context
irony_context_deterministic_alignment <- irony_context_performance %>%
  filter(Agent != "human") %>%
  mutate(
    DeterministicAccuracy = 1,
    AbsoluteDistance = abs(Accuracy - DeterministicAccuracy),
    AlignmentPercent = 100 *
      (1 - AbsoluteDistance),
    Baseline = "Deterministic baseline"
  ) %>%
  filter(!is.na(Generation))

# 9.6) Combine irony-context dual-reference alignment
irony_context_alignment <- bind_rows(
  irony_context_human_alignment %>%
    select(Agent, Generation, Context, Baseline, Accuracy, AlignmentPercent, N_Scenarios),
  irony_context_deterministic_alignment %>%
    select(Agent, Generation, Context, Baseline, Accuracy, AlignmentPercent, N_Scenarios)
) %>%
  mutate(
    Context = factor(Context, levels = c("Irony context", "Non-irony context")),
    Baseline = factor(Baseline, levels = c("Human baseline", "Deterministic baseline"))
  ) %>%
  arrange(Baseline, Context, Generation)

# 9.7) Context-specific developmental trends
irony_context_trends_raw <- irony_context_alignment %>%
  group_by(Baseline, Context) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, AlignmentPercent)),
    Spearman_rho = cor(Generation, AlignmentPercent, method = "spearman", use = "complete.obs"),
    p_raw = cor.test(Generation, AlignmentPercent, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  )

irony_context_trends <- irony_context_trends_raw %>%
  group_by(Baseline) %>%
  mutate(
    p_adjusted = p.adjust(p_raw, method = "holm"),
    Correction = "Holm within baseline (2 irony-context tests)",
    Significant_raw = p_raw <= 0.050,
    Significant_adjusted = p_adjusted <= 0.050,
    Direction = case_when(
        Spearman_rho > 0 ~ "Increasing alignment",
        Spearman_rho < 0 ~ "Decreasing alignment",
        TRUE ~ "No monotonic direction"
      )
  ) %>%
  ungroup()

# 9.8) Irony-context baseline gaps
irony_context_gap <- irony_context_alignment %>%
  select(Agent, Generation, Context, Baseline, AlignmentPercent) %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`) %>%
  arrange(Context, Generation)

# 9.9) Irony-context baseline-gap trends
irony_context_gap_trends <- irony_context_gap %>%
  group_by(Context) %>%
  summarise(
    N_Models = sum(complete.cases(Generation, BaselineGap)),
    Spearman_rho = cor(Generation, BaselineGap, method = "spearman", use = "complete.obs"),
    p_raw = cor.test(Generation, BaselineGap, method = "spearman", exact = FALSE)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    p_adjusted = p.adjust(p_raw, method = "holm"),
    Correction = "Holm (2 irony-context baseline-gap tests)",
    Significant_raw = p_raw <= 0.050,
    Significant_adjusted = p_adjusted <= 0.050,
    Direction = case_when(
        Spearman_rho > 0 ~ "Increasing relative deterministic alignment",
        Spearman_rho < 0 ~ "Increasing relative human alignment",
        TRUE ~ "No monotonic direction"
      )
  )

# 11.1) Bootstrap Human-baseline Social-inference alignment
set.seed(2632)
n_bootstrap <- 1000

bootstrap_tom_module_once <- function(long_data, module_name, bootstrap_id){
  # Resample LLM responses WITHIN each scenario, as in the adaptiveCue script.
  bootstrap_scenario <- long_data %>%
    filter(Agent != "human", !is.na(Response)) %>%
    group_by(Agent, Scenario) %>%
    summarise(
      N_Response = n(),
      Accuracy = mean(as.numeric(sample(Response, size = n(), replace = TRUE)), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(BootstrapID = bootstrap_id, Generation = match(Agent, model_order), Module = module_name) %>%
    filter(!is.na(Generation))

  list(scenario = bootstrap_scenario)
}

bootstrap_tom_once <- function(bootstrap_id){
  bootstrap_falsebelief <- bootstrap_tom_module_once(falsebelief_long, "False belief", bootstrap_id)
  bootstrap_irony <- bootstrap_tom_module_once(irony_long, "Irony", bootstrap_id)
  bootstrap_fauxpas <- bootstrap_tom_module_once(fauxpas_long, "Faux pas", bootstrap_id)

  bootstrap_scenario_performance <- bind_rows(
    bootstrap_falsebelief$scenario,
    bootstrap_irony$scenario,
    bootstrap_fauxpas$scenario
  ) %>%
    select(BootstrapID, Agent, Generation, Module, Scenario, N_Response, Accuracy)

  # Human accuracy is matched within the SAME module and scenario.
  # Average absolute distances (NOT the absolute difference in mean accuracies).
  bootstrap_module_alignment <- bootstrap_scenario_performance %>%
    left_join(human_tom_scenario_reference, by = c("Module", "Scenario")) %>%
    group_by(BootstrapID, Agent, Generation, Module) %>%
    summarise(
      N_Scenarios = sum(complete.cases(Accuracy, HumanAccuracy)),
      AlignmentPercent = 100 * mean(1 - abs(Accuracy - HumanAccuracy), na.rm = TRUE),
      Accuracy = mean(Accuracy, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Baseline = "Human baseline")

  bootstrap_overall_alignment <- bootstrap_module_alignment %>%
    group_by(BootstrapID, Agent, Generation) %>%
    summarise(
      N_Modules = sum(!is.na(AlignmentPercent)),
      AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Module = "Social inference", Baseline = "Human baseline") %>%
    select(BootstrapID, Agent, Generation, Module, Baseline, AlignmentPercent, N_Modules)

  list(
    scenario = bootstrap_scenario_performance,
    module = bootstrap_module_alignment,
    overall = bootstrap_overall_alignment
  )
}

tom_bootstrap_results <- map(seq_len(n_bootstrap), bootstrap_tom_once)

bootstrap_tom_scenario_performance <- map_dfr(tom_bootstrap_results, "scenario")

bootstrap_tom_module_alignment <- map_dfr(tom_bootstrap_results, "module")

# 11.2) Bootstrap confidence intervals for monotonic trends
bootstrap_tom_module_dual_alignment <- bind_rows(
  bootstrap_tom_module_alignment %>%
    transmute(BootstrapID, Agent, Generation, Module, Baseline = "Human baseline", AlignmentPercent),
  bootstrap_tom_module_alignment %>%
    transmute(
      BootstrapID, Agent, Generation, Module, Baseline = "Deterministic baseline", AlignmentPercent = 100 *
        (1 - abs(Accuracy - 1))
    )
)

bootstrap_tom_overall_dual_alignment <- bootstrap_tom_module_dual_alignment %>%
  group_by(BootstrapID, Agent, Generation, Baseline) %>%
  summarise(AlignmentPercent = mean(AlignmentPercent, na.rm = TRUE), .groups = "drop")

bootstrap_tom_module_gap <- bootstrap_tom_module_dual_alignment %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`)

bootstrap_tom_overall_gap <- bootstrap_tom_overall_dual_alignment %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`)

# Irony-context bootstrap also uses per-scenario human alignment.
bootstrap_irony_context_performance <- bootstrap_tom_scenario_performance %>%
  filter(Module == "Irony") %>%
  mutate(
    Context = case_when(
      str_detect(Scenario, "-I$") ~ "Irony context",
      str_detect(Scenario, "-NI$") ~ "Non-irony context",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Context)) %>%
  left_join(human_tom_scenario_reference, by = c("Module", "Scenario")) %>%
  group_by(BootstrapID, Agent, Generation, Context) %>%
  summarise(
    HumanAlignmentPercent = 100 * mean(1 - abs(Accuracy - HumanAccuracy), na.rm = TRUE),
    Accuracy = mean(Accuracy, na.rm = TRUE),
    .groups = "drop"
  )

bootstrap_irony_context_dual_alignment <- bind_rows(
  bootstrap_irony_context_performance %>%
    transmute(
      BootstrapID, Agent, Generation, Context,
      Baseline = "Human baseline", AlignmentPercent = HumanAlignmentPercent
    ),
  bootstrap_irony_context_performance %>%
    transmute(
      BootstrapID, Agent, Generation, Context,
      Baseline = "Deterministic baseline", AlignmentPercent = 100 * (1 - abs(Accuracy - 1))
    )
)

bootstrap_irony_context_gap <- bootstrap_irony_context_dual_alignment %>%
  pivot_wider(names_from = Baseline, values_from = AlignmentPercent) %>%
  mutate(BaselineGap = `Deterministic baseline` - `Human baseline`)

bootstrap_trend_values <- bind_rows(
  bootstrap_tom_overall_dual_alignment %>%
    transmute(BootstrapID, Generation, Analysis = "Overall alignment", Baseline, Module = NA_character_, Context = NA_character_, Value = AlignmentPercent),
  bootstrap_tom_module_dual_alignment %>%
    transmute(BootstrapID, Generation, Analysis = "Module alignment", Baseline, Module, Context = NA_character_, Value = AlignmentPercent),
  bootstrap_tom_overall_gap %>%
    transmute(BootstrapID, Generation, Analysis = "Overall baseline gap", Baseline = NA_character_, Module = NA_character_, Context = NA_character_, Value = BaselineGap),
  bootstrap_tom_module_gap %>%
    transmute(BootstrapID, Generation, Analysis = "Module baseline gap", Baseline = NA_character_, Module, Context = NA_character_, Value = BaselineGap),
  bootstrap_irony_context_dual_alignment %>%
    transmute(BootstrapID, Generation, Analysis = "Irony-context alignment", Baseline, Module = NA_character_, Context, Value = AlignmentPercent),
  bootstrap_irony_context_gap %>%
    transmute(BootstrapID, Generation, Analysis = "Irony-context baseline gap", Baseline = NA_character_, Module = NA_character_, Context, Value = BaselineGap)
)

bootstrap_trend_rho <- bootstrap_trend_values %>%
  group_by(BootstrapID, Analysis, Baseline, Module, Context) %>%
  summarise(Spearman_rho = cor(Generation, Value, method = "spearman", use = "complete.obs"), .groups = "drop")

bootstrap_trend_ci <- bootstrap_trend_rho %>%
  filter(is.finite(Spearman_rho)) %>%
  group_by(Analysis, Baseline, Module, Context) %>%
    summarise(
    CI_Lower = quantile(Spearman_rho, probs = 0.025, na.rm = TRUE, names = FALSE), CI_Upper =
      quantile(Spearman_rho, probs = 0.975, na.rm = TRUE, names = FALSE), CI_Method = "Percentile bootstrap",
    Confidence_Level = 0.95, N_Boot_Valid = n(), .groups = "drop"
  )

tom_primary_trends <- tom_primary_trends %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Overall alignment") %>%
      select(Baseline, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid), by = "Baseline"
  )

tom_module_trends <- tom_module_trends %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Module alignment") %>%
      select(Baseline, Module, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid),
    by = c("Baseline", "Module")
  )

tom_overall_gap_result <- bind_cols(
  tom_overall_gap_result, bootstrap_trend_ci %>%
    filter(Analysis == "Overall baseline gap") %>%
    select(CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid)
)

tom_module_gap_trends <- tom_module_gap_trends %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Module baseline gap") %>%
      select(Module, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid), by = "Module"
  )

irony_context_trends <- irony_context_trends %>%
  mutate(Context = as.character(Context)) %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Irony-context alignment") %>%
      select(Baseline, Context, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid),
    by = c("Baseline", "Context")
  )

irony_context_gap_trends <- irony_context_gap_trends %>%
  mutate(Context = as.character(Context)) %>%
  left_join(
    bootstrap_trend_ci %>%
      filter(Analysis == "Irony-context baseline gap") %>%
      select(Context, CI_Lower, CI_Upper, CI_Method, Confidence_Level, N_Boot_Valid), by = "Context"
  )

tom_primary_trends <- add_bayesian_trend_evidence(
  trend_table = tom_primary_trends, source_data = tom_overall_alignment, by_cols = "Baseline",
  y_col = "AlignmentPercent"
)

tom_module_trends <- add_bayesian_trend_evidence(
  trend_table = tom_module_trends, source_data = tom_module_alignment, by_cols = c("Baseline", "Module"),
  y_col = "AlignmentPercent"
)

tom_overall_gap_result <- add_bayesian_trend_evidence(trend_table = tom_overall_gap_result, source_data = tom_overall_gap, y_col = "BaselineGap")

tom_module_gap_trends <- add_bayesian_trend_evidence(
  trend_table = tom_module_gap_trends, source_data = tom_module_gap, by_cols = "Module", y_col = "BaselineGap"
)

irony_context_trends <- add_bayesian_trend_evidence(
  trend_table = irony_context_trends, source_data = irony_context_alignment, by_cols = c("Baseline", "Context"),
  y_col = "AlignmentPercent"
)

irony_context_gap_trends <- add_bayesian_trend_evidence(
  trend_table = irony_context_gap_trends, source_data = irony_context_gap, by_cols = "Context",
  y_col = "BaselineGap"
)


# 12) Export analysis results
bootstrap_socialinference_alignment <- map_dfr(tom_bootstrap_results, "overall")

write.xlsx(
  list(
    Bootstrap_alignment = bootstrap_socialinference_alignment,
    Alignment_By_Scenario = tom_scenario_alignment,
    Alignment_By_Task = tom_module_alignment, Alignment_Overall = tom_overall_alignment,
    Alignment_Task_Trend = tom_module_trends, Alignment_Overall_Trend = tom_primary_trends,
    Baseline_Gap_By_Task = tom_module_gap, Baseline_Gap_Overall = tom_overall_gap,
    Baseline_Gap_Task_Trend = tom_module_gap_trends, Baseline_Gap_Overall_Trend = tom_overall_gap_result,
    Irony_Ctx_Alignment = irony_context_alignment, Irony_Ctx_Alignment_Trend = irony_context_trends,
    Irony_Ctx_Baseline_Gap = irony_context_gap, Irony_Ctx_Baseline_Gap_Trend = irony_context_gap_trends
  ), file = file.path(output_dir, "Results_ToM.xlsx"), overwrite = TRUE
)


# 13) Analysis completion messages
message("Theory of Mind dual-reference analysis completed.")
message("Results saved to: ", file.path(output_dir, "Results_ToM.xlsx"))


# 14) Visualization: Social inference development
# Overall panel on the left; false belief, irony and faux pas on the right.
# Human: blue circles; perfect detection: green triangles.
# Show fitted lines only for significant trends using existing significance flags.

# 14.1) Figure settings
blue_color <- "#3b85bd"

green_color <- "#9db648"

blue_trend_color <- scales::alpha(blue_color, 0.80)

green_trend_color <- scales::alpha(green_color, 0.80)

reference_color <- "grey70"

# geom_text() uses mm-based sizing.
# Convert to approximately 6 pt.
figure_text_size <- 6 / ggplot2::.pt

# 14.2) Display-label helpers
baseline_display_levels <- c("Empirical human baseline", "Perfect detection (100%)")

make_display_baseline <- function(x){
  case_when(
    as.character(x) == "Human baseline" ~ "Empirical human baseline",
    as.character(x) == "Deterministic baseline" ~ "Perfect detection (100%)",
    TRUE ~ as.character(x)
  )
}

# 14.4) Overall Social inference plot data
tom_overall_plot <- tom_overall_alignment %>%
  mutate(
    DisplayBaseline = make_display_baseline(Baseline),
    DisplayBaseline = factor(DisplayBaseline, levels = baseline_display_levels)
  ) %>%
  arrange(DisplayBaseline, Generation)

# 14.5) Overall trajectory significance
tom_overall_human_significant <- tom_primary_trends %>%
  filter(Baseline == "Human baseline") %>%
  pull(Significant)

tom_overall_perfect_significant <- tom_primary_trends %>%
  filter(Baseline == "Deterministic baseline") %>%
  pull(Significant)

# 14.7) Panel a: Overall Social inference
pan_tom_overall <- ggplot(
  tom_overall_plot,
  aes(
    x = Generation, y = AlignmentPercent, color = DisplayBaseline, shape = DisplayBaseline, group = DisplayBaseline
  )
) +
  geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.30, color = reference_color)

# 14.8) Significant empirical-human trend
if(length(tom_overall_human_significant) == 1 && isTRUE(tom_overall_human_significant)){
  pan_tom_overall <- pan_tom_overall +
    geom_smooth(
      data = tom_overall_plot %>%
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

# 14.9) Significant perfect-detection trend
if(length(tom_overall_perfect_significant) == 1 && isTRUE(tom_overall_perfect_significant)){
  pan_tom_overall <- pan_tom_overall +
    geom_smooth(
      data = tom_overall_plot %>%
        filter(DisplayBaseline == "Perfect detection (100%)"),
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

# 14.10) Complete Panel a
pan_tom_overall <- pan_tom_overall +
  geom_line(linewidth = 0.48) +
  geom_point(size = 1.75, stroke = 0.70) +
  geom_text(
    data = tom_overall_plot %>%
      filter(DisplayBaseline == "Empirical human baseline"),
    aes(x = Generation, y = AlignmentPercent, label = sprintf("%.1f%%", AlignmentPercent)),
    inherit.aes = FALSE,
    color = blue_color,
    size = figure_text_size,
    vjust = -1.00,
    show.legend = FALSE
  ) +
  geom_text(
    data = tom_overall_plot %>%
      filter(DisplayBaseline == "Perfect detection (100%)"),
    aes(x = Generation, y = AlignmentPercent, label = sprintf("%.1f%%", AlignmentPercent)),
    inherit.aes = FALSE,
    color = green_color,
    size = figure_text_size,
    vjust = 1.45,
    show.legend = FALSE
  ) +
  scale_color_manual(values = c("Empirical human baseline" = blue_color, "Perfect detection (100%)" = green_color)) +
  scale_shape_manual(values = c("Empirical human baseline" = 1, "Perfect detection (100%)" = 2)) +
  scale_x_continuous(limits = c(1, 9), breaks = 1:9, labels = model_labels, expand = expansion(mult = c(0.025, 0.025))) +
  scale_y_continuous(breaks = seq(0, 100, by = 25), labels = scales::label_number(suffix = "%")) +
  coord_cartesian(ylim = c(0, 105), clip = "off") +
  labs(
    tag = "a",
    title = "Social inference",
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
    legend.position = c(0.49, 0.12),
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

pan_tom_overall


# 15) Module-specific plot data
falsebelief_alignment_plot <- tom_module_alignment %>%
  filter(Module == "False belief") %>%
  mutate(
    DisplayBaseline = make_display_baseline(Baseline),
    DisplayBaseline = factor(DisplayBaseline, levels = baseline_display_levels)
  ) %>%
  arrange(DisplayBaseline, Generation)

irony_alignment_plot <- tom_module_alignment %>%
  filter(Module == "Irony") %>%
  mutate(
    DisplayBaseline = make_display_baseline(Baseline),
    DisplayBaseline = factor(DisplayBaseline, levels = baseline_display_levels)
  ) %>%
  arrange(DisplayBaseline, Generation)

fauxpas_alignment_plot <- tom_module_alignment %>%
  filter(Module == "Faux pas") %>%
  mutate(
    DisplayBaseline = make_display_baseline(Baseline),
    DisplayBaseline = factor(DisplayBaseline, levels = baseline_display_levels)
  ) %>%
  arrange(DisplayBaseline, Generation)

# 15.1) Extract module-specific significance
# No p-value correction across False belief, Faux pas, and Irony.
falsebelief_human_significant <- tom_module_trends %>%
  filter(Baseline == "Human baseline", Module == "False belief") %>%
  pull(Significant)

falsebelief_perfect_significant <- tom_module_trends %>%
  filter(Baseline == "Deterministic baseline", Module == "False belief") %>%
  pull(Significant)

irony_human_significant <- tom_module_trends %>%
  filter(Baseline == "Human baseline", Module == "Irony") %>%
  pull(Significant)

irony_perfect_significant <- tom_module_trends %>%
  filter(Baseline == "Deterministic baseline", Module == "Irony") %>%
  pull(Significant)

fauxpas_human_significant <- tom_module_trends %>%
  filter(Baseline == "Human baseline", Module == "Faux pas") %>%
  pull(Significant)

fauxpas_perfect_significant <- tom_module_trends %>%
  filter(Baseline == "Deterministic baseline", Module == "Faux pas") %>%
  pull(Significant)

# 15.3) Helper function for Panels b-d
make_tom_module_panel <- function(plot_data, panel_tag, panel_title, human_significant, perfect_significant, show_x_axis = FALSE){
  p <- ggplot(
    plot_data,
    aes(
      x = Generation, y = AlignmentPercent, color = DisplayBaseline, shape = DisplayBaseline,
      group = DisplayBaseline
    )
  ) +
    geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.30, color = reference_color)

  if(length(human_significant) == 1 && isTRUE(human_significant)){
    p <- p +
      geom_smooth(
        data = plot_data %>%
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
  if(length(perfect_significant) == 1 && isTRUE(perfect_significant)){
    p <- p +
      geom_smooth(
        data = plot_data %>%
          filter(DisplayBaseline == "Perfect detection (100%)"),
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
  p <- p +
    geom_line(linewidth = 0.48) +
    geom_point(size = 1.75, stroke = 0.70) +
    geom_text(
      data = plot_data %>%
        filter(DisplayBaseline == "Empirical human baseline"),
      aes(x = Generation, y = AlignmentPercent, label = sprintf("%.1f%%", AlignmentPercent)),
      inherit.aes = FALSE,
      color = blue_color,
      size = figure_text_size,
      vjust = -0.95,
      show.legend = FALSE
    ) +
    geom_text(
      data = plot_data %>%
        filter(DisplayBaseline == "Perfect detection (100%)"),
      aes(x = Generation, y = AlignmentPercent, label = sprintf("%.1f%%", AlignmentPercent)),
      inherit.aes = FALSE,
      color = green_color,
      size = figure_text_size,
      vjust = 1.35,
      show.legend = FALSE
    ) +
    scale_color_manual(values = c("Empirical human baseline" = blue_color, "Perfect detection (100%)" = green_color)) +
    scale_shape_manual(values = c("Empirical human baseline" = 1, "Perfect detection (100%)" = 2)) +
    scale_x_continuous(limits = c(1, 9), breaks = 1:9, labels = model_labels, expand = expansion(mult = c(0.025, 0.025))) +
    scale_y_continuous(breaks = seq(0, 100, by = 25), labels = scales::label_number(suffix = "%")) +
    coord_cartesian(ylim = c(0, 105), clip = "off") +
    labs(
      tag = panel_tag,
      title = panel_title,
      x = if(show_x_axis){
          "LLM generation"
        }else{
          NULL
        },
      y = NULL,
      color = NULL,
      shape = NULL
    ) +
    theme_classic(base_size = 6) +
    theme(
      axis.text.x = if(show_x_axis){
          element_text(angle = 35, hjust = 1, vjust = 1, color = "black", size = 6)
        }else{
          element_blank()
        },
      axis.ticks.x = if(show_x_axis){
          element_line()
        }else{
          element_blank()
        },
      axis.line.x = if(show_x_axis){
          element_line()
        }else{
          element_blank()
        },
      axis.title.x = if(show_x_axis){
          element_text(color = "black", size = 6)
        }else{
          element_blank()
        },
      axis.text.y = element_text(color = "black", size = 6),
      axis.title.y = element_blank(),
      legend.position = "none",
      plot.title.position = "panel",
      plot.title = element_text(size = 6, face = "bold", hjust = 0.5),
      plot.tag = element_text(face = "bold", size = 6),
      plot.tag.position = c(0, 1),
      plot.margin = margin(4, 5, 4, 4)
    )

  return(p)
}


# 16) Panel b: False-belief detection
pan_tom_falsebelief <- make_tom_module_panel(
  plot_data = falsebelief_alignment_plot,
  panel_tag = "b",
  panel_title = "False-belief detection",
  human_significant = falsebelief_human_significant,
  perfect_significant = falsebelief_perfect_significant,
  show_x_axis = FALSE
)

pan_tom_falsebelief


# 17) Panel c: Irony detection
pan_tom_irony <- make_tom_module_panel(
  plot_data = irony_alignment_plot,
  panel_tag = "c",
  panel_title = "Irony detection",
  human_significant = irony_human_significant,
  perfect_significant = irony_perfect_significant,
  show_x_axis = FALSE
)

pan_tom_irony


# 18) Panel d: Faux-pas detection
pan_tom_fauxpas <- make_tom_module_panel(
  plot_data = fauxpas_alignment_plot,
  panel_tag = "d",
  panel_title = "Faux-pas detection",
  human_significant = fauxpas_human_significant,
  perfect_significant = fauxpas_perfect_significant,
  show_x_axis = TRUE
)

pan_tom_fauxpas


# 19) Fixed combined figure design
# Overall panel spans the left column; three task panels are stacked on the right.
tom_design <- "
AB
AC
AD
"

Pic_tom_complete <-
  pan_tom_overall +
  pan_tom_falsebelief +
  pan_tom_irony +
  pan_tom_fauxpas +
  plot_layout(design = tom_design, widths = c(1, 1), heights = c(1, 1, 1))


# 20) Export final combined ToM figure
ggsave(
  file.path(output_dir, "viso_ToM_complete.pdf"),
  plot = Pic_tom_complete,
  width = 6,
  height = 4,
  units = "in",
  device = grDevices::pdf
)


# 21) Visualization completion messages
message("Combined Social inference visualization completed.")
message("Figure saved to: ", file.path(output_dir, "viso_ToM_complete.pdf"))
