# Temperature robustness check: AQuA-RAT and SAT-MATH
# Programmed by Feng XIAO (2026-11-11)
# Input: rawdata_math.xlsx; GPT-4o and GPT-4.1, temperature 0 vs. 1.
# Temperature 1: 10 responses per item, coded by majority vote.
# Majority coding: >5 correct = 1, <5 = 0; 5:5 ties are excluded.
# Statistical test: two-sided exact McNemar test on paired item scores.


# 0) Preparation
pkg <- c("readxl", "dplyr", "purrr", "openxlsx")
lapply(pkg, require, character.only = TRUE)

# Use the script directory in RStudio.
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_path <- rstudioapi::getActiveDocumentContext()$path
  if (nzchar(script_path)) {
    setwd(dirname(script_path))
  }
}

input_file <- "rawdata_math.xlsx"
output_dir <- "Outputs"
model_order <- c("4o", "4.1")
test_order <- c("AQuA-RAT", "SAT-MATH")
test_items <- c("AQuA-RAT" = 254L, "SAT-MATH" = 220L)


# 1) Data input
required_sheets <- c(model_order, paste0(model_order, "_robustcheck"))
math_raw <- setNames(required_sheets, required_sheets) %>%
  purrr::map(function(sheet_name) readxl::read_excel(input_file, sheet = sheet_name))


# 2) Code item-level accuracy
# Temperature 0: single binary score per item.
# Temperature 1: ten binary scores; 5:5 ties are removed.
math_coded <- purrr::map_dfr(model_order, function(model_name) {
  original <- math_raw[[model_name]]
  robust <- math_raw[[paste0(model_name, "_robustcheck")]]

  purrr::map_dfr(test_order, function(test_name) {
    n_items <- test_items[[test_name]]
    score_cols <- paste0(test_name, 1:10)

    original_test <- original %>%
      dplyr::select(`No.`, Temp0 = dplyr::all_of(test_name)) %>%
      dplyr::filter(`No.` %in% seq_len(n_items))

    robust_test <- robust %>%
      dplyr::select(`No.`, dplyr::all_of(score_cols)) %>%
      dplyr::filter(`No.` %in% seq_len(n_items))

    paired <- dplyr::inner_join(original_test, robust_test, by = "No.") %>%
      dplyr::arrange(`No.`)

    if (nrow(paired) != n_items ||
        anyDuplicated(original_test$`No.`) || anyDuplicated(robust_test$`No.`)) {
      stop(paste("Item matching failed:", model_name, test_name))
    }

    temp0 <- as.numeric(paired$Temp0)
    repetitions <- as.matrix(paired[, score_cols])
    storage.mode(repetitions) <- "numeric"

    if (anyNA(temp0) || anyNA(repetitions) ||
        any(!temp0 %in% c(0, 1)) || any(!repetitions %in% c(0, 1))) {
      stop(paste("Missing or invalid binary scores:", model_name, test_name))
    }

    n_correct <- rowSums(repetitions)
    temp1 <- dplyr::case_when(
      n_correct > 5 ~ 1,
      n_correct < 5 ~ 0,
      TRUE ~ NA_real_
    )

    tibble::tibble(
      Agent = paste0("GPT-", model_name),
      Test = test_name,
      Item = paired$`No.`,
      Temp0 = temp0,
      Temp1 = temp1
    )
  })
})


# 3) Robustness analysis: exact McNemar test
# Compare scores from the SAME items, removing 5:5 ties from both conditions.
math_robustness <- math_coded %>%
  dplyr::group_by(Agent, Test) %>%
  dplyr::group_modify(function(data, key) {
    n_ties <- sum(is.na(data$Temp1))
    valid <- data %>% dplyr::filter(!is.na(Temp1))

    gain <- sum(valid$Temp0 == 0 & valid$Temp1 == 1)
    loss <- sum(valid$Temp0 == 1 & valid$Temp1 == 0)
    n_discordant <- gain + loss

    # Exact two-sided McNemar = binomial test of discordant pairs.
    p_value <- if (n_discordant == 0) 1 else
      stats::binom.test(gain, n_discordant, p = 0.5, alternative = "two.sided")$p.value

    tibble::tibble(
      N_Included = nrow(valid),
      N_Ties_Removed = n_ties,
      Accuracy_Temp0 = 100 * mean(valid$Temp0),
      Accuracy_Temp1 = 100 * mean(valid$Temp1),
      Difference_pp = 100 * (mean(valid$Temp1) - mean(valid$Temp0)),
      N_Gained = gain,
      N_Lost = loss,
      McNemar_p = p_value
    )
  }) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    Agent = factor(Agent, levels = paste0("GPT-", model_order)),
    Test = factor(Test, levels = test_order)
  ) %>%
  dplyr::arrange(Agent, Test)


# 4) Export analysis results
openxlsx::write.xlsx(
  list(Temperature_Robustness = math_robustness),
  file = file.path(output_dir, "Results_math_temperature_robustness.xlsx"),
  overwrite = TRUE
)

print(math_robustness, n = Inf)
message("Temperature robustness check completed.")
message("Results saved to: ", file.path(output_dir, "Results_math_temperature_robustness.xlsx"))
