# LLM evolutionary trajectory
# Exact permutation tests for significant monotonic trends
# Requires: six Results_*.xlsx workbooks in the "Outputs" folder
# Programmed by Feng XIAO (updated on 2026-10-23)

# 0) Preparation
# Load packages actually used
pkg <- c("readxl", "dplyr", "ggplot2", "patchwork", "openxlsx", "tibble")
lapply(pkg, require, character.only = TRUE)

# Global ggplot defaults
theme_set(theme_classic(base_size = 6))

if (requireNamespace("rstudioapi", quietly = TRUE)) {
  p <- tryCatch(rstudioapi::getActiveDocumentContext()$path, error = function(e) "")
  if (length(p) == 1 && nzchar(p)) setwd(dirname(p))
}
# 0.1) Analysis settings
output_dir <- "Outputs"
if (!dir.exists(output_dir)) {
  stop("Folder not found: ", normalizePath(output_dir, mustWork = FALSE))
}
input_dir <- output_dir

# Figure settings (same visual conventions as the main analysis scripts)
blue_color <- "#3b85bd"
red_color <- "#c44942"
reference_color <- "grey70"
figure_base_size <- 6
figure_columns <- 2L

# 1) Data input
# Locate the original exported workbooks; numbered copies are also supported.
find_book <- function(stem) {
  paths <- list.files(input_dir, pattern = "\\.xlsx$", full.names = TRUE)
  names <- basename(paths)
  hits <- paths[names == paste0(stem, ".xlsx") |
    (startsWith(names, paste0(stem, "(")) & endsWith(names, ").xlsx"))]
  if (length(hits) == 0) stop("Missing workbook in Outputs: ", stem, ".xlsx")
  if (length(hits) > 1) stop("Ambiguous workbook copies for ", stem, ": ", paste(basename(hits), collapse = ", "))
  hits[[1]]
}

# 1.1) Map the original Results sheets to the corresponding nine-model values.
# Every row describes one existing original trend table, its corresponding
# nine-model outcome table, and any keys shared between the two.
# Only originally significant tests are DRAWN; entire Holm families are
# recomputed with permutation p-values before classifying their robustness.
registry <- tibble::tribble(
  ~book, ~trends, ~values, ~keys, ~outcome,
  "Results_socialcue", "Alignment_Cue_Framing_Trend", "Alignment_By_Cue_Framing", "Baseline,Cue,Framing", "AlignmentPercent",
  "Results_socialcue", "Alignment_Cue_Trend", "Alignment_By_Cue", "Baseline,Cue", "AlignmentPercent",
  "Results_socialcue", "Alignment_Overall_Trend", "Alignment_Overall", "Baseline", "AlignmentPercent",
  "Results_socialcue", "Baseline_Gap_Cue_Framing_Trend", "Baseline_Gap_By_Cue_Framing", "Cue,Framing", "BaselineGap",
  "Results_socialcue", "Baseline_Gap_Cue_Trend", "Baseline_Gap_By_Cue", "Cue", "BaselineGap",
  "Results_socialcue", "Baseline_Gap_Overall_Trend", "Baseline_Gap_Overall", "", "BaselineGap",
  "Results_socialcue", "FE_Alignment_Cue_Trend", "FE_Alignment_By_Cue", "Baseline,Cue", "AlignmentPercent",
  "Results_socialcue", "FE_Alignment_Overall_Trend", "FE_Alignment_Overall", "Baseline", "AlignmentPercent",
  "Results_socialcue", "FE_Baseline_Gap_Cue_Trend", "FE_Baseline_Gap_By_Cue", "Cue", "BaselineGap",
  "Results_socialcue", "FE_Baseline_Gap_Overall_Trend", "FE_Baseline_Gap_Overall", "", "BaselineGap",
  "Results_socialcue", "Sens_Align_Cue_Framing_Trend", "Sens_Alignment_By_Cue_Framing", "Cue,Framing", "SensitivityAlignmentPercent",
  "Results_socialcue", "Sens_Alignment_Cue_Trend", "Sens_Alignment_By_Cue", "Cue", "SensitivityAlignmentPercent",
  "Results_socialcue", "Sens_Alignment_Overall_Trend", "Sens_Alignment_Overall", "", "SensitivityAlignmentPercent",
  "Results_ToM", "Alignment_Task_Trend", "Alignment_By_Task", "Baseline,Module", "AlignmentPercent",
  "Results_ToM", "Alignment_Overall_Trend", "Alignment_Overall", "Baseline", "AlignmentPercent",
  "Results_ToM", "Baseline_Gap_Task_Trend", "Baseline_Gap_By_Task", "Module", "BaselineGap",
  "Results_ToM", "Baseline_Gap_Overall_Trend", "Baseline_Gap_Overall", "", "BaselineGap",
  "Results_ToM", "Irony_Ctx_Alignment_Trend", "Irony_Ctx_Alignment", "Baseline,Context", "AlignmentPercent",
  "Results_ToM", "Irony_Ctx_Baseline_Gap_Trend", "Irony_Ctx_Baseline_Gap", "Context", "BaselineGap",
  "Results_moral", "Alignment_Original_Trend", "Alignment_Original", "Baseline", "AlignmentPercent",
  "Results_moral", "Baseline_Gap_Original_Trend", "Baseline_Gap_Original", "", "BaselineGap",
  "Results_moral", "Ctx_Alignment_Effect_Trend", "Ctx_Alignment_By_Effect", "Baseline,EffectType", "AlignmentPercent",
  "Results_moral", "Ctx_Alignment_Overall_Trend", "Ctx_Alignment_Overall", "Baseline", "AlignmentPercent",
  "Results_moral", "Ctx_Baseline_Gap_Effect_Trend", "Ctx_Baseline_Gap_By_Effect", "EffectType", "BaselineGap",
  "Results_moral", "Ctx_Baseline_Gap_Overall_Trend", "Ctx_Baseline_Gap_Overall", "", "BaselineGap",
  "Results_math", "Alignment_Test_Trend", "Alignment_By_Test", "Baseline,Test", "AlignmentPercent",
  "Results_math", "Alignment_Overall_Trend", "Alignment_Overall", "Baseline", "AlignmentPercent",
  "Results_math", "Baseline_Gap_Test_Trend", "Baseline_Gap_By_Test", "Test", "BaselineGap",
  "Results_math", "Baseline_Gap_Overall_Trend", "Baseline_Gap_overall", "", "BaselineGap",
  "Results_SocialRat", "Alignment_Overall_Trend", "Alignment_Overall", "Baseline", "AlignmentPercent",
  "Results_social_vs_nonsocial", "Alignment_Diff_Overall_Trend", "Alignment_Difference_Overall", "", "AlignmentDifference",
  "Results_social_vs_nonsocial", "Alignment_Difference_Test_Trend", "Alignment_Difference_By_Test", "Math_Test", "AlignmentDifference"
)

# 2) Exact permutation analysis
# All permutations of the NINE labels (exact).
# Construct once and reuse for every selected analysis.
perms <- matrix(integer(0), nrow = 1, ncol = 0)
for (k in 1:9) {
  old <- perms; n_old <- nrow(old)
  perms <- matrix(NA_integer_, nrow = n_old * k, ncol = k)
  for (at in seq_len(k)) {
    ix <- ((at - 1L) * n_old + 1L):(at * n_old)
    if (at > 1L) perms[ix, seq_len(at - 1L)] <- old[, seq_len(at - 1L), drop = FALSE]
    perms[ix, at] <- k
    if (at <= ncol(old)) perms[ix, (at + 1L):k] <- old[, at:ncol(old), drop = FALSE]
  }
}
stopifnot(nrow(perms) == factorial(9), ncol(perms) == 9)

cache <- new.env(parent = emptyenv())
exact_spearman <- function(y) {
  stopifnot(length(y) == 9L, all(is.finite(y)))
  rk <- rank(y, ties.method = "average")
  key <- paste(format(rk, digits = 16), collapse = "|")
  if (exists(key, envir = cache, inherits = FALSE)) return(get(key, envir = cache))
  ry <- rk - mean(rk)
  rx <- (1:9) - 5
  den <- sqrt(sum(rx^2) * sum(ry^2))
  if (den == 0) stop("Constant response; Spearman undefined")
  # Row-wise dot product of permuted rank vector with fixed ordinal ranks.
  rhos <- rowSums(matrix(ry[as.vector(perms)], nrow = nrow(perms), ncol = 9) *
                    matrix(rx, nrow = nrow(perms), ncol = 9, byrow = TRUE)) / den
  observed <- unname(cor(1:9, y, method = "spearman"))
  p_exact <- mean(abs(rhos) >= abs(observed) - 1e-12)
  out <- list(rho = observed, p = p_exact, dist = rhos)
  assign(key, out, envir = cache)
  out
}

# 3) Calculate permutation tests
# Read every original test (including nonsignificant Holm family members).
# The selection for output is determined ONLY by original p/p_adjusted.
all_rows <- list()
result_distributions <- list()
for (idx in seq_len(nrow(registry))) {
  r <- registry[idx, ]
  f <- find_book(r$book)
  tr <- as.data.frame(readxl::read_excel(f, sheet = r$trends))
  val <- as.data.frame(readxl::read_excel(f, sheet = r$values))
  keys <- if (nzchar(r$keys)) strsplit(r$keys, ",", fixed = TRUE)[[1]] else character()
  if (!all(c(keys, "Generation", r$outcome) %in% names(val))) stop("Columns missing in ", r$book, ":", r$values)
  if (!all(keys %in% names(tr))) stop("Key missing in ", r$book, ":", r$trends)
  adjusted_col <- intersect(c("p_adjusted", "Adjusted_p_value"), names(tr))
  raw_col <- intersect(c("p_raw", "p_value"), names(tr))
  if (length(raw_col) == 0) stop("No original p value: ", r$trends)
  # Original significance: adjusted p when available, otherwise raw p.
  original_p_col <- if (length(adjusted_col)) adjusted_col[[1]] else raw_col[[1]]
  for (j in seq_len(nrow(tr))) {
    trow <- tr[j, , drop = FALSE]
    subset <- val
    for (key in keys) subset <- subset[as.character(subset[[key]]) == as.character(trow[[key]][1]), , drop = FALSE]
    subset <- subset[is.finite(suppressWarnings(as.numeric(subset$Generation))), , drop = FALSE]
    subset <- subset[order(as.numeric(subset$Generation)), , drop = FALSE]
    if (nrow(subset) != 9 || !identical(as.integer(subset$Generation), 1:9)) {
      stop("Expected one observation for each of 9 models in: ", r$book, "/", r$values,
           " [", paste(vapply(keys, function(k) paste0(k, "=", trow[[k]][1]), character(1)), collapse = ", "), "]")
    }
    y <- as.numeric(subset[[r$outcome]])
    perm <- exact_spearman(y)
    orig_rho <- as.numeric(trow$Spearman_rho[1])
    rho_matches <- isTRUE(all.equal(perm$rho, orig_rho, tolerance = 2e-5))
    # Excel exports can collapse tiny floating-point differences into ties.
    # Do not silently pretend the Excel-based test reproduces the source test.
    # Continue for audit, and visibly flag any originally significant mismatch.

    key_text <- if(length(keys)) paste(vapply(keys, function(k) paste(trow[[k]][1]), character(1)), collapse = " / ") else "Overall"
    title <- paste(sub("^Results_", "", r$book), r$trends, key_text, sep = " | ")
    original_p <- as.numeric(trow[[original_p_col]][1])
    orig_sig <- is.finite(original_p) && original_p <= 0.05
    # Holm family is defined by the source trend table + parent category
    # (e.g., test-specific math comparisons within the same Baseline).
    parent <- "all"
    if (length(adjusted_col) && "Baseline" %in% names(tr) && "Baseline" %in% keys) parent <- as.character(trow$Baseline[1])
    if (length(adjusted_col) && "Cue" %in% names(tr) && "Cue" %in% keys &&
        ("Framing" %in% keys)) parent <- paste(parent, as.character(trow$Cue[1]), sep = "|")
    family <- paste(r$book, r$trends, parent, sep = "::")
    row_id <- length(all_rows) + 1L
    all_rows[[row_id]] <- data.frame(
      ID = row_id, Analysis = title, Workbook = r$book, TrendSheet = r$trends,
      OriginalRho = orig_rho, ExcelRecomputedRho = perm$rho,
      RhoMatches = rho_matches, RhoDifference = perm$rho - orig_rho,
      OriginalP = original_p,
      OriginallySignificant = orig_sig, HolmFamily = family,
      NeedsHolm = length(adjusted_col)>0,
      PermutationP = perm$p, stringsAsFactors = FALSE
    )
    if (orig_sig) {
      # Compress the exact distribution into histogram bins to reduce memory.
      breaks <- seq(-1.0175, 1.0175, by = 0.035)
      hist_info <- hist(perm$dist, breaks = breaks, plot = FALSE, include.lowest = TRUE)
      result_distributions[[as.character(row_id)]] <- data.frame(
        ID = row_id, Rho = hist_info$mids,
        Density = hist_info$counts / (length(perm$dist) * diff(hist_info$breaks)[1])
      )
    }
  }
}
results <- dplyr::bind_rows(all_rows)
results$PermutationAdjustedP <- results$PermutationP
for (family in unique(results$HolmFamily[results$NeedsHolm])) {
  ii <- which(results$HolmFamily == family)
  results$PermutationAdjustedP[ii] <- p.adjust(results$PermutationP[ii], method = "holm")
}
results$PermutationSignificant <- results$PermutationAdjustedP <= 0.05
results$ConfirmingOriginal <- results$OriginallySignificant & results$PermutationSignificant & results$RhoMatches
results$NeedsFullPrecisionData <- results$OriginallySignificant & !results$RhoMatches
results$OriginalPType <- ifelse(results$NeedsHolm, "Holm-adjusted", "raw")
# 4) Select originally significant trends and export results
# Save the internal audit (all tests) and the original significant subset.
# The complete Holm families remain in the audit for transparency.
selected <- results[results$OriginallySignificant, , drop = FALSE]
if (any(selected$NeedsFullPrecisionData)) {
  bad <- selected[selected$NeedsFullPrecisionData, ]
  warning("Exact test cannot reproduce original Spearman rho from exported Excel values for: \n",
          paste(paste0(bad$Analysis, " [original rho=", round(bad$OriginalRho, 6),
                       ", Excel rho=", round(bad$ExcelRecomputedRho, 6), "]"), collapse="\n"),
          "\nThese panels are flagged as UNVERIFIED. Re-export full-precision outcome values from the source R analysis to resolve.")
}
if (nrow(selected) == 0) stop("No originally significant tests found.")

# 4.1) Human-readable figure titles
# Keep the underlying original analysis identifiers in the Excel output.
format_analysis_title <- function(book, sheet, label){
  if(book == "Results_socialcue"){
    if(sheet == "Alignment_Cue_Trend"){
      return(paste0("Adaptive cue: ", if(grepl("Group size", label)) "group size" else "kinship",
                    " (risk-averse reference)"))
    }
    if(sheet == "Alignment_Overall_Trend") return("Adaptive cues: overall risk-averse alignment")
    if(sheet == "Sens_Align_Cue_Framing_Trend") return("Adaptive cues: group-size sensitivity (positive framing)")
  }
  if(book == "Results_moral") return("Normative reasoning: context-invariance advantage")
  if(book == "Results_math"){
    if(sheet == "Alignment_Test_Trend"){
      ref <- if(grepl("Human baseline", label, fixed = TRUE)) "human alignment" else "perfect-accuracy alignment"
      test <- if(grepl("SAT", label, fixed = TRUE)) "SAT-MATH" else "AQuA-RAT"
      return(paste0("Mathematics: ", test, " (", ref, ")"))
    }
    if(sheet == "Alignment_Overall_Trend"){
      ref <- if(grepl("Human baseline", label, fixed = TRUE)) "human alignment" else "perfect-accuracy alignment"
      return(paste0("Mathematics: overall ", ref))
    }
    if(sheet == "Baseline_Gap_Overall_Trend") return("Mathematics: perfect-accuracy advantage")
  }
  if(book == "Results_social_vs_nonsocial"){
    if(sheet == "Alignment_Diff_Overall_Trend") return("Social vs. non-social: overall alignment difference")
    return("Social vs. non-social: AQuA-RAT alignment difference")
  }
  # Fallback covers new significant tests introduced in future versions.
  paste(sub("^Results_", "", book), gsub("_", " ", sheet), label, sep = ": ")
}

selected$PanelTitle <- vapply(seq_len(nrow(selected)), function(i){
  parts <- strsplit(selected$Analysis[i], " | ", fixed = TRUE)[[1]]
  format_analysis_title(selected$Workbook[i], selected$TrendSheet[i], parts[length(parts)])
}, character(1))

# Distinguish multiple titles automatically if future data create duplicates.
selected$PanelTitle <- make.unique(selected$PanelTitle)

# 4.2) Export Excel result tables
# Values in the original workbook are not modified.
selected <- selected %>%
  mutate(
    Result = ifelse(!RhoMatches, "Check exported precision",
                    ifelse(PermutationSignificant, "Confirmed", "Not confirmed"))
  )

openxlsx::write.xlsx(
  list(
    Significant_Trends = selected,
    All_Trend_Audit = results
  ),
  file = file.path(output_dir, "Results_Permutation.xlsx"),
  overwrite = TRUE
)

# 5) Visualization: Exact permutation distributions
# Each panel has its own X/Y axis and title; no in-panel p or rho annotation.
# A red vertical line marks the observed Spearman rho recomputed from Excel.
# Seven rows x two columns for fourteen originally significant trends.

draw_data <- dplyr::bind_rows(result_distributions)

# 5.1) Make each panel as an independent ggplot to retain all axis labels.
make_permutation_panel <- function(i){
  info <- selected[i, ]
  hist_data <- draw_data %>% filter(ID == info$ID)

  ggplot(hist_data, aes(x = Rho, y = Density)) +
    geom_col(width = 0.034, fill = blue_color, color = NA, alpha = 0.72) +
    geom_vline(xintercept = 0, linetype = "dashed", color = reference_color, linewidth = 0.30) +
    geom_vline(xintercept = info$ExcelRecomputedRho,
               color = red_color, linewidth = 0.65) +
    scale_x_continuous(
      limits = c(-1.03, 1.03),
      breaks = c(-1, -0.5, 0, 0.5, 1),
      labels = c("-1", "-.5", "0", ".5", "1"),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      limits = c(0, 1.65),
      breaks = c(0, 0.5, 1.0, 1.5),
      expand = expansion(mult = c(0, 0.01))
    ) +
    labs(
      tag = letters[i],
      title = info$PanelTitle,
      x = "Permuted Spearman's rho",
      y = "Null density"
    ) +
    theme_classic(base_size = figure_base_size) +
    theme(
      axis.text = element_text(color = "black", size = 6),
      axis.title = element_text(color = "black", size = 6),
      axis.ticks = element_line(linewidth = 0.25),
      axis.line = element_line(linewidth = 0.30),
      plot.title.position = "panel",
      plot.title = element_text(size = 6, face = "bold", hjust = 0.5, margin = margin(b = 3)),
      plot.tag = element_text(face = "bold", size = 6),
      plot.tag.position = c(0, 1),
      plot.margin = margin(8, 7, 6, 5)
    )
}

# 5.2) Assemble all panels without empty positions.
permutation_panels <- lapply(seq_len(nrow(selected)), make_permutation_panel)
figure_rows <- ceiling(length(permutation_panels) / figure_columns)

# For the current 14 significant trends the 7 x 2 grid fills exactly.
Pic_permutation_complete <- patchwork::wrap_plots(
  permutation_panels,
  ncol = figure_columns,
  byrow = TRUE
)

print(Pic_permutation_complete)

# 6) Export final combined figure
# The width follows the main scripts; height scales to preserve panel legibility.
ggsave(
  file.path(output_dir, "viso_Permutation.pdf"),
  plot = Pic_permutation_complete,
  width = 8,
  height = figure_rows * 1.70,
  units = "in",
  device = grDevices::pdf,
  limitsize = FALSE
)

# 7) Completion messages
message("Exact permutation analysis completed.")
message("Originally significant trends: ", nrow(selected))
message("Permutation significant: ", sum(selected$PermutationSignificant))
message("Full-precision agreement: ", sum(selected$RhoMatches), " / ", nrow(selected))
message("Figure saved to: ", file.path(output_dir, "viso_Permutation.pdf"))
message("Results saved to: ", file.path(output_dir, "Results_Permutation.xlsx"))
