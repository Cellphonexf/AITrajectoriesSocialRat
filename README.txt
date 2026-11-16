# Trajectories and Boundaries of Human-Like Social Rationality in AI: Evidence from Nine Successive Versions of ChatGPT

Welcome to the GitHub repository for our research project. This repository contains the data and R scripts used to reproduce the behavioral-alignment analyses, robustness checks, statistical tests, and figures reported in the study. The analysis covers nine GPT models, from GPT-3.5 through GPT-5.4, across social rationality and logical problem-solving tasks.

**Data-use notice:** These files are shared to support transparency and reproduction of the reported analyses. Human benchmark data originate from previously published studies and datasets; users should consult and respect the terms of the original sources. Please contact the authors before reusing any non-public project data for purposes beyond reproducing this study.

## Data

The six Excel workbooks contain GPT responses or coded outcomes, together with the corresponding human benchmark data where applicable. 

**Social rationality:**
- `rawdata_socialcue.xlsx`: Risk decisions involving group size and kinship constituents under positive and negative framing (social cues of adaptation).
- `rawdata_falsebelief.xlsx`: False-belief detection responses and human comparison data.
- `rawdata_irony.xlsx`: Irony detection responses in ironic and non-ironic contexts and human comparison data.
- `rawdata_fauxpas.xlsx`: Faux-pas detection responses and human comparison data.
- `rawdata_moraldilemmas.xlsx`: Moral choices in original, Action–Omission, and Yes–No versions of five dilemmas (normative reasoning).

**Logical problem-solving:**
- `rawdata_math.xlsx`: Item-level correctness data for AQuA-RAT (254 items) and SAT-MATH (220 items). The workbook also contains the supplementary GPT-4o and GPT-4.1 temperature-robustness responses in the `4o_robustcheck` and `4.1_robustcheck` worksheets.

**Human benchmark sources:**
- Social cues of adaptation: previously collected in-person human experiments compiled in Xiao and Wang (2025).
- Cognitive inference: human theory-of-mind data from Strachan et al. (2024).
- Normative reasoning: human responses from Study 2 of Cheung et al. (2025).
- Logical problem-solving: published aggregate human accuracy benchmarks from AGIEval (Zhong et al., 2024): 85% for AQuA-RAT and 66% for SAT-MATH. These benchmarks are not newly collected item-level human responses.

## Scripts

The eight R scripts below correspond to the repository filenames.

- `DataAnalysis&Viso_adaptiveCue.R`  
  Calculates condition-level risk-averse choice proportions; alignment with empirical human behavior and the uniformly risk-averse reference; framing effects and framing invariance; cue-sensitivity slopes; baseline-alignment differences; developmental trends; bootstrap results; and visualizations.

- `DataAnalysis&Viso_socialInference.R`  
  Calculates scenario-level accuracy and alignment in false-belief, irony, and faux-pas tasks, including ironic and non-ironic contexts. It evaluates human-baseline alignment, perfect-detection alignment, their differences, bootstrap results, developmental trends, and visualizations.

- `DataAnalysis&Viso_normative.R`  
  Calculates utilitarian-choice proportions for the original moral dilemmas and contextual reformulations. It evaluates alignment with human and uniformly utilitarian references, Action–Omission and Yes–No reformulation effects, context invariance, alignment differences, bootstrap results, developmental trends, and visualizations.

- `DataAnalysis&Viso_math.R`  
  Scores AQuA-RAT and SAT-MATH responses, calculates accuracy and alignment with published human baselines and the perfect-accuracy reference, examines alignment differences and developmental trends, and generates visualizations. For GPT-3.5 through GPT-4.1, each item is scored from one response at temperature 0. For GPT-5 through GPT-5.4, each item is evaluated 10 times at temperature 1 and coded by majority vote; 5–5 ties are excluded.

- `DataAnalysis&Viso_socialRat.R`  
  Integrates social cues of adaptation, cognitive inference, and normative reasoning with equal domain weights. Our analyses include overall human-baseline alignment, overall task-specific extremeness alignment (uniform risk aversion, perfect detection, and uniform utilitarian responding), the model-wise extremeness-minus-human alignment difference, and leave-one-domain-out robustness checks for both reference types. It also combines domain-level human-alignment bootstrap outputs and produces the overall dual-reference trajectory and nine-model domain-profile figure. The source bootstrap data support the overall human-alignment confidence interval; no extremeness-alignment or overall-gap confidence interval is imputed from those human-only bootstrap results.

- `DataAnalysis&Viso_social_vs_nonSocial.R`  
  Compares human-baseline alignment between overall social rationality and logical problem-solving. It tests the social-minus-logical alignment-difference trajectory and supplementary comparisons with AQuA-RAT and SAT-MATH separately, including Holm correction for the test-specific comparisons, and generates the corresponding figure. Cross-domain extremeness-alignment comparisons are not part of the reported analysis.

- `Temperature_RobustnessCheck.R`  
  Robustness check: compares GPT-4o and GPT-4.1 performance on the same AQuA-RAT and SAT-MATH items under the original temperature-0, single-response setting and a temperature-1, 10-response majority-vote setting. Items with a 5–5 tie in the repeated-response condition are excluded from both paired conditions. Two-sided exact McNemar tests (exact binomial tests of discordant item pairs) assess differences in item-level correctness. The script reports paired accuracies, accuracy differences, tie counts, and test results.

- `PermutationTest.R`  
  Robustness check: evaluates originally significant monotonic trends using exact two-sided permutation tests over all 9! (362,880) permutations of the nine model scores. For comparisons involving Holm correction, it recomputes permutation-based adjusted p-values using the complete corresponding test families. It exports both the originally significant results and an all-trends audit, checks whether correlations recomputed from exported Excel values match the original statistics, flags precision discrepancies where applicable, and produces the supplementary permutation-distribution figure.

## Output Files

Generated results are saved in the `Outputs/` directory. Principal outputs include:

**Statistical results:**
- `Results_socialcue.xlsx`: Social-cue alignment, framing, sensitivity, and bootstrap analyses.
- `Results_ToM.xlsx`: Cognitive-inference alignment and bootstrap analyses.
- `Results_moral.xlsx`: Normative-reasoning alignment, reformulation, and bootstrap analyses.
- `Results_math.xlsx`: Mathematical benchmark accuracy, dual-reference alignment, and trends.
- `Results_SocialRat.xlsx`: Overall social-rationality human and extremeness alignment, their difference, leave-one-domain-out robustness, and supporting scores.
- `Results_social_vs_nonsocial.xlsx`: Social-minus-logical human-baseline alignment differences and tests.
- `Results_math_temperature_robustness.xlsx`: Temperature robustness and exact McNemar results for GPT-4o and GPT-4.1.
- `Results_Permutation.xlsx`: Originally significant exact permutation results and all-trends audit.

**Figures:**
- `viso_socialcue_complete.pdf`
- `viso_ToM_complete.pdf`
- `viso_moral_complete.pdf`
- `viso_math.pdf`
- `viso_social_rationality.pdf`
- `viso_social_vs_nonsocial.pdf`
- `viso_Permutation.pdf`

The manuscript figures may have received additional formatting in Adobe Illustrator.

## Usage

1. Place the six data workbooks and eight R scripts in the same directory. Use the canonical script filenames listed above. In RStudio, the scripts attempt to use the active script's directory as the working directory; otherwise, set the working directory manually to the project folder.
2. Run the four domain/benchmark scripts in any order: `DataAnalysis&Viso_adaptiveCue.R`, `DataAnalysis&Viso_socialInference.R`, `DataAnalysis&Viso_normative.R`, and `DataAnalysis&Viso_math.R`. These generate the source results in `Outputs/`, including the domain-level `Bootstrap_alignment` worksheets needed for the integrated social-rationality bootstrap.
3. Run `DataAnalysis&Viso_socialRat.R` once `Results_socialcue.xlsx`, `Results_ToM.xlsx`, and `Results_moral.xlsx` are available. It generates `Results_SocialRat.xlsx` and `viso_social_rationality.pdf`.
4. Run `DataAnalysis&Viso_social_vs_nonSocial.R` once `Results_SocialRat.xlsx` and `Results_math.xlsx` are available. This performs the human-baseline social-versus-logical comparison.
5. Run `Temperature_RobustnessCheck.R` after `Outputs/` exists. The input workbook `rawdata_math.xlsx` must include the original `4o` and `4.1` worksheets and the supplementary `4o_robustcheck` and `4.1_robustcheck` worksheets. This analysis is independent of the social-versus-logical comparison.
6. Run `PermutationTest.R` after the six upstream result workbooks (`Results_socialcue.xlsx`, `Results_ToM.xlsx`, `Results_moral.xlsx`, `Results_math.xlsx`, `Results_SocialRat.xlsx`, and `Results_social_vs_nonsocial.xlsx`) have been generated. Inspect the `RhoMatches` and `NeedsFullPrecisionData` audit columns before treating a permutation result as verified.

## Software Requirements

- R version 4.6.0 (as reported in the manuscript).
- R packages: `tidyr`, `dplyr`, `readxl`, `purrr`, `stringr`, `ggplot2`, `patchwork`, `openxlsx`, `BayesFactor`, `scales`, and `tibble`.
- RStudio is recommended for automatic working-directory detection; install `rstudioapi` for this convenience. Without RStudio, set the working directory manually.

## Updated November 16, 2026
