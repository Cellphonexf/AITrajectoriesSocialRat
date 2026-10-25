# Trajectories and Boundaries of Human-Like Social Rationality in AI: Evidence from Nine Successive Versions of ChatGPT

Welcome to the GitHub repository for our research project. This repository contains the data and R scripts used to reproduce the behavioral-alignment analyses, statistical tests, and figures reported in the study.

**Data-use notice:** These files are shared to support transparency and reproduction of the reported analyses. The human benchmark data originate from previously published studies; users should consult and respect the terms of the original sources. Please contact the authors before reusing any non-public project data for purposes beyond reproducing this study.

## Data

The six Excel workbooks contain task responses or coded outcomes for nine GPT models (GPT-3.5 through GPT-5.4), together with the corresponding human benchmark data where applicable.

**Social rationality:**
- `rawdata_socialcue.xlsx`: Risk decisions involving group-size and kinship cues under positive and negative framing (adaptive-cue sensitivity).
- `rawdata_falsebelief.xlsx`: False-belief detection responses and human comparison data.
- `rawdata_irony.xlsx`: Irony recognition responses in ironic and non-ironic contexts and human comparison data.
- `rawdata_fauxpas.xlsx`: Faux-pas detection responses and human comparison data.
- `rawdata_moraldilemmas.xlsx`: Moral choices in original, Action–Omission, and Yes–No versions of five dilemmas (normative reasoning).

**Non-social problem-solving:**
- `rawdata_math.xlsx`: Item-level correctness data for AQuA-RAT (254 items) and SAT-MATH (220 items), used as non-social problem-solving benchmarks. 

## Scripts

- `DataAnalysis&Viso_adaptiveCue.R`  
  Computes choice proportions, alignment with human and uniformly risk-averse references, framing effects, cue-sensitivity measures, developmental trends, and the corresponding visualizations.

- `DataAnalysis&Viso_socialInference.R`  
  Computes scenario-level accuracy and alignment before averaging within false belief, irony, and faux pas; evaluates human and perfect-detection references, irony-context differences, developmental trends, and visualizations.

- `DataAnalysis&Viso_normative.R`  
  Computes utilitarian-choice proportions, alignment with human and uniformly utilitarian references, Action–Omission and Yes–No reformulation effects, context-invariance alignment, and visualizations.

- `DataAnalysis&Viso_math.R`  
  Scores AQuA-RAT and SAT-MATH responses, calculates accuracy and alignment with human and perfect-accuracy references, tests trends, and generates visualizations. For GPT-5-series models, item outcomes are determined by majority vote across 10 responses; 5–5 ties are excluded.

- `DataAnalysis&Viso_socialRat.R`  
  Integrates the three social-rationality domains using equal domain weights, evaluates overall human alignment, and generates the overall trajectory and domain-profile figure. This script uses the preceding domain-level results, including bootstrap outputs.

- `DataAnalysis&Viso_social_vs_nonSocial.R`  
  Compares human alignment in overall social rationality with non-social problem-solving and with each mathematical benchmark separately, producing alignment-difference trends and a figure.

- `PermutationTest.R`  
  Performs exact two-sided permutation tests for the originally significant monotonic trends by enumerating all 9! (362,880) possible orderings of the nine model scores. It applies Holm correction within the specified comparison families and produces an audit workbook and the supplementary permutation figure.

## Output Files

All scripts save generated results in the `Outputs/` directory. Principal outputs include:

**Statistical results:**
- `Results_socialcue.xlsx`
- `Results_ToM.xlsx`
- `Results_moral.xlsx`
- `Results_math.xlsx`
- `Results_SocialRat.xlsx`
- `Results_social_vs_nonsocial.xlsx`
- `Results_Permutation.xlsx`

**Figures:**
- `viso_socialcue_complete.pdf`
- `viso_ToM_complete.pdf`
- `viso_moral_complete.pdf`
- `viso_math.pdf`
- `viso_social_rationality.pdf`
- `viso_social_vs_nonsocial.pdf`
- `viso_Permutation.pdf`

The manuscript figures may have received additional formatting via Adobe Illustrator.

## Usage

1. Place the six data workbooks and seven R scripts in the same directory. The scripts are written to set the working directory to the active script's folder when run in RStudio; otherwise set the working directory manually.
2. Run the four domain/benchmark scripts first, in any order: `DataAnalysis&Viso_adaptiveCue.R`, `DataAnalysis&Viso_socialInference.R`, `DataAnalysis&Viso_normative.R`, and `DataAnalysis&Viso_math.R`.
3. Run `DataAnalysis&Viso_socialRat.R` after the three social-domain workbooks have been produced.
4. Run `DataAnalysis&Viso_social_vs_nonSocial.R` after both `Results_SocialRat.xlsx` and `Results_math.xlsx` are available.
5. Run `PermutationTest.R` last, after all six upstream `Results_*.xlsx` workbooks have been generated.

## Software Requirements

- R (version 4.6.0, as reported in the manuscript).
- R packages: `tidyr`, `dplyr`, `readxl`, `purrr`, `stringr`, `ggplot2`, `patchwork`, `openxlsx`, `BayesFactor`, `scales`, and `tibble`.
- RStudio is recommended for automatic working-directory detection (`rstudioapi`); it is not required if the working directory is set manually.

## Updated October 25, 2026