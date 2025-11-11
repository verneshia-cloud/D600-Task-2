# D600 Task 2 (UGN1) — Logistic Regression on Housing Dataset

This repository contains my full submission for WGU D600 Task 2 (UGN1). It includes data splits, R scripts for EDA/modeling, optimization artifacts, evaluation outputs, and the final report.

## Structure
```
D600-Task-2-UGN1/
├── data/                     # Raw & split datasets (train/test)
├── scripts/                  # R scripts used for the analysis
├── outputs/                  # Saved figures & tables for rubric evidence
├── docs/                     # Report, task overview, evaluation report
└── README.md
```

## Repro Steps
1. Open R (RStudio recommended). Install packages if needed (see `docs/packages_list.txt`).
2. Run in order:
   - `scripts/01_descriptives_visuals.R` (creates C2 & C3 outputs)
   - `scripts/02_model_train_optimize.R` (fits & optimizes the model; saves D2 artifacts)
   - `scripts/03_evaluate_predict.R` (generates train/test confusion matrices; D3 & D4)
   - `scripts/04_assumption_checks.R` (VIF, Box-Tidwell, ROC, residuals; E5 evidence)
3. All figures/tables should appear under `outputs/` and be embedded into the DOCX in `docs/`.

## Notes
- Replace placeholders with your actual screenshots and numbers if scripts produce different artifacts.
- Keep the repo **public** for evaluator access.
