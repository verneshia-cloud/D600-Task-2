# scripts/02_optimize_model.R
# Reproducible optimization: stepwise AIC + ROC threshold tuning (Youden J)

set.seed(42)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(MASS)        # stepAIC
  library(pROC) ; library(broom)
})

# ---- Load data ----
train <- read_csv("train.csv", show_col_types = FALSE)
test  <- read_csv("test.csv",  show_col_types = FALSE)

# Ensure DV is binary numeric (0/1)
train <- train %>% mutate(is_luxury = ifelse(is_luxury %in% c("Yes", "1", 1), 1, 0))
test  <- test  %>% mutate(is_luxury = ifelse(is_luxury %in% c("Yes", "1", 1), 1, 0))

# ---- Stage 1: Model specification via stepwise AIC ----
m_full <- glm(is_luxury ~ price + crime_rate + property_tax_rate + previous_sale_price,
              data = train, family = binomial())

m_opt  <- stepAIC(m_full, direction = "both", trace = FALSE)

# Save model summary + final formula for the report
dir.create("outputs", showWarnings = FALSE)
sink("outputs/D2_stepAIC_summary.txt"); print(summary(m_opt)); sink()
writeLines(deparse(formula(m_opt)), "outputs/D2_final_formula.txt")

# Coefficients table (for E6 later)
coef_tbl <- broom::tidy(m_opt, conf.int = TRUE, conf.level = 0.95, exponentiate = FALSE)
readr::write_csv(coef_tbl, "outputs/D2_coef_table.csv")

# ---- Stage 2: Threshold tuning on train (Youden J) ----
train_prob <- predict(m_opt, type = "response")
roc_tr <- pROC::roc(response = train$is_luxury, predictor = train_prob, quiet = TRUE)

# Best threshold by Youden's J
best_coords <- coords(roc_tr, x = "best", best.method = "youden",
                      ret = c("threshold","sensitivity","specificity","ppv","npv"))
thr <- as.numeric(best_coords["threshold"])

thr_df <- data.frame(
  threshold = thr,
  sensitivity = as.numeric(best_coords["sensitivity"]),
  specificity = as.numeric(best_coords["specificity"]),
  ppv = as.numeric(best_coords["ppv"]),
  npv = as.numeric(best_coords["npv"])
)
readr::write_csv(thr_df, "outputs/D2_threshold_selection.csv")

# Save ROC plot with threshold annotated (screenshot this for the report)
png("outputs/D2_ROC_train.png", width = 1800, height = 1200, res = 200)
plot(roc_tr, main = "Train ROC – Optimized Model")
text(0.65, 0.2, labels = paste0("Best threshold (Youden J): ", round(thr, 3)))
dev.off()
