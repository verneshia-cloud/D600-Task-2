# 05_export_equation_and_samples.R
# Export final equation (coeffs + OR) and sample predictions table for report

library(tidyverse)
library(MASS)
library(broom)
library(forcats)

dir.create("outputs", showWarnings = FALSE)

train <- read.csv("data/train.csv")
test  <- read.csv("data/test.csv")

# --- rename target from is_luxury -> IsLuxury in BOTH datasets ---
if ("is_luxury" %in% names(train)) names(train)[names(train) == "is_luxury"] <- "IsLuxury"
if ("is_luxury" %in% names(test))  names(test) [names(test)  == "is_luxury"]  <- "IsLuxury"

# --- normalize outcome to factor c("No","Yes") in BOTH datasets ---
normY <- function(df){
  if (!is.factor(df$IsLuxury)) {
    if (is.numeric(df$IsLuxury)) {
      df$IsLuxury <- factor(ifelse(df$IsLuxury == 1, "Yes", "No"), levels = c("No","Yes"))
    } else {
      df$IsLuxury <- factor(df$IsLuxury)
      lv <- levels(df$IsLuxury)
      if (!all(c("No","Yes") %in% lv)) {
        df$IsLuxury <- forcats::fct_collapse(df$IsLuxury, No = lv[1], Yes = lv[2])
      }
      df$IsLuxury <- factor(df$IsLuxury, levels = c("No","Yes"))
    }
  }
  df
}
train <- normY(train)
test  <- normY(test)

# predictors used in rubric
preds <- c("price","crime_rate","property_tax_rate","previous_sale_price")
stopifnot(all(preds %in% names(train)))

# --- optimized model on train (same as D2) ---
full <- glm(IsLuxury ~ price + crime_rate + property_tax_rate + previous_sale_price,
            data = train, family = binomial())
opt  <- stepAIC(full, direction = "both", trace = FALSE)

# ---------- E6: equation pieces ----------
coef_tbl <- broom::tidy(opt) |>
  mutate(odds_ratio = exp(estimate))
write.csv(coef_tbl, "outputs/E6_equation_coefficients_and_OR.csv", row.names = FALSE)

# ---------- D4: sample predictions (first 10 rows of test) ----------
thr <- if (file.exists("outputs/D2_threshold_auc_train.csv")) {
  read.csv("outputs/D2_threshold_auc_train.csv")$threshold[1]
} else {
  0.5
}

prob_te <- predict(opt, newdata = test, type = "response")
pred_te <- ifelse(prob_te >= thr, 1, 0)

out <- tibble(
  row          = seq_along(prob_te),   # <-- changed from row_number()
  prob_luxury  = round(prob_te, 4),
  class_at_thr = pred_te,
  actual       = ifelse(test$IsLuxury == "Yes", 1, 0)
)

write.csv(head(out, 10), "outputs/D4_sample_test_predictions.csv", row.names = FALSE)
