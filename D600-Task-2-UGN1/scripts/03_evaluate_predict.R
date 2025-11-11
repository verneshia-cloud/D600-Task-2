# 03_evaluate_predict.R
# Confusion & accuracy on train (optimized model) and test (same threshold)

library(tidyverse)
library(MASS)
library(caret)
library(pROC)
library(gridExtra)
library(forcats)

dir.create("outputs", showWarnings = FALSE)

train <- read.csv("data/train.csv")
test  <- read.csv("data/test.csv")

# --- rename your target from is_luxury -> IsLuxury in BOTH datasets ---
if ("is_luxury" %in% names(train)) names(train)[names(train) == "is_luxury"] <- "IsLuxury"
if ("is_luxury" %in% names(test))  names(test) [names(test)  == "is_luxury"] <- "IsLuxury"

# --- normalize outcome in BOTH datasets to factor c("No","Yes") ---
normY <- function(df){
  if (!is.factor(df$IsLuxury)) {
    if (is.numeric(df$IsLuxury)) {
      df$IsLuxury <- factor(ifelse(df$IsLuxury == 1, "Yes", "No"),
                            levels = c("No","Yes"))
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
stopifnot(all(preds %in% names(train)), all(preds %in% names(test)))

# --- refit optimized model on train (same as in D2) ---
full <- glm(IsLuxury ~ price + crime_rate + property_tax_rate + previous_sale_price,
            data = train, family = binomial())
opt  <- stepAIC(full, direction = "both", trace = FALSE)

# --- load the fixed threshold from D2 (Youden on train ROC) ---
thr_file <- "outputs/D2_threshold_auc_train.csv"
thr <- if (file.exists(thr_file)) read.csv(thr_file)$threshold[1] else NA_real_
if (is.na(thr)) thr <- 0.5

# ---------- D3: TRAIN ----------
prob_tr <- predict(opt, type = "response")
pred_tr <- factor(ifelse(prob_tr >= thr, "Yes", "No"), levels = c("No","Yes"))
cm_tr   <- caret::confusionMatrix(pred_tr, train$IsLuxury, positive = "Yes")

png("outputs/D3_train_confusion.png", width = 720, height = 420)
gridExtra::grid.arrange(gridExtra::tableGrob(cm_tr$table))
dev.off()

write.csv(as.data.frame(cm_tr$table), "outputs/D3_train_confusion.csv", row.names = FALSE)
write.csv(data.frame(Accuracy = cm_tr$overall["Accuracy"]),
          "outputs/D3_train_accuracy.csv", row.names = FALSE)

# ---------- D4: TE
