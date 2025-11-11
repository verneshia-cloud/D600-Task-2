# 02_optimize_model.R
# Stepwise AIC + model summary (AIC/BIC/pseudo-R2) + coefficients/OR + ROC + Youden threshold
library(tidyverse)
library(MASS)
library(pscl)
library(pROC)
library(broom)
library(forcats)

dir.create("outputs", showWarnings = FALSE)

train <- read.csv("data/train.csv")

# --- rename your target from is_luxury -> IsLuxury ---
if ("is_luxury" %in% names(train)) {
  names(train)[names(train) == "is_luxury"] <- "IsLuxury"
}

# --- normalize outcome to factor c("No","Yes") ---
if (!is.factor(train$IsLuxury)) {
  if (is.numeric(train$IsLuxury)) {
    train$IsLuxury <- factor(ifelse(train$IsLuxury == 1, "Yes", "No"),
                             levels = c("No","Yes"))
  } else {
    train$IsLuxury <- factor(train$IsLuxury)
    lv <- levels(train$IsLuxury)
    if (!all(c("No","Yes") %in% lv)) {
      train$IsLuxury <- forcats::fct_collapse(train$IsLuxury, No = lv[1], Yes = lv[2])
    }
    train$IsLuxury <- factor(train$IsLuxury, levels = c("No","Yes"))
  }
}

# predictors used in the rubric
preds <- c("price","crime_rate","property_tax_rate","previous_sale_price")
stopifnot(all(preds %in% names(train)))

# --- full mod

