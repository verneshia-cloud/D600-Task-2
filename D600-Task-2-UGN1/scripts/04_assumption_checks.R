# 04_assumption_checks.R
# VIF, Box–Tidwell, ROC (train/test), Hosmer–Lemeshow

library(tidyverse)
library(MASS)
library(car)
library(pROC)
library(ResourceSelection)
library(gridExtra)
library(forcats)

dir.create("outputs", showWarnings = FALSE)

train <- read.csv("data/train.csv")
test  <- read.csv("data/test.csv")

# --- rename target from is_luxury -> IsLuxury in BOTH datasets ---
if ("is_luxury" %in% names(train)) names(train)[names(train) == "is_luxury"] <- "IsLuxury"
if ("is_luxury" %in% names(test))  names(test) [names(test)  == "is_luxury"] <- "IsLuxury"

# --- normalize outcome to factor c("No","Yes") in BOTH datasets ---
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
stopifnot(all(preds %in% names(train)))

# ---
