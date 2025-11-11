# 00_split_data.R
# Stratified 70/30 split on IsLuxury; writes data/train.csv and data/test.csv

library(tidyverse)
set.seed(42)

dir.create("data", showWarnings = FALSE)

full_path <- "data/D600_Task_2_full.csv"

# ---------- 1) Decide input ----------
if (!file.exists(full_path)) {
  if (file.exists("data/train.csv") && file.exists("data/test.csv")) {
    message("[00] train/test already exist; no split performed.")
    quit(save = "no")
  } else {
    stop("[00] Could not find ", full_path,
         ". Either place your full dataset there OR skip this script because you already have train/test.")
  }
}

# ---------- 2) Load full dataset ----------
df <- read.csv(full_path, check.names = FALSE)

# ---------- 3) Normalize target to IsLuxury (No/Yes) ----------
# (your raw file uses 'is_luxury')
if ("is_luxury" %in% names(df)) {
  names(df)[names(df) == "is_luxury"] <- "IsLuxury"
}
if (!("IsLuxury" %in% names(df))) {
  stop("[00] Could not find outcome column 'IsLuxury' (or 'is_luxury').")
}

# Make factor with levels No/Yes consistently
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

# ---------- 4) Keep rubric variables ----------
keep <- c("IsLuxury","price","crime_rate","property_tax_rate","previous_sale_price")
missing_keep <- setdiff(keep, names(df))
if (length(missing_keep)) {
  stop("[00] Missing required columns in full dataset: ", paste(missing_keep, collapse = ", "))
}
df <- df[, keep]

# ---------- 5) Stratified 70/30 split ----------
split_one_group <- function(d, p_train = 0.70) {
  n <- nrow(d)
  n_tr <- floor(p_train * n)
  idx_tr <- sample(seq_len(n), n_tr)
  list(train = d[idx_tr, , drop = FALSE],
       test  = d[-idx_tr, , drop = FALSE])
}

parts <- df %>%
  group_by(IsLuxury) %>%
  group_split() %>%
  lapply(split_one_group)

train <- bind_rows(lapply(parts, `[[`, "train"))
test  <- bind_rows(lapply(parts, `[[`, "test"))

# ---------- 6) Write outputs ----------
write.csv(train, "data/train.csv", row.names = FALSE)
write.csv(test,  "data/test.csv",  row.names = FALSE)

message("[00] Wrote data/train.csv (", nrow(train), " rows) and data/test.csv (", nrow(test), " rows).")
