# Stratified 70/30 split on IsLuxury; saves data/train.csv and data/test.csv
library(tidyverse); set.seed(42)
df <- read.csv("data/D600_Task_2_full.csv")  # <== if you already have train/test, skip this file and load your full CSV here
stopifnot("IsLuxury" %in% names(df))
# make factor with levels No/Yes
if (!is.factor(df$IsLuxury)) {
  df$IsLuxury <- if (is.numeric(df$IsLuxury)) factor(ifelse(df$IsLuxury==1,"Yes","No"), levels=c("No","Yes"))
  else factor(df$IsLuxury, levels=c("No","Yes"))
}
# keep only rubric vars (C1)
keep <- c("IsLuxury","price","crime_rate","property_tax_rate","previous_sale_price")
stopifnot(all(keep %in% names(df))); df <- df[keep]
# stratified 70/30
idx <- df %>% mutate(id=row_number()) %>% group_by(IsLuxury) %>% group_split()
get_split <- function(d){
  n <- nrow(d); n_tr <- floor(0.7*n); tr <- sample(d$id, n_tr); list(tr=tr, te=setdiff(d$id,tr))
}
S <- lapply(idx, get_split)
tr_idx <- unlist(lapply(S, `[[`, "tr"))
te_idx <- unlist(lapply(S, `[[`, "te"))
train <- df[tr_idx, ]; test <- df[te_idx, ]
dir.create("data", showWarnings=FALSE)
write.csv(train, "data/train.csv", row.names=FALSE)
write.csv(test,  "data/test.csv",  row.names=FALSE)
