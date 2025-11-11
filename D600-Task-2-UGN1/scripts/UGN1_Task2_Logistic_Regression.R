# --- D2 Optimization Note ---
# The model optimization (stepwise AIC + ROC threshold tuning)
# is implemented in scripts/02_optimize_model.R
# Run run_optimization.R to generate the D2 artifacts in ./outputs
# ---------------------------

# ================================================
# WGU D600 — UGN1 Task 2: Logistic Regression (RStudio)
# Author: Vineshia Smith
# ================================================

# ---- Robust preamble ----
options(repos = c(CRAN = "https://cloud.r-project.org"))
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = "true")

# Auto-set working directory to this script’s folder (RStudio)
try({
  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
    p <- rstudioapi::getActiveDocumentContext()$path
    if (!is.null(p) && nzchar(p)) setwd(dirname(p))
  }
}, silent = TRUE)

message("Working directory: ", getwd())

# ---- Packages ----
pkgs <- c(
  "tidyverse","janitor","skimr","GGally",
  "rsample","broom","MASS","car","pscl","lmtest",
  "yardstick","pROC","caret","ResourceSelection"
)
inst <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(inst)) install.packages(inst, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ---- Locate dataset (robust) ----
expected_name <- "D600 Task 2 Dataset 1 Housing Information.csv"
data_path <- expected_name
if (!file.exists(data_path)) {
  cand <- list.files(".", pattern = "^D600.*Task\\s*2.*Housing Information\\.csv$",
                     recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(cand) >= 1) {
    data_path <- cand[1]
    message("Found dataset at: ", data_path)
  } else {
    message("Select the dataset CSV in the file chooser…")
    data_path <- file.choose()
  }
}
stopifnot(file.exists(data_path))

# ---- I/O ----
dir.create("outputs", showWarnings = FALSE)

# ---- Read & clean ----
df <- readr::read_csv(data_path, show_col_types = FALSE) %>% janitor::clean_names()
drop_candidates <- c("id","unique_id","record_id")
df <- df %>% dplyr::select(-any_of(drop_candidates))

# ---- Encode categoricals & DV ----
to_factor <- intersect(names(df), c("fireplace","garage","house_color"))
for (v in to_factor) df[[v]] <- as.factor(df[[v]])

if ("is_luxury" %in% names(df)) {
  if (is.numeric(df$is_luxury)) df$is_luxury <- factor(ifelse(df$is_luxury==1,"Yes","No"), levels=c("No","Yes"))
  if (is.character(df$is_luxury)) df$is_luxury <- factor(df$is_luxury, levels=c("No","Yes"))
  if (is.factor(df$is_luxury)) df$is_luxury <- forcats::fct_relevel(df$is_luxury, "No","Yes")
} else stop("Dependent variable 'is_luxury' not found in data.")

# ---- Missingness (simple) ----
num_vars <- names(df)[sapply(df, is.numeric)]
cat_vars <- names(df)[sapply(df, is.factor)]
for (v in num_vars) if (anyNA(df[[v]])) df[[v]][is.na(df[[v]])] <- median(df[[v]], na.rm=TRUE)
mode_of <- function(x) names(sort(table(x), decreasing=TRUE))[1]
for (v in cat_vars) if (anyNA(df[[v]])) df[[v]][is.na(df[[v]])] <- mode_of(df[[v]])

# ---- DV & IVs ----
dv  <- "is_luxury"
ivs <- setdiff(names(df), dv)

# ---- Descriptive statistics ----
num_desc <- df %>% dplyr::select(all_of(intersect(ivs, num_vars))) %>%
  summarise(across(everything(),
                   list(n=~sum(!is.na(.x)), mean=~mean(.x,na.rm=TRUE), sd=~sd(.x,na.rm=TRUE),
                        min=~min(.x,na.rm=TRUE), q25=~quantile(.x,0.25,na.rm=TRUE),
                        median=~median(.x,na.rm=TRUE), q75=~quantile(.x,0.75,na.rm=TRUE),
                        max=~max(.x,na.rm=TRUE)), .names="{.col}.{.fn}"))
print(num_desc, width = Inf)
readr::write_csv(num_desc, "outputs/num_desc.csv")

cat_counts <- purrr::map(intersect(ivs, cat_vars), ~df %>% count(.data[[.x]], name="n"))
names(cat_counts) <- intersect(ivs, cat_vars)
print(cat_counts)
for (nm in names(cat_counts)) readr::write_csv(cat_counts[[nm]], file.path("outputs", paste0("counts_", nm, ".csv")))
skimr::skim(df)

# ---- Visualizations (saved to /outputs) ----
for (v in intersect(ivs, num_vars)) {
  p1 <- ggplot(df, aes(x = .data[[v]])) + geom_histogram(bins = 30) + labs(title = paste("Histogram:", v))
  ggsave(file.path("outputs", paste0("hist_", v, ".png")), p1, width = 6, height = 4, dpi = 150)
  p2 <- ggplot(df, aes(y = .data[[v]])) + geom_boxplot() + labs(title = paste("Boxplot:", v))
  ggsave(file.path("outputs", paste0("box_", v, ".png")), p2, width = 6, height = 4, dpi = 150)
}
for (v in intersect(ivs, cat_vars)) {
  p <- ggplot(df, aes(x = .data[[v]])) + geom_bar() + labs(title = paste("Bar:", v))
  ggsave(file.path("outputs", paste0("bar_", v, ".png")), p, width = 7, height = 4, dpi = 150)
}
for (v in intersect(ivs, num_vars)) {
  p <- ggplot(df, aes(x = .data[[v]], y = as.numeric(.data[[dv]]) - 1)) +
    geom_point(alpha = 0.2) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
    labs(title = paste("is_luxury vs", v, "(logit smoother)"), y = "is_luxury (0/1)")
  ggsave(file.path("outputs", paste0("dv_vs_", v, ".png")), p, width = 6, height = 4, dpi = 150)
}
for (v in intersect(ivs, cat_vars)) {
  p <- ggplot(df, aes(x = .data[[v]], y = as.numeric(.data[[dv]]) - 1)) +
    stat_summary(fun = mean, geom = "bar") +
    labs(title = paste("Mean P(is_luxury=Yes) by", v), y = "Mean of DV (0/1)")
  ggsave(file.path("outputs", paste0("dv_by_", v, ".png")), p, width = 7, height = 4, dpi = 150)
}

# ---- D1: Train/Test split (70/30, stratified) ----
set.seed(123)
split <- rsample::initial_split(df %>% dplyr::select(all_of(c(dv, ivs))), prop = 0.7, strata = dv)
train <- rsample::training(split)
test  <- rsample::testing(split)
message(paste("Train rows:", nrow(train), "| Test rows:", nrow(test)))
readr::write_csv(train, "train.csv")
readr::write_csv(test,  "test.csv")

# ---- D2: Model & optimization (stepwise AIC) ----
full_formula <- as.formula(paste(dv, "~", paste(ivs, collapse = " + ")))
glm_full <- glm(full_formula, data = train, family = binomial)
glm_step <- MASS::stepAIC(glm_full, direction = "both", trace = FALSE)

aic_val <- AIC(glm_step)
bic_val <- stats::BIC(glm_step)   # <-- fixed here
pseudo  <- tryCatch(pscl::pR2(glm_step), error = function(e) NULL)
pseudo_mcfadden <- if (!is.null(pseudo)) pseudo["McFadden"][[1]] else NA_real_

print(summary(glm_step))
readr::write_csv(tibble::tibble(AIC = aic_val, BIC = bic_val, McFadden_R2 = pseudo_mcfadden),
                 "outputs/model_info.csv")
readr::write_csv(broom::tidy(glm_step), "outputs/coef_table.csv")

# ---- D3: Train confusion matrix, accuracy, ROC/AUC ----
train$prob <- predict(glm_step, newdata = train, type = "response")
train$pred <- factor(ifelse(train$prob >= 0.5, "Yes", "No"), levels = c("No","Yes"))
cm_train <- caret::confusionMatrix(train$pred, train[[dv]], positive = "Yes")
acc_train <- unname(cm_train$overall["Accuracy"])
roc_train <- pROC::roc(response = train[[dv]], predictor = train$prob, levels = c("No","Yes"))
auc_train <- as.numeric(pROC::auc(roc_train))
readr::write_csv(as.data.frame(cm_train$table), "outputs/confusion_train.csv")
readr::write_csv(tibble::tibble(accuracy = acc_train, AUC = auc_train), "outputs/metrics_train.csv")
png("outputs/roc_train.png", width = 800, height = 600); plot(roc_train); dev.off()

# ---- D4: Test predictions using optimized variables only ----
opt_terms <- attr(terms(glm_step), "term.labels")
glm_opt <- glm(as.formula(paste(dv, "~", paste(opt_terms, collapse = " + "))),
               data = train, family = binomial)
test$prob <- predict(glm_opt, newdata = test, type = "response")
test$pred <- factor(ifelse(test$prob >= 0.5, "Yes", "No"), levels = c("No","Yes"))
cm_test <- caret::confusionMatrix(test$pred, test[[dv]], positive = "Yes")
acc_test <- unname(cm_test$overall["Accuracy"])
roc_test <- pROC::roc(response = test[[dv]], predictor = test$prob, levels = c("No","Yes"))
auc_test <- as.numeric(pROC::auc(roc_test))
readr::write_csv(as.data.frame(cm_test$table), "outputs/confusion_test.csv")
readr::write_csv(tibble::tibble(accuracy = acc_test, AUC = auc_test), "outputs/metrics_test.csv")
png("outputs/roc_test.png", width = 800, height = 600); plot(roc_test); dev.off()

# ---- E: Assumptions & diagnostics ----
vif_vals <- tryCatch(car::vif(glm_step), error = function(e) NA)
capture.output(vif_vals, file = "outputs/vif_values.txt")

pos_nums <- intersect(ivs, num_vars)
bt_df <- train
for (v in pos_nums) bt_df[[v]] <- bt_df[[v]] + 1e-6
bt_terms <- paste0(pos_nums, " + ", pos_nums, ":log(", pos_nums, ")")
bt_formula <- as.formula(paste(dv, "~", paste(bt_terms, collapse = " + ")))
bt_fit <- tryCatch(glm(bt_formula, data = bt_df, family = binomial), error = function(e) NULL)
if (!is.null(bt_fit)) capture.output(summary(bt_fit), file = "outputs/box_tidwell_summary.txt")

nums_to_plot <- head(names(sort(sapply(train[pos_nums], var, na.rm = TRUE), decreasing = TRUE)), 6)
for (v in nums_to_plot) {
  p <- ggplot(train, aes(x = .data[[v]], y = residuals(glm_step, type = "pearson"))) +
    geom_point(alpha = .2) + geom_smooth() + labs(title = paste("Residuals vs", v))
  ggsave(file.path("outputs", paste0("residuals_vs_", v, ".png")), p, width = 6, height = 4, dpi = 150)
}

hl <- tryCatch(ResourceSelection::hoslem.test(as.numeric(train[[dv]]) - 1, fitted(glm_step), g = 10),
               error = function(e) NULL)
if (!is.null(hl)) capture.output(hl, file = "outputs/hosmer_lemeshow.txt")

cooks <- cooks.distance(glm_step)
influential <- order(cooks, decreasing = TRUE)[1:min(20, length(cooks))]
readr::write_csv(tibble::tibble(row = influential, cooks = cooks[influential]), "outputs/top_cooks.csv")

readr::write_csv(
  tibble::tibble(
    AIC = aic_val, BIC = bic_val, McFadden_R2 = pseudo_mcfadden,
    Accuracy_Train = acc_train, Accuracy_Test = acc_test,
    AUC_Train = auc_train, AUC_Test = auc_test,
    N_Train = nrow(train), N_Test = nrow(test),
    Positives_Train = sum(train[[dv]] == "Yes"),
    Positives_Test  = sum(test[[dv]]  == "Yes")
  ),
  "outputs/metrics_summary.csv"
)

message("Done. Artifacts saved in ./outputs, plus train.csv and test.csv.")
