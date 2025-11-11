# 99_force_generate_all.R — force-generate missing outputs with clear messages

says <- function(...) cat("[OK]", ..., "\n")
oops <- function(...) stop("[ERROR] ", paste(..., collapse=" "))

dir.create("outputs", showWarnings = FALSE)

library(tidyverse); library(MASS); library(pscl); library(pROC); library(broom)
library(caret); library(car); library(ResourceSelection); library(gridExtra); library(forcats)

# ---- load data & normalize outcome ----
train <- read.csv("data/train.csv"); test <- read.csv("data/test.csv")
if ("is_luxury" %in% names(train)) names(train)[names(train) == "is_luxury"] <- "IsLuxury"
if ("is_luxury" %in% names(test))  names(test) [names(test)  == "is_luxury"]  <- "IsLuxury"

normY <- function(df){
  if (!is.factor(df$IsLuxury)) {
    if (is.numeric(df$IsLuxury)) {
      df$IsLuxury <- factor(ifelse(df$IsLuxury==1,"Yes","No"), levels=c("No","Yes"))
    } else {
      df$IsLuxury <- factor(df$IsLuxury)
      lv <- levels(df$IsLuxury)
      if (!all(c("No","Yes") %in% lv)) df$IsLuxury <- forcats::fct_collapse(df$IsLuxury, No=lv[1], Yes=lv[2])
      df$IsLuxury <- factor(df$IsLuxury, levels=c("No","Yes"))
    }
  }
  df
}
train <- normY(train); test <- normY(test)

preds <- c("price","crime_rate","property_tax_rate","previous_sale_price")
if (!all(preds %in% names(train))) oops("Missing predictors in train:", paste(setdiff(preds, names(train)), collapse=", "))
if (!all(preds %in% names(test)))  oops("Missing predictors in test:",  paste(setdiff(preds, names(test)),  collapse=", "))

# ---- fit optimized model on train ----
full <- glm(IsLuxury ~ price + crime_rate + property_tax_rate + previous_sale_price, data=train, family=binomial())
opt  <- stepAIC(full, direction="both", trace=FALSE)

# ================= D2 =================
pR2 <- pscl::pR2(opt)[["McFadden"]]
coef_tbl <- broom::tidy(opt) |>
  dplyr::mutate(odds_ratio = exp(estimate)) |>
  dplyr::select(term, estimate, std.error, statistic, p.value, odds_ratio)
write.csv(coef_tbl, "outputs/D2_model_coefficients.csv", row.names=FALSE)

sink("outputs/D2_model_summary.txt")
cat("Final formula:\n"); print(formula(opt))
cat("\nAIC:", AIC(opt), "  BIC:", BIC(opt), "\n")
cat("\nPseudo-R2 (McFadden):", round(pR2,4), "\n\n")
print(coef(summary(opt)))
sink()

prob_tr <- predict(opt, type="response")
roc_tr  <- pROC::roc(response=train$IsLuxury, predictor=prob_tr, levels=c("No","Yes"))
thr     <- as.numeric(pROC::coords(roc_tr, "best", best.method="youden", ret="threshold"))
if (is.na(thr)) thr <- 0.5
write.csv(data.frame(threshold=thr, AUC=as.numeric(pROC::auc(roc_tr))), "outputs/D2_threshold_auc_train.csv", row.names=FALSE)
png("outputs/D2_train_ROC.png", width=650, height=550); plot(roc_tr, main=paste0("Train ROC (AUC=", round(pROC::auc(roc_tr),3),")")); dev.off()
says("Wrote D2 files.")

# ================= D3/D4/E7 (train & test) =================
prob_te <- predict(opt, newdata=test, type="response")
pred_tr <- factor(ifelse(prob_tr >= thr, "Yes","No"), levels=c("No","Yes"))
pred_te <- factor(ifelse(prob_te >= thr, "Yes","No"), levels=c("No","Yes"))

cm_tr <- caret::confusionMatrix(pred_tr, train$IsLuxury, positive="Yes")
png("outputs/D3_train_confusion.png", width=720, height=420); gridExtra::grid.arrange(gridExtra::tableGrob(cm_tr$table)); dev.off()
write.csv(as.data.frame(cm_tr$table), "outputs/D3_train_confusion.csv", row.names=FALSE)
write.csv(data.frame(Accuracy=cm_tr$overall["Accuracy"]), "outputs/D3_train_accuracy.csv", row.names=FALSE)

cm_te <- caret::confusionMatrix(pred_te, test$IsLuxury, positive="Yes")
png("outputs/D4_test_confusion.png", width=720, height=420); gridExtra::grid.arrange(gridExtra::tableGrob(cm_te$table)); dev.off()
write.csv(as.data.frame(cm_te$table), "outputs/D4_test_confusion.csv", row.names=FALSE)
write.csv(data.frame(Accuracy=cm_te$overall["Accuracy"]), "outputs/D4_test_accuracy.csv", row.names=FALSE)

auc_tr <- as.numeric(pROC::auc(pROC::roc(train$IsLuxury, prob_tr, levels=c("No","Yes"))))
auc_te <- as.numeric(pROC::auc(pROC::roc(test$IsLuxury,  prob_te, levels=c("No","Yes"))))
metrics <- tibble(split=c("train","test"), threshold=thr,
                  accuracy=c(cm_tr$overall["Accuracy"], cm_te$overall["Accuracy"]),
                  precision=c(cm_tr$byClass["Precision"], cm_te$byClass["Precision"]),
                  recall=c(cm_tr$byClass["Sensitivity"], cm_te$byClass["Sensitivity"]),
                  specificity=c(cm_tr$byClass["Specificity"], cm_te$byClass["Specificity"]),
                  f1=c(cm_tr$byClass["F1"], cm_te$byClass["F1"]),
                  auc=c(auc_tr, auc_te))
write.csv(metrics, "outputs/E7_metrics_train_test.csv", row.names=FALSE)
says("Wrote D3/D4/E7 files.")

# ================= E5 (assumptions) =================
v <- car::vif(opt); write.csv(data.frame(predictor=names(v), VIF=as.numeric(v)), "outputs/E5_assumption_vif.csv", row.names=FALSE)

df_bt <- train; shifts <- c(); bt_p <- c()
for (vname in preds){
  minv <- min(df_bt[[vname]], na.rm=TRUE)
  shift <- ifelse(minv<=0, abs(minv)+.Machine$double.eps*10, 0)
  if (shift>0) df_bt[[vname]] <- df_bt[[vname]] + shift
  shifts <- c(shifts, shift)
  pval <- tryCatch({ suppressWarnings(car::boxTidwell(as.formula(paste("as.numeric(IsLuxury) ~", vname)), data=df_bt)$test[1,"p.value"]) }, error=function(e) NA_real_)
  bt_p <- c(bt_p, pval)
}
write.csv(tibble(predictor=preds, shift_applied=shifts, p_value_logterm=bt_p), "outputs/E5_assumption_box_tidwell.csv", row.names=FALSE)

png("outputs/E5_train_ROC.png", width=650, height=550); plot(roc_tr, main=paste0("Train ROC (AUC=", round(pROC::auc(roc_tr),3),")")); dev.off()
roc_te <- pROC::roc(test$IsLuxury, prob_te, levels=c("No","Yes"))
png("outputs/E5_test_ROC.png", width=650, height=550); plot(roc_te, main=paste0("Test ROC (AUC=", round(pROC::auc(roc_te),3),")")); dev.off()

y_test <- ifelse(test$IsLuxury=="Yes",1,0)
hl <- ResourceSelection::hoslem.test(y_test, prob_te, g=10)
sink("outputs/E5_hosmer_lemeshow_test.txt"); print(hl); sink()
says("Wrote E5 files.")
