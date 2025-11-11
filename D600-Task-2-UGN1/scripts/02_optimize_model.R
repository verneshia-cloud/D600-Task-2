# Stepwise AIC (both) + model summary + AIC, BIC, pseudo-R2, coeffs, p-values + ROC threshold by Youden
library(tidyverse); library(MASS); library(pscl); library(pROC); library(broom)
dir.create("outputs", showWarnings=FALSE)
train <- read.csv("data/train.csv")
if (!is.factor(train$IsLuxury)) train$IsLuxury <- factor(ifelse(train$IsLuxury==1,"Yes","No"), levels=c("No","Yes"))
preds <- c("price","crime_rate","property_tax_rate","previous_sale_price"); stopifnot(all(preds %in% names(train)))

full <- glm(IsLuxury ~ price + crime_rate + property_tax_rate + previous_sale_price, data=train, family=binomial())
opt  <- stepAIC(full, direction="both", trace=FALSE)

pR2 <- pscl::pR2(opt)[["McFadden"]]
coef_tbl <- broom::tidy(opt) |> mutate(odds_ratio = exp(estimate)) |>
  select(term, estimate, std.error, statistic, p.value, odds_ratio)
write.csv(coef_tbl, "outputs/D2_model_coefficients.csv", row.names=FALSE)

sink("outputs/D2_model_summary.txt")
cat("Final formula:\n"); print(formula(opt))
cat("\nAIC:", AIC(opt), "  BIC:", BIC(opt), "\n")
cat("\nPseudo-R2 (McFadden):", round(pR2,4), "\n\n")
print(coef(summary(opt)))
sink()

# ROC on training + Youden threshold (frozen for test)
prob_tr <- predict(opt, type="response")
roc_tr  <- pROC::roc(response=train$IsLuxury, predictor=prob_tr, levels=c("No","Yes"))
thr     <- as.numeric(coords(roc_tr, "best", best.method="youden", ret="threshold"))
if (is.na(thr)) thr <- 0.5
write.csv(data.frame(threshold=thr, AUC=as.numeric(auc(roc_tr))), "outputs/D2_threshold_auc_train.csv", row.names=FALSE)
png("outputs/D2_train_ROC.png", width=650, height=550); plot(roc_tr, main=paste0("Train ROC (AUC=", round(auc(roc_tr),3),")")); dev.off()
