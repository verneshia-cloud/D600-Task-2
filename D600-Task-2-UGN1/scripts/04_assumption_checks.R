# VIF, Box–Tidwell, ROC (train/test), Hosmer–Lemeshow
library(tidyverse); library(MASS); library(car); library(pROC); library(ResourceSelection); library(gridExtra)
dir.create("outputs", showWarnings=FALSE)
train <- read.csv("data/train.csv"); test <- read.csv("data/test.csv")
toY <- function(d){ if(!is.factor(d$IsLuxury)) d$IsLuxury <- factor(ifelse(d$IsLuxury==1,"Yes","No"), levels=c("No","Yes")); d }
train <- toY(train); test <- toY(test)
preds <- c("price","crime_rate","property_tax_rate","previous_sale_price"); stopifnot(all(preds %in% names(train)))
full <- glm(IsLuxury ~ price + crime_rate + property_tax_rate + previous_sale_price, data=train, family=binomial())
opt  <- stepAIC(full, direction="both", trace=FALSE)

# VIF
v <- car::vif(opt); write.csv(data.frame(predictor=names(v), VIF=as.numeric(v)), "outputs/E5_assumption_vif.csv", row.names=FALSE)

# Box–Tidwell (ensure positivity by shifting if needed)
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

# ROC (train/test)
prob_tr <- predict(opt, type="response"); roc_tr <- pROC::roc(train$IsLuxury, prob_tr, levels=c("No","Yes"))
png("outputs/E5_train_ROC.png", width=650, height=550); plot(roc_tr, main=paste0("Train ROC (AUC=", round(pROC::auc(roc_tr),3),")")); dev.off()
prob_te <- predict(opt, newdata=test, type="response"); roc_te <- pROC::roc(test$IsLuxury, prob_te, levels=c("No","Yes"))
png("outputs/E5_test_ROC.png", width=650, height=550); plot(roc_te, main=paste0("Test ROC (AUC=", round(pROC::auc(roc_te),3),")")); dev.off()

# Hosmer–Lemeshow on test
y_test <- ifelse(test$IsLuxury=="Yes",1,0)
hl <- ResourceSelection::hoslem.test(y_test, prob_te, g=10)
sink("outputs/E5_hosmer_lemeshow_test.txt"); print(hl); sink()
