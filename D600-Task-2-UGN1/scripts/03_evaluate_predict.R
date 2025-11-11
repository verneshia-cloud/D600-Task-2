# Confusion & accuracy on train (optimized model) and test (same threshold)
library(tidyverse); library(MASS); library(caret); library(pROC)
dir.create("outputs", showWarnings=FALSE)
train <- read.csv("data/train.csv"); test <- read.csv("data/test.csv")
toY <- function(d){ if(!is.factor(d$IsLuxury)) d$IsLuxury <- factor(ifelse(d$IsLuxury==1,"Yes","No"), levels=c("No","Yes")); d }
train <- toY(train); test <- toY(test)
preds <- c("price","crime_rate","property_tax_rate","previous_sale_price"); stopifnot(all(preds %in% names(train)), all(preds %in% names(test)))
full <- glm(IsLuxury ~ price + crime_rate + property_tax_rate + previous_sale_price, data=train, family=binomial())
opt  <- stepAIC(full, direction="both", trace=FALSE)

thr <- read.csv("outputs/D2_threshold_auc_train.csv")$threshold[1]; if(is.na(thr)) thr <- 0.5

prob_tr <- predict(opt, type="response"); pred_tr <- factor(ifelse(prob_tr>=thr,"Yes","No"), levels=c("No","Yes"))
cm_tr <- caret::confusionMatrix(pred_tr, train$IsLuxury, positive="Yes")
png("outputs/D3_train_confusion.png", width=720, height=420); gridExtra::grid.arrange(gridExtra::tableGrob(cm_tr$table)); dev.off()
write.csv(as.data.frame(cm_tr$table), "outputs/D3_train_confusion.csv", row.names=FALSE)
write.csv(data.frame(Accuracy=cm_tr$overall["Accuracy"]), "outputs/D3_train_accuracy.csv", row.names=FALSE)

prob_te <- predict(opt, newdata=test, type="response"); pred_te <- factor(ifelse(prob_te>=thr,"Yes","No"), levels=c("No","Yes"))
cm_te <- caret::confusionMatrix(pred_te, test$IsLuxury, positive="Yes")
png("outputs/D4_test_confusion.png", width=720, height=420); gridExtra::grid.arrange(gridExtra::tableGrob(cm_te$table)); dev.off()
write.csv(as.data.frame(cm_te$table), "outputs/D4_test_confusion.csv", row.names=FALSE)
write.csv(data.frame(Accuracy=cm_te$overall["Accuracy"]), "outputs/D4_test_accuracy.csv", row.names=FALSE)

# Extra metrics (E7)
auc_tr <- as.numeric(pROC::auc(pROC::roc(train$IsLuxury, prob_tr, levels=c("No","Yes"))))
auc_te <- as.numeric(pROC::auc(pROC::roc(test$IsLuxury,  prob_te, levels=c("No","Yes"))))
metrics <- tibble(split=c("train","test"), threshold=thr,
  accuracy=c(cm_tr$overall["Accuracy"], cm_te$overall["Accuracy"]),
  precision=c(cm_tr$byClass["Precision"], cm_te$byClass["Precision"]),
  recall=c(cm_tr$byClass["Sensitivity"], cm_te$byClass["Sensitivity"]),
  specificity=c(cm_tr$byClass["Specificity"], cm_te$byClass["Specificity"]),
  f1=c(cm_tr$byClass["F1"], cm_te$byClass["F1"]), auc=c(auc_tr, auc_te))
write.csv(metrics, "outputs/E7_metrics_train_test.csv", row.names=FALSE)
