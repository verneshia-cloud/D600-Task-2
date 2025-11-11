# Export final equation (coeffs + OR) and sample predictions table for report
library(tidyverse); library(MASS); library(broom)
dir.create("outputs", showWarnings=FALSE)
train <- read.csv("data/train.csv"); test <- read.csv("data/test.csv")
if (!is.factor(train$IsLuxury)) train$IsLuxury <- factor(ifelse(train$IsLuxury==1,"Yes","No"), levels=c("No","Yes"))
full <- glm(IsLuxury ~ price + crime_rate + property_tax_rate + previous_sale_price, data=train, family=binomial())
opt  <- stepAIC(full, direction="both", trace=FALSE)

# E6: equation pieces
coef_tbl <- broom::tidy(opt) |> mutate(odds_ratio = exp(estimate))
write.csv(coef_tbl, "outputs/E6_equation_coefficients_and_OR.csv", row.names=FALSE)

# D4: sample predictions (first 10 rows of test)
thr <- if (file.exists("outputs/D2_threshold_auc_train.csv")) read.csv("outputs/D2_threshold_auc_train.csv")$threshold[1] else 0.5
prob_te <- predict(opt, newdata=test, type="response")
pred_te <- ifelse(prob_te>=thr, 1, 0)
out <- tibble(row=row_number(), prob_luxury=round(prob_te,4), class_at_thr=pred_te, actual=ifelse(test$IsLuxury=="Yes",1,0))
write.csv(head(out,10), "outputs/D4_sample_test_predictions.csv", row.names=FALSE)
