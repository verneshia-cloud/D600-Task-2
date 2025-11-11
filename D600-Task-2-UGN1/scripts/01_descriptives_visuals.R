# 01_descriptives_visuals.R  (C2 & C3)
library(tidyverse)
library(gridExtra)

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

# numeric predictors used in the task
num_vars <- c("price","crime_rate","property_tax_rate","previous_sale_price")
stopifnot(all(num_vars %in% names(train)))

# ----- C2: Descriptive stats -> PNG + CSV -----
summ <- train |>
  summarise(across(all_of(num_vars),
                   list(N     = ~sum(!is.na(.)),
                        Mean  = ~mean(., na.rm = TRUE),
                        SD    = ~sd(.,   na.rm = TRUE),
                        Min   = ~min(.,  na.rm = TRUE),
                        Q1    = ~quantile(., .25, na.rm = TRUE),
                        Median= ~median(.,       na.rm = TRUE),
                        Q3    = ~quantile(., .75, na.rm = TRUE),
                        Max   = ~max(.,  na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# save table as CSV and PNG
write.csv(round(t(summ), 4), "outputs/C2_descriptives_table.csv", row.names = TRUE)

png("outputs/C2_descriptives.png", width = 1500, height = 450)
gridExtra::grid.arrange(tableGrob(round(t(summ), 4)))
dev.off()

# class counts (for report)
train |>
  count(IsLuxury) |>
  write.csv("outputs/C2_class_counts.csv", row.names = FALSE)

# ----- C3: Univariate histograms + bivariate boxplots -----
for (v in num_vars) {
  g1 <- ggplot(train, aes(x = .data[[v]])) +
    geom_histogram(bins = 30) +
    labs(title = paste("Univariate:", v), x = v, y = "Count")
  ggsave(paste0("outputs/C3_univariate_", v, ".png"), g1, width = 6, height = 4, dpi = 150)
  
  g2 <- ggplot(train, aes(x = IsLuxury, y = .data[[v]])) +
    geom_boxplot() +
    labs(title = paste(v, "by IsLuxury"), x = "IsLuxury", y = v)
  ggsave(paste0("outputs/C3_bivariate_vs_IsLuxury_", v, ".png"), g2, width = 6, height = 4, dpi = 150)
}

