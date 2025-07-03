# ==============================================
# SAFE + ROBUST MICE IMPUTATION WITH ALL FIXES
# ==============================================

# Load required libraries
library(mice)
library(VIM)
library(tidyverse)
library(caret)

# Step 1: Load data
data <- read.csv("F:/Nutrition.csv")
cat("✅ Data loaded successfully\n")

# Step 2: Remove columns with too many missing values (> 50%)
missing_pct <- colMeans(is.na(data))
high_missing <- names(missing_pct[missing_pct > 0.5])
cat("🧹 Dropping columns with >50% missing:\n")
print(high_missing)
data <- data[, !(names(data) %in% high_missing)]

# Step 3: Remove near-zero variance columns
nzv <- nearZeroVar(data, saveMetrics = TRUE)
nzv_cols <- rownames(nzv[nzv$nzv == TRUE, ])
cat("🧹 Dropping near-zero variance columns:\n")
print(nzv_cols)
data <- data[, !(names(data) %in% nzv_cols)]

# Step 4: Drop linearly dependent columns (perfect correlation)
# Remove one of each pair of highly correlated variables (correlation > 0.98)
numeric_data <- data[, sapply(data, is.numeric)]
cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")

# Find highly correlated pairs
high_corr <- findCorrelation(cor_matrix, cutoff = 0.98, names = TRUE)
cat("🧹 Dropping highly correlated variables:\n")
print(high_corr)
data <- data[, !(names(data) %in% high_corr)]

# Step 5: Convert character variables to factor except 'Country'
data_for_mice <- data %>%
  mutate(across(where(is.character), ~if (cur_column() == "Country") . else as.factor(.)))

# Step 6: Define imputation methods
init_mice <- mice(data_for_mice, maxit = 0, printFlag = FALSE)
methods <- init_mice$method

# Safely assign methods
for (col in names(data_for_mice)) {
  if (col == "Country") {
    methods[col] <- ""
  } else if (is.numeric(data_for_mice[[col]])) {
    methods[col] <- "pmm"
  } else if (is.factor(data_for_mice[[col]])) {
    methods[col] <- "polyreg"
  } else {
    methods[col] <- "cart"
  }
}

# Optional: Skip known problematic variable
if ("Stunting_survey" %in% names(methods)) {
  cat("⚠️ Skipping problematic variable: Stunting_survey\n")
  methods["Stunting_survey"] <- ""
}

# Step 7: Perform MICE imputation
cat("🔁 Running MICE Imputation...\n")
mice_result <- mice(data_for_mice, 
                    method = methods,
                    m = 5,
                    maxit = 10,
                    seed = 123,
                    printFlag = TRUE)

# Step 8: Extract completed dataset
nutrition_imputed <- complete(mice_result, 1)

# Step 9: Save to CSV
write.csv(nutrition_imputed, "F:/Nutrition_Imputed.csv", row.names = FALSE)
cat("\n✅ Final imputed dataset saved to: F:/Nutrition_Imputed.csv\n")
