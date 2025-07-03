# Load required libraries
library(tidyverse)
library(mixtools)
library(cluster)
library(factoextra)
library(corrplot)
library(VIM)
library(mice)
library(flexmix)

# Load your dataset
nutrition_data <- read.csv("F:/Nutrition_Imputed.csv")

# Rename columns appropriately
colnames(nutrition_data) <- c(
  "Country", "Year", "UN_Region", "UN_sub_region", 
  "Stunting_model", "Overweight_model", "hcpi_a", "fcpi_a", 
  "Literacy_female", "Health_expenditure_GDP", 
  "Urban_population_pct", "Rural_population_pct", 
  "Under5_mortality_rate", "Unemployment_pct"
)

# Convert necessary columns to numeric
numeric_cols <- c("Year", "Stunting_model", "Overweight_model", "hcpi_a", 
                  "fcpi_a", "Literacy_female", "Health_expenditure_GDP", 
                  "Urban_population_pct", "Rural_population_pct", 
                  "Under5_mortality_rate", "Unemployment_pct")

nutrition_data <- nutrition_data %>%
  mutate(across(all_of(numeric_cols), ~as.numeric(as.character(.))))

# Check missing value proportions
missing_prop <- nutrition_data %>%
  summarise(across(everything(), ~sum(is.na(.)) / n())) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_prop")
print(missing_prop)

# Remove columns with more than 50% missing (if any)
keep_cols <- missing_prop %>%
  filter(missing_prop < 0.5) %>%
  pull(variable)

nutrition_clean <- nutrition_data %>%
  select(all_of(keep_cols))

# Simple imputation (median for numeric, mode for categorical)
nutrition_imputed <- nutrition_clean %>%
  mutate(across(where(is.numeric), ~ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), ~ifelse(is.na(.), 
                                             names(which.max(table(.))), .)))

# EDA — Select numeric variables
numeric_data <- nutrition_imputed %>%
  select(where(is.numeric)) %>%
  select(-Year) %>%  # Remove year
  na.omit()

# Standardize
numeric_scaled <- scale(numeric_data)

# Correlation plot
cor_matrix <- cor(numeric_scaled, use = "complete.obs")
corrplot(cor_matrix, method = "color", type = "upper", 
         order = "hclust", tl.cex = 0.8, tl.col = "black")

# GMM - Univariate (on Stunting_model as example)
bic_values <- c()
aic_values <- c()
k_range <- 2:6

set.seed(123)
for(k in k_range) {
  tryCatch({
    gmm_fit <- normalmixEM(numeric_scaled[, "Stunting_model"], k = k, maxit = 1000)
    bic_values <- c(bic_values, 2 * gmm_fit$loglik - k * log(nrow(numeric_scaled)))
    aic_values <- c(aic_values, 2 * gmm_fit$loglik - 2 * k)
  }, error = function(e) {
    bic_values <- c(bic_values, NA)
    aic_values <- c(aic_values, NA)
  })
}

# Plot BIC/AIC
plot_data <- data.frame(k = k_range, BIC = bic_values, AIC = aic_values)
plot_data %>%
  pivot_longer(cols = c(BIC, AIC), names_to = "Criterion", values_to = "Value") %>%
  ggplot(aes(x = k, y = Value, color = Criterion)) +
  geom_line() + geom_point() +
  labs(title = "Model Selection: BIC and AIC vs Number of Components",
       x = "Number of Components", y = "Criterion Value") +
  theme_minimal()

# Multivariate GMM using flexmix
key_vars <- c("Stunting_model", "Under5_mortality_rate", "Health_expenditure_GDP", 
              "Literacy_female", "Urban_population_pct")

available_vars <- intersect(key_vars, names(numeric_data))
multivar_data <- numeric_data %>%
  select(all_of(available_vars)) %>%
  na.omit()

multivar_scaled <- scale(multivar_data)

gmm_models <- list()
for(k in 2:5) {
  tryCatch({
    gmm_models[[paste0("k", k)]] <- flexmix(multivar_scaled ~ 1, 
                                            k = k, 
                                            model = FLXMCmvnorm())
  }, error = function(e) {
    message("Error fitting model with k = ", k)
  })
}

model_comparison <- data.frame(
  k = 2:5,
  AIC = sapply(gmm_models, function(x) if (!is.null(x)) AIC(x) else NA),
  BIC = sapply(gmm_models, function(x) if (!is.null(x)) BIC(x) else NA)
)

print(model_comparison)

best_k <- model_comparison$k[which.min(model_comparison$BIC)]
best_model <- gmm_models[[paste0("k", best_k)]]
cluster_assignments <- clusters(best_model)

nutrition_with_clusters <- nutrition_imputed %>%
  slice(1:length(cluster_assignments)) %>%
  mutate(Cluster = as.factor(cluster_assignments))

# PCA visualization
pca_result <- prcomp(multivar_scaled)
pca_data <- data.frame(
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  Cluster = as.factor(cluster_assignments)
)

ggplot(pca_data, aes(PC1, PC2, color = Cluster)) +
  geom_point(size = 2, alpha = 0.7) +
  stat_ellipse(geom = "polygon", aes(fill = Cluster), alpha = 0.2) +
  labs(title = "PCA of Nutrition Clusters") +
  theme_minimal()

# Summary of clusters
cluster_summary <- nutrition_with_clusters %>%
  group_by(Cluster) %>%
  summarise(across(all_of(available_vars), mean, na.rm = TRUE), .groups = "drop")

print(cluster_summary)

# Diagnostics
posterior_probs <- posterior(best_model)
posterior_df <- data.frame(
  observation = 1:nrow(posterior_probs),
  posterior_probs
) %>%
  pivot_longer(-observation, names_to = "component", values_to = "probability")

ggplot(posterior_df, aes(observation, probability, fill = component)) +
  geom_bar(stat = "identity") +
  labs(title = "Posterior Probabilities", x = "Observation", y = "Probability") +
  theme_minimal()

cat("\n=== GENERALIZED MIXTURE MODEL ANALYSIS COMPLETE ===\n")
cat("Dataset shape:", nrow(nutrition_imputed), "rows,", ncol(nutrition_imputed), "columns\n")
cat("Optimal number of mixture components:", best_k, "\n")
