# Load required libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(knitr)

# Load the dataset ----
df <- read.csv("/Users/pranup/Downloads/Analytics_Practicum/Project3/Consumer_Data.csv")
# View structure and column names
str(df)
colnames(df)
# 3. Data Preprocessing ------
# Summary Statistics for Numerical Variables ----------
library(dplyr)
# Exclude binary variables and near-constant columns
# Define a list of true numeric predictors (manually filtered)
numeric_vars <- c("Year_Birth", "Income", "Kidhome", "Teenhome", "Recency",
                  "MntWines", "MntFruits", "MntMeatProducts", "MntFishProducts",
                  "MntSweetProducts", "MntGoldProds", "NumDealsPurchases",
                  "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases",
                  "NumWebVisitsMonth")
# Subset only those columns
df_numeric <- df %>% dplyr::select(all_of(numeric_vars))
# Summary statistics
num_summary <- summary(df_numeric)
print(num_summary)

# Frequency Table for Categorical Variables ----------
# Frequency table for nominal categorical variables
cat_vars <- c("Education", "Marital_Status")
for (col in cat_vars) {
  cat("\nFrequency of", col, ":\n")
  print(table(df[[col]]))
}
# Frequency table for binary categorical variables
binary_vars <- c("AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3", 
                 "AcceptedCmp4", "AcceptedCmp5", "Complain", "Response")
for (col in binary_vars) {
  cat("\nFrequency of", col, ":\n")
  print(table(df[[col]]))
}

# Convert to character for modification
df$Marital_Status <- as.character(df$Marital_Status)
# Group less common variations into "Single"
df$Marital_Status[df$Marital_Status %in% c("YOLO", "Alone", "Absurd")] <- "Single"
# Convert back to factor
df$Marital_Status <- as.factor(df$Marital_Status)
# View updated frequencies
table(df$Marital_Status)

# Missing Values Check ----------
# Count missing values per column
missing_counts <- colSums(is.na(df))
print("Missing Values per Column:")
print(missing_counts)
# Zero Values in Numeric Columns ----------
# Subset only numerical columns
#df_numeric <- df[, numeric_vars]
# Count zero values per numerical column
zero_counts <- colSums(df_numeric == 0, na.rm = TRUE)
# Display results
print("Zero Values per Numerical Column:")
print(zero_counts)

# Count duplicate rows
duplicate_count <- sum(duplicated(df))
print(paste("Number of duplicate rows:", duplicate_count))

# Impute missing Income values with median (excluding NAs)
df$Income[is.na(df$Income)] <- median(df$Income, na.rm = TRUE)
summary(df$Income)
# Identify Outlier years and Impute with median
sum(df$Year_Birth == 1893)
df$Year_Birth[df$Year_Birth < 1900] <- median(df$Year_Birth[df$Year_Birth >= 1900])
summary(df$Year_Birth)
sum(df$Year_Birth == 1900)
df$Year_Birth[df$Year_Birth < 1901] <- median(df$Year_Birth[df$Year_Birth >= 1901])
summary(df$Year_Birth)

# Outlier Detection using IQR Method ----
# Set up boxplots for visual inspection
par(mfrow = c(3, 3))
for (col in numeric_vars) {
  boxplot(df[[col]],
          main = paste("Boxplot of", col),
          col = "lightgray",
          horizontal = TRUE)
}

# Set IQR threshold multiplier
iqr_threshold <- 1.5

# Create a copy of the dataset to flag outliers
df_outlier_flagged <- df

# Loop through numeric variables and flag outliers
for (col in numeric_vars) {
  Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
  Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
  IQR_val <- Q3 - Q1
  
  lower_bound <- Q1 - iqr_threshold * IQR_val
  upper_bound <- Q3 + iqr_threshold * IQR_val
  
  # Flag outliers
  flag_col <- paste0("outlier_", col)
  df_outlier_flagged[[flag_col]] <- df[[col]] < lower_bound | df[[col]] > upper_bound
}

# Count number of outliers per variable
outlier_summary <- sapply(grep("^outlier_", names(df_outlier_flagged), value = TRUE), function(col) {
  sum(df_outlier_flagged[[col]], na.rm = TRUE)
})

# Print summary
cat("Outlier counts per numeric variable:\n")
print(outlier_summary)
# Create a data frame with only rows that have at least one outlier
df_outliers_iqr <- df_outlier_flagged[rowSums(df_outlier_flagged[, grep("^outlier_", names(df_outlier_flagged))]) > 0, ]

# Exploring Variable Relationships ----
# Reset plotting area before plotting next charts
par(mfrow = c(1, 1))
# Load correlation plotting library
library(corrplot)
# Compute correlation matrix
cor_matrix <- cor(df[, numeric_vars], use = "complete.obs")
# Plot correlation heatmap
corrplot(cor_matrix, method = "color", type = "lower", 
         tl.col = "black", tl.srt = 45, 
         title = "Correlation Heatmap of Numerical Variables", 
         mar = c(0, 0, 2, 0))

# Income vs. Total Spending (scatterplot)
df <- df %>%
  mutate(Total_Spending = MntWines + MntFruits + MntMeatProducts +
           MntFishProducts + MntSweetProducts + MntGoldProds)

library(scales)

ggplot(df, aes(x = Income, y = Total_Spending)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  scale_x_continuous(labels = comma) +
  labs(title = "Income vs. Total Spending",
       x = "Income", y = "Total Spending") +
  theme_minimal()


# Response Rate by Education
ggplot(df, aes(x = Education, fill = factor(Response))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("grey70", "skyblue"), 
                    labels = c("No", "Yes")) +
  labs(title = "Response Rate by Education Level",
       x = "Education", y = "Proportion", fill = "Response") +
  theme_minimal()

# Response Rate by Marital Status
ggplot(df, aes(x = Marital_Status, fill = factor(Response))) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("gray70", "skyblue"),
                    labels = c("No", "Yes")) +
  labs(title = "Response Rate by Marital Status",
       x = "Marital Status", y = "Proportion", fill = "Response") +
  theme_minimal()


# 4. Predictor Analysis and Relevancy -----
# Load necessary libraries
library(car)
library(ggpubr)

# Correlation already completed in previous section using corrplot

# VIF for multicollinearity among numeric variables
# Build a dummy linear model to check multicollinearity using existing numeric_vars
vif_model <- lm(Income ~ ., data = df[, numeric_vars])
vif_values <- vif(vif_model)

# Print the VIF values
print("VIF Values for Numeric Variables:")
print(vif_values)

# Chi-Square Tests for Binary Categorical Variables
# Run pairwise chi-square tests
chi_results <- list()

for (i in 1:(length(binary_vars) - 1)) {
  for (j in (i + 1):length(binary_vars)) {
    tbl <- table(df[[binary_vars[i]]], df[[binary_vars[j]]])
    test <- chisq.test(tbl)
    
    chi_results[[paste(binary_vars[i], binary_vars[j], sep = "_vs_")]] <- list(
      variable_pair = c(binary_vars[i], binary_vars[j]),
      p_value = test$p.value,
      statistic = test$statistic,
      result = ifelse(test$p.value < 0.05, "Significant", "Not Significant")
    )
  }
}

# Display formatted results
for (name in names(chi_results)) {
  cat("\n", name, "\n")
  print(chi_results[[name]])
}

# Visualizing relationship between numeric variables and Response
par(mfrow = c(3, 3))

for (var in numeric_vars) {
  boxplot(df[[var]] ~ df$Response,
          main = paste("Response vs", var),
          xlab = "Response (0 = No, 1 = Yes)",
          ylab = var,
          col = c("lightblue", "lightgreen"))
}

# Create a results list
numeric_response_tests <- list()

# Loop over each numeric variable
for (var in numeric_vars) {
  # Run Wilcoxon test between groups Response = 0 and 1
  test_result <- wilcox.test(df[[var]] ~ df$Response)
  
  # Store results
  numeric_response_tests[[var]] <- list(
    p_value = test_result$p.value,
    statistic = test_result$statistic,
    result = ifelse(test_result$p.value < 0.05, "Significant", "Not Significant")
  )
}

# Display the results
for (var in names(numeric_response_tests)) {
  cat("\n", var, ":\n")
  print(numeric_response_tests[[var]])
}

colnames(df)

# 5. Data engineering and transformation -----

library(ggplot2)
library(tidyr)
library(dplyr)

# Select original skewed spending variables
spending_vars <- c("MntWines", "MntFruits", "MntMeatProducts", 
                   "MntFishProducts", "MntSweetProducts", "MntGoldProds", "Total_Spending")

# Apply log1p transformation to all skewed variables
df <- df %>%
  mutate(across(all_of(spending_vars), ~ log1p(.), .names = "log_{.col}"))

# Combine original and log-transformed data in long format
df_long_combined <- df %>%
  dplyr::select(all_of(spending_vars), starts_with("log_")) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  mutate(Scale = ifelse(grepl("^log_", Variable), "Log-Transformed", "Original"),
         Variable = gsub("^log_", "", Variable))  # Clean variable names

# Plot using facet_wrap
ggplot(df_long_combined, aes(x = Value)) +
  geom_histogram(fill = "skyblue", bins = 30, color = "black") +
  facet_grid(Variable ~ Scale, scales = "free") +
  labs(title = "Distribution of Spending Variables (Original vs Log-Transformed)",
       x = "Value", y = "Frequency") +
  theme_minimal()

# Create Age variable from Year_Birth (reference year is 2015 based on up-to-date enrollment dates in Dt_Consumer)
df$Age <- 2015 - df$Year_Birth

# Ordinal Encoding for Education
edu_levels <- c("Basic", "2n Cycle", "Graduation", "Master", "PhD")
df$Education_Ord <- factor(df$Education, levels = edu_levels, ordered = TRUE)
df$Education_Ord <- as.numeric(df$Education_Ord)

# Dummy Encoding for Marital_Status
marital_dummies <- model.matrix(~ Marital_Status - 1, data = df)
df <- cbind(df, marital_dummies)

# Winsorize Income at the 95th percentile
winsor_limit <- quantile(df$Income, 0.95, na.rm = TRUE)

# Replace values above the 95th percentile with the 95th percentile value
df$Income <- ifelse(df$Income > winsor_limit, 
                    winsor_limit, 
                    df$Income)

# Check max value after winsorization
max(df$Income)

# Visualization of Winsorized Income VS. Total Spending
ggplot(df, aes(x = Income, y = Total_Spending)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = FALSE) +
  scale_x_continuous(limits = c(min(df$Income), max(df$Income))) +
  labs(title = "Scatter Plot of Winsorized Income vs. Total Spending",
       x = "Winsorized Income", y = "Total Spending") +
  theme_minimal()

summary(df)

# 6. Dimension Reduction ------
# Remove original skewed spending variables (replaced by log-transformed versions)
spending_vars_original <- c("MntWines", "MntFruits", "MntMeatProducts",
                            "MntFishProducts", "MntSweetProducts", "MntGoldProds", "Total_Spending")

# Remove irrelevant or non-significant variables
irrelevant_vars <- c("Year_Birth", "NumDealsPurchases", "NumWebVisitsMonth", "Complain")

# Remove constants or unused internal fields (if not removed yet)
constant_vars <- c("Z_CostContact", "Z_Revenue", "ID", "Dt_Customer")

# Combine all variables to drop
vars_to_drop <- c(spending_vars_original, irrelevant_vars, constant_vars)

# Create reduced dataset
df_reduced <- df[, !(names(df) %in% vars_to_drop)]

# Drop original versions of Education and Marital_Status
df_reduced <- df_reduced %>% 
  dplyr::select(-Education, -Marital_Status)

# Check structure of reduced dataset
str(df_reduced)
summary(df_reduced)

# 7.	Data partitioning ----

# Check class distribution of the Response variable
table(df_reduced$Response)
# Check proportions
prop.table(table(df_reduced$Response))

library(caret)
library(dplyr)
set.seed(123)
# Classification Task: Predicting Response
# Create stratified split for classification
class_index <- createDataPartition(df_reduced$Response, p = 0.8, list = FALSE)

# Split the data
train_class <- df_reduced[class_index, ]
test_class <- df_reduced[-class_index, ]

# Regression Task: Predicting log_Total_Spending
# Create random split for regression (no stratification)
reg_index <- createDataPartition(df_reduced$log_Total_Spending, p = 0.8, list = FALSE)

# Split the data
train_reg <- df_reduced[reg_index, ]
test_reg <- df_reduced[-reg_index, ]

# 8. Model Selection -----
# Segmentation - K-means clustering
# Classification - logistic regression, Random forest classification
# Regression - linear regression, Random forest regression

# 9. Model Fitting -----
# 1. Create Family_Size only for clustering
df_reduced$Family_Size <- with(df_reduced,
                               1 * (Marital_StatusSingle + Marital_StatusDivorced + Marital_StatusWidow) +
                                 2 * (Marital_StatusMarried + Marital_StatusTogether) +
                                 Kidhome + Teenhome
)
str(df_reduced)

# Select clustering variables (excluding Response)
clustering_vars <- c(
  "Income", "Recency", "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases",
  "log_MntWines", "log_MntFruits", "log_MntMeatProducts", "log_MntFishProducts",
  "log_MntSweetProducts", "log_MntGoldProds", "log_Total_Spending",
  "Education_Ord", "Age", "Family_Size",
  "Marital_StatusDivorced", "Marital_StatusMarried", "Marital_StatusSingle",
  "Marital_StatusTogether", "Marital_StatusWidow"
)

df_cluster <- df_reduced[, clustering_vars]

# Normalize the variables (important for k-means)
df_cluster_scaled <- scale(df_cluster)

# Run k-means for k = 2 and k = 3
set.seed(123)
kmeans_2 <- kmeans(df_cluster_scaled, centers = 2, nstart = 25)
kmeans_3 <- kmeans(df_cluster_scaled, centers = 3, nstart = 25)

# Add cluster labels back to the dataset
df_reduced$Cluster_K2 <- kmeans_2$cluster
df_reduced$Cluster_K3 <- kmeans_3$cluster

# Check cluster sizes
table(df_reduced$Cluster_K2)
table(df_reduced$Cluster_K3)

# Supervised models-----
# Classification Task -----
# Load required libraries
library(caret)
library(randomForest)
library(ROSE)

# Resample training data to balance classes using oversampling
set.seed(123)
train_class_bal <- ovun.sample(Response ~ ., data = train_class, method = "over")$data

# Logistic Regression
log_model <- glm(Response ~ ., data = train_class_bal, family = binomial)
log_preds_prob <- predict(log_model, newdata = test_class, type = "response")
log_preds_class <- ifelse(log_preds_prob > 0.5, 1, 0)

# Accuracy for Logistic Regression
log_cm <- confusionMatrix(as.factor(log_preds_class), as.factor(test_class$Response))
log_accuracy <- log_cm$overall["Accuracy"]
print(paste("Logistic Regression Accuracy:", round(log_accuracy, 4)))

# Random Forest
set.seed(123)
rf_model <- randomForest(as.factor(Response) ~ ., data = train_class_bal, ntree = 100)
rf_preds <- predict(rf_model, newdata = test_class)

# Accuracy for Random Forest
rf_cm <- confusionMatrix(rf_preds, as.factor(test_class$Response))
rf_accuracy <- rf_cm$overall["Accuracy"]
print(paste("Random Forest Accuracy:", round(rf_accuracy, 4)))

# Regression Task -----
# Linear Regression
lm_model <- lm(log_Total_Spending ~ ., data = train_reg)
lm_preds <- predict(lm_model, newdata = test_reg)

# Random Forest Regression
set.seed(123)
rf_reg_model <- randomForest(log_Total_Spending ~ ., data = train_reg, ntree = 100)
rf_reg_preds <- predict(rf_reg_model, newdata = test_reg)

# Inverse transformation: expm1(predicted log values)
lm_preds_original <- expm1(lm_preds)
rf_reg_preds_original <- expm1(rf_reg_preds)
actual_original <- expm1(test_reg$log_Total_Spending)

# Recalculate RMSE in original scale
lm_rmse_original <- sqrt(mean((lm_preds_original - actual_original)^2))
rf_reg_rmse_original <- sqrt(mean((rf_reg_preds_original - actual_original)^2))

print(paste("Linear Regression RMSE (original scale):", round(lm_rmse_original, 2)))
print(paste("Random Forest Regression RMSE (original scale):", round(rf_reg_rmse_original, 2)))

# 10. Report Model Performance -----
# For Segmentation ----
# Load required package
library(cluster)
# Reset layout to avoid overlap with previous plots
par(mfrow = c(1, 1))
# K = 2
silhouette_k2 <- silhouette(kmeans_2$cluster, dist(df_cluster_scaled))
plot(silhouette_k2,
     main = "Silhouette Plot (K = 2)",
     col = 2:3, border = NA)
cat("Average Silhouette Score for K = 2:", round(mean(silhouette_k2[, 3]), 3), "\n")
# K = 3
silhouette_k3 <- silhouette(kmeans_3$cluster, dist(df_cluster_scaled))
plot(silhouette_k3,
     main = "Silhouette Plot (K = 3)",
     col = 2:4, border = NA)
cat("Average Silhouette Score for K = 3:", round(mean(silhouette_k3[, 3]), 3), "\n")

# For Classification ----
# Logistic Regression
log_cm <- confusionMatrix(as.factor(log_preds_class), as.factor(test_class$Response), positive = "1")
print(log_cm)

# Load required libraries
library(pROC)
# ROC Curve for Logistic Regression
log_roc <- roc(test_class$Response, log_preds_prob)
plot(log_roc, main = "ROC Curve - Logistic Regression", col = "blue")
auc_log <- log_roc$auc
cat("Logistic Regression AUC:", round(auc_log, 4), "\n")

# Random Forest Classification
rf_cm <- confusionMatrix(as.factor(rf_preds), as.factor(test_class$Response), positive = "1")
print(rf_cm)
# ROC Curve for Random Forest
# Convert predicted class probabilities (not just 0/1) from RF
rf_preds_prob <- predict(rf_model, newdata = test_class, type = "prob")[, 2]  # Prob for class '1'
rf_roc <- roc(test_class$Response, rf_preds_prob)
plot.roc(rf_roc, col = "darkgreen", main = "ROC Curve: Random Forest")
auc_rf <- rf_roc$auc
cat("Random Forest AUC:", round(auc_rf, 4), "\n")

# Variable Importance Plot for Random Forest Classification
varImpPlot(rf_model, main = "Variable Importance - Random Forest Classification")

# Load necessary libraries
library(Metrics)

# Linear Regression Evaluation
lm_mae <- mae(actual_original, lm_preds_original)
lm_mse <- mse(actual_original, lm_preds_original)
lm_r2 <- cor(actual_original, lm_preds_original)^2
lm_mape <- mape(actual_original, lm_preds_original)

n <- length(actual_original)
p <- length(lm_model$coefficients) - 1
lm_adj_r2 <- 1 - ((1 - lm_r2) * (n - 1) / (n - p - 1))

cat("Linear Regression (Original Scale):\n")
cat("  MAE  :", round(lm_mae, 2), "\n")
cat("  MSE  :", round(lm_mse, 2), "\n")
cat("  RMSE :", round(lm_rmse_original, 2), "\n")
cat("  R²   :", round(lm_r2, 4), "\n")
cat("  Adj R²:", round(lm_adj_r2, 4), "\n")
cat("  MAPE :", round(lm_mape * 100, 2), "%\n\n")

# Random Forest Regression Evaluation
rf_mae <- mae(actual_original, rf_reg_preds_original)
rf_mse <- mse(actual_original, rf_reg_preds_original)
rf_r2 <- cor(actual_original, rf_reg_preds_original)^2
rf_mape <- mape(actual_original, rf_reg_preds_original)

rf_adj_r2 <- 1 - ((1 - rf_r2) * (n - 1) / (n - p - 1))

cat("Random Forest Regression (Original Scale):\n")
cat("  MAE  :", round(rf_mae, 2), "\n")
cat("  MSE  :", round(rf_mse, 2), "\n")
cat("  RMSE :", round(rf_reg_rmse_original, 2), "\n")
cat("  R²   :", round(rf_r2, 4), "\n")
cat("  Adj R²:", round(rf_adj_r2, 4), "\n")
cat("  MAPE :", round(rf_mape * 100, 2), "%\n")

# Variable Importance Plot for Random Forest Regression
varImpPlot(rf_reg_model,
           main = "Variable Importance - Random Forest Regression",
           type = 2)  # type = 2 uses Mean Decrease in Gini


# 11. Model Evaluation ----
# Segmentation ----
# Required libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# Numeric Variable Profiling
num_vars <- c("Income", "log_Total_Spending", "Age", "Recency",
              "NumWebPurchases", "NumCatalogPurchases", "NumStorePurchases",
              "log_MntWines", "log_MntFruits", "log_MntMeatProducts",
              "log_MntFishProducts", "log_MntSweetProducts", "log_MntGoldProds",
              "Education_Ord", "Family_Size")

# Summary stats by Cluster_K2
df_reduced %>%
  group_by(Cluster_K2) %>%
  summarise(across(all_of(num_vars), list(mean = mean, median = median), .names = "{.col}_{.fn}"))

# Categorical Variable Profiling
cat_vars <- c("Response", "AcceptedCmp1", "AcceptedCmp2", "AcceptedCmp3",
              "AcceptedCmp4", "AcceptedCmp5")

# Function to create bar plots
plot_cat_cluster <- function(var) {
  ggplot(df_reduced, aes(x = .data[[var]], fill = factor(Cluster_K2))) +
    geom_bar(position = "fill") +
    scale_y_continuous(labels = scales::percent) +
    labs(
      title = paste("Proportion of", var, "by Cluster"),
      x = var,
      y = "Proportion",
      fill = "Cluster"
    ) +
    theme_minimal()
}

# Example categorical bar plots
p1 <- plot_cat_cluster("Response")
p2 <- plot_cat_cluster("AcceptedCmp1")
p3 <- plot_cat_cluster("AcceptedCmp2")
p4 <- plot_cat_cluster("AcceptedCmp3")
p5 <- plot_cat_cluster("AcceptedCmp4")
p6 <- plot_cat_cluster("AcceptedCmp5")

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 2)

# Define marital status dummies (already dummy-encoded earlier)
marital_vars <- c("Marital_StatusMarried", "Marital_StatusSingle",
                  "Marital_StatusTogether", "Marital_StatusDivorced",
                  "Marital_StatusWidow")

# Generate plots
marital_plots <- lapply(marital_vars, plot_cat_cluster)

# Display plots in grid
do.call(grid.arrange, c(marital_plots, ncol = 2))


# Boxplots of Numeric Variables by Cluster
plot_box_by_cluster <- function(var) {
  ggplot(df_reduced, aes(x = factor(Cluster_K2), y = .data[[var]], fill = factor(Cluster_K2))) +
    geom_boxplot() +
    labs(title = paste(var, "by Cluster"),
         x = "Cluster", y = var) +
    theme_minimal()
}

# Example boxplots
plot_box_by_cluster("Income")
plot_box_by_cluster("Age")

# Boxplot of total spending by cluster
df_reduced$Total_Spending_Exp <- expm1(df_reduced$log_Total_Spending)

ggplot(df_reduced, aes(x = factor(Cluster_K2), y = Total_Spending_Exp, fill = factor(Cluster_K2))) +
  geom_boxplot() +
  labs(title = "Total Spending by Cluster (Original Scale)",
       x = "Cluster", y = "Total Spending ($)") +
  theme_minimal()

