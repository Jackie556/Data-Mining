#Start_here

#LIbrary
library(readr)
library(caret)
library(ggplot2)
library(dplyr)
library(stringr)
library(corrplot)
library(tidyr)
library(GGally)
library(randomForest)
library(xgboost)
library(glmnet)
library(e1071)
library(Metrics)  
library(rpart)
library(rpart.plot)


#main
data<-read.csv("C:/Users/jacki/OneDrive/Desktop/Data Mining thesis/Data set/Used_Car_Price.csv")
data
is.na(data)
sum(is.na(data))
str(data)
summary(data)
dim(data)




#Data_preprocessing

# Creating a column for the first three letters of the car name
data <- data %>% 
  mutate(car_company = str_sub(data$CarName, 1, 3))

>>>>>>>>write.csv(data, "C:/Users/jacki/OneDrive/Desktop/Data Mining thesis/R-Codes/Data-Mining/preprocessed_dataset.csv", row.names = FALSE)

df<-read.csv("C:/Users/jacki/OneDrive/Desktop/Data Mining thesis/R-Codes/Data-Mining/preprocessed_dataset.csv")
df

df <- df[, !(names(df) %in% c("car_ID", "symboling","CarName"))]

#data_visualizations



# For numerical variables


# Histogram for continuous variables


#normality_test(histogram with normal curve)


num_df <- df %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")


p_hist<-ggplot(num_df, aes(x = value)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 0.3) +
  stat_function(
    fun = function(x, mean, sd) dnorm(x, mean=mean, sd=sd),
    args = list(mean = mean(num_df$value, na.rm=TRUE), sd = sd(num_df$value, na.rm=TRUE)),
    color = "black", size = 0.5
  ) +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  theme_minimal(base_size = 8) +
  theme(strip.text = element_text(size = 9),
        plot.margin = margin(2,2,2,2,"mm")) +
  labs(title = "Small Histograms with Normal Curves for Each Numeric Variable")


ggsave(
  filename = "numeric_histograms_big.png",
  plot     = p_hist,
  width    = 16,   # inches
  height   = 12,   # inches
  dpi      = 300
)


#Outlier_detection

# Converting the data into a long format for easy plotting

p_box<-ggplot(num_df, aes(x = variable, y = value)) +
  geom_boxplot(fill = "steelblue", outlier.color = "red", width = 0.4) +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(size = 9)
  ) +
  labs(
    title = "Compact Boxplots for Each Continuous Variable",
    x = NULL,
    y = NULL
  )
p_box
ggsave(
  filename = "Boxplots for Continuous Variable.png",
  plot     = p_box,
  width    = 16,   # inches
  height   = 12,   # inches
  dpi      = 300
)


#Quantifying Outliers

num_cols <- names(df)[sapply(df, is.numeric)]

outlier_summary <- sapply(num_cols, function(col) {
  x <- df[[col]]
  Q1 <- quantile(x, 0.25, na.rm=TRUE)
  Q3 <- quantile(x, 0.75, na.rm=TRUE)
  IQR <- Q3 - Q1
  sum(x < (Q1 - 1.5*IQR) | x > (Q3 + 1.5*IQR), na.rm=TRUE)
})

outlier_summary


# For categorical variables

#counts and percentages per company

df_counts <- df %>%
  count(car_company, name = "n") %>%
  mutate(pct = 100 * n / sum(n))

# 2) Plot with percentages as labels
p_count <- ggplot(df_counts, aes(x = car_company, y = n, fill = car_company)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(pct, 1), "%")),
    vjust = -0.3, size = 3
  ) +
  scale_fill_manual(values = rainbow(length(unique(df$car_company)))) +
  labs(
    title = "Count of Cars by Company",
    x = "Car Company",
    y = "Number of Cars"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3) Save
ggsave(
  filename = "Count plot of car company_pct.png",
  plot     = p_count,
  width    = 16,
  height   = 12,
  dpi      = 300
)

#Bar_plots



# Identify categorical columns (factor/character)

cat_cols <- setdiff(
  names(df)[sapply(df, function(x) is.character(x) || is.factor(x))],
  "CarName"
)

# Gather to long format
cat_df <- df %>%
  select(all_of(cat_cols)) %>%
  pivot_longer(cols = everything(),
               names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()


#Faceted bar plots with percentage labels

p_bar <- ggplot(cat_df, aes(x = value, y = n, fill = value)) +
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(
    aes(label = paste0(round(pct, 1), "%")),
    vjust = -0.3, size = 2.5
  ) +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  theme_minimal(base_size = 10) +
  labs(
    title = "Bar Plots for All Categorical Variables",
    x = NULL,
    y = "Count"
  ) +
  theme(
    axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1),
    strip.text  = element_text(size = 9)
  )


# Save as a big, high‑resolution image

ggsave(
  filename = "categorical_barplots.png",
  plot     = p_bar,
  width    = 18,   # inches
  height   = 12,   # inches
  dpi      = 300
)

#Multicolinearity

#Heatmap
names(df)

# Remove 'car_id' and 'symboling' columns
df_corr <- df[, !(names(df) %in% c("car_ID", "symboling"))]

# Select only numeric columns
numeric_df <- df_corr[sapply(df_corr, is.numeric)]

# Calculate correlation matrix
cor_matrix <- cor(numeric_df, use = "complete.obs")
print(cor_matrix)

>>>>>write.csv(cor_matrix, "C:/Users/jacki/OneDrive/Desktop/Data Mining thesis/R-Codes/Data-Mining/cor_matrix.csv", row.names = FALSE)



#Correlation_matrix

num_wide <- df[sapply(df, is.numeric)]


p<-ggpairs(num_wide, cardinality_threshold = 20)

# save as large PNG (change path as needed)
ggsave(
  filename = "correlation_matrix_ggpairs.png",
  plot = p,
  width = 20,    # in inches
  height = 20,   # in inches
  dpi = 300
)

# Plot heatmap

library(reshape2) 



#  Convert correlation matrix to long format
cor_long <- melt(cor_matrix, varnames = c("Var1", "Var2"), value.name = "cor")

p_heat <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = cor)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(cor, 2)), size = 2.5) +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limits = c(-1, 1)
  ) +
  coord_equal() +
  theme_minimal(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title  = element_blank()
  ) +
  labs(fill = "Corr")

# 4) Save big, high‑resolution image
ggsave(
  filename = "Heat_map.png",
  plot     = p_heat,
  width    = 16,   # inches
  height   = 16,   # inches
  dpi      = 300
)

#Outlier_eradicating

# Remove 'car_ID', 'symboling', and 'carName' (adjust variable names as in your data)
remove_vars <- c("car_ID", "symboling", "carName")
cols_to_remove <- names(df)[tolower(names(df)) %in% tolower(remove_vars)]
df_removed <- df[ , !names(df) %in% cols_to_remove]

# Assume df_removed is your dataframe with car_ID, symboling, carName already removed

df_no_outliers <- df_removed

# Remove rows with outliers for each numeric variable (IQR method)
for(col in names(df_no_outliers)[sapply(df_no_outliers, is.numeric)]) {
  Q1 <- quantile(df_no_outliers[[col]], 0.25, na.rm=TRUE)
  Q3 <- quantile(df_no_outliers[[col]], 0.75, na.rm=TRUE)
  IQR_value <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR_value
  upper <- Q3 + 1.5 * IQR_value
  df_no_outliers <- df_no_outliers[df_no_outliers[[col]] >= lower & df_no_outliers[[col]] <= upper, ]
}

# Save cleaned dataset to CSV (update path as needed)
>>>>>>write.csv(df_no_outliers, "dataset_no_outliers.csv", row.names = FALSE)

dataset_no_outliers<-read.csv("dataset_no_outliers.csv")
summary(dataset_no_outliers)





#With_Outliers



# For df dataset (WITH outliers)
# Use the SAME variables and structure, only dataset changes
cat_var <- c("aspiration","doornumber","carbody",
              "drivewheel","enginetype",
              "cylindernumber","fuelsystem")

# For df (with outliers)
data[cat_var] <- lapply(df[cat_var], factor)


# Now check levels again
sapply(data[cat_var], nlevels)

# 1. Check factor structure if needed
sapply(data[cat_var], nlevels)
lapply(data[cat_var], levels)

set.seed(123)  # same seed pattern

# 2. Get total number of rows
n_df <- nrow(data)

# 3. 80% of rows for training
train_size_df <- floor(0.8 * n_df)

# 4. Randomly sample indices
train_indices_df <- sample(seq_len(n_df), size = train_size_df)

# 5. Create subsets
train_data_df <- data[train_indices_df, ]
test_data_df  <- data[-train_indices_df, ]

cat("Training Set (80%):", nrow(train_data_df), "rows\n")
cat("Testing Set  (20%):", nrow(test_data_df), "rows\n")

# 6. Fit linear model on df
lm_with <- lm(price ~ aspiration + doornumber + carbody +
                drivewheel + enginetype + enginesize +
                curbweight + horsepower +
                carlength + carwidth + wheelbase + citympg,
              data = train_data_df)

summary(lm_with)

# 7. Predictions and metrics for df
pred_df <- predict(lm_with, newdata = test_data_df)
actual_df <- test_data_df$price

ss_total_df <- sum((actual_df - mean(actual_df))^2)
ss_res_df   <- sum((actual_df - pred_df)^2)
r_squared_df <- 1 - ss_res_df / ss_total_df

rmse_df <- sqrt(mean((actual_df - pred_df)^2))
mae_df  <- mean(abs(actual_df - pred_df))

cat("DF (with outliers) Test R²  =", r_squared_df, "\n")
cat("DF (with outliers) Test RMSE=", rmse_df, "\n")
cat("DF (with outliers) Test MAE =", mae_df,  "\n")


# and we already predicted on test_data_df earlier:
pred_with   <- predict(lm_with, newdata = test_data_df)
actual_with <- test_data_df$price

plot_lm_with <- data.frame(
  Actual    = actual_with,
  Predicted = pred_with
)

p_lm_with <- ggplot(plot_lm_with, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Linear Regression: Actual vs Predicted Price (With Outliers)",
    x = "Actual Price",
    y = "Predicted Price"
  )

print(p_lm_with)

ggsave(
  filename = "LM_actual_vs_predicted_with_outliers.png",
  plot     = p_lm_with,
  width    = 7,
  height   = 5,
  dpi      = 300
)


#Without_outliers

sapply(dataset_no_outliers[cat_vars], nlevels)
lapply(dataset_no_outliers[cat_vars], levels)

#spliting_dataset
cat_vars <- c("aspiration","doornumber","carbody",
              "drivewheel","enginetype",
              "cylindernumber","fuelsystem")

set.seed(123) # Keeps your random split consistent

# 1. Get total number of rows
n <- nrow(dataset_no_outliers)

# 2. Calculate 80% of the rows for training
# We use floor() to get a whole number
train_size <- floor(0.8 * n)

# 3. Randomly sample indices
train_indices <- sample(seq_len(n), size = train_size)

# 4. Create the subsets
train_data <- dataset_no_outliers[train_indices, ]
test_data  <- dataset_no_outliers[-train_indices, ]

# 5. Verify the split
cat("Training Set (80%):", nrow(train_data), "rows\n")
cat("Testing Set  (20%):", nrow(test_data), "rows\n")


lm_without<- lm(price ~ aspiration + doornumber + carbody +
                drivewheel + enginetype +
                cylindernumber + fuelsystem + enginesize +
                  curbweight + horsepower +
                  carlength + carwidth + wheelbase + citympg,
                data = train_data)


summary(lm_without)

# Predictions on test set
pred <- predict(lm_without, newdata = test_data)

actual <- test_data$price

# R-squared (on test data)
ss_total <- sum((actual - mean(actual))^2)
ss_res   <- sum((actual - pred)^2)
r_squared <- 1 - ss_res / ss_total

# RMSE
rmse <- sqrt(mean((actual - pred)^2))

# MAE (often called MASE informally in simple work)
mae  <- mean(abs(actual - pred))

cat("Test R²  =", r_squared, "\n")
cat("Test RMSE=", rmse, "\n")
cat("Test MAE =", mae,  "\n")



# lm_without fitted on train_data, predictions on test_data:

plot_lm <- data.frame(
  Actual    = actual,
  Predicted = pred
)

p_lm <- ggplot(plot_lm, aes(x = Actual, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Linear Regression: Actual vs Predicted Price (Without Outliers)",
    x = "Actual Price",
    y = "Predicted Price"
  )

print(p_lm)

ggsave(
  filename = "LM_actual_vs_predicted(Without Outliers).png",
  plot     = p_lm,
  width    = 7,
  height   = 5,
  dpi      = 300
)




# =========================
# WITH OUTLIERS  (data)
# =========================

cat_var <- c("aspiration","doornumber","carbody",
             "drivewheel","enginetype",
             "cylindernumber","fuelsystem")

# ensure factors
data[cat_var] <- lapply(data[cat_var], factor)

set.seed(123)

n_df  <- nrow(data)
train_size_df <- floor(0.8 * n_df)
train_indices_df <- sample(seq_len(n_df), size = train_size_df)

train_data_df <- data[train_indices_df, ]
test_data_df  <- data[-train_indices_df, ]

cat("Training Set (80%):", nrow(train_data_df), "rows\n")
cat("Testing Set  (20%):", nrow(test_data_df), "rows\n")

# same predictors as lm_with
rf_with <- randomForest(
  price ~ aspiration + doornumber + carbody +
    drivewheel + enginetype + enginesize +
    curbweight + horsepower +
    carlength + carwidth + wheelbase + citympg,
  data = train_data_df,
  ntree = 500,
  mtry  = floor(sqrt(ncol(train_data_df) - 1)),
  importance = TRUE
)

# predictions and metrics
rf_pred_df <- predict(rf_with, newdata = test_data_df)
actual_df  <- test_data_df$price

ss_total_df <- sum((actual_df - mean(actual_df))^2)
ss_res_df   <- sum((actual_df - rf_pred_df)^2)
r_squared_df <- 1 - ss_res_df / ss_total_df
rmse_df <- sqrt(mean((actual_df - rf_pred_df)^2))
mae_df  <- mean(abs(actual_df - rf_pred_df))

cat("RF (with outliers) Test R²  =", r_squared_df, "\n")
cat("RF (with outliers) Test RMSE=", rmse_df, "\n")
cat("RF (with outliers) Test MAE =", mae_df,  "\n")

# variable importance

# 1) Extract importance (for regression: IncNodePurity or %IncMSE)
imp <- importance(rf_with)          # matrix
imp_df <- as.data.frame(imp)
imp_df$Variable <- rownames(imp_df)
rownames(imp_df) <- NULL

# Use %IncMSE if present; otherwise IncNodePurity
measure <- if("IncNodePurity" %in% names(imp_df)) "IncNodePurity" else "%IncMSE"

# 2) ggplot bar chart, sorted
p_imp <- imp_df %>%
  arrange(.data[[measure]]) %>%
  mutate(Variable = factor(Variable, levels = Variable)) %>%
  ggplot(aes(x = Variable, y = .data[[measure]])) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance (with Outliers)",
    x = "Variable",
    y = measure
  ) +
  theme_minimal(base_size = 11)

print(p_imp)

# Optionally save big
ggsave("rf_variable_importance.png", p_imp,
       width = 8, height = 6, dpi = 300)


# =========================
# WITHOUT OUTLIERS (dataset_no_outliers)
# =========================

cat_vars <- c("aspiration","doornumber","carbody",
              "drivewheel","enginetype",
              "cylindernumber","fuelsystem")

dataset_no_outliers[cat_vars] <- lapply(dataset_no_outliers[cat_vars], factor)

set.seed(123)

n_no  <- nrow(dataset_no_outliers)
train_size_no <- floor(0.8 * n_no)
train_indices_no <- sample(seq_len(n_no), size = train_size_no)

train_data <- dataset_no_outliers[train_indices_no, ]
test_data  <- dataset_no_outliers[-train_indices_no, ]

cat("Training Set (80%):", nrow(train_data), "rows\n")
cat("Testing Set  (20%):", nrow(test_data), "rows\n")

rf_without <- randomForest(
  price ~ aspiration + doornumber + carbody +
    drivewheel + enginetype +
    cylindernumber + fuelsystem + enginesize +
    curbweight + horsepower +
    carlength + carwidth + wheelbase + citympg,
  data = train_data,
  ntree = 500,
  mtry  = floor(sqrt(ncol(train_data) - 1)),
  importance = TRUE
)

rf_pred <- predict(rf_without, newdata = test_data)
actual  <- test_data$price

ss_total <- sum((actual - mean(actual))^2)
ss_res   <- sum((actual - rf_pred)^2)
r_squared <- 1 - ss_res / ss_total
rmse <- sqrt(mean((actual - rf_pred)^2))
mae  <- mean(abs(actual - rf_pred))

cat("RF (without outliers) Test R²  =", r_squared, "\n")
cat("RF (without outliers) Test RMSE=", rmse, "\n")
cat("RF (without outliers) Test MAE =", mae,  "\n")


# Extract importance from rf_without
imp_no <- importance(rf_without)      # matrix
imp_no_df <- as.data.frame(imp_no)
imp_no_df$Variable <- rownames(imp_no)
rownames(imp_no_df) <- NULL

# Choose measure: IncNodePurity (regression default) or %IncMSE if available
measure_no <- if ("IncNodePurity" %in% names(imp_no_df)) "IncNodePurity" else "%IncMSE"

# Build ggplot variable-importance graph
p_imp_no <- imp_no_df %>%
  arrange(.data[[measure_no]]) %>%
  mutate(Variable = factor(Variable, levels = Variable)) %>%
  ggplot(aes(x = Variable, y = .data[[measure_no]])) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Random Forest Variable Importance (No Outliers)",
    x = "Variable",
    y = measure_no
  ) +
  theme_minimal(base_size = 11)

print(p_imp_no)

# Save as big high‑resolution image (optional)
ggsave(
  filename = "rf_without_var_importance.png",
  plot     = p_imp_no,
  width    = 8,
  height   = 6,
  dpi      = 300
)





# =========================
# SVR Regression (SVR) WITHOUT outliers
# =========================

set.seed(123)

# reuse your split: dataset_no_outliers -> train_data, test_data
n_no  <- nrow(dataset_no_outliers)
train_size_no <- floor(0.8 * n_no)
train_indices_no <- sample(seq_len(n_no), size = train_size_no)

train_data <- dataset_no_outliers[train_indices_no, ]
test_data  <- dataset_no_outliers[-train_indices_no, ]

# make sure categorical vars are factors
cat_vars <- c("aspiration","doornumber","carbody",
              "drivewheel","enginetype",
              "cylindernumber","fuelsystem")
train_data[cat_vars] <- lapply(train_data[cat_vars], factor)
test_data[cat_vars]  <- lapply(test_data[cat_vars],  factor)

# SVR regression model (radial kernel)
svr_model <- svm(
  price ~ aspiration + doornumber + carbody +
    drivewheel + enginetype +
    cylindernumber + fuelsystem + enginesize +
    curbweight + horsepower +
    carlength + carwidth + wheelbase + citympg,
  data   = train_data,
  type   = "eps-regression",
  kernel = "radial",
  cost   = 10,
  gamma  = 0.01
)

print(svr_model)

# Predictions
svr_pred <- predict(svr_model, newdata = test_data)
actual   <- test_data$price

# Metrics
ss_total <- sum((actual - mean(actual))^2)
ss_res   <- sum((actual - svr_pred)^2)
r_squared <- 1 - ss_res / ss_total
rmse <- sqrt(mean((actual - svr_pred)^2))
mae  <- mean(abs(actual - svr_pred))

cat("SVR (no outliers) Test R²  =", r_squared, "\n")
cat("SVR (no outliers) Test RMSE=", rmse, "\n")
cat("SVR (no outliers) Test MAE =", mae,  "\n")

# =========================
# Nice graph: Actual vs Predicted
# =========================

plot_df <- data.frame(
  Actual    = actual,
  Predicted = svm_pred
)

p_svr <- ggplot(plot_df, aes(x = Actual, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  theme_minimal(base_size = 11) +
  labs(
    title = "SVR: Actual vs Predicted Price (No Outliers)",
    x = "Actual Price",
    y = "Predicted Price"
  )

print(p_svr)

ggsave(
  filename = "SVR_actual_vs_predicted_no_outliers.png",
  plot     = p_svr,
  width    = 7,
  height   = 5,
  dpi      = 300
)



set.seed(123)

# ================
# WITH outliers
# ================

# use your full dataset with outliers; adjust name if needed
df_with <- data    # or df, whichever you used above

cat_vars <- c("aspiration","doornumber","carbody",
              "drivewheel","enginetype",
              "cylindernumber","fuelsystem")

df_with[cat_vars] <- lapply(df_with[cat_vars], factor)

# 80/20 split
n_w  <- nrow(df_with)
train_size_w <- floor(0.8 * n_w)
train_idx_w  <- sample(seq_len(n_w), size = train_size_w)

train_data_w <- df_with[train_idx_w, ]
test_data_w  <- df_with[-train_idx_w, ]

cat("Training Set (80%):", nrow(train_data_w), "rows\n")
cat("Testing Set  (20%):", nrow(test_data_w), "rows\n")

# ensure factors in both sets
train_data_w[cat_vars] <- lapply(train_data_w[cat_vars], factor)
test_data_w[cat_vars]  <- lapply(test_data_w[cat_vars],  factor)

# SVM regression model (radial kernel)
svr_with <- svm(
  price ~ aspiration + doornumber + carbody +
    drivewheel + enginetype +
    cylindernumber + fuelsystem + enginesize +
    curbweight + horsepower +
    carlength + carwidth + wheelbase + citympg,
  data   = train_data_w,
  type   = "eps-regression",
  kernel = "radial",
  cost   = 10,
  gamma  = 0.01
)

print(svr_with)

# Predictions
svr_pred_w <- predict(svr_with, newdata = test_data_w)
actual_w   <- test_data_w$price

# Metrics
ss_total_w <- sum((actual_w - mean(actual_w))^2)
ss_res_w   <- sum((actual_w - svr_pred_w)^2)
r_squared_w <- 1 - ss_res_w / ss_total_w
rmse_w <- sqrt(mean((actual_w - svr_pred_w)^2))
mae_w  <- mean(abs(actual_w - svr_pred_w))

cat("SVR (with outliers) Test R²  =", r_squared_w, "\n")
cat("SVR (with outliers) Test RMSE=", rmse_w, "\n")
cat("SVR (with outliers) Test MAE =", mae_w,  "\n")

# ================
# Graph: Actual vs Predicted
# ================

plot_w <- data.frame(
  Actual    = actual_w,
  Predicted = svr_pred_w
)

p_svr_w <- ggplot(plot_w, aes(x = Actual, y = Predicted)) +
  geom_point(color = "darkgreen", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  theme_minimal(base_size = 11) +
  labs(
    title = "SVR: Actual vs Predicted Price (With Outliers)",
    x = "Actual Price",
    y = "Predicted Price"
  )

print(p_svr_w)

ggsave(
  filename = "SVR_actual_vs_predicted_with_outliers.png",
  plot     = p_svr_w,
  width    = 7,
  height   = 5,
  dpi      = 300
)


#Lasso Regression

set.seed(123)

# -----------------------
# 1. Prepare data (no outliers)
# -----------------------
dat <- dataset_no_outliers

cat_vars <- c("aspiration","doornumber","carbody",
              "drivewheel","enginetype",
              "cylindernumber","fuelsystem")

dat[cat_vars] <- lapply(dat[cat_vars], factor)

# model matrix (one‑hot encoding for factors)
X <- model.matrix(
  price ~ aspiration + doornumber + carbody +
    drivewheel + enginetype +
    cylindernumber + fuelsystem + enginesize +
    curbweight + horsepower +
    carlength + carwidth + wheelbase + citympg,
  data = dat
)[, -1]   # remove intercept column

y <- dat$price

# train/test split indices
n  <- nrow(dat)
train_size <- floor(0.8 * n)
train_idx  <- sample(seq_len(n), size = train_size)

X_train <- X[train_idx, ]
y_train <- y[train_idx]
X_test  <- X[-train_idx, ]
y_test  <- y[-train_idx]

# -----------------------
# 2. Fit LASSO (alpha = 1) with CV
# -----------------------
set.seed(123)
cv_lasso <- cv.glmnet(
  X_train, y_train,
  alpha = 1,                 # Lasso
  family = "gaussian"
)

# best lambda
lambda_best <- cv_lasso$lambda.min
lambda_best

# final Lasso model
lasso_model <- glmnet(
  X_train, y_train,
  alpha = 1,
  lambda = lambda_best,
  family = "gaussian"
)

# -----------------------
# 3. Predict & metrics
# -----------------------
lasso_pred <- predict(lasso_model, newx = X_test, s = lambda_best)[, 1]

ss_total <- sum((y_test - mean(y_test))^2)
ss_res   <- sum((y_test - lasso_pred)^2)
r_squared <- 1 - ss_res / ss_total
rmse <- sqrt(mean((y_test - lasso_pred)^2))
mae  <- mean(abs(y_test - lasso_pred))

cat("Lasso (no outliers) Test R²  =", r_squared, "\n")
cat("Lasso (no outliers) Test RMSE=", rmse, "\n")
cat("Lasso (no outliers) Test MAE =", mae,  "\n")

# -----------------------
# 4. Graph: Actual vs Predicted
# -----------------------
plot_lasso <- data.frame(
  Actual    = y_test,
  Predicted = lasso_pred
)

p_lasso <- ggplot(plot_lasso, aes(x = Actual, y = Predicted)) +
  geom_point(color = "purple", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Lasso Regression: Actual vs Predicted Price (No Outliers)",
    x = "Actual Price",
    y = "Predicted Price"
  )

print(p_lasso)

ggsave(
  filename = "Lasso_actual_vs_predicted_no_outliers.png",
  plot     = p_lasso,
  width    = 7,
  height   = 5,
  dpi      = 300
)



set.seed(123)

# -----------------------
# 1. Prepare data (WITH outliers)
# -----------------------
dat_w <- data    # or df, whichever is your full dataset

cat_vars <- c("aspiration","doornumber","carbody",
              "drivewheel","enginetype",
              "cylindernumber","fuelsystem")

dat_w[cat_vars] <- lapply(dat_w[cat_vars], factor)

# model matrix (one‑hot encoding for factors)
X_w <- model.matrix(
  price ~ aspiration + doornumber + carbody +
    drivewheel + enginetype +
    cylindernumber + fuelsystem + enginesize +
    curbweight + horsepower +
    carlength + carwidth + wheelbase + citympg,
  data = dat_w
)[, -1]   # remove intercept column

y_w <- dat_w$price

# train/test split (80/20)
n_w  <- nrow(dat_w)
train_size_w <- floor(0.8 * n_w)
train_idx_w  <- sample(seq_len(n_w), size = train_size_w)

Xw_train <- X_w[train_idx_w, ]
yw_train <- y_w[train_idx_w]
Xw_test  <- X_w[-train_idx_w, ]
yw_test  <- y_w[-train_idx_w]

# -----------------------
# 2. Fit LASSO (alpha = 1) with CV
# -----------------------
set.seed(123)
cv_lasso_w <- cv.glmnet(
  Xw_train, yw_train,
  alpha = 1,
  family = "gaussian"
)

lambda_best_w <- cv_lasso_w$lambda.min

lasso_with <- glmnet(
  Xw_train, yw_train,
  alpha = 1,
  lambda = lambda_best_w,
  family = "gaussian"
)

# -----------------------
# 3. Predict & metrics
# -----------------------
lasso_pred_w <- predict(lasso_with, newx = Xw_test, s = lambda_best_w)[, 1]

ss_total_w <- sum((yw_test - mean(yw_test))^2)
ss_res_w   <- sum((yw_test - lasso_pred_w)^2)
r_squared_w <- 1 - ss_res_w / ss_total_w
rmse_w <- sqrt(mean((yw_test - lasso_pred_w)^2))
mae_w  <- mean(abs(yw_test - lasso_pred_w))

cat("Lasso (with outliers) Test R²  =", r_squared_w, "\n")
cat("Lasso (with outliers) Test RMSE=", rmse_w, "\n")
cat("Lasso (with outliers) Test MAE =", mae_w,  "\n")

# -----------------------
# 4. Graph: Actual vs Predicted
# -----------------------
plot_lasso_w <- data.frame(
  Actual    = yw_test,
  Predicted = lasso_pred_w
)

p_lasso_w <- ggplot(plot_lasso_w, aes(x = Actual, y = Predicted)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  theme_minimal(base_size = 11) +
  labs(
    title = "Lasso Regression: Actual vs Predicted Price (With Outliers)",
    x = "Actual Price",
    y = "Predicted Price"
  )

print(p_lasso_w)

ggsave(
  filename = "Lasso_actual_vs_predicted_with_outliers.png",
  plot     = p_lasso_w,
  width    = 7,
  height   = 5,
  dpi      = 300
)

#DEcision Tree


set.seed(123)

# ----- data without outliers -----
dat_no <- dataset_no_outliers

cat_vars <- c("aspiration","doornumber","carbody",
              "drivewheel","enginetype",
              "cylindernumber","fuelsystem")
dat_no[cat_vars] <- lapply(dat_no[cat_vars], factor)

n_no  <- nrow(dat_no)
train_size_no <- floor(0.8 * n_no)
idx_no <- sample(seq_len(n_no), size = train_size_no)

train_no <- dat_no[idx_no, ]
test_no  <- dat_no[-idx_no, ]

# model
dt_no <- rpart(
  price ~ aspiration + doornumber + carbody +
    drivewheel + enginetype +
    cylindernumber + fuelsystem + enginesize +
    curbweight + horsepower +
    carlength + carwidth + wheelbase + citympg,
  data = train_no,
  method = "anova",
  control = rpart.control(cp = 0.01, minsplit = 10)
)

rpart.plot(dt_no, main = "Decision Tree (No Outliers)")

# predictions & metrics
pred_no <- predict(dt_no, newdata = test_no)
actual_no <- test_no$price

ss_total_no <- sum((actual_no - mean(actual_no))^2)
ss_res_no   <- sum((actual_no - pred_no)^2)
r2_no   <- 1 - ss_res_no / ss_total_no
rmse_no <- sqrt(mean((actual_no - pred_no)^2))
mae_no  <- mean(abs(actual_no - pred_no))

cat("Tree (no outliers) R²  =", r2_no,  "\n")
cat("Tree (no outliers) RMSE=", rmse_no,"\n")
cat("Tree (no outliers) MAE =", mae_no, "\n")

# graph: actual vs predicted
plot_no <- data.frame(Actual = actual_no, Predicted = pred_no)

p_dt_no <- ggplot(plot_no, aes(x = Actual, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  theme_minimal(base_size = 11) +
  labs(title = "Decision Tree: Actual vs Predicted (No Outliers)",
       x = "Actual Price", y = "Predicted Price")

print(p_dt_no)

ggsave("DT_actual_vs_predicted_no_outliers.png",
       p_dt_no, width = 7, height = 5, dpi = 300)




# ----- data with outliers -----
dat_w <- data   # or df, your full dataset

dat_w[cat_vars] <- lapply(dat_w[cat_vars], factor)

n_w  <- nrow(dat_w)
train_size_w <- floor(0.8 * n_w)
idx_w <- sample(seq_len(n_w), size = train_size_w)

train_w <- dat_w[idx_w, ]
test_w  <- dat_w[-idx_w, ]

dt_w <- rpart(
  price ~ aspiration + doornumber + carbody +
    drivewheel + enginetype +
    cylindernumber + fuelsystem + enginesize +
    curbweight + horsepower +
    carlength + carwidth + wheelbase + citympg,
  data = train_w,
  method = "anova",
  control = rpart.control(cp = 0.01, minsplit = 10)
)

rpart.plot(dt_w, main = "Decision Tree (With Outliers)")

pred_w <- predict(dt_w, newdata = test_w)
actual_w <- test_w$price

ss_total_w <- sum((actual_w - mean(actual_w))^2)
ss_res_w   <- sum((actual_w - pred_w)^2)
r2_w   <- 1 - ss_res_w / ss_total_w
rmse_w <- sqrt(mean((actual_w - pred_w)^2))
mae_w  <- mean(abs(actual_w - pred_w))

cat("Tree (with outliers) R²  =", r2_w,  "\n")
cat("Tree (with outliers) RMSE=", rmse_w,"\n")
cat("Tree (with outliers) MAE =", mae_w, "\n")

plot_w <- data.frame(Actual = actual_w, Predicted = pred_w)

p_dt_w <- ggplot(plot_w, aes(x = Actual, y = Predicted)) +
  geom_point(color = "darkgreen", alpha = 0.7) +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed") +
  theme_minimal(base_size = 11) +
  labs(title = "Decision Tree: Actual vs Predicted (With Outliers)",
       x = "Actual Price", y = "Predicted Price")

print(p_dt_w)

ggsave("DT_actual_vs_predicted_with_outliers.png",
       p_dt_w, width = 7, height = 5, dpi = 300)








