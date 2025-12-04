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


