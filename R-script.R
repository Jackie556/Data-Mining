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
num_df<- df[, !(names(df) %in% c("car_ID", "symboling"))]


num_df <- df %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")


ggplot(num_df, aes(x = value)) +
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


#Outlier_detection

# Converting the data into a long format for easy plotting

ggplot(num_df, aes(x = variable, y = value)) +
  geom_boxplot(fill = "cornflowerblue") +
  labs(title = "Boxplots of Continuous Variables (Outliers shown as points)",
       x = "Variable",
       y = "Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(num_df, aes(x = variable, y = value)) +
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


ggplot(df, aes(x = df$car_company, fill = car_company)) +
  geom_bar() +
  scale_fill_manual(values = rainbow(length(unique(df$car_company)))) +
  labs(title = "Count of Cars by Company",
       x = "Car Company",
       y = "Number of Cars") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Bar_plots



# Identify categorical columns (factor/character)

cat_cols <- setdiff(
  names(df)[sapply(df, function(x) is.character(x) || is.factor(x))],
  "CarName"
)

# Gather to long format
cat_df <- df %>%
  select(all_of(cat_cols)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Bar plots for each category variable (small plots with many columns)
ggplot(cat_df, aes(x = value, fill = value)) +
  geom_bar(show.legend = FALSE, width = 0.7) +
  facet_wrap(~ variable, scales = "free", ncol = 4) +
  theme_minimal(base_size = 10) +
  labs(title = "Small Bar Plots for All Categorical Variables",
       x = NULL, y = "Count") +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust=1),
        strip.text = element_text(size = 9))

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


ggpairs(num_wide, cardinality_threshold = 20)


# Plot heatmap
corrplot(cor_matrix, 
         method = "color", 
         type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(200),
         tl.col = "black", 
         tl.srt = 45,
         addCoef.col = "black", 
         number.cex = 0.7)

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

summary(data)
summary(dataset_no_outliers)

#comparisons
numeric_summary <- function(df) {
  num_cols <- names(df)[sapply(df, is.numeric)]
  data.frame(
    Variable = num_cols,
    Mean = sapply(df[num_cols], mean, na.rm=TRUE),
    Median = sapply(df[num_cols], median, na.rm=TRUE),
    SD = sapply(df[num_cols], sd, na.rm=TRUE),
    Min = sapply(df[num_cols], min, na.rm=TRUE),
    Max = sapply(df[num_cols], max, na.rm=TRUE)
  )
}

summary_with_outliers <- numeric_summary(data)
summary_no_outliers   <- numeric_summary(dataset_no_outliers)

>>>>write.csv(summary_with_outliers, "summary_with_outliers.csv", row.names=FALSE)
>>>>write.csv(summary_no_outliers, "summary_no_outliers.csv", row.names=FALSE)


#spliting_dataset

set.seed(123)  # For reproducibility

# For the dataset with outliers
n <- nrow(data)
train_indices <- sample(seq_len(n), size = floor(0.7 * n))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# For the dataset without outliers
n_no <- nrow(dataset_no_outliers)
train_indices_no <- sample(seq_len(n_no), size = floor(0.7 * n_no))
train_data_no <- dataset_no_outliers[train_indices_no, ]
test_data_no <- dataset_no_outliers[-train_indices_no, ]




#Model_fitting



#Linear Regression


# Convert predictors to factors (as needed)
train_data$fueltype       <- as.factor(train_data$fueltype)
train_data$aspiration     <- as.factor(train_data$aspiration)
train_data$doornumber     <- as.factor(train_data$doornumber)
train_data$carbody        <- as.factor(train_data$carbody)
train_data$drivewheel     <- as.factor(train_data$drivewheel)
train_data$enginelocation <- as.factor(train_data$enginelocation)
train_data$enginetype     <- as.factor(train_data$enginetype)
train_data$fuelsystem     <- as.factor(train_data$fuelsystem)
# Repeat for other needed categorical predictors

# Fit the linear regression model to the training data
lm_model <- lm(price ~ fueltype + aspiration + doornumber +
                 carbody + drivewheel + enginelocation
               + wheelbase + carlength + carwidth + carheight
               + curbweight + enginetype + enginesize  + boreratio +
                 stroke + compressionratio +
                 horsepower + peakrpm + citympg + highwaympg,
               data = train_data)

# View model summary
summary(lm_model)


# Convert predictors in test_data to factors as necessary
test_data$fueltype       <- as.factor(test_data$fueltype)
test_data$aspiration     <- as.factor(test_data$aspiration)
test_data$doornumber     <- as.factor(test_data$doornumber)
test_data$carbody        <- as.factor(test_data$carbody)
test_data$drivewheel     <- as.factor(test_data$drivewheel)
test_data$enginelocation <- as.factor(test_data$enginelocation)
test_data$enginetype     <- as.factor(test_data$enginetype)
# Add more if needed

# Fit the linear model *on test data*
lm_model_test <- lm(price ~ fueltype + aspiration + doornumber +
                      carbody + drivewheel + enginelocation +
                      wheelbase + carlength + carwidth + carheight + boreratio +
                      stroke + compressionratio + horsepower +
                      peakrpm + citympg + highwaympg,
                    data = test_data)

# View the summary of the model
summary(lm_model_test)

# Predict prices for the test set (using model from train set)
lm_pred <- predict(lm_model,data = test_data)

# Preview predictions
head(lm_pred)




actual <- test_data$price
rmse <- sqrt(mean((actual - lm_pred)^2))
mae <- mean(abs(actual - lm_pred))
ss_total <- sum((actual - mean(actual))^2)
ss_res <- sum((actual - lm_pred)^2)
r_squared <- 1 - ss_res/ss_total

cat("Test RMSE=", rmse, "\n")
cat("Test MAE=", mae, "\n")
cat("Test R²=", r_squared, "\n")
















#Without outliers
cat_vars <- c("fueltype", "aspiration", "doornumber", "carbody", "drivewheel", "enginelocation", "enginetype", "cylindernumber", "fuelsystem")
sapply(train_data_no[cat_vars], function(x) length(unique(x)))

# Convert categorical predictors in train_data_no to factors (do this for all relevant variables)

train_data_no$aspiration     <- as.factor(train_data_no$aspiration)
train_data_no$doornumber     <- as.factor(train_data_no$doornumber)
train_data_no$carbody        <- as.factor(train_data_no$carbody)
train_data_no$drivewheel     <- as.factor(train_data_no$drivewheel)
train_data_no$enginetype     <- as.factor(train_data_no$enginetype)


# Add others if any

# Fit the model using outlier-free training data
lm_model_no <-lm(price ~  aspiration + doornumber +
                    carbody + drivewheel +
                    wheelbase + carlength + carwidth + carheight +
                    curbweight + enginetype + enginesize +
                    boreratio +
                    stroke + compressionratio + horsepower +
                    peakrpm + citympg + highwaympg,
                  data = train_data_no)

# Model diagnostics
summary(lm_model_no)

#test data

test_data_no$aspiration     <- as.factor(test_data_no$aspiration)
test_data_no$doornumber     <- as.factor(test_data_no$doornumber)
test_data_no$carbody        <- as.factor(test_data_no$carbody)
test_data_no$drivewheel     <- as.factor(test_data_no$drivewheel)
test_data_no$enginetype     <- as.factor(test_data_no$enginetype)


# Remove any other categorical vars with only one present level!

# Fit linear model to the test data (drop one-level factors from formula as needed)
lm_model_test_no <- lm(price ~ aspiration + doornumber +
                         carbody + drivewheel +
                         wheelbase + carlength + carwidth + carheight +
                         curbweight + enginetype + enginesize +
                           boreratio +
                         stroke + compressionratio + horsepower +
                         peakrpm + citympg + highwaympg,
                       data = test_data_no)

# View summary
summary(lm_model_test_no)

predictions <- predict(lm_model_no, newdata = test_data_no)
head(predictions)


actual <- test_data_no$price

# RMSE (Root Mean Square Error)
rmse <- sqrt(mean((actual - predictions)^2))

# MAE (Mean Absolute Error)
mae <- mean(abs(actual - predictions))

# R² (Coefficient of Determination)
ss_total <- sum((actual - mean(actual))^2)
ss_res <- sum((actual - predictions)^2)
r_squared <- 1 - ss_res/ss_total

cat("Test RMS=", rmse, "\n")
cat("Test MAE=", mae, "\n")
cat("Test R²=", r_squared, "\n")






