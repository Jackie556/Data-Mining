#Start_here

#LIbrary
library(readr)
library(caret)
library(ggplot2)
library(dplyr)
library(stringr)




#main
data<-read.csv("C:/Users/jacki/OneDrive/Desktop/Data Mining thesis/Data set/Used_Car_Price.csv")
data
is.na(data)
sum(is.na(data))
str(data)
summary(data)
dim(data)

#data_visualizations
# For numerical variables
hist(data$price, main="Distribution of Price", xlab="Price")
hist(data$enginesize, main="Engine Size Of Car",xlab="Engine Size")
table(data$fueltype)
hist(data$fueltype,main = "Fuel Type Of Car",xlab="Fuel Type")
plot(density(data$horsepower), main="Horsepower Density")

# For categorical variables



# Assuming company and car_name columns are present in your data

# Create a column for the first three letters of the car name
data <- data %>% 
  mutate(car_company = str_sub(data$CarName, 1, 3))

write.csv(data, "C:/Users/jacki/OneDrive/Desktop/Data Mining thesis/R-Codes/Data-Mining/preprocessed_dataset.csv", row.names = FALSE)




