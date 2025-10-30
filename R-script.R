#Start_here

#LIbrary
library(readr)
library(caret)
library(ggplot2)




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
table(data$fuel_type)
barplot(table(data$body_style), main="Body Style Distribution")







