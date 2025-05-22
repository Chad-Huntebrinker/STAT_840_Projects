#Chad Huntebrinker
#Problem 2.65
#Load the library and data in to R
library(readxl)

excel_data <- read_excel("SENIC_Data.xlsx")

#First
#For Infection_Risk, plot the Data
plot(Length_of_Stay ~ Infection_Risk, data = excel_data, pch = 16, xlab = "Infection Risk", ylab = "Length of Stay")

#Fit a Linear Regression model
model_7 <- lm(Length_of_Stay ~ Infection_Risk, data = excel_data)

#Find b coefficients
b7_coefficients <- model_7$coefficients

#Add the fitted line
abline(b7_coefficients, lwd =2, lty =2, col="red")

#Second
#For Available_facilities_and_services, plot the Data
plot(Length_of_Stay ~ Available_facilities_and_services, data = excel_data, pch = 16, xlab = "Available facilities and services", ylab = "Length of Stay")

#Fit a Linear Regression model
model_8 <- lm(Length_of_Stay ~ Available_facilities_and_services, data = excel_data)

#Find b coefficients
b8_coefficients <- model_8$coefficients

#Add the fitted line
abline(b8_coefficients, lwd =2, lty =2, col="green")

#Third
#For Routine_Chest_X-ray, plot the Data
plot(Length_of_Stay ~ `Routine_Chest_X-ray`, data = excel_data, pch = 16, xlab = "Routine Chest X-ray", ylab = "Length of Stay")

#Fit a Linear Regression model
model_9 <- lm(Length_of_Stay ~ `Routine_Chest_X-ray`, data = excel_data)

#Find b coefficients
b9_coefficients <- model_9$coefficients

#Add the fitted line
abline(b9_coefficients, lwd =2, lty =2, col="blue")

#Get the summary of each model
sum7 <- summary(model_7)
sum8 <- summary(model_8)
sum9 <- summary(model_9)
sum7$coefficients[2,4]
sum8$coefficients[2,4]
sum9$coefficients[2,4]

#Calculate the t score
b7 <- summary(model_7)$coefficients[2, 1]
se_b7 <- summary(model_7)$coefficients[2, 2]

b8 <- summary(model_8)$coefficients[2, 1]
se_b8 <- summary(model_8)$coefficients[2, 2]

b9 <- summary(model_9)$coefficients[2, 1]
se_b9 <- summary(model_9)$coefficients[2, 2]

#a = 0.05
t_7 <- b7 / se_b7
t_8 <- b8 / se_b8
t_9 <- b9 / se_b9

#Calculate the lower and upper confidence bounds for each variable
b7 - (qt(0.975, 112) * se_b7)
b7 + (qt(0.975, 112) * se_b7)

b8 - (qt(0.975, 112) * se_b8)
b8 + (qt(0.975, 112) * se_b8)

b9 - (qt(0.975, 112) * se_b9)
b9 + (qt(0.975, 112) * se_b9)
