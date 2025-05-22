#Chad Huntebrinker

#Load the library and data in to R
library(readxl)

excel_data <- read_excel("Copier_Maintenance_Data.xlsx")

#Total_Minutes ~ Number_Of_Copiers
#Fit the model
model_3 <- lm(Total_Minutes~Number_Of_Copiers,data=excel_data)
sum_of_model_3 <- summary(model_3)

#Plot to see the graph
plot(Total_Minutes~Number_Of_Copiers, data = excel_data)
abline(model_3,col="blue")

#Get the mean service time for 6 copiers serviced
Y_hat <- predict(model_3, data.frame(Number_Of_Copiers=6))

#Now get the confidence interval
CI <- predict(model_3, se.fit = TRUE, data.frame(Number_Of_Copiers=6), interval = "confidence", level = 0.90)

#Problem 2.14b
#We're getting a 90% prediction interval when 6 copiers are serviced
predict(model_3, data.frame(Number_Of_Copiers=6),interval="prediction",level=0.90)

#The interval 74.46433 <= Yh <= 104.7983 is wider than 
#the confidence interval in part a and we would expect it to be.

#Problem 2.14d
#Getting the boundary values of the 90 percent confidence band of the regression line at Xh = 6
W <- sqrt(2*qf(1-0.1,2,45-2))
lower_bound <- Y_hat - W * CI$se.fit
upper_bound <- Y_hat + W * CI$se.fit

#This confidence band (86.6, 92.7) is wider than the confidence interval in part a and we would expect it to be.