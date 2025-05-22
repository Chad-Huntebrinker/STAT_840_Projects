#Chad Huntebrinker
#Problem 2.14a
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


#Get b1 and se_b1
b1 <- sum_of_model_3$coefficients[2, 1]
se_b1 <- sum_of_model_3$coefficients[2, 2]

#Get the mean service time for 6 copiers serviced
Y_hat <- predict(model_3, data.frame(Number_Of_Copiers=6))

#Now get the confidence interval
predict(model_3, data.frame(Number_Of_Copiers=6), interval = "confidence", level = 0.90)


