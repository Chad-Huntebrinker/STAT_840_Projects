#Chad Huntebrinker
#Problem 2.6b
#Load the library and data in to R
library(readxl)

excel_data <- read_excel("Airfreight_Breakage_Data.xlsx")

#Number_Broker ~ Number_Transferred
#Fit the model
model_2 <- lm(Number_Broker~Number_Transferred,data=excel_data)
sum_of_model_2 <- summary(model_2)
sum_of_model_2
sum_of_model_2$coefficients[2,4]

#Plot to see the graph
plot(Number_Broker~Number_Transferred, data = excel_data)
abline(model_2,col="blue")


#Calculate the t score
b1 <- sum_of_model_2$coefficients[2, 1]
se_b1 <- sum_of_model_2$coefficients[2, 2]

#a = 0.05
t_1 <- b1 / se_b1

#If the t_1 > than the t score, than we conclude that H1 is true (which is B1 != 0)
t_1 > qt(0.975, 8)

#Calculate the p_value
p_1 <- 2 * (1 - pt(t_1, 8))

#If the p score is less than alpha, than H1 is true (which is B1 != 0)
p_1 < 0.01
