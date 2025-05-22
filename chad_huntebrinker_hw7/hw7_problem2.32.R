#Chad Huntebrinker

library(readxl)

excel_data <- read_excel("Crime_Rate_Data.xlsx")

# Fit the model and get the summary of the model
model_4 <- lm(Crime_Rate~Highschool_Diploma_Percentage,data=excel_data)
sum_of_model_4 <- summary(model_4)
sum_of_model_4

# Plot the graph just to see
plot(Crime_Rate~Highschool_Diploma_Percentage,data=excel_data)

#Problem 2.32a
model_4_reduced <- lm(Crime_Rate~1,data=excel_data)
summary(model_4)
summary(model_4_reduced)

#Problem 2.32b
#1)
SSE_F <- deviance(model_4)

#2)
SSE_R <- deviance(model_4_reduced)

#3)
dfF = 82

#4)
dfR = 83

#5)
F_stat <- ((SSE_R - SSE_F) / (dfR - dfF)) / (SSE_F / dfF)

#6)
#H0: B1 = 0: F* <= qf(1 - 0.01, dfR - dfF, dfF)
#Ha: B1 != 0: F* > qf(1 - 0.01, dfR - dfF, dfF)
#Check to see if Ha is true:
F_stat
qf(1 - 0.01, dfR - dfF, dfF)
F_stat > qf(1 - 0.01, dfR - dfF, dfF)
#Conclusion: B1 != 0, there is a linear association

#Problem 2.32c
#t-test for with alpha = 0.01 that B1 != 0
abs(sum_of_model_4$coefficients[2,3]) > qt(1 - 0.01/2, 82)
abs(sum_of_model_4$coefficients[2,3])
sqrt(F_stat)
#Yes, they are