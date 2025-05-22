#Chad Huntebrinker

library(leaps)
library(readxl)

excel_data <- read_excel("Job_Proficiency_Data.xlsx")

#Fit the model
model_1 <- lm(Y~X1 + X3 + X4, data=excel_data)
sum_of_model_1 <- summary(model_1)

#Problem 9.21
#Calculate the SSE and PRESS models
model_1_SSE <- sum(model_1$residuals^2)

model_1_PRESS <- sum((model_1$residuals / (1 - hatvalues(model_1)))^2)

model_1_PRESS
model_1_SSE
#Since PRESS is larger than the SSE, this suggest that the model might be overfitted and MSE is
#not a reliable indicator.

#Problem 9.22a
excel_data2 <- read_excel("Job_Proficiency_Data2.xlsx")

pairs(excel_data)
cor(excel_data[,-1])

pairs(excel_data2)
cor(excel_data2[,-1])

#Yes, they are reasonably similiar

#Problem 9.22b
model_2 <- lm(Y~X1 + X3 + X4, data=excel_data2)
#Validation Model
summary(model_2)
summary(model_2)$coefficients[, "Std. Error"]
sum(model_2$residuals^2)
anova(model_2)["Residuals", "Mean Sq"]
#Y = 122.76705 + 0.31238X1 + 1.40676X3 + 0.42838X4

#Regular Model
sum_of_model_1
sum_of_model_1$coefficients[, "Std. Error"]
sum(model_1$residuals^2)
anova(model_1)["Residuals", "Mean Sq"]
#Y = 124.20002 + 0.29633X1 + 1.35697X3 + 0.51742X4

#Problem 9.22c
model_2_prediction <- predict(model_2)
model_2_MSPE <- mean((excel_data2$Y - model_2_prediction)^2)
model_2_MSPE
model_1_MSE <- mean((excel_data$Y - predict(model_1))^2)
model_1_MSE
#MSE is lower than MSPE but not significantly; thus, there isn't much
#evidence of bias.

#Problem 9.22d
full_data <- rbind(excel_data, excel_data2)
model_3 <- lm(Y~X1 + X3 + X4, data=full_data)
summary(model_3)
summary(model_1)

#The intercept standard deviations are reduced, but not the other ones; in fact, they increased.